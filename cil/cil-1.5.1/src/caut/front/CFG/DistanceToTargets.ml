(** Find distances between instructions. *)


(**/**) (* various helpers *)
module InstructionSet = Set.Make (Instruction)
module TargetSet = struct
    include InstructionSet

    exception Limit
    let limit = 20
    let hash x =
        let count = ref 0 in
        let hash = ref 0 in
        try
            InstructionSet.iter begin fun instr ->
                if !count >= limit then raise Limit;
                hash := Hashtbl.hash (Instruction.hash instr, hash);
                incr count
            end x;
            !hash
        with Limit ->
            !hash

    let of_list = List.fold_left (fun targets target -> InstructionSet.add target targets) InstructionSet.empty
end
module TargetsHash = Hashtbl.Make (struct
    type t = bool * TargetSet.t
    let equal (xa, xb) (ya, yb) = xa == ya && TargetSet.equal xb yb
    let hash (xa, xb) = Hashtbl.hash (xa, TargetSet.hash xb)
end)
module InstructionHash = Hashtbl.Make (Instruction)
(**/**)


(** Find the shortest distance from an {!Instruction.t} to a list of target {!Instruction.t}s.

    Distances are calculated by counting instructions along a path up to a function return within the function of
    the source instruction. If a function call occurs along a path, the minimum of the distance from the first
    instruction to the targets in the call targets, or the shortest distance through the call targets, is added.

    @return the shortest distance to one of the targets, or {!max_int} if none of the targets are reachable.
*)
let find = (* val find : InstructionHash.key ->
       ?interprocedural:bool -> Instruction.t list -> int *)
    let targets_hash = TargetsHash.create 0 in
    fun instr ?(interprocedural=true) targets ->
        if targets = [] then invalid_arg "DistanceToTargets.find: targets must be a non-empty list";
        let targets = TargetSet.of_list targets in

        let distance_hash = try
            TargetsHash.find targets_hash (interprocedural, targets)

        with Not_found -> Profiler.global#call "DistanceToTargets.find (uncached)" begin fun () ->
            let distance_hash = InstructionHash.create 0 in
            let rec update worklist =
                let worklist = Profiler.global#call "DistanceToTargets.update" begin fun () ->
                    (* pick the an instruction from the worklist *)
                    let instr = InstructionSet.max_elt worklist in
                    let worklist = InstructionSet.remove instr worklist in

                    (* compute the new distance by taking the minimum of:
                            - 0 if the instruction is a target
                            - or, 1 + the minimum distance of its successors + the minimum distance through its call targets
                            - or, 1 + the minimum distance of its call targets
                    *)
                    let dist =
                        if InstructionSet.mem instr targets then
                            0
                        else
                            let calc_dist instrs =
                                List.fold_left begin fun dist instr ->
                                    try min dist (InstructionHash.find distance_hash instr) with Not_found -> dist
                                end max_int instrs
                            in

                            let dist = calc_dist (Instruction.successors instr) in
                            let dist = match Instruction.call_targets instr with
                                | [] ->
                                    (* no call targets, just successors *)
                                    dist
                                | call_targets ->
                                    (* compute the distance through call targets, interprocedurally to targets in calles, and successors *)
                                    let through_dist =
                                        let through_dist = dist + List.fold_left (fun d call_target -> min d (DistanceToReturn.find call_target)) max_int call_targets in
                                        if through_dist < 0 then max_int (* overflow *) else through_dist
                                    in
                                    if interprocedural then
                                        (* compute the distances of targets in call targets interprocedurally *)
                                        let call_target_dist = calc_dist call_targets in
                                        (* take the minimum of the above *)
                                        min through_dist call_target_dist
                                    else
                                        (* intraprocedural *)
                                        through_dist
                            in
                            let dist =
                                let dist = 1 + dist in
                                if dist < 0 then max_int (* overflow *) else dist
                            in
                            dist
                    in

                    (* update the distance if changed *)
                    let updated =
                        try
                            let dist' = InstructionHash.find distance_hash instr in
                            if dist < dist' then InstructionHash.replace distance_hash instr dist;
                            dist < dist'
                        with Not_found ->
                            InstructionHash.add distance_hash instr dist;
                            dist < max_int
                    in

                    (* if updated, add this instruction's predecessors and call sites to the worklist *)
                    if updated then
                        List.fold_left (fun worklist instr -> InstructionSet.add instr worklist) worklist
                            (List.rev_append (Instruction.predecessors instr) (Instruction.call_sites instr))
                    else
                        worklist
                end in

                (* recurse on the remainder of the worklist *)
                if not (InstructionSet.is_empty worklist) then update worklist
            in

            (* start from the targets *)
            update targets;
            TargetsHash.add targets_hash (interprocedural, targets) distance_hash;
            distance_hash
        end in

        try
            InstructionHash.find distance_hash instr
        with Not_found ->
            max_int


(** Find the shortest distance from an {!Instruction.t} to a list of target {!Instruction.t}s through function returns
    in a calling context given as a list of {!Instruction.t} successors of function calls.

    Distances through function returns are computed by recursively unwinding the calling context, taking the
    shortest distance from the source instruction through function returns down the calling context to targets in the
    calling context.

    @return the shortest distance to one of the targets, or {!max_int} if none of the targets are reachable.
*)
let find_in_context = (* val find_in_context : Instruction.t ->
       Instruction.t list -> ?interprocedural:bool -> Instruction.t list -> int *)
    let module Memo = Memo.Make (struct
        module InstructionList = ListPlus.MakeHashedList (Instruction)
        type t = bool * InstructionList.t * InstructionList.t
        let equal (xi, xc, xt) (yi, yc, yt) = xi == yi && InstructionList.equal xc yc && InstructionList.equal xt yt
        let hash (xi, xc, xt) = Hashtbl.hash (xi, InstructionList.hash xc, InstructionList.hash xt)
    end) in
    let unwind = Memo.memo_rec "DistanceToTargets.unwind" begin fun unwind (interprocedural, context, targets) ->
        Profiler.global#call "DistanceToTargets.unwind (uncached)" begin fun () ->
            (* compute the distance from the instr through function returns to uncovered in the call context *)
            match context with
                | instr::rest ->
                    let dist = find instr ~interprocedural targets in
                    let return_dist =
                        let return_dist = 1 + DistanceToReturn.find instr + unwind (interprocedural, rest, targets) in
                        if return_dist < 0 then max_int (* overflow *) else return_dist
                    in
                    min dist return_dist
                | [] ->
                    max_int
        end
    end in
    fun instr context ?(interprocedural=true) targets ->
        unwind (interprocedural, instr::context, targets)

