(** Find distances from instructions to function returns. *)


(**/**) (* various helpers *)
module InstructionSet = Set.Make (Instruction)
module InstructionHash = Hashtbl.Make (Instruction)
(**/**)


(** Find the shortest distance from an {!Instruction.t} to a function return.

    Distances are calculated by counting instructions along a path to a function return within the function of
    the source instruction. If a function call occurs along a path, the shortest distance through the call targets is
    added.

    @return the shortest distance to a function return, or {!max_int} if no function returns are reachable.
*)
let find = (* val find : Instruction.t -> int *)
    let distance_hash = InstructionHash.create 0 in
    fun instr ->
        let fundec = Instruction.fundec_of instr in

        if not (InstructionHash.mem distance_hash fundec) then Profiler.global#call "DistanceToReturn.find (uncached)" begin fun () ->
            let return_of fundec =
                List.fold_left begin fun return_of return ->
                    InstructionSet.add return return_of
                end InstructionSet.empty (Instruction.return_of fundec)
            in
            let rec update worklist = if not (InstructionSet.is_empty worklist) then
                let worklist = Profiler.global#call "update" begin fun () ->
                    (* pick the instruction from the worklist closest to the end of function *)
                    let instr = InstructionSet.max_elt worklist in
                    let worklist = InstructionSet.remove instr worklist in

                    (* compute the new distance by taking the minimum of:
                            - 0 if the instruction is a return (has no successors);
                            - or, 1 + the minimum distance of its successors + the minimum distance through its call targets;
                       adding uncomputed call targets to the worklist *)
                    let dist, worklist =
                        if Instruction.successors instr = [] then
                            (0, worklist)
                        else
                            (* compute the distance through successors *)
                            let dist = List.fold_left begin fun dist instr ->
                                try min dist (InstructionHash.find distance_hash instr) with Not_found -> dist
                            end max_int (Instruction.successors instr) in

                            let dist, worklist = match Instruction.call_targets instr with
                                | [] ->
                                    (* no call targets, just successors *)
                                    (dist, worklist)
                                | call_targets ->
                                    (* compute the distance through call targets; add call targets to worklist if not computed *)
                                    let through_dist, worklist = List.fold_left begin fun (through_dist, worklist) call_target ->
                                        (* tag the start of the function so that it won't be recomputed unnecessarily *)
                                        if not (InstructionHash.mem distance_hash call_target) then begin
                                            InstructionHash.add distance_hash call_target max_int;
                                            (through_dist, InstructionSet.fold InstructionSet.add (return_of call_target) worklist)
                                        end else
                                            (min through_dist (InstructionHash.find distance_hash call_target), worklist)
                                    end (max_int, worklist) call_targets in
                                    let dist =
                                        let dist = dist + through_dist in
                                        if dist < 0 then max_int (* overflow *) else dist
                                    in
                                    (dist, worklist)
                            in
                            let dist =
                                let dist = 1 + dist in
                                if dist < 0 then max_int (* overflow *) else dist
                            in
                            (dist, worklist)
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

                    (* if updated, add this instruction's predecessors and call sites to the worklist. *)
                    if updated then
                        List.fold_left (fun worklist instr -> InstructionSet.add instr worklist) worklist
                            (List.rev_append (Instruction.predecessors instr) (Instruction.call_sites instr))
                    else
                        worklist
                end in

                (* recurse on the remainder of the worklist *)
                update worklist
            in

            (* first, tag the start of the function so that it won't be recomputed unnecessarily *)
            if not (InstructionHash.mem distance_hash fundec) then
                InstructionHash.add distance_hash fundec max_int;

            (* start from return sites of this fundec *)
            update (return_of fundec);
        end;

        try
            InstructionHash.find distance_hash instr
        with Not_found ->
            max_int

