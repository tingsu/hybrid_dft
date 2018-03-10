(** Instruction-level control-flow graph abstraction for Otter. *)


(* TODO: Refactor OtterCore to use this module for it's program counter, as the one canonical abstraction for Otter.
   In particular, parts of OtterCore.Core.prepare_file should be moved here, as this module relies on the invariants
   set up there. *)

(* Use a private type to disallow direct construction of the type beyond the module below, so that the instruction must
   be constructed via one of the smart constructors which strictly enforce the invariants of this type.
   (see <http://caml.inria.fr/pub/docs/manual-ocaml/manual021.html#toc76>). *)
module T : sig
    type t = private {
        file : Cil.file;
        fundec : Cil.fundec;
        stmt : Cil.stmt;
        instrs : Cil.instr list;
    }
    val make : Cil.file -> Cil.fundec -> Cil.stmt -> Cil.instr list -> t
    val of_stmt_first : Cil.file -> Cil.fundec -> Cil.stmt -> t
    val of_stmt_last : Cil.file -> Cil.fundec -> Cil.stmt -> t
    val of_fundec : Cil.file -> Cil.fundec -> t
    val of_instr : Cil.file -> Cil.fundec -> Cil.stmt -> Cil.instr -> t
    val with_instrs : t -> Cil.instr list -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
end = struct
    (* An Otter instruction is an optional instruction in a statement in a function in a file; in particular, Cil.Instr
       statements are stored with a list of the current and remaining instructions, whereas other statements are
       stored with an empty instruction list. Note also that the instruction list is compared only by length, since
       some Otter functions mutate the current instruction (e.g.,
       OtterCore.Interceptors.intercept_function_by_name_* ). *)
    type t = {
        file : Cil.file;
        fundec : Cil.fundec;
        stmt : Cil.stmt;
        instrs : Cil.instr list;
    }

    (** Make an instruction. *)
    let make file fundec stmt instrs = match stmt.Cil.skind with
        | Cil.Instr instrs' when List.length instrs <= List.length instrs' -> { file = file; fundec = fundec; stmt = stmt; instrs = instrs }
        | Cil.Instr _ -> invalid_arg "Instruction.make: instrs must be equal or shorter in length to the Cil.Instr in stmt"
        | _ when instrs = [] -> { file = file; fundec = fundec; stmt = stmt; instrs = instrs }
        | _ -> invalid_arg "Instruction.make: instrs must be empty when stmt is not Cil.Instr"

    (** Make an instruction from a {!Cil.stmt} only, taking the first instruction if it is a {!Cil.Instr}. *)
    let of_stmt_first file fundec stmt = match stmt.Cil.skind with
        | Cil.Instr instrs -> { file = file; fundec = fundec; stmt = stmt; instrs = instrs } (*su: a bug?*)
        | _ -> { file = file; fundec = fundec; stmt = stmt; instrs = [] }

    (** Make an instruction from a {!Cil.stmt} only, taking the last instruction if it is a {!Cil.Instr}. *)
    let of_stmt_last file fundec stmt = match stmt.Cil.skind with
        | Cil.Instr [] -> { file = file; fundec = fundec; stmt = stmt; instrs = [] }
        | Cil.Instr instrs -> { file = file; fundec = fundec; stmt = stmt; instrs = [ List.hd (List.rev instrs) ] }
        | _ -> { file = file; fundec = fundec; stmt = stmt; instrs = [] }

    (** Make an instruction from a {!Cil.fundec} only, taking the first statement. *)
    let of_fundec file fundec = of_stmt_first file fundec (List.hd fundec.Cil.sbody.Cil.bstmts)

    (** Make an instruction by updating {!instr} in an instruction. *)
    let with_instrs instruction instrs = match instruction.stmt.Cil.skind with
        | Cil.Instr instrs' when List.length instrs <= List.length instrs' -> { instruction with instrs = instrs } (*su: a bug?*)
        | Cil.Instr _ -> invalid_arg "Instruction.with_instrs: instrs must be equal or shorter in length to the Cil.Instr in instruction.stmt"
        | _ -> invalid_arg "Instruction.with_instrs: instruction.stmt must be Cil.Instr"

    (** Make an instruction from a {!Cil.instr} only. *)
    let of_instr file fundec stmt instr = 
        match stmt.Cil.skind with
        | Cil.Instr instrs -> 
            let rec suffix = function [] -> [] | instr'::instrs -> if instr == instr' then instr'::instrs else suffix instrs in
            let instruction = of_stmt_first file fundec stmt in
            with_instrs instruction (suffix instrs)
        | _ -> invalid_arg "Instruction.with_instrs: instruction.stmt must be Cil.Instr"

    (** Compare two instructions. *)
    let compare x y = if x == y then 0 else
        match CilData.CilFile.compare x.file y.file with
            | 0 ->
                begin match CilData.CilFundec.compare x.fundec y.fundec with
                    | 0 ->
                        (* Note that this relies on Cil.computeCFGInfo, called in OtterCore.Core.prepare_file. *)
                        begin match Pervasives.compare x.stmt.Cil.sid y.stmt.Cil.sid with
                            | 0 ->
                                (* Instructions are compared by length only, since Otter may mutate the instruction. *)
                                if x.instrs == y.instrs then
                                    0
                                else
                                    Pervasives.compare (List.length x.instrs) (List.length y.instrs)
                            | i ->
                                i
                        end
                    | i ->
                        i
                end
            | i ->
                i

    (** Compare two instructions for equality. *)
    let equal x y = compare x y = 0

    (** Compute a hash for an instruction. *)
    let hash { file = file; fundec = fundec; stmt = stmt; instrs = instrs } =
        Hashtbl.hash (CilData.CilFile.hash file, CilData.CilFundec.hash fundec, CilData.CilStmt.hash stmt, List.length instrs)
end

include T

(** Return the location *)
let location  { stmt = stmt; instrs = instrs } =
    match instrs with
    | [] -> Cil.get_stmtLoc stmt.Cil.skind
    | instr :: _ -> Cil.get_instrLoc instr


(** Print an instruction. *)
let printer ff ({ stmt = stmt; instrs = instrs } as i) =
    let loc = location i in
    match stmt.Cil.skind with
        | Cil.Instr _ -> Format.fprintf ff "%a:%a:%d" Printcil.loc loc Printcil.n_stmt stmt (List.length instrs)
        | _ -> Format.fprintf ff "%a:%a" Printcil.loc loc Printcil.n_stmt stmt


(** Find the successors for an instruction. *)
let successors ({ file = file; fundec = fundec; stmt = stmt; instrs = instrs } as instruction) = match instrs with
    | _::[] | [] ->
        (* last Cil.instr in a Cil.Instr, an empty Cil.Instr, or a non-Cil.Instr *)
        List.map (of_stmt_first file fundec) stmt.Cil.succs
    | _::rest ->
        (* the remaining Cil.instr in a Cil.Instr *)
        [ with_instrs instruction rest ]


(** Find the predecessors for an instruction. *)
let predecessors ({ file = file; fundec = fundec; stmt = stmt; instrs = instrs } as instruction) = match stmt.Cil.skind with
    | Cil.Instr instrs' when (List.length instrs) = (List.length instrs') ->
        (* first Cil.instr in a Cil.Instr, an empty Cil.Instr, or non-Cil.Instr *)
        List.map (of_stmt_last file fundec) stmt.Cil.preds
    | Cil.Instr instrs' ->
        (* some instruction in the middle of a Cil.Instr *)
        [ with_instrs instruction ((List.nth (List.rev instrs') (List.length instrs))::instrs) ]
    | _ ->
        (* non-Cil.Instr *)
        List.map (of_stmt_last file fundec) stmt.Cil.preds


(** Find the enclosing function for an instruction. *)
let fundec_of ({ file = file; fundec = fundec; _ }) =
    of_fundec file fundec


(** Find all the return statements of this instruction (if it is the first instruction of its function). *)
let return_of =
    let module Memo = Memo.Make (T) in
    let return_of = Memo.memo "Instruction.return_of" begin fun { file = file; fundec = fundec; _ } ->
        let return_of = ref [] in

        (* extract Cil.Return of this function *)
        ignore begin Cil.visitCilFunction begin object
            inherit Cil.nopCilVisitor
            method vstmt stmt = match stmt.Cil.skind with
                | Cil.Return _ ->
                    return_of := (of_stmt_first file fundec stmt)::!return_of;
                    Cil.SkipChildren
                | _ ->
                    Cil.DoChildren
        end end fundec end;

        !return_of
    end in
    fun instruction ->
        if equal instruction (fundec_of instruction) then
            return_of instruction
        else
            [] (* or raise some exception? *)


(** Find all the call sites of this instruction (if it is the first instruction of its function). *)
let call_sites =
    let module Memo = Memo.Make (struct
        type t = CilData.CilFile.t * CilData.CilFundec.t
        let hash (file, fundec) = Hashtbl.hash (CilData.CilFile.hash file, CilData.CilFundec.hash fundec)
        let equal (xfile, xfundec) (yfile, yfundec) = CilData.CilFile.equal xfile yfile && CilData.CilFundec.equal xfundec yfundec
    end) in
    let call_sites =
        Memo.memo "Instruction.call_sites"
            begin fun (file, fundec) ->
                let call_sites = ref [] in

                (* iterate over caller functions *)
                let callers = CilCallgraph.find_callers file fundec in
                List.iter begin fun caller ->
                    (* iterate over statements in caller functions *)
                    ignore begin Cil.visitCilFunction begin object
                        inherit Cil.nopCilVisitor
                        method vstmt stmt = match stmt.Cil.skind with
                            | Cil.Instr instrs ->
                                (* iterate over instructions, which contains Cil.Call *)
                                let rec traverse = function
                                    | (Cil.Call (_, fexp, _, _))::rest as instrs ->
                                        (* find Cil.Call that calls the target *)
                                        let call_targets = CilCallgraph.resolve_exp_to_fundecs file fexp in
                                        if List.memq fundec call_targets then
                                            call_sites := (make file caller stmt instrs)::!call_sites;
                                        traverse rest
                                    | _::rest ->
                                        traverse rest
                                    | [] ->
                                        Cil.SkipChildren
                                in
                                traverse instrs
                            | _ ->
                                Cil.DoChildren
                    end end caller end
                end callers;

                !call_sites
            end
    in
    fun ({ file = file; fundec = fundec } as instruction) ->
        if equal instruction (of_fundec file fundec) then
            call_sites (file, fundec)
        else
            [] (* or raise some exception? *)


(** Find all the (first instruction of the) call targets of this instruction (if it is a call instruction). *)
let call_targets =
    let module Memo = Memo.Make (struct
        type t = CilData.CilFile.t * CilData.CilExp.t
        let hash (f, e) = Hashtbl.hash (CilData.CilFile.hash f, CilData.CilExp.hash e)
        let equal (xf, xe) (yf, ye) = CilData.CilFile.equal xf yf && CilData.CilExp.equal xe ye
    end) in
    let call_targets = Memo.memo "Instruction.call_target" begin fun (file, fexp) ->
        (* iterate over callee functions and extract the first statement *)
        List.map (of_fundec file) (CilCallgraph.resolve_exp_to_fundecs file fexp)
    end in
    fun { file = file; instrs = instrs } -> match instrs with
        | (Cil.Call (_, fexp, _, _))::_ ->
            call_targets (file, fexp)
        | _ ->
            [] (* or raise some exception? *)


(** Find all the return targets of this instruction (if it is a return instruction). *)
let return_targets ({ file = file; fundec = fundec } as instruction) =
    match successors instruction with
        | [] -> call_sites (of_fundec file fundec)
        | _ -> [] (* or raise some exception? *)


(** Find all the return sites of this instruction (if it is a call instruction). *)
let return_sites =
    let module Memo = Memo.Make (T) in
    let return_sites = Memo.memo "Instruction.return_sites" begin fun instruction ->
        List.concat (List.map return_of (call_targets instruction))
    end in
    fun ({ file = file; instrs = instrs } as instruction) -> match instrs with
        | (Cil.Call _)::_ ->
            return_sites instruction
        | _ ->
            [] (* or raise some exception? *)


(** Find a list of instructions by file name and line number from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Instruction.t} in
        @param line the line to find the instructions, as a [(filename, line)] pair
        @return the list of instructions as {!Instruction.t list}
        @raise Not_found if no instructions can be found at [line] in [file]
*)
let by_line file line =
    try
        let instrs = FindCil.instrs_by_line file line in
        List.fold_left (fun instructions (fundec, stmt, instr) ->
            (of_instr file fundec stmt instr) :: instructions
        ) [] instrs
    with Not_found ->
        let stmts = FindCil.stmts_by_line file line in
        List.fold_left (fun instructions (fundec, stmt) ->
            (of_stmt_last file fundec stmt ) :: instructions
        ) [] stmts

