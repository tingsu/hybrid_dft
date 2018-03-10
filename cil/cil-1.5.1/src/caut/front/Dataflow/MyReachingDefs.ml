(** A wrapper for Cil's crappy Reachingdefs interface *)
open Cil

module VarinfoMap = Map.Make(CilData.CilVar)
module InstructionSet = Set.Make(Instruction)

let vidVarinfoHash = Hashtbl.create 0 

(** Prepare the vidVarinfoHash for the file that contains the instruction. *)
let prepareVarinfoHash = 
    let module Computed = Hashtbl.Make(CilData.CilFile) in
    let computed = Computed.create 0 in
    fun instruction -> begin
        let file = instruction.Instruction.file in
        if not (Computed.mem computed file) then
            let _ = Computed.add computed file () in
            Cil.visitCilFileSameGlobals begin object
                inherit Cil.nopCilVisitor
                method vvdec varinfo = (* add vid-varinfo into vidVarinfoHash *)
                    Hashtbl.add vidVarinfoHash varinfo.vid varinfo;
                    Cil.SkipChildren
            end end file
    end

(** Run Reachingdefs.computeRDs on fundec that contains the instruction, if it hasn't been computed yet. *)
let computeRDs =
    let module Computed = Hashtbl.Make(CilData.CilFundec) in
    let computed = Computed.create 0 in
    fun instruction -> begin
        let fundec = instruction.Instruction.fundec in
        (** We comment this conditional checking to force RD computing *)
        (* if not (Computed.mem computed fundec) then *)
            let _ = Computed.add computed fundec () in
            (* Assumed that fundec is prepared by Core *)
            Reachingdefs.computeRDs fundec
    end
        
(** Return a mapping from varinfos to sets of instructions and a boolean. 
    If the set contains i, then instruction i that defines the varinfo reaches the input instruction. 
    If the boolean is true, then there is a path to the input instruction which there is no definition of that varinfo. 
    Also, if a varinfo is unmapped, then no definition of that varinfo reaches the input instruction.
*)
let getRDs instruction = 
    computeRDs instruction;
    prepareVarinfoHash instruction;
    let get_triple instruction = 
        let stmt = instruction.Instruction.stmt in
        match Reachingdefs.getRDs stmt.sid with
        | Some triple -> 
            begin match stmt.skind with
            | Instr instrs -> 
                let triples = Reachingdefs.instrRDs instrs stmt.sid triple false in
                let cur_instr = List.hd instruction.Instruction.instrs in
                let rec find_triple = function
                    | instr :: instrs, triple :: triples -> if instr == cur_instr then Some triple else find_triple (instrs, triples)
                    | [], [] -> None
                    | _ -> failwith "MyreachingDefs.getRDs: instrs and triples are of different lengths"
                in
                find_triple (instrs, triples)
            | _ -> Some triple
            end
        | None -> None
    in
    (* create varinfo_map *)
    let varinfo_map = ref VarinfoMap.empty in
    let _ = match get_triple instruction with
    | Some (_,_,vidmap) -> 
        Reachingdefs.IH.iter (
            fun vid set ->
                let varinfo = Hashtbl.find vidVarinfoHash vid in
                let instruction_set = ref InstructionSet.empty in
                let has_undef_path = ref false in
                Reachingdefs.IOS.iter (
                    function
                    | Some defId -> 
                        begin match Reachingdefs.getDefIdStmt defId with
                        | None -> ()
                        | Some stmt ->
                            match stmt.skind with
                            | Instr instrs -> 
                                (* Find the shortest tail of instrs whose head defines varinfo *)
                                let rec find = function
                                    | [] -> None
                                    | instr :: instrs -> match find instrs with
                                        | Some instrs -> Some instrs
                                        | None -> match instr with
                                            | Set((Var(varinfo'),NoOffset), _, _) when CilData.CilVar.equal varinfo varinfo' -> Some (instr :: instrs)
                                            | _ -> None
                                in
                                begin match find instrs with
                                | Some instrs -> 
                                    let reaching_instruction = Instruction.of_stmt_first instruction.Instruction.file instruction.Instruction.fundec stmt in
                                    let reaching_instruction = Instruction.with_instrs reaching_instruction instrs in
                                    instruction_set := InstructionSet.add reaching_instruction (!instruction_set)
                                | None -> ()
                                end
                            | _ -> failwith "MyreachingDefs.getRDs: a definition must be an instruction" 
                        end
                    | None -> has_undef_path := true
                )set;
       		(* add elements into varinfo_map *)
                varinfo_map := VarinfoMap.add varinfo (!instruction_set, !has_undef_path) (!varinfo_map)
        ) vidmap
    | None -> ()
    in 
    !varinfo_map
