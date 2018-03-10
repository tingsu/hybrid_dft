open Cil

module RD = MyReachingDefs
module UD = Usedef
module E = Errormsg

let get_lrhs instruction =  
    match instruction.Instruction.stmt.skind with
    | Cil.If (exp, _, _, _) -> None, Some exp
    | Cil.Instr _ -> 
        begin match instruction.Instruction.instrs with
            | Cil.Set (lval, exp, _) :: _ -> Some lval, Some exp
            | _ -> None, None
        end
    | _ -> None, None

(**
 *  TODO:
 *  Here, a definition is an assignment instruction. rhs is the right-hand-side of the definition, 
 *  which must be a Cil.exp.
 *
 *  Given a definition, compute a mapping from values to sets of lists of definitions, 
 *  each of which must lead to the value.
 *
 *  Algorithm: given a definition, 
 *  1. Find the varinfos involved in the rhs.
 *  2. For each varinfo, find its reaching definitions.
 *  3. For each reaching definition, recursively find the mapping.
 *  4. Rearrange data found in (2) and (3), to get a mapping (for each varinfo) which maps possible values to the list(s) of definitions,
 *  5. For each combination of values that varinfos can take (i.e., for each element in the Cartesian product {values of varinfo x} X {values of varinfo y} X ...), evaluate the rhs with the combination, and merge all the associated sets of reaching definitions to be the final one.
 *)
let find instruction =
    let varinfo_map = RD.getRDs instruction in
    let lhs, rhs = get_lrhs instruction in
    match rhs with
    | Some (Lval(Var(varinfo),NoOffset)) ->
        let (instruction_set, _) = RD.VarinfoMap.find varinfo varinfo_map in
        begin try Some (RD.InstructionSet.choose instruction_set) with Not_found -> None end
    | _ -> None

(** data affection 
    which stmt (an intermediate goal) affects which var ?
*)
type data_affection = {
	
	mutable data_affection_stmt: Cil.stmt ;
	mutable data_affection_stmt_line: int;
	
	mutable data_affection_var_id: int;
	mutable data_affection_var_name: string;
	
}

(** another version of finding intermediate goals *)
let find_direct_reaching_definition instruction = 
	
	let debug = false in

    let varinfo_map = RD.getRDs instruction in
    let lhs, rhs = get_lrhs instruction in
    match rhs with
    | Some (rhs_exp) -> 
		let data_affection_list = ref ([]:data_affection list) in (* a data affection list *)
	 	(* compute use vars *)
		let useVS = UD.computeUseExp rhs_exp in
    	UD.VS.iter (* iterate all use vars *)
	  		begin fun uv ->
	  			if debug = true then begin
					E.log "use var name : %s\n" uv.vname				
				end;
				(* get the varinfo_map set corresponding to uv *) 
				let data_affection_instrs = 
					(try Some(RD.VarinfoMap.find uv varinfo_map) 
					with 
					| Not_found -> None
					) (* varinfo_map may be empty, we have fixed it !! *)
				in
				
				match data_affection_instrs with
				| Some (instruction_set,_) -> 
				   if debug = true then begin
   	   			   	E.log "has interg\n"
   	   			   end;
				   RD.InstructionSet.iter (* iterate all intermediate goals stmts *)
					  begin fun instruction ->
						let st = instruction.Instruction.stmt in
						(* get the head instruction of CIL Instruction, refer to "Instruction.ml" for detail. *)
						let head_ins = List.hd instruction.Instruction.instrs in
						let item = {
								data_affection_var_id = uv.vid;
								data_affection_var_name = uv.vname;
								data_affection_stmt = st;
								data_affection_stmt_line = (Cil.get_instrLoc head_ins).line
							}
						in
						data_affection_list := !data_affection_list @[item]
					  end
				   instruction_set
				| None -> 
					if debug = true then begin
						E.log "not has interg\n"
					end;
					()
	  		end
	 	useVS;
	   Some(!data_affection_list)
    | None -> None
