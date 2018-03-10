(* cautcover.ml 
   1. insert the "bc_cover_fun(...)" for branch coverage
   2. extract CFG
*)

open Cil
open String
open Cfg
module E = Errormsg

let file_name = ref ""
let branch_coverage = ref 0

class ifstmtVisitor = object(self)
  inherit nopCilVisitor

	method vstmt st = 
		(* bc_cover_fun(branch_id,branch_choice,func_cnt) *)
		let cover_fun = emptyFunction "bc_cover_fun" in 
		(* default 0 *)
		let func_cnt = 0 in
		match st.skind with
		|If (e,b1,b2,loc) ->
				
				let then_instr = Call(None,Lval((Var(cover_fun.svar)),NoOffset),[integer st.sid;integer 1;integer func_cnt],!currentLoc) in
				let then_stmt = mkStmtOneInstr then_instr in
				let else_instr = Call(None,Lval((Var(cover_fun.svar)),NoOffset),[integer st.sid;integer 0;integer func_cnt],!currentLoc) in
				let else_stmt = mkStmtOneInstr else_instr in				
				b1.bstmts <- [then_stmt] @ b1.bstmts;
				b2.bstmts <- [else_stmt] @ b2.bstmts;
				DoChildren
		| _ ->
				DoChildren
end

let insertCoverFunction (f:file) =
	visitCilFile (new ifstmtVisitor) f


let domain (f:file) =
	
	(* compute CFG *)
	(* computeFileCFG f; 
	Replaced with --domakeCFG to convert "break"/"case"/"switch"/"default" to "if"/"goto" *)

	let dump = true in

	(* set db name as filename *)
	let db_name = Buffer.create 20 in
	Buffer.add_string db_name !file_name;
	Buffer.add_string db_name ".db";
	if dump = true then
		ignore (E.log "create db : %s\n" (Buffer.contents db_name));

	(* create CFG *)
	ignore(CautcreateCFG.createCFG f !file_name (Buffer.contents db_name));
	if dump = true then
		ignore (E.log "create CFG\n");

	if !branch_coverage == 1 then begin
		(* insert cover function *)
		insertCoverFunction f;
		if dump = true then
			ignore (E.log "insert cover function\n")
	end
	

let feature : featureDescr = 
{
	fd_name = "cautcover";
	fd_enabled = ref false;
	fd_description = "insert coverage function";

	fd_extraopt = [
			("-file", Arg.Set_string file_name, " set the file name");
			("-bc",Arg.Set_int branch_coverage, " do branch coverage");
		      ];
	fd_doit =  (function (f: file) -> 
			domain f);
	fd_post_check = true
}
