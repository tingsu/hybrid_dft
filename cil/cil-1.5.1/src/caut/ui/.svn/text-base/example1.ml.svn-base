open Cil
open Pretty
open Cfg
module E = Errormsg

class linestmtVisitor = object(self)
  inherit nopCilVisitor

	method vstmt st = 
		(* line(line_no) *)
		let line_fun = emptyFunction "line" in 
		match st.skind with
		| If(e,b_t,b_f,loc) ->
			let instr = Call(None,Lval((Var(line_fun.svar)),NoOffset),[integer loc.line],!currentLoc) in
			self#queueInstr [instr] ;
			DoChildren
		
		| _ -> DoChildren

end

class ifstmtVisitor = object(self)
  inherit nopCilVisitor

	method vstmt st = 
		(* cover(branch_id,branch_choice) *)
		let cover_fun = emptyFunction "cover" in 
		(* default 0 *)
		match st.skind with
		|If (e,b1,b2,loc) ->
				
				let then_instr = Call(None,Lval((Var(cover_fun.svar)),NoOffset),[integer st.sid;integer 1],!currentLoc) in
				let then_stmt = mkStmtOneInstr then_instr in
				let else_instr = Call(None,Lval((Var(cover_fun.svar)),NoOffset),[integer st.sid;integer 0],!currentLoc) in
				let else_stmt = mkStmtOneInstr else_instr in				
				b1.bstmts <- [then_stmt] @ b1.bstmts;
				b2.bstmts <- [else_stmt] @ b2.bstmts;
				DoChildren
		| _ ->
				DoChildren
end



let process (f:file) =	
	computeFileCFG f; 
	visitCilFile (new linestmtVisitor) f 
	(*visitCilFile (new ifstmtVisitor) f *)
	


let feature : featureDescr = 
{
	fd_name = "test";
	fd_enabled = ref false;
	fd_description = "insert coverage function";

	fd_extraopt = [
		      ];
	fd_doit =  (function (f: file) -> 
			process f);
	fd_post_check = true
}
