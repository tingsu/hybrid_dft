(** cautexco.ml
	Author: Jiang Siyuan
			Stanley
	Date:4/25/2010
	Description: extract the condition expression
*)

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

(* create this class to perform simplifing the if statement.*)
class conditionExprVisitor = object(self)
inherit nopCilVisitor
	val mutable func_list = ([] : fundec list)

	(** Change the if statement *)
	method private changeIf (if_st : stmt) : stmt =
		match if_st.skind with
			|If(e, b1, b2, l) -> 
				(match e with (**Find whether the condition is an expersion or just a variable*)
					| BinOp(bp, e1, e2, t) -> begin (**the condition is an expersion.*)
						(**Make a new assignment statement of the form flag = expr.*)
						let cond = Cil.makeTempVar (List.hd func_list) t in
						let newCondExpr = [Set(var cond, e, !currentLoc)] in
						self#queueInstr newCondExpr;
						(**Change the if statement to if(flag) *)
						let new_st = Cil.mkStmt (If(Lval(var cond), b1, b2, l)) in
						let str = (match new_st.skind with
							| Block _ -> "\n!!!!if statement change to a block!\n"
							| If _ -> "\n!!!!if statement doesn't change!\n"
							| _ -> "\n!!!!if statement change to a unknown!\n"
						) in
						(*print_string str;*)
						new_st.sid <- if_st.sid;
						(*print_string "new if statement:";print_int new_st.sid;print_string "\n";*)
						new_st
					end
					| UnOp (unop, exp11, t1) -> begin (**the condition is an expersion.*)
						(**Make a new assignment statement of the form flag = expr.*)
						let cond = Cil.makeTempVar (List.hd func_list) t1 in
						let newCondExpr = [Set(var cond, e, !currentLoc)] in
						self#queueInstr newCondExpr;
						(**Change the if statement to if(flag) *)
						let new_st = Cil.mkStmt (If(Lval(var cond), b1, b2, l)) in
						let str = (match new_st.skind with
							| Block _ -> "\n!!!!if statement change to a block!\n"
							| If _ -> "\n!!!!if statement doesn't change!\n"
							| _ -> "\n!!!!if statement change to a unknown!\n"
						) in
						(*print_string str;*)
						new_st.sid <- if_st.sid;
						(*print_string "new if statement:";print_int new_st.sid;print_string "\n";*)
						new_st
					end
					| _ -> if_st (**the condition is a simple variable, so do nothing.*)
				)
			|_ -> E.s (bug "conditionExprVisitor: changeIf : input is not a if statement")
	
	(**Locate the function location.*)
	method vfunc fc = 
		func_list <- [fc];
		DoChildren

	(**Visit the If statement and replace the condition expersion to a simple variable.
	* Assume the if statement is of the from "if(expr)", we will change it into the form of
	*	"flag = expr; if(flag)".*)
	method vstmt st =
		match st.skind with
			|If(e, b1, b2, l) -> 
				let new_st = self#changeIf st in
				ChangeDoChildrenPost (new_st, (self#changeIf))
			| _ -> DoChildren
end


class updateIfSid = object(self)
	inherit nopCilVisitor
	method vstmt (st:stmt) = 
		match st.skind with
			| Block bl -> 
				(*ignore(E.log "\nstmt : %a" d_stmt st);*)
				let doStmt (x : stmt) =
					match x.skind with 
						| If _ -> 
							(*print_string "if statement:";print_int x.sid;print_string "\n";*)
							x.sid <- st.sid;
							(*print_string "if statement:";print_int x.sid;print_string "\n";*)
						| _ -> ()
				in
				List.iter doStmt bl.bstmts;
				DoChildren
			| _ -> DoChildren
end


let default_process (f : file) = 
	visitCilFile (new conditionExprVisitor) f;
	visitCilFile (new updateIfSid) f

let feature : featureDescr = 
  { fd_name = "cautexco";
    fd_enabled = ref false;
    fd_description = "conditional expression Extracted";
    fd_extraopt = [];
    fd_doit = default_process;
    fd_post_check = true;
}
