(** cautprep.ml
	Author: Jiang Siyuan
	Date:4/25/2010
	Description: traslating the &&,|| operations' behavior
*)

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let stmtAdded = ref ([] : stmt list)

let isLog = 1 

(* given two expressions and their logical bonds, 
			* output the altered expressions and 
			* updated the statements list that need being added front of the original expression *)
let change 
  (* input *)(exp1 : exp) (exp2 : exp) (myTyp : typ) mySet (biop : binop)
  (* output *): exp =
		if isLog = 1 then ignore(E.log "Change: exp1 = %a, exp2 = %a\n" d_exp exp1 d_exp exp2);
		(*let (t_e21,stmt21) = mySet exp1 in*)
		let (t_e22,stmt22) = mySet exp2 in
		(* make a branch statement *)
		let blk1 = mkBlock [] in
		let blk2 = mkBlock [stmt22] in
		let t_biop = 
			if biop = LAnd then Ne else Eq in
		let t_exp = BinOp(t_biop, exp1, 
			Const(CInt64(Int64.of_int 0, IInt, None)), 
			TInt(IInt,[])) in
		let tmp_instr = If(t_exp, blk2, blk1, !currentLoc) in
		let b_stmt = mkStmt tmp_instr in
		(* store these two statements in the stmtAdded *)
		stmtAdded := !stmtAdded @ [b_stmt];(*stmtAdded := !stmtAdded @ [stmt21; b_stmt];*)
		(* replace the e21, e22 with the new tmp var *)
		(BinOp (biop, exp1, t_e22, myTyp))(*(BinOp (biop, t_e21, t_e22, myTyp))*)

class myStmVisitor (fi : fundec) = object (self)
	inherit nopCilVisitor

	method private setTemp (e1 : exp) = 
		let t = match (typeOf e1) with
			(* TArray can't be assigned *)
			| TArray (typ1',_ , _) -> 
				makeTempVar fi ( TPtr (typ1',[]) )
			| _ -> makeTempVar fi (typeOf e1)
		in
		let tmpInstr = Set(var t, e1, !currentLoc) in
		let retStmt = mkStmtOneInstr tmpInstr in
		(* Add this instruction before the current statement *)
		(Lval(var t), retStmt)

	method private mea (myExp : exp) : exp = 
		match myExp with
			| Const _ -> myExp
			| Lval _ -> myExp
			| SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> 
				ignore(E.error ("error number : 0002")); 
				myExp
			| UnOp ( uop, u_exp, u_typ) -> 
				let ret_exp = self#mea u_exp in
				UnOp (uop, ret_exp, u_typ)
			| BinOp (biop, e21, e22, typ2) -> 
				if isLog = 1 then ignore(E.log "BinOp: exp21 = %a, exp22 = %a\n" d_exp e21 d_exp e22);
				(match biop with
					| PlusA | PlusPI | IndexPI | MinusA | MinusPI | MinusPP | Mult | Div | Mod | Shiftlt 
					| Shiftrt | Lt | Gt | Le | Ge | Eq | Ne | BAnd | BXor | BOr ->
						let ret_exp1 = self#mea e21 in
						let ret_exp2 = self#mea e22 in
						BinOp (biop, ret_exp1, ret_exp2, typ2)
					| LAnd ->
						let ret_exp1 = self#mea e21 in
						let ret_exp2 = self#mea e22 in
						if isLog = 1 then ignore(E.log "goto LAnd\n");
						change ret_exp1 ret_exp2 typ2 self#setTemp LAnd
					| LOr ->
						let ret_exp1 = self#mea e21 in
						let ret_exp2 = self#mea e22 in
						if isLog = 1 then ignore(E.log "goto LOr\n");
						change ret_exp1 ret_exp2 typ2 self#setTemp LOr
				)
			| CastE (c_typ, c_exp) -> 
				let ret_exp = self#mea c_exp in
				CastE (c_typ, ret_exp)
			| AddrOf _ -> myExp
			| StartOf _ -> myExp
	
	method vexpr (e1 : exp) : exp visitAction =
		let rst_exp = self#mea e1 in
		ChangeTo rst_exp

end 

let rec myIter 
  (* input *) (myList : stmt list) ( myFun : (stmt -> stmt list) ) 
  (* output *): stmt list =
	let str = ref "" in
	try 
		let myHead = List.hd myList in
		let rstStmts = myFun myHead in
		(try
			let myTail = List.tl myList in
			rstStmts @ ( myIter myTail myFun )
		with Failure "tl" -> rstStmts)
	with Failure "hd" -> []

class traVisitor (fi : fundec) = object (self)
	inherit nopCilVisitor
	
	method private doStmt (st : stmt) : stmt list =
		stmtAdded := [];
		let sVst = new myStmVisitor fi in
		let rst1 = [visitCilStmt sVst st] in
		(!stmtAdded) @ rst1
	
	method vblock (blk : block) = 
		(* visit stmt, and add stmts if need *)
		blk.bstmts <- myIter blk.bstmts self#doStmt;
		(*DoChildren*)
		SkipChildren
		
end

let doGlobal = function 
	GFun(fi, _) ->
		let vst = new traVisitor fi in
		fi.sbody <- visitCilBlock vst fi.sbody;
	| _ -> ()

let default_process (f: file) = 
	iterGlobals f doGlobal

let feature : featureDescr = 
{
	fd_name = "cauttra";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : Translation : Translate &&,|| operators' behaviors delicately";
    fd_extraopt = [];
    fd_doit = (function (f: file) -> default_process f);
    fd_post_check = true;
}
