(** cautprep.ml
	Author: Jiang Siyuan
	Date:4/25/2010
	Description: traslating the &&,|| operations' behavior
*)

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let isLog = 0

let stmtAdded = ref ([] : stmt list)

class myExpVisitor (fi : fundec) = object (self)
	inherit nopCilVisitor

	(* given two expressions and their logical bonds, 
			* output the altered expressions and 
			* updated the statements list that need being added front of the original expression *)
	method private change 
	  (* input *)(exp1 : exp) (exp2 : exp) (myTyp : typ) mySet (biop : binop)
	  (* output *): exp =
			if isLog = 1 then ignore(E.log "Change: exp1 = %a, exp2 = %a\n" d_exp exp1 d_exp exp2);
			let (t_e22,stmt22) = mySet exp2 in
			(* make a branch statement *)
			let blk1 = mkBlock [stmt22] in
			let blk2 = mkBlock [] in
			let t_exp = 
				if biop = LAnd then exp1
				else UnOp(LNot, exp1, TInt(IInt,[])) in
			let tmp_stmtk = If(t_exp, blk1, blk2, !currentLoc) in
			let b_stmt = mkStmt tmp_stmtk in
			(* store these two statements in the stmtAdded *)
			stmtAdded := !stmtAdded @ [b_stmt];
			(* replace the e21, e22 with the new tmp var *)
			(BinOp (biop, exp1, t_e22, myTyp))
	
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
						self#change ret_exp1 ret_exp2 typ2 self#setTemp LAnd
					| LOr ->
						let ret_exp1 = self#mea e21 in
						let ret_exp2 = self#mea e22 in
						if isLog = 1 then ignore(E.log "goto LOr\n");
						self#change ret_exp1 ret_exp2 typ2 self#setTemp LOr
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

class myStmVisitor (fi : fundec) = object (self)
	inherit nopCilVisitor
	
	method vstmt (st : stmt) : stmt visitAction = 
		match st.skind with
			| Instr instr_list ->
				if (List.length instr_list) > 1 then begin
					let instrStmt_list = ref ([] : stmt list) in
					List.iter (fun x -> 
						let tmpStmt = mkStmt (Instr [x]) in
						tmpStmt.sid <- st.sid;
						instrStmt_list := (!instrStmt_list)@[tmpStmt]
					) instr_list;
					let new_stmt = mkStmt (Block (mkBlock !instrStmt_list)) in
					new_stmt.sid <- st.sid;
					ChangeDoChildrenPost (new_stmt, fun x -> x.sid <- st.sid; x)
				end
				else begin
					let ret_st = visitCilStmt (new myExpVisitor fi) st in
					if isLog = 1 then ignore(E.log "\nret_st = %a\n" d_stmt ret_st);
					if isLog = 1 then ignore(List.iter (fun x -> (E.log "\nstmtAdded = %a\n" d_stmt x)) !stmtAdded);
					let ret = mkStmt (Block(mkBlock (!stmtAdded@[ret_st]) )) in
					ret.sid <- st.sid;
					stmtAdded := [];
					ChangeTo ret
				end
			| Return _   ->
				let ret_st = visitCilStmt (new myExpVisitor fi) st in
				if isLog = 1 then ignore(E.log "\nret_st = %a\n" d_stmt ret_st);
				if isLog = 1 then ignore(List.iter (fun x -> (E.log "\nstmtAdded = %a\n" d_stmt x)) !stmtAdded);
				let ret = mkStmt (Block(mkBlock (!stmtAdded@[ret_st]) )) in
				ret.sid <- st.sid;
				stmtAdded := [];
				ChangeTo ret
			| If ( exp, bl1, bl2, loc) ->
				(* because cautexco.ml have make all exp become a variable. *)
				DoChildren
			| Switch _ | TryFinally _ | TryExcept _ -> 
				ignore(E.error ("| Switch _ | TryFinally _ | TryExcept _ in cauttra.ml")); 
				DoChildren
			| Loop ( bl1, loc, stmt_opt, stmt_opt2) -> DoChildren
			| Block _ | Goto _ | Break _ | Continue _ -> DoChildren
end

let doGlobal = function 
	GFun(fi, _) ->
		let vst = new myStmVisitor fi in
		fi.sbody <- visitCilBlock vst fi.sbody;
	| _ -> ()

class printSidVst = object (self)
	inherit nopCilVisitor
	
	method vstmt (st : stmt) : stmt visitAction = 
		if isLog = 1 then ignore( E.log "\nst = %a, st.sid = %d" d_stmt st st.sid );
		DoChildren
end
	
let default_process (f: file) = 
	visitCilFile (new printSidVst) f;
	iterGlobals f doGlobal;
	if isLog = 1 then ignore( E.log "\nnew Sids\n" );
	visitCilFile (new printSidVst) f

let feature : featureDescr = 
{
	fd_name = "cauttra";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : Translation : Translate &&,|| operators' behaviors delicately";
    fd_extraopt = [];
    fd_doit = (function (f: file) -> default_process f);
    fd_post_check = true;
}
