(** cautprep.ml
	Author: Jiang Siyuan
	Date:4/25/2010
	Description: 1. change the split action of struct. All structs variable are changed to denoted by pointer.
	 2. simplify the if statement more. change the from of if(expr) to flag = expr; if(flag).
	 3. "mem (mkcast a) offset" is replaced by "tmp = mkcast a" "mem tmp offset" 
*)

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let isLog = 0

type taExp = exp (* Three address expression *)
type bExp = exp  (* Basic expression *)

(* Turn an expression into a three address expression (and queue some instructions in the process) *)
let rec makeThreeAddress 
    (setTemp: taExp -> bExp) (* Given an expression save it into a temp and return that temp, including queueing instrs *)
    (e: exp) : taExp = 
  match e with 
	| Const _ -> e (* unchanged *)
	| Lval lv -> Lval (simplifyLval setTemp lv) (* simplify lval *)
	| SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ (* turn to constant *) ->
		E.s (bug "cautsimplify : have sizeof ... plz call docautprep before cautsimplify")
	| UnOp (uo, e1, tres) -> UnOp (uo, makeBasic setTemp e1, tres) (* simplify e1 *)
	| BinOp (bo, e1, e2,tres) -> 
		BinOp (bo, makeBasic setTemp e1, makeBasic setTemp e2, tres) (* simplify e1, e2 *)
	| CastE (t, e) -> CastE(t, makeBasic setTemp e) (* simplify e *)
	| AddrOf lv ->( (* when there is a &*lval, cannot change to tmp=*lval, tmp1=&tmp *)
		if isLog = 1 then ignore (E.log "\nmakeThreeAddress : AddrOf (lv) %a " d_lval lv);
		match lv with
			| (Var _, NoOffset) -> AddrOf lv
			| (Var vi, off) -> 
				E.s (bug "makeThreeAddress: after decom shall not appear array, struct..." )
			| (Mem e1, off) -> 
				E.s (bug "makeThreeAddress: unexpected situation : AddrOf (Mem ..) : %a" d_exp e)
		)
	| StartOf (Var vi, off) -> begin
		match off with 
			| NoOffset -> e (* has simplify the off in the deconVar *)
			| Field (fi, off') -> 
				E.s (bug "please call set --dodeconVar before --dosimplify. If you have, send bug infomation: 
					dodeconVar hasn't simplify the ( = %a )'s off in the deconVar" d_exp e)
			| Index (e1, off') -> 
				E.s (bug "please call set --dodeconVar before --dosimplify. If you have, send bug infomation: 
					dodeconVar hasn't simplify the ( = %a )'s off in the deconVar" d_exp e)
	end
	| StartOf (Mem e, off) -> E.s (bug "makeThreeAddress: unexpected situation : StartOf (Mem e...) : %a" d_exp e)

(* simplify a lval *)
and simplifyLval
	(setTemp: taExp -> bExp)
	(lv: lval) : lval =
	match lv with
		| (Var _, NoOffset) -> lv
		| (Var vi, off) -> E.s (bug "simplifyLval : plz call --docautdeconVar before docautsimplify")
		| (Mem e1, NoOffset) ->
			let e1' = makeBasic setTemp e1 in
			(Mem e1', NoOffset)
		| (Mem e1, off) -> E.s (bug "simplifyLval : plz call --docautdeconVar before docautsimplify")

(* Make a basic expression *)      
and makeBasic (setTemp: taExp -> bExp) (e: exp) : bExp = 
  (* Make it a three address expression first *)
  let e' = makeThreeAddress setTemp e in
  (* See if it is a basic one *)
  match e' with
	| Const _ -> e' (* unchanged *)
	| Lval lv -> (match lv with |(Var _, NoOffset) -> e' |_ -> setTemp e')
	| SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ 
		-> E.s (bug "Simplify: makeBasic found SizeOf: %a" d_exp e')
	| UnOp (uo, e1, tres) -> setTemp e'
	| BinOp (bo, e1, e2,tres) -> setTemp e'
	| CastE (t, e) -> setTemp e'
	| AddrOf lv ->
		( (* when there is a &*lval, cannot change to tmp=*lval, tmp1=&tmp *)
		if isLog = 1 then ignore (E.log "\nmakeBasic : AddrOf (lv) %a " d_lval lv);
		match lv with
			| (Var _, NoOffset) -> setTemp e'
			| (Var vi, off) -> 
				E.s (bug "makeBasic: after decom shall not appear array, struct..." )
			| (Mem e1, off) -> E.s (bug "makeBasic: unexpected situation : AddrOf (Mem ..) : %a" d_exp e)
		)
	| StartOf (Var vi, off) -> begin
		match off with 
			| NoOffset -> e' (* has simplify the off in the deconVar *)
			| Field (fi, off') -> 
				E.s (bug "please call set --dodeconVar before --dosimplify. If you have, send bug infomation: 
					dodeconVar hasn't simplify the ( = %a )'s off in the deconVar" d_exp e')
			| Index (e1, off') -> 
				E.s (bug "please call set --dodeconVar before --dosimplify. If you have, send bug infomation: 
					dodeconVar hasn't simplify the ( = %a )'s off in the deconVar" d_exp e')
	end
	| StartOf (Mem em, off) -> E.s (bug "makeBasic: unexpected situation : StartOf (Mem e...) : %a" d_exp e)

(** This is a visitor that will turn all expressions into three address code *)
class threeAddressVisitor (fi: fundec) = object (self)
	inherit nopCilVisitor

	method private setTemp (e1: exp) : exp = 
		let t = match (typeOf e1) with
			(* TArray can't be assigned *)
			| TArray (typ1',_ , _) -> 
				makeTempVar fi ( TPtr (typ1',[]) )
			| _ -> makeTempVar fi (typeOf e1)
		in
		(* Add this instruction before the current statement *)
		self#queueInstr [Set(var t, e1, !currentLoc)];
		Lval(var t)
		
	(* We'll ensure that this gets called only for top-level expressions 
	 * inside functions. We must turn them into three address code. *)
	method vexpr (e: exp) = 
		if isLog = 1 then ignore(E.log "vexpr e: %a" d_exp e);
		let e' = makeThreeAddress self#setTemp e in
		ChangeTo e'
	
	(* We want the argument in calls to be simple variables *)
	method vinst (i: instr) =
		match i with 
		Call (someo, f, args, loc) -> 
			(*let f' = makeBasic self#setTemp f in*)
			let args' = List.map (makeBasic self#setTemp) args in 
			ChangeTo [ Call (someo, f, args', loc) ]
		| _ -> DoChildren

	(* This method will be called only on top-level "lvals" (those on the 
	 * left of assignments and function calls) *)
	method vlval (lv : lval) = 
		if isLog = 1 then ignore(E.log "vexpr e: %a" d_lval lv);
		match lv with
			| (lh, NoOffset) -> (
				match lh with 
					| Var vi -> DoChildren (* has simplify the off in the deconVar *)
					| Mem em -> 
						let em' = makeBasic self#setTemp em in
						ChangeTo (Mem em',NoOffset)
			)
			| (lh, Field (fi, off')) ->
				E.s (bug "please call set --dodeconVar before --dosimplify. 
					If you have, send bug infomation: 
					dodeconVar hasn't simplify the ( = %a )'s off in the deconVar" d_lval lv)
			| (lh, Index (e1, off')) -> 
				E.s (bug "please call set --dodeconVar before --dosimplify. 
					If you have, send bug infomation: 
					dodeconVar hasn't simplify the ( = %a )'s off in the deconVar" d_lval lv)
					
	method vstmt (st : stmt) = 
		match st.skind with
		| Return (Some e', loc) ->
			(match e' with
			| Const _ -> DoChildren
			| Lval (Var v, NoOffset) -> DoChildren
			| _ -> 
				let e'' = makeBasic self#setTemp e' in
				st.skind <- Return (Some e'', loc);
				DoChildren
			)
		| _ -> DoChildren
end

let doGlobal = function 
    GFun(fi, _) ->  
      (* Visit the body and change all expressions into three address code *)
      let v = new threeAddressVisitor fi in
      fi.sbody <- visitCilBlock v fi.sbody;
  | _ -> ()

let feature : featureDescr = 
  { fd_name = "cautsimplify";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : compiles CIL to 3-address code";
    fd_extraopt = [];
	(**Notice here: Stanley add the visitCilFile action to perform simplifing the if statement.*)
    fd_doit = (function f -> iterGlobals f doGlobal);
    fd_post_check = true;
}
