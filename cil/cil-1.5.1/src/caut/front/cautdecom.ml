(** cautprep.ml
	Author: Jiang Siyuan
	Date:6/9/2010
	Description: deconstruct complex data structures
*)

open Pretty
open Cil
module E = Errormsg

let isLog = 0

let rec deconExpr
	(setTemp : exp -> exp)
	(e : exp) : exp =
	match e with
		| Const _ -> e
		| Lval lv -> Lval (deconLval setTemp lv)
		| SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> ( constFold true e )
		| UnOp (uo, e1, t) -> 
			let e1 = deconExpr setTemp e1 in
			UnOp (uo, e1, t)
		| BinOp (bo, e1, e2, t) ->
			let e1 = deconExpr setTemp e1 in
			let e2 = deconExpr setTemp e2 in
			BinOp (bo, e1, e2, t)
		| CastE (t, e1) ->
			let e1 = deconExpr setTemp e1 in
			CastE (t, e1)
		| AddrOf lv -> AddrOf (deconLval setTemp lv)
		| StartOf (Var vi, off) -> begin
			(* StartOf didn't mean offset could change to NoOffset! *)
			if isLog = 1 then ignore (E.log "\ndeconExpr : StartOf (var e, off) = %a  " d_exp e);
			if isLog = 1 then ignore (E.log "--> StartOf  lval = %a   " d_lval (Var vi, off) );
			let l = deconLval setTemp (Var vi, off) in
			if isLog = 1 then ignore (E.log "l = %a\n" d_lval l);
			StartOf l
		end
		| StartOf (Mem e, off) -> begin
			(* StartOf didn't mean off could change to NoOffset! *)
			if isLog = 1 then ignore (E.log "\nndeconExpr : StartOf (Mem e, off) = %a  " d_exp e);
			if isLog = 1 then ignore (E.log "--> StartOf  lval = %a   " d_lval (Mem e, off) );
			let l = deconLval setTemp (Mem e, off) in
			if isLog = 1 then ignore (E.log "l = %a\n" d_lval l);
			StartOf l
		end

(* deconstruct lval *)
and deconLval
	(setTemp : exp -> exp)
	(lv : lval) : lval  =
	(* Add, watching for a zero *)
	let add (e1: exp) (e2: exp) = 
		BinOp(PlusA, e1, e2, (typeOf e1))
	in
	let getLhostType (lh:lhost) : typ = 
		match lh with
			| Var vi -> 
				if isLog = 1 then ignore(E.log "host type : Var of %a" d_type (unrollType vi.vtype) );
				unrollType vi.vtype
			| Mem e -> 
				(*E.s (bug "host type : Mem of %a" d_type (unrollType (typeOf e)));*)
				if isLog = 1 then ignore(E.log "host type : Mem of %a" d_type (unrollType (typeOf e)) );
				let e_typ = unrollType (typeOf e) in
				match e_typ with
					|TPtr (t, _) -> t
					| _ -> E.s (bug "getLhostType : lhost : Mem e : e is not a pointer!")
	in
	(* get the sub-element's type *)
	let rec getDType
		(t : typ)
		(off : offset option)
		: typ option =
		match unrollType t with
			| TVoid _ | TInt _ | TFloat _ | TFun (_, _, _, _) | TEnum (_,_) | TBuiltin_va_list _ | TPtr (_, _) -> E.s (bug "getDType: %a which type should not have offset" d_type)
			| TArray (t', _, _) -> Some t'
			| TNamed (_, _) -> E.s (bug "getDType: TNamed should not be here")
			| TComp (ci, _) -> begin
				match off with 
					| Some NoOffset -> E.s (bug "getDType : shall not appear NoOffset : type = %a" d_type t)
					| Some Index (_, o) -> E.s (bug "getDType: index on a TComp")
					| Some Field (fi, o) -> Some fi.ftype
					| None -> None
			end
	in
	let getOffE (t : typ) (off : offset) : (exp * offset) = 
		match off with
			| NoOffset -> zero, NoOffset
			| Field (fi, restoff) -> 
				let start, len = bitsOffset t (Field(fi, NoOffset)) in
				if start land 7 <> 0 then begin
					(* We have a bitfield *)
					assert (restoff = NoOffset);
					zero, restoff
				end else
					(* in the end, the offset should be byte-size when struct/union *)
					integer (start / 8), restoff
			| Index (ei, restoff) -> 
				(* in the end, the offset should be array's element's size when struct/union *)
				(*(constFold true ei), restoff *)
				(deconExpr setTemp ei), restoff
	in
	let getBase (lv: lval) : exp = mkAddrOrStartOf lv in
	let classifyOff (off: offset) : string =
		match off with
			| Field (fi, off') when fi.fbitfield = None-> "field"
			| Field (fi, off') -> "bit field"
			| _ -> "others"
	in
	let rec deconLvalInner
		(htyp : typ)
		(p : exp) (* need make sure p is a expr of lval *)
		(off : offset) : lval =
		let dtyp = getDType htyp (Some off) in
		if isLog = 1 then ignore(E.log "\n\n#: lval = %a" d_exp p);
		if isLog = 1 then ignore(E.log "    hosttyp = %a" d_type htyp);
		match dtyp with
		| Some t -> begin
			if isLog = 1 then ignore(E.log "   dtyp = %a" d_type t);
			let pLval = match p with 
				|Lval (Mem exp_, off_) -> exp_
				|_ -> E.s (bug "deconLvalInner:p shall be a Lval")
			in
			let base = if (classifyOff off) = "field" then 
					CastE (!upointType, pLval) 
				else 
					CastE (TPtr (t,[]), pLval)
			in
			let (offE,restoff) = getOffE t off in
			if isLog = 1 then ignore(E.log "  off = %a" d_exp offE);
			if isLog = 1 then ignore(E.log "  base = %a" d_exp base);
			if (classifyOff off) = "bit field" then begin
				(* if is a bit field, do nothing *)
				if isLog = 1 then ignore (E.log "\n--is a bitfield \n");
				match p with Lval lv -> lv
					| _ -> E.s (bug "deconLvalInner: it's should be lval here : %a" d_exp p)
			end else begin
				(* if is not a bit field, do as follows *)
				if isLog = 1 then ignore (E.log "\n\nbase = %a   " d_exp base);
				if isLog = 1 then ignore (E.log "\noffsetE = %a   " d_exp offE);
				let p' =  mkCast (add base offE) (TPtr(t,[]))  in (* make sure p' is a ptr *)
				if not (restoff = NoOffset) then
					deconLvalInner t ( Lval (Mem p',NoOffset) ) restoff (* make sure input 'p' is a expr of lval *)
				else (Mem p',NoOffset)
			end
		end
		| None -> begin
			if isLog = 1 then ignore(E.log "\n--dtyp = none, have not sub-element!\n");
			match p with Lval lv -> lv
				| _ -> E.s (bug "deconLvalInner: it's should be lval here : %a" d_exp p)
		end
	in
	match lv with
		| (Var v, NoOffset) -> lv
		| (Mem e, NoOffset) -> begin
			let a = deconExpr setTemp e in
			match typeOf a with 
				| TPtr (_, _) -> Mem a, NoOffset
				| _ -> 
					E.s (bug "deconLval : (Mem e, NoOffset) : after deconExpr e:%a, now has become type of %a" d_exp e d_type (typeOf a) )
		end
		| (lh, off) -> 
			if isLog = 1 then ignore (E.log "\n1: lval = %a   " d_lval (lh, off));
			(* get the host's type *)
			let htyp = getLhostType lh in
			if isLog = 1 then ignore (E.log "host type = %a   " d_type htyp);
			(* get the sub-member's type *)
			let dtyp = getDType htyp (Some off) in
			match dtyp with
				| Some t -> begin
					if isLog = 1 then ignore (E.log "dtype = %a   " d_type t);
					(* get the current offset to the integer expr *)
					let (offE,restoff) = getOffE t off in
					if isLog = 1 then ignore (E.log "off = %a   " d_exp offE);
					(* get the addrOf or StartOf lh's base *)
					let base = getBase (lh, NoOffset) in
					let base = if (classifyOff off) = "field" then 
							CastE (!upointType , base)
						else base in
					if isLog = 1 then ignore (E.log "base = %a   " d_exp base);
					if (classifyOff off) = "bit field" then begin
						(* ignore bit field *)
						lv
					end else begin
						(* ee' 's type is depend on base's type *)
						if isLog = 1 then ignore (E.log "\n\nbase = %a   " d_exp base);
						if isLog = 1 then ignore (E.log "\noffsetE = %a   \n\n" d_exp offE);
						let ee' = add base offE in
						if isLog = 1 then ignore (E.log "  --> exp = %a   " d_exp ee');
						(*let p = setTemp ee' in*)
						let p = mkCast (add base offE) (TPtr(t,[])) in (* make sure p is a TPtr *)
						(* recursive deconstruct the child member *)
						if not (restoff = NoOffset) then
							(*let p' = deconLvalInner t ( Lval (Mem p,NoOffset) ) restoff in*)
							deconLvalInner t (Lval (Mem p, NoOffset)) restoff
							(*p'*)
						else
							(*Mem p, NoOffset*) (* need to make sure p is a TPtr *)
							Mem p, NoOffset
					end
				end
				| None -> lv

class decontructionVarVisitor (fi: fundec) = object (self)
inherit nopCilVisitor

	method private setTemp (e1: exp) : exp = 
		let typ1 = match (typeOf e1) with
			(* TArray can't be assigned *)
			| TArray (typ1',_ , _) -> TPtr (typ1',[])
			| _ -> (typeOf e1)
		in
		let t = makeTempVar fi typ1 in
		(* Add this instruction before the current statement *)
		self#queueInstr [Set(var t, e1, !currentLoc)];
		Lval(var t)

	method vexpr (e: exp) = 
		ChangeTo (deconExpr self#setTemp e)

	method vlval (lv: lval) = 
		ChangeTo (deconLval self#setTemp lv)

end

let doGlobal = function 
	GFun(fi, _) ->  
		(* Visit the body and change all expressions into three address code *)
		let v = new decontructionVarVisitor fi in
		fi.sbody <- visitCilBlock v fi.sbody;
	| _ -> ()

let feature : featureDescr = 
  { fd_name = "cautdecom";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : deconstruct var: struct, union, array, not include bit field";
    fd_extraopt = [];
    fd_doit = (function f -> iterGlobals f doGlobal);
    fd_post_check = true;
}
