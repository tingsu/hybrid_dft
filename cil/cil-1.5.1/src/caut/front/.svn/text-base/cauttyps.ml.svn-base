(** cautprep.ml
	Author: Jiang Siyuan
	Date:4/25/2010
	Description: collecting type system, cautinin.ml natively includes it.
*)
module E = Errormsg

open Cil
open String
open Pretty
open Errormsg

exception Fail of string

(* E.log for caut test*)
let isLog = 0
 
(******************************************* type system ************************************************)


(* diy types *)
type typeContent = {name: string; ctype: string;}

(* globals *)
let type_list = ref ([] : (Cil.typ * int) list)
let type_id = ref 20
let tmp_var_cnt = ref 0

(* manipulating ptr *)
let rec getPtrDimension ptr =
	match ptr with
		TPtr(t, _) -> 
			let (s, n) = getPtrDimension t	in
			(s, n + 1)
		|_ -> (ptr, 0)

(* manipulating array *)
let rec getArrDimension arr = 
	match arr with
		|TArray(tt,eo,_) ->
			let (s, n, ds) = getArrDimension tt in
			( s, n+1, [(lenOfArray eo)] @ ds )
		|_ -> (arr, 0, [])

(* manipulating comp *)
let getCompTtype c = 
	if c.cstruct = true then "struct"
	else "union"
let getCompFields t =
	match t with
		| TComp (c, _) -> c.cfields
		|_ -> []
let getBitlen (b:int option) = 
	match b with 
		| Some(bitlen) -> bitlen
		| None -> (-1)
            			
(* manipulating enum *)
let getEnumMem t =
	match t with
		| TEnum (e,_)-> e.eitems
		| _ -> []
let genEnumItemsArr items =
	let arrStr = ref "{" in
	let value x = match x with 
		(name,exp,loc) -> (match (constFold true exp) with 
			Const c -> (match c with
				CInt64 (i,k,sop) -> i
				|_ -> raise (Fail "Error: when register the enum type!")
				)
			|_ ->raise (Fail "Error: when register the enum type!")
		)
	in
	(try arrStr := !arrStr ^ (string_of_int (Int64.to_int (value (List.hd items)))); with Failure "hd" -> (););
	(try List.iter (
		fun x -> arrStr := !arrStr ^ "," ^ (string_of_int (Int64.to_int (value x)));
	) (List.tl items) with Failure "tl" -> (););
	!arrStr ^ "}"

(* manipulating funcion *)
let getFuncCon t = 
	match t with
		| TFun(rt,args,_,_) -> (rt, Cil.argsToList args)
		| _ -> raise (Fail "Error: use getFuncCon but not input a type of tfun. Line 82")

(* manipulating primitive type *)
let get_primitivetype_enum t = (* return -1 if t is not a primitive type *)
	match t with
		TVoid(_) -> 0
		| TInt(IChar,_) -> 1
		| TInt(ISChar,_) -> 2
		| TInt(IUChar,_) -> 7
		| TInt(IInt,_) -> 4
		| TInt(IUInt,_) -> 9
		| TInt(IShort,_) -> 3
		| TInt(IUShort,_) -> 8
		| TInt(ILong,_) -> 5
		| TInt(IULong,_) -> 10
		| TInt(ILongLong,_) -> 6
		| TInt(IULongLong,_) -> 11
		| TFloat(FFloat,_) -> 12
		| TFloat(FDouble,_) -> 13
		| TFloat(FLongDouble,_) -> 14
		| _ -> (-1)

let rec get_type_content t :typeContent = 
	match t with
		TVoid(_) -> {name = "void"; ctype = "primitive_type"; }
		| TInt(IChar,_) -> {name = "char"; ctype = "primitive_type";}
		| TInt(ISChar,_) -> {name = "signed char"; ctype = "primitive_type"; }
		| TInt(IUChar,_) -> {name = "unsigned char" ; ctype = "primitive_type";}
		| TInt(IInt,_) -> {name = "int"; ctype = "primitive_type"; }
		| TInt(IUInt,_) -> {name = "unsigned int"; ctype = "primitive_type";}
		| TInt(IShort,_) -> {name = "short" ; ctype = "primitive_type"; }
		| TInt(IUShort,_) -> {name = "unsigned short" ;ctype = "primitive_type";}
		| TInt(ILong,_) -> {name = "long"; ctype = "primitive_type";}
		| TInt(IULong,_) -> {name = "unsigned long" ; ctype = "primitive_type"; }
		| TInt(ILongLong,_) -> {name = "long long" ; ctype = "primitive_type";}
		| TInt(IULongLong,_) -> {name = "unsigned long long" ; ctype = "primitive_type";}
		| TFloat(FFloat,_) -> {name = "float" ; ctype = "primitive_type";}
		| TFloat(FDouble,_) -> {name = "double" ; ctype = "primitive_type";}
		| TFloat(FLongDouble,_) -> {name = "long double"; ctype = "primitive_type";}
		| TArray (tt,eop,_) -> 
			let rec getArrName t level dimens = 
				if level = 0 then (get_type_content t).name
				else (getArrName t (level-1) dimens) ^ "[" ^ (string_of_int (List.nth dimens (level-1)) )^ "]"
			in
			let (arr_type, arr_level, dimens) = getArrDimension t in
			{name = (getArrName arr_type arr_level dimens); ctype = "array_type";}
		| TFun(rt,args,_,_) -> 
			let retTName = "r_" ^ (get_type_content rt).name in
			let argTNames = ref "_a_" in
			List.iter (
				fun x -> 
					match x with (name,t,_) -> argTNames := !argTNames ^ (get_type_content t).name ^ "_"; 
			) (Cil.argsToList args);
			{name = retTName ^ (!argTNames) ; ctype = "func_type";}
		| TPtr(tt, _) -> 
			let rec getPtrName t level = (* return the ptr's name *)
				if level = 0 then (get_type_content t).name
				else (getPtrName t (level-1)) ^ "*"
			in
			let (ptr_type, ptr_level) = getPtrDimension t in
			{name = (getPtrName ptr_type ptr_level); ctype = "pointer_type";}
		| TNamed(t',_) -> 
			get_type_content (unrollType t)
		| TComp(c,_) -> {name = c.cname; ctype = ("comp_type_" ^ (getCompTtype c) ); }
		| TEnum(e,_) -> {name = e.ename; ctype = "enum_type"; }
		| _ -> {name = "unsigned long"; ctype = "unknown";}

let same_type t1 t2 = 
	(get_type_content t1) = (get_type_content t2)

let get_typeid t =
	let tid = try
		List.find (fun x ->
			match x with (xt, _) ->
				same_type xt t
		)!type_list
	with Not_found -> (t, -1) in
	let rst = match tid with (_, ti) -> ti in
	rst

let type_exists t = 
	List.exists(fun x -> match x with (xt, _) -> same_type xt t) !type_list 

(* record primitive types *)
let record_primitive_types = 
	if isLog = 1 then ignore (E.log ("\nrecord Primitive Types!\n"));
	for i = 0 to 14 do
		let ret = 
			match i with
				|0 -> type_list := !type_list @ [TVoid([]), i];
				|1 -> type_list := !type_list @ [TInt(IChar,[]), i];
				|2 -> type_list := !type_list @ [TInt(ISChar,[]), i];
				|3 -> type_list := !type_list @ [TInt(IShort,[]), i];
				|4 -> type_list := !type_list @ [TInt(IInt,[]), i];
				|5 -> type_list := !type_list @ [TInt(ILong,[]), i];
				|6 -> type_list := !type_list @ [TInt(ILongLong,[]), i];
				|7 -> type_list := !type_list @ [TInt(IUChar,[]), i];
				|8 -> type_list := !type_list @ [TInt(IUShort,[]), i];
				|9 -> type_list := !type_list @ [TInt(IUInt,[]), i];
				|10 -> type_list := !type_list @ [TInt(IULong,[]), i];
				|11 -> type_list := !type_list @ [TInt(IULongLong,[]), i];
				|12 -> type_list := !type_list @ [TFloat(FFloat,[]), i];
				|13 -> type_list := !type_list @ [TFloat(FDouble,[]), i];
				|14 -> type_list := !type_list @ [TFloat(FLongDouble,[]), i];
				|_ -> E.log("\nError:!impossible!\n");
		in
		ret
	done

(* record types *)
let rec record_type (t: Cil.typ) = 
	(*E.log "record_type : %a\t" d_type t;*)
	if not (type_exists t) then begin
		type_list := !type_list @ [t, !type_id];
		type_id := !type_id + 1;
		ignore(match t with
			(* add the sub-type first *)
			TPtr(pt, _) -> 
				(*E.log "Ptr : %a -> %a\n" d_type t d_type pt;*)
				if not (type_exists pt) then record_type pt;
			| TArray(at, _, _) -> 
				if not (type_exists at) then record_type at;
			| TComp(c, _) -> 
				List.iter(fun x -> 
					if not (type_exists x.ftype) then record_type x.ftype) c.cfields;
			| TFun(rett,args,_,_) ->
				if not (type_exists rett) then record_type rett;
				List.iter(fun (name,ft,_) -> 
					if not (type_exists ft) then record_type ft;) (Cil.argsToList args);
			| _ -> ();
		);
	end else ()

(* visitor *)
class recordTypeVisitor = object(self) (* visitor: record types *)
inherit nopCilVisitor
	method vtype t = 
		if isLog = 1 then ignore(E.log "\ntypeVisitor : %a\t" d_type t);
		ignore(match t with
			TNamed _ -> 
				(*E.log "TNamed\t";*)
				record_type (Cil.unrollTypeDeep t); (* for a typedef... *)
			| _ -> 
				(*E.log "Not TNamed\t";*)
				record_type t;
		);
		DoChildren
		
	method vglob g = 
		if isLog = 1 then ignore(E.log "\ntypeVisitor : %a\t" d_global g);
		ignore(match g with
			|GCompTag (ci, loc) -> record_type (TComp (ci,[]));
			|GEnumTag (ei, loc) -> record_type (TEnum (ei,[]));
			|_ -> ();
		);
		DoChildren
end
		
(* register progress *)
let register_types type_list' : fundec = (* register all types in the source file *)
	(* the big function *)
	let registerFun = emptyFunction "__CAUT_register_types" in 
	let ctype_t = TNamed({tname = "__CAUT_ctype_t"; ttype = TVoid([]); treferenced = false;},[]) in
	let __CAUT_t = makeLocalVar registerFun "__CAUT_t" ctype_t in
	let __CAUT_reg_ret = makeLocalVar registerFun "__CAUT_reg_ret" (TInt(IInt,[])) in
	
	(* the called function *)
	let f = fun (t, id) -> 
		let tc : typeContent = get_type_content t in
		if isLog = 1 then ignore(E.log "t = %a, id = %d;\n" d_type t id);
		let __CAUT_create_ = "__CAUT_create_" in
		let createDataTypeF = emptyFunction (__CAUT_create_ ^ tc.ctype) in
		let createMembTypeF = emptyFunction (__CAUT_create_ ^ "comp_member") in

		let fn_call = 
			match tc.ctype with
			| "primitive_type" -> 
				(* ctype_t create_primitive_type(const char *name, int namelen, int typesize, enum primitivetype_t type); *)
				[Call (
					Some (Var __CAUT_t, NoOffset), 
					Lval(Var createDataTypeF.svar, NoOffset), 
					[Const(CStr tc.name); integer (String.length tc.name); 	SizeOf(t); integer id],
					!currentLoc )]
			| "pointer_type" -> 
				(* ctype_t create_pointer_type(const char *name, int namelen, int subtype); *)
				(* let (ptrt,level) = getPtrDimension t in*)
				let ptrt = match t with TPtr(t', _) -> t' |_ -> E.s (bug "pointer_type : not a ptr!") in
				[Call (
					Some (Var __CAUT_t, NoOffset), 
					Lval(Var createDataTypeF.svar, NoOffset), 
					[Const(CStr tc.name); integer (String.length tc.name); integer (get_typeid ptrt)],
					!currentLoc )]
			| "comp_type_struct" | "comp_type_union" -> 
				(* ctype_t create_comp_type_struct(const char *name, int namelen, int typesize, int membercnt); *)
				(* ctype_t create_comp_type_union(const char *name, int namelen, int typesize, int membercnt); *)
				let genCompCreateMemFn t = (* generate create comp member function *)
					(* ctype_t create_comp_member(ctype_t comptype, const char *name, int namelen, int membertype, int bitlen, int seq); *)
					match t with
					| TComp (c, _) -> 
						let seq = ref 0 in
						let funList = ref ([] : instr list) in
						List.iter ( fun x ->  
							funList := !funList @ 
								[Call (
									Some (Var __CAUT_t, NoOffset),
									Lval (Var createMembTypeF.svar, NoOffset), 
									[
									 Lval(Var __CAUT_t, NoOffset); 
									 Const(CStr x.fname);
									 integer (String.length x.fname);
									 integer (get_typeid x.ftype);
									 integer (getBitlen x.fbitfield);
									 integer !seq
									],
									!currentLoc 
								 )
								];
							seq := !seq + 1;
						) c.cfields;
						!funList
					|_ -> []
				in
				let fields = getCompFields t in
				[Call (
					Some (Var __CAUT_t, NoOffset), 
					Lval(Var createDataTypeF.svar, NoOffset), 
					[Const(CStr tc.name); integer (String.length tc.name); SizeOf(t); integer (List.length fields)],
					!currentLoc )]
				@ (genCompCreateMemFn t)
			| "array_type" -> 
				(* ctype_t create_array_type(const char *name, int namelen, int membertype, int dimension, const int *dimlens); *)
				let (array_t,dimen,dimens) = getArrDimension t in
				let tmp_var = makeTempVar registerFun (TArray(TInt(IInt,[]),Some(integer dimen),[])) in
				let genArrDimensionsArr dimens : instr list =
					let arrDimenInstrs = ref([] : instr list) in
					let index = ref 0 in
					List.iter (fun x -> 
						arrDimenInstrs := !arrDimenInstrs @
							[Set (
								(Var tmp_var, Index(integer !index, NoOffset)),
								integer x,
								!currentLoc
							)];
						index := !index + 1;
					) dimens;
					!arrDimenInstrs
				in
				(genArrDimensionsArr dimens)
				@
				[Call (
					Some (Var __CAUT_t, NoOffset), 
					Lval(Var createDataTypeF.svar, NoOffset), 
					[
					 Const(CStr tc.name);
					 integer (String.length tc.name);
					 integer (get_typeid array_t);
					 integer dimen;
					 Lval(Var tmp_var, NoOffset)],
					!currentLoc )]
			| "enum_type" ->
				(* ctype_t create_enum_type(const char *name, int namelen, int membercnt, const int *members); *)
				let items = getEnumMem t in 
				let tmp_var = makeTempVar registerFun (TArray(TInt(IInt,[]),Some(integer (List.length items)),[])) in
				let genEnumItems items : instr list =
					let enumItemsInstrs = ref([] : instr list) in
					let index = ref 0 in
					List.iter (fun (s,x,_) -> 
						enumItemsInstrs := !enumItemsInstrs @
							[Set (
								(Var tmp_var, Index(integer !index, NoOffset)),
								x,
								!currentLoc
							)];
						index := !index + 1;
					) items;
					!enumItemsInstrs
				in
				(genEnumItems items)
				@
				[Call (
					Some (Var __CAUT_t, NoOffset), 
					Lval(Var createDataTypeF.svar, NoOffset), 
					[
					 Const(CStr tc.name);
					 integer (String.length tc.name);
					 integer (List.length items);
					 Lval(Var tmp_var, NoOffset)],
					!currentLoc )]
			| "func_type" -> 
				(* ctype_t create_func_type(const char *name, int namelen, int paramcnt, const int *paramtypes, int returntype); *)
				let (rett,args) = getFuncCon t in
				let (setArgs, tmp) = if (List.length args)>0 then begin
					let tmp_var = makeTempVar registerFun (TArray(TInt(IInt,[]),Some(integer (List.length args)),[])) in
					let genParas args : instr list =
						let parasInstrs = ref([] : instr list) in
						let index = ref 0 in
						List.iter (fun x -> 
							(match x with (name, ptyp, _) ->
							parasInstrs := !parasInstrs @
								[Set (
									(Var tmp_var, Index(integer !index, NoOffset)),
									integer (get_typeid ptyp),
									!currentLoc
								)];
							index := !index + 1;
							)
						) args;
						!parasInstrs
					in
					(genParas args), Lval(Var tmp_var, NoOffset)
				end else ([], integer 0) in
				setArgs
				@
				[Call (
					Some (Var __CAUT_t, NoOffset), 
					Lval(Var createDataTypeF.svar, NoOffset), 
					[
					 Const(CStr tc.name);
					 integer (String.length tc.name);
					 integer (List.length args);
					 tmp;
					 integer (get_typeid rett)],
					!currentLoc )]
			| _ -> E.s (bug "unlisted in datatypes.h")
		in
		registerFun.sbody.bstmts <- registerFun.sbody.bstmts @ [mkStmt (Instr(fn_call))];

		let registerDataTypeF = emptyFunction "__CAUT_register_datatype" in
		setFunctionTypeMakeFormals 
			registerDataTypeF
			(TFun(TInt(IInt,[]), Some([("id",TInt(IInt,[]),[]); ("datatype",ctype_t,[])]), false, []));
		let regFn = Call (
			Some (Var __CAUT_reg_ret, NoOffset), 
			Lval(Var registerDataTypeF.svar, NoOffset), 
			[integer id; Lval(Var __CAUT_t, NoOffset)],
			!currentLoc )
		in
		registerFun.sbody.bstmts <- registerFun.sbody.bstmts @ [mkStmtOneInstr regFn];
	in	
	if isLog = 1 then ignore(E.log "\nstart registering the type list!\n");
	List.iter f !type_list';
	registerFun.sbody.bstmts <- registerFun.sbody.bstmts @ [mkStmt (Return (None, !currentLoc))];
	registerFun

(* print process *)
let print_typelist l= 
	E.log ("\n");
	List.iter (fun x-> 
		(match x with 
			(t,id) -> if isLog = 1 then ignore (E.log "%s;;" (get_type_content t).name )
		)
	) !l;
	if isLog = 1 then ignore (E.log "\nlenth: %d\n" (List.length !l) )
	
(*************************************** default process **************************************)
let default_process (f: file) =
	record_primitive_types;
	if isLog = 1 then ignore (E.log ("\ntype Visitor!\n"));
	visitCilFile (new recordTypeVisitor) f; (* type system *)
	if isLog = 1 then ignore (print_typelist type_list);
	if isLog = 1 then ignore (E.log ("\nregister types!\n"));
	let regFun = register_types type_list in
	if isLog = 1 then ignore (E.log ("\nadd function in file!\n"));
	f.globals <- f.globals @ [GFun(regFun,!currentLoc)] 	 (* register types *)
	
let feature : featureDescr = 
  { fd_name = "cauttyps";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : generate caut type system";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      default_process f);
    fd_post_check = true
  } 
