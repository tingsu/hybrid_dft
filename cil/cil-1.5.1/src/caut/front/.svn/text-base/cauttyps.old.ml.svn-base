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
let caut_test = true

(*let caut_log (fmt : ('a,unit,doc,unit) format4) : 'a = 
  let f d = fprint !logChannel 80 d; flush !logChannel in
  if caut_test = true then Pretty.gprintf f fmt else Pretty.gprintf f fmt
 *)
 
(******************************************* type system ************************************************)
(* diy types *)
type typeContent = {name: string; ttype: string;}

(* globals *)
let type_list = ref ([] : (Cil.typ * int) list)
let type_id = ref 20
let tmp_var_cnt = ref 0
let beforeFn = "\n\t__CAUT_t = "
let beforeFnR = "\n\t__CAUT_reg_ret = "
let cautCreateTypePrefix = "__CAUT_create_"
let cautRegisterPrefix = "__CAUT_register_datatype"

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
			( s, n+1, ds @ [(lenOfArray eo)] )
		|_ -> (arr, 0, [])
let genArrDimensionsArr dimens =
	let arrStr = ref "{" in
	(try arrStr := !arrStr ^ (string_of_int (List.hd dimens));
	with Failure "hd" -> (););
	(try  List.iter (
		fun x-> arrStr := !arrStr ^ "," ^ (string_of_int x); 
	) (List.tl dimens);
	with Failure "tl" -> (););
	!arrStr ^ "}"

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

(* manipulating type *)
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
		TVoid(_) -> {name = "void"; ttype = "primitive_type"; }
		| TInt(IChar,_) -> {name = "char"; ttype = "primitive_type";}
		| TInt(ISChar,_) -> {name = "signed char"; ttype = "primitive_type"; }
		| TInt(IUChar,_) -> {name = "unsigned char" ; ttype = "primitive_type";}
		| TInt(IInt,_) -> {name = "int"; ttype = "primitive_type"; }
		| TInt(IUInt,_) -> {name = "unsigned int"; ttype = "primitive_type";}
		| TInt(IShort,_) -> {name = "short" ; ttype = "primitive_type"; }
		| TInt(IUShort,_) -> {name = "unsigned short" ;ttype = "primitive_type";}
		| TInt(ILong,_) -> {name = "long"; ttype = "primitive_type";}
		| TInt(IULong,_) -> {name = "unsigned long" ; ttype = "primitive_type"; }
		| TInt(ILongLong,_) -> {name = "long long" ; ttype = "primitive_type";}
		| TInt(IULongLong,_) -> {name = "unsigned long long" ; ttype = "primitive_type";}
		| TFloat(FFloat,_) -> {name = "float" ; ttype = "primitive_type";}
		| TFloat(FDouble,_) -> {name = "double" ; ttype = "primitive_type";}
		| TFloat(FLongDouble,_) -> {name = "long double"; ttype = "primitive_type";}
		| TArray (tt,eop,_) -> 
			let rec getArrName t level dimens = 
				if level = 0 then (get_type_content t).name
				else (getArrName t (level-1) dimens) ^ "[" ^ (string_of_int (List.nth dimens (level-1)) )^ "]"
			in
			let (arr_type, arr_level, dimens) = getArrDimension t in
			{name = (getArrName arr_type arr_level dimens); ttype = "array_type";}
		| TFun(rt,args,_,_) -> 
			let retTName = "r_" ^ (get_type_content rt).name in
			let argTNames = ref "_a_" in
			List.iter (
				fun x -> 
					match x with (name,t,_) -> argTNames := !argTNames ^ (get_type_content t).name ^ "_"; 
			) (Cil.argsToList args);
			{name = retTName ^ (!argTNames) ; ttype = "func_type";}
		| TPtr(tt, _) -> 
			let rec getPtrName t level = (* return the ptr's name *)
				if level = 0 then (get_type_content t).name
				else (getPtrName t (level-1)) ^ "*"
			in
			let (ptr_type, ptr_level) = getPtrDimension t in
			{name = (getPtrName ptr_type ptr_level); ttype = "pointer_type";}
		| TNamed(t',_) -> 
			get_type_content (unrollType t)
		| TComp(c,_) -> {name = c.cname; ttype = ("comp_type_" ^ (getCompTtype c) ); }
		| TEnum(e,_) -> {name = e.ename; ttype = "enum_type"; }
		| _ -> {name = "unsigned long"; ttype = "unknown";}
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
class typeVisitor = object(self) (* visitor: record types *)
inherit nopCilVisitor
	method vtype t = 
		(*E.log "\ntypeVisitor : %a\t" d_type t;*)
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
		(*E.log "\ntypeVisitor : %a\t" d_global g;*)
		ignore(match g with
			|GCompTag (ci, loc) -> record_type (TComp (ci,[]));
			|GEnumTag (ei, loc) -> record_type (TEnum (ei,[]));
			|_ -> ();
		);
		DoChildren
end

(* functions that help register_types *)
let genTmpVar cnt = (* generate temp var *)
	let tmpVar = "tmp" ^ (string_of_int !cnt) in
	tmpVar
let genCompCreateMemFn t = (* generate create comp member function *)
	let seq = ref 0 in
	match t with
		| TComp (c, _) -> 
			let createMemFns = ref "" in
			List.iter (
				fun x -> 
					(* ctype_t create_comp_member(ctype_t comptype, const char *name, int namelen, int membertype, int bitlen, int seq); *)
					createMemFns := !createMemFns ^ beforeFn ^ cautCreateTypePrefix ^ "comp_member" 
						^(Printf.sprintf "(_t,\"%s\",%d,%d,%d,%d);" x.fname (String.length x.fname) (get_typeid x.ftype) (getBitlen x.fbitfield) !seq);
					seq := !seq + 1;
			) c.cfields;
			!createMemFns
		|_ -> ""

(* register progress *)
let register_types execute = (* register all types in the source file *)
(* For each type: 
	__CAUT_t = __CAUT_create_XXX_type(...);
	__CAUT_register_datatype(__CAUT_t, ...);*)
	if execute then begin
		print_string "\nvoid __CAUT_register_types()\n{";
		print_string "\n\tctype_t __CAUT_t,__CAUT_ct;";
		print_string "\n\tint __CAUT_reg_ret;";
		List.iter (
			fun (t, id) -> 
				let tc : typeContent = get_type_content t in
				(* int register_datatype(int id, ctype_t datatype); *)
				let regFn = beforeFnR ^ cautRegisterPrefix ^ (Printf.sprintf "(%d,__CAUT_t);" id) in
				let fn_call = match tc.ttype with
					| "primitive_type" -> 
						(* ctype_t create_primitive_type(const char *name, int namelen, int typesize, enum primitivetype_t type); *)
						beforeFn ^ cautCreateTypePrefix ^ tc.ttype
							^ (Printf.sprintf "(\"%s\",%d,sizeof(%s),%d);" tc.name (String.length tc.name) tc.name (get_typeid t))
							^ regFn
					| "pointer_type" -> 
						(* ctype_t create_pointer_type(const char *name, int namelen, int subtype); *)
						let (ptrt,level) = getPtrDimension t in
						beforeFn ^ cautCreateTypePrefix ^ tc.ttype
							^ (Printf.sprintf "(\"%s\",%d,%d);" tc.name (String.length tc.name) (get_typeid ptrt))
							^ regFn
					(* ctype_t create_comp_type_struct(const char *name, int namelen, int typesize, int membercnt); *)
					| "comp_type_struct" -> 
						let fields = getCompFields t in
						beforeFn ^ cautCreateTypePrefix ^ tc.ttype 
							^ (Printf.sprintf "(\"%s\",%d,sizeof(struct %s),%d);" tc.name (String.length tc.name) tc.name (List.length fields))
							^ (genCompCreateMemFn t)
							^ regFn
					(* ctype_t create_comp_type_union(const char *name, int namelen, int typesize, int membercnt); *)
					| "comp_type_union" -> 
						let fields = getCompFields t in
						beforeFn ^ cautCreateTypePrefix ^ tc.ttype 
							^ (Printf.sprintf "(\"%s\",%d,sizeof(struct %s),%d);" tc.name (String.length tc.name) tc.name (List.length fields))
							^ (genCompCreateMemFn t)
							^ regFn
					(* ctype_t create_array_type(const char *name, int namelen, int membertype, int dimension, const int *dimlens); *)
					| "array_type" -> 
						let (array_t,dimen,dimens) = getArrDimension t in
						let tmp_var = genTmpVar tmp_var_cnt in
						tmp_var_cnt := !tmp_var_cnt + 1;
						(Printf.sprintf "\n\tint %s[" tmp_var) ^ (string_of_int dimen) ^ "] = " ^ (genArrDimensionsArr dimens) ^ ";"
							^ beforeFn ^ cautCreateTypePrefix ^ tc.ttype
							^ (Printf.sprintf "(\"%s\",%d,%d,%d,%s);" tc.name (String.length tc.name) (get_typeid array_t) dimen tmp_var)
							^ regFn
					(* ctype_t create_enum_type(const char *name, int namelen, int membercnt, const int *members); *)
					| "enum_type" ->
						let items = getEnumMem t in 
						let tmp_var = genTmpVar tmp_var_cnt in
						tmp_var_cnt := !tmp_var_cnt + 1;
						(Printf.sprintf "\n\tint %s[" tmp_var) ^ (string_of_int (List.length items)) ^ "] = " ^ (genEnumItemsArr items) ^ ";"
							^ beforeFn ^ cautCreateTypePrefix ^ tc.ttype
							^ (Printf.sprintf "(\"%s\",%d,%d,%s);" tc.name (String.length tc.name) (List.length items) tmp_var)
							^ regFn
					(* ctype_t create_func_type(const char *name, int namelen, int paramcnt, const int *paramtypes, int returntype); *)
					| "func_type" -> 
						let (rett,args) = getFuncCon t in
						let tmp_var = genTmpVar tmp_var_cnt in
						tmp_var_cnt := !tmp_var_cnt + 1;
						beforeFn ^ cautCreateTypePrefix ^ tc.ttype
							^ (Printf.sprintf "(\"%s\",%d,%d,%s,%d);" tc.name (String.length tc.name) (List.length args) tmp_var (get_typeid rett))
							^ regFn
					| _ -> E.s (bug "unlisted in datatypes.h")
				in
				print_string fn_call;
		) !type_list;
		print_string "\n}\n";
	end else ()

(* print progress *)
let print_typelist l= 
	print_string "\n";
	List.iter (function x-> 
		(match x with 
			(t,id) -> print_string (get_type_content t).name;
				print_string ";;";
		)
	) !l;
	print_string "\nlenth: ";
	print_int (List.length !l);
	print_string "\n"

(*************************************** default process **************************************)
let default_process (f: file) = 
	(*E.log ("\ntype Visitor!\n");*)
	record_primitive_types;
	visitCilFile (new typeVisitor) f; (* type system *)
	(*print_typelist type_list;*)
	(*E.log ("\nregister types!\n");*)
	register_types true	 (* register types *)
	
let feature : featureDescr = 
  { fd_name = "cauttyps";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : caut type system";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      default_process f);
    fd_post_check = true
  } 
