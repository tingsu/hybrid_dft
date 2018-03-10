(** cautprep.ml
	Author: Sun Tao, Jiang Siyuan
	Date:4/25/2010
	Description: inserting interfaces based on the original statements
*)
module E = Errormsg

open Cil
open String

exception Fail of string

let isLog = 0

(******************************getting original branches*******************************)
let bra_sids = ref ([] : int list)

let sbl_doGlobal (g:global) = 
	match g with
		| GText str ->
			( try (
				let len_str = String.length str in
				(*print_string ("\nstr:" ^ str ^ "\n");
				print_string "\nlength of str:"; print_int len_str; print_string ";\n";*)
				let len_pre = String.length "/*CAUT branches:\n" in
				(*print_string "\nlength of pre:"; print_int len_pre; print_string ";\n";*)
				if (String.sub str 0 len_pre) = "/*CAUT branches:\n" then begin
					let rec get_braSid (str2 : string) =
						(*print_string ("\nget_braSid("^str2^")"); print_string ";\n";*)
						let rec lstNum (subs : string) (num : int) : int = 
							(*print_string ("\n\tlstNum("^subs^")"); print_int num; print_string ";\n";*)
							if ((String.length subs) = 0 ||subs.[0] = '#') then begin num end
							else begin
								let len = String.length subs in
								let subss = String.sub subs 1 (len-1) in
								lstNum subss (num+1)
							end
						in
						if (String.length str2 = 0 ) then begin
							()
						end
						else begin
							let len_str2 = String.length str2 in
							let str3 = String.sub str2 1 (len_str2-1) in
							let n = lstNum str3 0 in
							(*print_string "\n\tn:"; print_int n; print_string ("  str3:"^str3);
							print_string ("\n\tstr:"^(String.sub str3 0 n));
							print_string "\n\tint:"; print_int (int_of_string (String.sub str3 0 n));*)
							bra_sids := (!bra_sids) @ [(int_of_string (String.sub str3 0 n))];
							get_braSid (String.sub str3 n (len_str2-1-n))
						end
					in
					get_braSid (String.sub str (len_pre) (len_str-len_pre-2))
				end
				else ()
			) with _ -> ()
			)
		| _->()

(*************************************** runtime interfaces *********************************************)
(** predefined types *)
type varIdentity = LVarId of Cil.varinfo * string * int | GVarId of Cil.varinfo * int
type loopIdentity = {self:int;children:int list;}

(** global data structures *)
(* Global data structure, used to record all the variables in this source file. *)
let varIdentityList = ref ([] : varIdentity list) 
(* A global list to record all the function names and their identities in this source file. *)
let funcIdentityList = ref ([] : (string * int) list) 
let loopIdentityList = ref([] : loopIdentity list) (* global list to record all the loops' ids and it's children's ids *)
let libFuncNames = ["malloc"] 
let unknownLoc = {line = -1; file = ""; byte = 0} (* empty: used to fill the blank *)
let empty_instr_list = ([]: instr list) (* empty: used to fill the blank *)
let empty_stmt_list = ([]: stmt list) (* empty: used to fill the blank *)
let conc = "_"
let prefix_declare = "__CAUT_declare_symbol"
let prefix_load = "__CAUT_load"
let prefix_op = "__CAUT_apply_op"
let prefix_store = "__CAUT_store_state"
let prefix_branch = "__CAUT_branch"
let prefix_push = "__CAUT_push"
let prefix_pop = "__CAUT_pop"
let prefix_enterfun = "__CAUT_enter_func"
let prefix_exitfun = "__CAUT_exit_func"
let prefix_cleanstack = "__CAUT_clean_stack"
let prefix_handle = "__CAUT_handle"

(***************************** auxiliary functions **********************************)
(** *)
let notCautFunction fn = 
	(* Judge whether the function with its name fn is a CAUT interface function, whose name begins with string "__CAUT_" *)
	not ((fn.[0] = '_') & (fn.[1] = '_') & (fn.[2] = 'C') & (fn.[3] = 'A') & (fn.[4] = 'U') & (fn.[5] = 'T') & (fn.[6] = '_'))
let rec mkStmtsInstrs instrs = match (List.length instrs) with	
	|0 -> []
	|_ ->
		[mkStmtOneInstr (List.hd instrs)] @ mkStmtsInstrs (List.tl instrs)
(*** VARIABLES ***)
let findVarScopedId v fn = (* find whether variable v exists, if it is local, fn is the name of the its function scope. *)
	List.exists(fun x->
		match x with 
			LVarId(lv, lf, lid) -> (* A local variable *)
				(
				match v.vglob with
					true -> false
					| false -> (v.vname = lv.vname) & (fn = lf)
				)
			| GVarId(gv, gid) -> (* A global variable *)
				(
				match v.vglob with
					true -> (v.vname = gv.vname)
					| false -> false
				)
 	)!varIdentityList
let getVarScopedId v fn = (* Get the identity of variable v, if it is local, fn is the name of the its function scope. *)
	let rst = try List.find(fun x->
		match x with 
			LVarId(lv, lf, lid) -> 
				(
				match v.vglob with
					true -> false
					| false -> (v.vname = lv.vname) & (fn = lf)
				)
			| GVarId(gv, gid) ->
				(
				match v.vglob with
					true -> (v.vname = gv.vname)
					| false -> false
				)
 	)!varIdentityList 
	with Not_found -> GVarId(v, -1)	(* This variable doesn't exist, then return a flag to denote its absense. *)
	in
	match rst with
		LVarId(_, _, i) -> i
		| GVarId(_, i) -> i
let getVarScopedIdTotallyUponVariable v = (* must given the full variable info *)
	let rst = try List.find(fun x->
		match x with 
			LVarId(lv, lf, lid) -> (* A local variable *)
				v = lv
			| GVarId(gv, gid) -> (* A global variable *)
				v = gv
 	)!varIdentityList
	with Not_found -> GVarId(v, -1) (* This variable doesn't exist, then return a flag to denote its absense. *)
	in
	match rst with
		LVarId(_, _, i) -> i
		| GVarId(_, i) -> i
let get_var_globalflag var =  match var.vglob with (* return global flag of a variable, 1: if var is static or global, 0: otherwise *)
	true ->
		1
	|false ->
		match var.vstorage with
		Static -> 1
		|_ -> 0
let rec get_mem_globalflag mem = match mem with (* return global flag of a mem, 1: if var is static or global, 0: otherwise *)
	Lval (lhost, offset) -> (match lhost with
		Var var -> get_var_globalflag var
		|Mem mem_mem -> get_mem_globalflag mem_mem)
	|Const const-> 1 (*raise (Fail "occurance const mem")*)
	|_ -> 0
let get_var_derefflag var = match var.vreferenced with (* return dereference's flag of variable 1: dereferenced 0: otherwise*)
	true -> 1
	|false -> 0
(***FUNCTIONS***)
let findFunc fn_name = (* Find whether the function with its name fn_name exists in funcIdentityList List. *)
	List.exists (fun x ->
		match x with (f, _) -> f = fn_name
	) !funcIdentityList
let getFuncId fn_name = (* Get the function identity with its name specified as fn_name. *) 
	let id = try List.find(fun x ->
			match x with (f, i) ->
				f = fn_name
		) !funcIdentityList
	with Not_found -> ("", -1) in
	match id with (f, i) -> i
let findLibFunc fn_name = (* To know if the function is a lib function *)
	List.exists (fun x -> x=fn_name) libFuncNames
let getFuncSort fn_name = (* To know if the function is known or a lib function or a unkown function *)
	if (findFunc fn_name) then 1 
	else if (findLibFunc fn_name) then 2
	else 0 
(***TYPES***)
let get_const_typename const = "const"^ "_" ^ 
	(match const with
		CInt64 (value, ikind, sop) -> (match ikind with
			|IChar -> "char"
			|ISChar -> "signed_char"
			|IShort -> "short"
			|IInt -> "int"
			|ILong -> "long"
			|ILongLong -> "long_long"
			|IUChar -> "unsigned_char"
			|IUShort -> "unsigned_short"
			|IUInt -> "unsigned_int"
			|IULong -> "unsigned_long"
			|IULongLong -> "unsigned_long_long"
			|IBool -> raise (Fail "occurance: bool")
			)
		|CReal (value, fkind, sop) ->(match fkind with
			|FFloat -> "float"
			|FDouble -> "double"
			|FLongDouble -> "long_double"
			)
		|CStr(_) -> "string"
		|CWStr(_) -> "string"
		|CChr(_) -> "char"
		|CEnum(_) -> raise (Fail "get const type name of Enum")
	)
(***OPERATORS***)
let get_opid expr = match expr with (* return operator's id, -1: no valid operator ; addr_of's id is in 'apply_op'*)
	(* follow the rule in statemgr.h (enum operator_t) *)
	Const const -> -1
	|Lval (lhost,offset) -> 
		(match lhost with
			Var vi -> -1
			| Mem exp -> 21
		)
	|SizeOf typ -> raise (Fail "occurance: get opid Sizeof()")(* have changed to constant in simplify.ml *)
	|SizeOfE exp -> raise (Fail "occurance: get opid SizeofE()")(* have changed to constant in simplify.ml *)
	|SizeOfStr str -> raise (Fail "occurance: get opid SizeofStr()")(* have changed to constant in simplify.ml *)
	|AlignOf typ -> raise (Fail "occurance: get opid AlignOf()")(* have changed to constant in simplify.ml *)
	|AlignOfE exp -> raise (Fail "occurance: get opid AlignOfE()")(* have changed to constant in simplify.ml *)
	|UnOp (unop, exp1, typ) -> 
		(match unop with
			Neg-> 1 (* 	1 Unary minus	*)
			|BNot->17	(* 	2 Bitwise complement (~)	*)
			|LNot->7	(* 	3 Logical Not (!)	*)
		)
	|BinOp (binop, exp1, exp2, typ) ->
		(match binop with
			PlusA->0 	(*	4 arithmetic +	*)
			|PlusPI->0 	(*	5 pointer + integer	*)
			|IndexPI->0	(*	6 pointer + integer but only when it arises from an expression e[i] when e is a pointer and not an array. *)
			|MinusA->1 	(*	7 arithmetic -	*)
			|MinusPI->1 	(*	8 pointer - integer	*)
			|MinusPP->1 	(*	9 pointer - pointer	*)
			|Mult->2	(* 10 * *)
			|Div->3 	(*	11 /	*)
			|Mod->4 	(*	12 %	*)
			|Shiftlt->18(*	13 shift left	*)
			|Shiftrt->19(*	14 shift right	*)
			|Lt->9 	(*	15 < (arithmetic comparison)	*)
			|Gt->8	(*	16 > (arithmetic comparison)	*)
			|Le->12	(*	17 <= (arithmetic comparison)	*)
			|Ge->11	(*	18 >= (arithmetic comparison)	*)
			|Eq->10	(*	19 == (arithmetic comparison)	*)
			|Ne->13	(*	20 != (arithmetic comparison)	*)
			|BAnd->14	(*	21 bitwise and	*)
			|BXor->16	(*	22 exclusive-or	*)
			|BOr->15(*	23 inclusive-or	*)
			|LAnd->5(*	24 logical and. *)
			|LOr->6	(*	25 logical or. *)
		)
	|CastE  (typ, exp1) ->23	(* 27 *)
	|AddrOf lval ->20	(* 26 *)
	|StartOf lval ->raise (Fail "occurance: get opid StartOf")(* have changed in simplify.ml *)

let replace_return_exp expopt loc curr_func = (* replace_return_exp: if return has a complex expression, then make a replacement *)
	match expopt with
		Some(exp) -> 
			(match exp with
				|SizeOf _ |SizeOfE _ |SizeOfStr _ |AlignOf _ |AlignOfE _ -> 
					E.s (bug "occurance: apply Sizeof()")(* have changed to constant in simplify.ml *)
				|Const _ -> (None,[])
				|Lval (Var v,_) -> (None,[])
				|_ ->
					let v = makeTempVar curr_func (typeOf exp) in
					let v_o = Some(v) in
					let set_instrs = [Set((Var(v),NoOffset), exp, loc)] in
					(v_o, set_instrs)
			)
		|None -> (None,[])
(***LOOP STRUCTURES***)
let rec updateChildrenList (childrenList: int list ref) = (function st ->
	childrenList := !childrenList @ [st.sid];
	match st.skind with
		| If (exp, block1, block2, location) -> 
			List.iter (updateChildrenList childrenList) block1.bstmts;
			List.iter (updateChildrenList childrenList) block2.bstmts;
		| Loop (block,location,st_o1,st_o2) ->
			List.iter (updateChildrenList childrenList) block.bstmts;
		| Block block->
			List.iter (updateChildrenList childrenList) block.bstmts;
		| TryFinally (block1, block2, location) ->
			List.iter (updateChildrenList childrenList) block1.bstmts;
			List.iter (updateChildrenList childrenList) block2.bstmts;
		| TryExcept (block1, (instrs, exp), block2, location) ->
			List.iter (updateChildrenList childrenList) block1.bstmts;
			List.iter (updateChildrenList childrenList) block2.bstmts;
		|_->();)
let findParentLoop childId =  (* find the child's direct parent loopid, if there's no parent loop then return -1 *)
	let parentLoop = ref ([]:loopIdentity list) in
	List.iter (function loopId -> 
		if (List.exists (function id -> id = childId) loopId.children) then parentLoop := !parentLoop @ [loopId];
	) !loopIdentityList;
	let smallestLoop = try ref (List.hd !parentLoop) with Failure "hd" -> ref {self = -1;children = [];}in
	List.iter (function x->if (List.length x.children)< (List.length !smallestLoop.children) then smallestLoop := x; ) !parentLoop;
	!smallestLoop.self
let findAllParentLoop (childId:int) (parentLoops:int list ref)= 
	(List.iter (function loopId -> 
		if (List.exists (function id -> id = childId) loopId.children) then parentLoops := !parentLoops @ [loopId.self];
	) !loopIdentityList;)
let rec checkGotoOut (loopId:int) (isGotoOut:int ref) = function stmt1 ->  (* only check the first level of gotos *)
	(* isGotoOut = 	-2:means if is not in a loop or there's no goto in if branch;
					-1:means goto's label is not in any of loops;
					0 :means goto's label is not going out
					1 :means goto's label is outside a loop and in another loop *)
	(match stmt1.skind with
		|Goto(s2,l2) ->
			let s2parentIds = ref ([]:int list) in
			findAllParentLoop (!s2).sid s2parentIds;
			let len = (List.length !s2parentIds) in
			(*print_int(len);*)
			if ( not(len = 0) ) then begin
				(* notion: if there's a goto is outside then the branch should be classified as going outside *)
				if not(List.exists (function p-> p=loopId) !s2parentIds) then begin isGotoOut := 1; end
				(* notion: if there's a goto is outside then we don't bother to check if other's goto is in a loop *)
				else begin if not(!isGotoOut = 1 || !isGotoOut = -1) then isGotoOut := 0; end
				(*isGotoOut := (List.nth !s2parentIds (len-1));*)(* change isGotoOut to represent the goto's label is in what loop *)
			end else begin if not(!isGotoOut = 1) then isGotoOut := (-1); end
		| If (exp, block1, block2, loc1) -> 
			ignore(checkLoopExit block1 stmt1.sid loopId isGotoOut);
			ignore(checkLoopExit block2 stmt1.sid loopId isGotoOut);
		| Switch (exp, block1, stmts, loc1) -> 
			E.s (bug "appear switch statement : %a. plz call --docautcfgbra before --docautinin" d_stmt stmt1)
		| Loop (block1, _, _, _) | Block (block1)->
			ignore(checkLoopExit block1 stmt1.sid loopId isGotoOut); 
		| _ -> ();
	)
and checkOut (loopId:int) (isOut:int ref) = function stmt1 ->
	(match stmt1.skind with 
		| Return (exp_opt, loc)-> isOut := -1
		| _ -> checkGotoOut loopId isOut stmt1
	)	
and checkLoopExit block1 (ifId:int) (loopId:int) (isGotoOut : int ref) = 
	(* 	if there's a goto's destination is outside the current loop(including the situation that not in any loop) then return 1 
		else if there's a 'return' statement then return 1
		else return 0 (because other operators like "break" have been replaced by goto)
		********return -2 when the if-else structure is not found*)
	let process = 
		if loopId = (-1) then isGotoOut := 0
		else (List.iter (checkOut loopId isGotoOut) block1.bstmts)
	in
	()

let scopeCursor = ref 1
let localCursor = ref 0
let glblCursor = ref 0
	
(************************************** VISITORS ***********************************)
(* A visitor to allocate identities to all the variables/functions, either global or local(static) *)
class varIdentityVisitor = object(self) 
	inherit nopCilVisitor
	
	method vglob gb = (*Visit all the global variables.*)
		match gb with 
		GVar(v, _, _) -> (*global variable.*)  
			ignore(if not ( List.exists (fun x -> match x with GVarId(xv , xc) -> xv=v |_ -> false) !varIdentityList) then begin
				varIdentityList := !varIdentityList @ [GVarId(v, (!glblCursor))];
				glblCursor := (!glblCursor) + 1;
			end);
			DoChildren
		| GVarDecl (v, _) -> (*Variable declartion, which may contains function declaration.*)
				ignore(match v.vtype with
					TFun _ -> ()
					| _ -> (*is a variable type, not function, then it's supposed to push into list.*)
						if not ( List.exists (fun x -> match x with GVarId(xv , xc) -> xv=v |_ -> false) !varIdentityList) then begin
							varIdentityList := !varIdentityList @ [GVarId(v, (!glblCursor))];
							glblCursor := (!glblCursor) + 1;
						end);
			DoChildren
		| _ -> DoChildren

	method vfunc fc = (*Visit each function definition to get all the local variables and generate function id*)
		funcIdentityList := !funcIdentityList @ [(fc.svar.vname, (!scopeCursor) - 1)];
		localCursor := 0;
		scopeCursor := (!scopeCursor) + 1; 
		let varList = fc.sformals @ fc.slocals in (*fetch all the local variables of this function call.*)
		List.iter (fun x -> 
			(*Add the variable into the global variable list.*)
			varIdentityList := !varIdentityList @ [LVarId(x, fc.svar.vname, (!localCursor))];
			localCursor := (!localCursor) + 1;
		) varList;
		DoChildren
end

(* A visitor to allocate identities to all the loops *)
class loopIdentityVisitor = object(self) (* generate loopIdentity *)
	inherit nopCilVisitor 
	method vstmt st = match st.skind with
		|Loop(block1,loc1,st1,st2) ->
			let childrenList = ref ([]:int list) in
			List.iter (updateChildrenList childrenList) block1.bstmts;
			loopIdentityList := !loopIdentityList @ [{self = st.sid; children = !childrenList}];
			DoChildren
		|_ -> 
			(*print_string "\nloopIndentityVisitor:"; print_int st.sid; print_string "\n";*)
			DoChildren
end

(* functions: 	(1) the actions that done in specific locations 
				(2) corresponding to the interfaces that needed to insert
				(3) every function return an instruction list, or a statement list *)
let rec load expr loc curr_fun = 
	match expr with (* load: return 'instr list' *)
		Const const -> 
			let fun_name = prefix_load ^ conc ^(get_const_typename const) in
			let load_fun = emptyFunction fun_name in
			[Call(None,Lval((Var(load_fun.svar)),NoOffset),[expr],loc)]
		|Lval lval -> load_lval lval loc curr_fun
		|SizeOf typ -> raise (Fail "occurance: load Sizeof()") (* have changed to constant in simplify.ml *)
		|SizeOfE exp -> raise (Fail "occurance: load SizeofE()") (* have changed to constant in simplify.ml *)
		|SizeOfStr str -> raise (Fail "occurance: load SizeofStr()") (* have changed to constant in simplify.ml *)
		|AlignOf typ -> raise (Fail "occurance: load AlignOf()") (* have changed to constant in simplify.ml *)
		|AlignOfE exp -> raise (Fail "occurance: load AlignOfE()") (* have changed to constant in simplify.ml *)
		|UnOp (unop, exp1, typ) -> 
			(match unop with 
			| Neg -> load (integer 0) loc curr_fun
			| _ ->[])@
			(load exp1 loc curr_fun)
		|BinOp (binop, exp1, exp2, typ) ->
			let instrs1 = load exp1 loc curr_fun in
			let instrs2 = load exp2 loc curr_fun in
			instrs1 @ instrs2
		|CastE  (typ, exp1) -> 
			let instrs1 = load exp1 loc curr_fun in
			let instrs2 = load_typ typ loc in
			instrs1 @ instrs2
		|AddrOf lval -> load_lval lval loc curr_fun
		|StartOf lval -> load_lval lval loc curr_fun 
and load_lval lval loc curr_fun = (* load: return 'instr list' *)
	let fun_name = prefix_load ^ conc ^ "symbol" in
	let load_fun = emptyFunction fun_name in
	(match lval with
		(Var var,_) ->
			let scopeid = integer (getVarScopedId var curr_fun.svar.vname) in (* input argument *)
			let global = integer (get_var_globalflag var) in (* input argument *)
			let typeid = integer (Cauttyps.get_typeid var.vtype) in (* input argument *)
			[Call(None,Lval((Var(load_fun.svar)),NoOffset),[scopeid;global;typeid],loc)]
		|(Mem mem,_) -> 
			let var = match mem with 
				|Lval (lhost,offset) ->
					(match lhost with Var(v) -> v
						|_ ->
							E.log "!!!!Error : load Mem -> not *Var";
							(makeVarinfo true "nomean" (TVoid []))
					)
				|_ -> 
					E.log "!!!Error : load Mem -> not *Var" ;
					(makeVarinfo true "nomean" (TVoid [])) in
			let scopeid = integer (getVarScopedId var curr_fun.svar.vname) in (* input argument *)
			let global = integer (get_mem_globalflag mem) in (* input argument *)
			let typeid = integer (-1) in (* input argument *)
			[Call(None,Lval((Var(load_fun.svar)),NoOffset),[scopeid;global;typeid],loc)]
	)
and load_typ typ loc = (* load: return 'instr list' *)
	let fun_name = prefix_load ^ conc ^ "type" in
	let load_fun = emptyFunction fun_name in
	let typeid = integer (Cauttyps.get_typeid typ) in
	[Call(None,Lval((Var(load_fun.svar)),NoOffset),[typeid],loc)]
			
let rec apply_op expr loc = (* apply_op: return 'instr list' *)
	let fun_name = prefix_op in
	let app_fun = emptyFunction fun_name in
	match expr with
		Const const -> []
		|SizeOf typ -> raise (Fail "occurance: apply Sizeof()")
		|SizeOfE exp -> raise (Fail "occurance: apply SizeofE()")
		|SizeOfStr str -> raise (Fail "occurance: apply SizeofStr()")
		|AlignOf typ -> raise (Fail "occurance: apply AlignOf()")
		|AlignOfE exp -> raise (Fail "occurance: apply AlignOfE()")
		|_ -> 
			let opid = get_opid expr in
			(match opid with 
				-1 -> []
				|_ -> [Call(None, Lval((Var(app_fun.svar)),NoOffset),[(integer opid)], loc)])
let store lval loc  curr_fun = (* store: return 'instr list' *)
	let fun_name = prefix_store in
	let store_fun = emptyFunction fun_name in
	let (var,deref) = (match lval with
		|(Var v, _) -> (v, integer 0)
		|(Mem mem, _) ->
			(match mem with 
					|Lval (lhost,offset) ->
						(match lhost with Var(v) -> (v, integer 1)
							|_ -> raise (Fail "Error : load Mem -> not *Var; simplify.ml miss something.")
						)
					|_ -> raise (Fail "Error : load Mem -> not *Var; simplify.ml miss something.")
			)
	)in
	let scopeid = integer (getVarScopedId var curr_fun.svar.vname) in (* input argument *)
	let typeid = integer (Cauttyps.get_typeid var.vtype) in (* input argument *)
	let global = integer (get_var_globalflag var) in (* input argument *)
	[Call(None,Lval((Var(store_fun.svar)),NoOffset),[scopeid;typeid;global;deref;(mkAddrOf (Var var, NoOffset))],loc)]
let branch choice func brid1 loc loopexit = (* branch: return 'instr list' *)
	let fun_name = prefix_branch in
	let load_fun = emptyFunction fun_name in
	let funid1 = getFuncId func.svar.vname in
	[Call(None,Lval((Var(load_fun.svar)),NoOffset),[integer funid1; integer brid1; integer choice(*; integer loopexit*)],loc)]
let declare (func : fundec) : instr list = (*Insert the declare symbol interface.*)
	let fn_name = func.svar.vname in (*get the function name, which is used to find its variables in global list.*)
	let varList = func.sformals @ func.slocals in (*fetch all the local variables of this function call.*)
	let decl_list = ref ([] : instr list) in
	List.iter (fun x -> 
		let scopedId = getVarScopedId x fn_name in
		let declFun = emptyFunction prefix_declare in
		let varType = Cauttyps.get_typeid x.vtype in
		let args = [Cil.integer scopedId] @ [Cil.integer 0] @ [Cil.integer varType]in
		let iDecl = Call(None, Lval(Var(declFun.svar), NoOffset), args, !currentLoc) in
		decl_list := !decl_list @ [iDecl];
	) varList;
	!decl_list
let pop (func : fundec) : instr list = 
	let fn_name = func.svar.vname in
	let formals = func.sformals in
	if not (List.length formals = 0) then begin
		let pop_instrs = ref ([] : instr list) in
		List.iter(fun x ->
			let scopedId = Cil.integer (getVarScopedId x fn_name) in
			let typeValue = Cil.integer (Cauttyps.get_typeid x.vtype) in
			let popFun = emptyFunction prefix_pop in
			let iPop = Call(None, Lval(Var(popFun.svar), NoOffset), [scopedId; typeValue; AddrOf(Var(x), NoOffset)], !currentLoc) in
			pop_instrs := !pop_instrs @ [iPop]
		)formals;
		!pop_instrs
	end else []
let clean_stack : instr list = 
	let cleanFun = emptyFunction prefix_cleanstack in
	let iClean = Call(None, Lval(Var(cleanFun.svar), NoOffset), [], !currentLoc) in
	[iClean]
let enter_func (fname:string): instr list =
	let enterFun = emptyFunction prefix_enterfun in
	let funcId = Cil.integer (getFuncId fname) in
	let iEnter = Call(None, Lval(Var(enterFun.svar), NoOffset), [funcId], !currentLoc) in
	[iEnter]
let exit_func : instr list =
	let exitFunc = emptyFunction prefix_exitfun in
	let iExit = Call(None, Lval(Var(exitFunc.svar), NoOffset), [], !currentLoc) in
	[iExit]
let push (fname : string) (args : exp list): instr list =
	if (List.length args <> 0) then begin(*whether this function call has arguments*)
	let instrs = ref([]:instr list) in
	List.iter (fun x ->
		(match x with
			Const xc -> 
				let pushFun = emptyFunction (prefix_push ^ conc ^(get_const_typename xc)) in
				let iPush = Call(None, Lval(Var(pushFun.svar), NoOffset), [x], !currentLoc) in
				instrs := [iPush] @ !instrs 
			| Lval(xlh, _) ->
				(match xlh with
					Var(xv) -> 
						let pushFun = emptyFunction (prefix_push^conc^"symbol") in
						let scopedId = Cil.integer (getVarScopedId xv fname) in	(*get this variaible scopedId*)
						let isGlbl = Cil.integer (get_var_globalflag xv) in
						let typeValue = Cil.integer (Cauttyps.get_typeid xv.vtype) in
						let iPush = Call(None, Lval(Var(pushFun.svar), NoOffset), [scopedId; isGlbl; typeValue], !currentLoc) in
						instrs := [iPush] @ !instrs 
				| _ ->())
			| _ -> ())
	) args;
	!instrs
	end else []
let handle_func (ret: lval option) (curr_fun_name	: string) (called_fun_name : string) : instr list =
	match called_fun_name with
	| "malloc" ->
		(match ret with Some (v,off) ->
			let handleFun = emptyFunction (prefix_handle ^ conc ^ "malloc") in
			(match v with Var(vi) ->
				let scopedId = integer (getVarScopedId vi curr_fun_name) in
				let isGlbl = integer (get_var_globalflag vi) in
				[Call(None, Lval(Var(handleFun.svar),NoOffset), [scopedId; isGlbl; Lval (v,off)],!currentLoc)]
			|_ -> [])
		|None -> [])
	| _ -> []
(* insertHelper: A visitor run all the functions in the userdefined places *)
class insertHelper = object(self) 
	val mutable return_var = ([]:varinfo option list)
	method assignment_before exp loc curr_fun = 
		(* add here: codes that wants to insert before an assignment *)
		load exp loc curr_fun
		@ (apply_op exp loc)
	method assignment_after lval loc curr_fun = 
		(* add here: codes that wants to insert after an assignment *)
		store lval loc curr_fun
	method call_before (curr_fun : fundec) (called_fn_name:string) (args : exp list)= 
		(* add here: codes that wants to insert before a function call *)
		let fun_sort = 	getFuncSort called_fn_name in
		match fun_sort with
			| 0 -> if isLog = 1 then ignore(E.log "call_after : func sort:0");[]
			| 1 -> if isLog = 1 then ignore(E.log "call_after : func sort:1");(push curr_fun.svar.vname args) @ (enter_func called_fn_name)
			| 2 -> if isLog = 1 then ignore(E.log "call_after : func sort:2");(push curr_fun.svar.vname args)
			| _ -> []
	method call_after (opt : lval option) loc curr_fun (called_fn_name:string)= 
		(* add here: codes that wants to insert after a function call *)
		let func_sort = getFuncSort called_fn_name in
		match func_sort with
		| 0 -> if isLog = 1 then ignore(E.log "call_after : func sort:0");
			(match opt with 
				| Some(lval) -> store lval loc curr_fun 
				| _ -> empty_instr_list)
		| 1 -> 
			if isLog = 1 then ignore(E.log "call_after : func sort:1");
			exit_func
			@ (match opt with 
				| Some(lval) -> store lval loc curr_fun 
				| _ -> empty_instr_list)
			@ (clean_stack)
		| 2 -> if isLog = 1 then ignore(E.log "call_after : func sort:2");
			handle_func opt curr_fun.svar.vname called_fn_name
		| _ -> []
	method if_true_head exp loc curr_fun brid (loopexit:int) = 
		(* add here: codes that wants to insert in head of an if-true-branch *)
		mkStmtsInstrs (load exp loc curr_fun)
		@ mkStmtsInstrs (apply_op exp loc)
		@ mkStmtsInstrs (branch 1 curr_fun brid loc loopexit)
	method if_false_head exp loc curr_fun brid (loopexit:int) = 
		(* add here: codes that wants to insert in head of an if-false-branch *)
		mkStmtsInstrs (load exp loc curr_fun)
		@ mkStmtsInstrs (apply_op exp loc)
		@ mkStmtsInstrs (branch 0 curr_fun brid loc loopexit)
	method function_head (func : fundec)= 
		(* add here: codes that wants to insert in head of a function body *)
		mkStmtsInstrs (declare func)
		@ mkStmtsInstrs (pop func)
		@ mkStmtsInstrs (clean_stack)
	method return_before expopt loc curr_fun = 
		(* add here: codes that wants to insert before return *)
		let (v_o, instrs) = replace_return_exp expopt loc curr_fun in
		return_var <- [v_o];
		match List.length instrs with
			0 -> []
			|_ ->
				let set_ins = List.hd instrs in
				let set_lval = (match set_ins with Set (lval,exp,loc) -> lval |_-> raise (Fail "error: replace_return_exp") ) in
				let set_exp = (match set_ins with Set (lval,exp,loc) -> exp |_-> raise (Fail "error: replace_return_exp") ) in
				(self#assignment_before set_exp loc curr_fun) @ instrs @ (self#assignment_after set_lval loc curr_fun)
	method return_replacement loc = 
		match List.hd return_var with
			Some v -> Some (mkStmt (Return(Some(Lval((Var v, NoOffset))),loc)))
			|None -> None
end

(* Visitor: traversing CIL tree and calling methods in insertHelper to insert codes *)
class lsabProcessVisitor helper = object(self) 
	inherit nopCilVisitor
	val mutable funcd_list = ([] : fundec list) (* recording function list to know current function *)
	method vstmt st = match st.skind with
		Instr is -> 
			DoChildren
		|Return (expopt, loc) -> 
			let curr_fun = List.hd funcd_list in
			let before_return_instrs = helper#return_before expopt loc curr_fun in
			self#queueInstr before_return_instrs;
			let new_return = helper#return_replacement loc in
			(match new_return with
				Some stmt -> ChangeTo(stmt)
				|None -> DoChildren)
		|Goto (stmt, location) -> 
			DoChildren
		|Break loc -> 
			E.s (bug "should not have break here : plz call --docautdocfgbra")
		|Continue loc -> 
			E.s (bug "should not have Continue here : plz call --docautdocfgbra")
		|If (e,b1,b2,loc) ->
			(* check if the branch is added by cautfront. if it is then do nothing *)
			if (List.exists (function x -> if (x = st.sid) then true else false) (!bra_sids)) then begin
				let curr_fun = List.hd funcd_list in
				let loopId = findParentLoop st.sid in
				(* true branch *)
				let isGotoOut = ref (-2) in
				ignore(checkLoopExit b1 st.sid loopId isGotoOut);
				let isExitLoop1 = if ((!isGotoOut = 1)||(!isGotoOut=(-1))) then 1 else 0 in
				let true_stmts = helper#if_true_head e loc curr_fun (st.sid+1) isExitLoop1 in
				b1.bstmts <- true_stmts @ b1.bstmts;
				(* false branch *)
				isGotoOut := (-2);
				ignore(checkLoopExit b2 st.sid loopId isGotoOut);
				let isExitLoop2 = if ((!isGotoOut = 1)||(!isGotoOut=(-1))) then 1 else 0 in
				let false_stmts = helper#if_false_head e loc curr_fun (-(st.sid+1)) isExitLoop2 in
				b2.bstmts <- false_stmts @ b2.bstmts;
				(* *)
				DoChildren
			end else begin DoChildren end
		|Switch (exp, block, stmts, location) -> 
			E.s (bug "should not have Switch here : plz call --docautdocfgbra")
		|Loop (block, location, stmt1opt, stmt2opt) -> 
			DoChildren
		|Block b -> 
			DoChildren
		|TryFinally (b1, b2, loc) -> 
			DoChildren
		|TryExcept (b1, (instrs, exp), b, loc) -> 
			DoChildren

	method vinst is =
		let curr_fun = List.hd funcd_list in 
		match is with
			Set (lval, exp, loc) ->
				let before_instrs = helper#assignment_before exp loc curr_fun in
				let after_instrs  = helper#assignment_after lval loc curr_fun in
				let final_instrs  = before_instrs @ [is] @ after_instrs in
				ChangeTo(final_instrs)
			|Call (opt, exp, exp_list, loc)->
				let requireInterface = match exp with (*whether this function call is a caut interface.*)
					Lval(Var(v), NoOffset) -> (notCautFunction v.vname)
					| _ -> false
				in
				let fname = match exp with Lval(Var v, _) -> v.vname |_ -> E.s (bug "get function name : error") in
				if(requireInterface) then begin
					let before_instrs = helper#call_before curr_fun fname exp_list in
					let after_instrs  = helper#call_after opt loc curr_fun fname in
					let final_instrs  = before_instrs @ [is] @ after_instrs in
					ChangeTo(final_instrs)
				end else SkipChildren
			|_->
				DoChildren

	method vfunc func = 
		(* maintain the function list ! check if this is right way to get the current function *)
		funcd_list <- [func];
		let head_of_function_stmts = helper#function_head func in
		func.sbody.bstmts <- head_of_function_stmts @ func.sbody.bstmts;
		DoChildren
end

(*************************************** CAUT_INPUT TRANSFORMATIONS *************************************)
let caut_input_fname = "__CAUT_INPUT"
let final_input_fname = "get_input"
class getInputVisitor = object(self) (* Visitor: seek CAUT_INPUT functions.. *)
	inherit nopCilVisitor
	val mutable argument_list = ([]:(string*int*int) list) (* string:name ; int:is_global ; int:scope_id *)
	method vinst inst = 
	match inst with 	
	| Call(l_opt,exp1,exp_list,loc) -> 
	(match exp1 with
		| Lval(lhost, offs) ->
		(match lhost with 
			| Var(fvar) ->
			(if fvar.vname = caut_input_fname 
				then begin
					List.iter (
						fun x -> 
							(match x with Lval(Var(argument_v),offset) ->
								let tmp_id = getVarScopedIdTotallyUponVariable argument_v in
								let tmp_name = argument_v.vname in
								let tmp_glb = get_var_globalflag argument_v in
								ignore(argument_list = List.append argument_list [(tmp_name,tmp_glb,tmp_id)]);
								| _ -> ();
							)
					) exp_list;
					let inputFun = emptyFunction final_input_fname in
					(match (List.length argument_list) with 1 ->
						(* Call: &x \ is.global \ scope.id *)
						let tmp = List.hd argument_list in
						(match tmp with (name,is_global,scope_id) ->
							let args = [(mkAddrOf (lhost,offs));Cil.integer is_global;Cil.integer scope_id] in
							let finalInstr = [Call(None,Lval((Var(inputFun.svar)),NoOffset),[],loc)] in
							ignore(argument_list = []);
							ChangeTo(finalInstr)
						)
						|_ ->
							ignore(argument_list = []);
							DoChildren;
					)
				end else begin DoChildren end
			)
			| _ -> DoChildren
		)
		| _ -> DoChildren
	)
	| _ -> DoChildren
end

let genGlblDeclFunc (varList:varIdentity list): fundec = (* generate __caut_declare_global function *)
	let declareGlobalsF = emptyFunction "__CAUT_declare_global" in
	setFunctionTypeMakeFormals declareGlobalsF (TFun(TVoid [], Some [], false, []));
	
	let declareSymbolF = emptyFunction "__CAUT_declare_symbol" in
	setFunctionTypeMakeFormals 
		declareSymbolF 
		(TFun(TVoid [], Some [("scopedvarid", TInt(IInt,[]),[]);("global", TInt(IInt,[]),[]);("type",TInt(IInt,[]),[])], false, []));
	
	List.iter(fun x ->
		match x with GVarId(v, i) -> 
			declareGlobalsF.sbody.bstmts <- declareGlobalsF.sbody.bstmts @
			[mkStmtOneInstr (Call (None, 
			Lval(Var declareSymbolF.svar, NoOffset), 
			[integer i; integer 1; integer (Cauttyps.get_typeid v.vtype)],
			!currentLoc ))]
		| _ -> ()
	)varList;
	
	declareGlobalsF.sbody.bstmts <- declareGlobalsF.sbody.bstmts @ [mkStmt (Return (None, !currentLoc))];
	declareGlobalsF

let doGlobalGenVarId (g : global) =
	let varIdVst = new varIdentityVisitor in
	match g with
	| GFun (fd, loc) ->
		if (notCautFunction fd.svar.vname) then begin
			if isLog = 1 then ignore(E.log "gen variable identity\n");
			ignore(visitCilGlobal varIdVst g)
		end
	| _ ->
		ignore(visitCilGlobal varIdVst g)

let doGlobalGenLoopId (g : global) = 
	let loopIdVst = new loopIdentityVisitor in
	match g with
	| GFun (fd, loc) ->
		if (notCautFunction fd.svar.vname) then begin
			if isLog = 1 then ignore(E.log "gen loop identity list\n");
			(* gen loop identity list, to check if goto's destination is outside a loop *)
			ignore(visitCilGlobal loopIdVst g)
		end
	| _ ->
		if isLog = 1 then ignore(E.log "gen loop identity list\n");
		(* gen loop identity list, to check if goto's destination is outside a loop *)
		ignore(visitCilGlobal loopIdVst g)

let doGlobalInsertInterfaces (g : global) = 
	let helper:insertHelper = new insertHelper in (* for lsabProcess *)
	let insertVst = new lsabProcessVisitor helper in
	match g with
	| GFun (fd, loc) ->
		if (notCautFunction fd.svar.vname) then begin
			if isLog = 1 then ignore(E.log "insert runtime interfaces\n");
			ignore(visitCilGlobal insertVst g) (* load/store/apply/branch interfaces *)
		end
	| _ ->
		if isLog = 1 then ignore(E.log "insert runtime interfaces\n");
		ignore(visitCilGlobal insertVst g) (* load/store/apply/branch interfaces *)

let rename_main_func (g : global) =
	match g with
	| GFun (fd, loc) ->
		if (fd.svar.vname = "main") then begin
			fd.svar.vname <- "test_main"
		end
	| _ ->
		()

let find_funcdec (name:string) (f:file) =
	let fd = ref (emptyFunction name) in
	(iterGlobals f (fun g -> match g with 
		|GFun( fun_dec, loc) -> (if fun_dec.svar.vname = name then fd:=fun_dec else ())
		|_ -> ()));
	!fd
	
let testDrivenFunc (test_unit_name : string) (f:file) : fundec = 
	let unit_name = if test_unit_name = "main" then "test_main" else test_unit_name in
	let mainFunc = emptyFunction "main" in
	
	let init_loggerF = emptyFunction ("__CAUT_init_logger") in
	let init_loggerC = Call(None,  Lval( Var init_loggerF.svar, NoOffset), [], !currentLoc ) in
	let register_typesF = emptyFunction ("__CAUT_register_types") in
	let register_typesC = Call(None,  Lval( Var register_typesF.svar, NoOffset), [], !currentLoc ) in
	
	let unitF = find_funcdec unit_name f in
	let unitArgs = ref ([] : exp list) in
	(for i = 0 to ((List.length unitF.sformals) - 1)
	do unitArgs := !unitArgs @ [Lval(Var(makeTempVar mainFunc (List.nth unitF.sformals i).vtype), NoOffset)]
	done); 
	let unitC = Call(None,  Lval(Var unitF.svar, NoOffset), !unitArgs, !currentLoc ) in
	
	let exit_loggerF = emptyFunction ("__CAUT_exit_logger") in
	let exit_loggerC = Call(None,  Lval( Var exit_loggerF.svar, NoOffset), [], !currentLoc ) in
	
	let calls = 
		[init_loggerC; register_typesC;] @ (enter_func unit_name) @ [unitC] @ (exit_func) @ [exit_loggerC] in
	mainFunc.sbody.bstmts <- mainFunc.sbody.bstmts @ [mkStmt (Instr(calls))];
	mainFunc

let rec getLibsFromName (name:string) (from: int) =
	try 
		let next_index = String.index_from name from ' ' in
		[String.sub name from (next_index-from)] @ (getLibsFromName name next_index)
	with Not_found -> 
		(
			if (String.length name) != 0 then [name]
			else []
		)
	
let includeLibsHead (f:file) (libs_name:string) : unit =
	let libs = getLibsFromName libs_name 0 in
	List.iter (fun x -> 
		f.globals <- [GText("#include <" ^ x ^ ".h>")] @ f.globals) libs	
	
(*************************************** default process ************************************************)
let default_process (f: file) (test_unit_name : string) (libs_name : string) = 
	iterGlobals f rename_main_func;
	
	(* search branch list *)
	iterGlobals f sbl_doGlobal;
	if isLog = 1 then ignore(E.log "\n%d\nbra_sids:\n" (List.length (!bra_sids)));
	if isLog = 1 then (List.iter (function x-> E.log "%d " x;) (!bra_sids));
	if isLog = 1 then ignore(E.log "\n");
	(**********************)
	iterGlobals f doGlobalGenVarId;
	iterGlobals f doGlobalGenLoopId;
	if isLog = 2 then ignore (E.log "\nloop Ids:\n"); 
	if isLog = 2 then ignore (
		List.iter (fun x-> 
			ignore(E.log "loop_id = %d children={" x.self);
			List.iter (fun y ->
				ignore(E.log "%d;" y)
			) x.children;
			ignore(E.log "\n")
		) !loopIdentityList);
	iterGlobals f doGlobalInsertInterfaces;	
	f.globals <- [GText("#include \"caut.h\"")] @ f.globals;
	f.globals <- f.globals @ [GFun((testDrivenFunc test_unit_name f),!currentLoc)];
	ignore(E.log "%s\n" libs_name);
	includeLibsHead f libs_name
	(*;
	if isLog = 1 then ignore(E.log "generate global declarations\n");
	f.globals <- f.globals @ [GFun(genGlblDeclFunc !varIdentityList,!currentLoc)]*) (* global declare interface is redundant *)

let tun = ref "main" 
let libs = ref ""

let feature : featureDescr = 
  { fd_name = "cautinin";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : caut insert interfaces";
    fd_extraopt = [
		("-testunit_inin", Arg.Set_string tun, " set the test unit, default is \"main\"");
		("-lib", Arg.Set_string libs, " set the standard libs used");
	];
    fd_doit = 
    (function (f: file) -> 
      default_process f !tun !libs);
    fd_post_check = true
  }
