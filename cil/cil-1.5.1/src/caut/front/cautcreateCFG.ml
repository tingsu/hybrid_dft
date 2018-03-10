(** cautcreateCFG.ml
	Author: Jiang Siyuan
	Date:2/1/2011
	Description: create CFG and save it in a file
*)

open Pretty
open Cil
open CautSqlite3Interface
module E = Errormsg

(*debug flag *)
let debug_flag = false 

(*branch node id*)
let branch_node_id_list = ref([]:int list)

(* cfgnode type *)
type cfgNode = {
	unit_name : string ; (* the unit name of this node *)
	root : int; (*the id of the root that this node belongs to*)
	id : int;  (*this node's id*)
	succ_list : int list ref; (*this node's successors's ids*)
	prec_list : int list ref; (*this node's previous nodes' ids*)
	prec_choice_list : int list ref; (*the choices' this node's previous nodes take*)
	operand_stack : int list ref; (*the operands in this nodes which are *)
	(*Fang*)
	mutable is_branch : int ;
	cfg_line_number : int;
	cfg_condition_expression : string ;
}

let node_list = ref ([] : cfgNode list)
let root_list = ref ([] : int list)
(* backup node_list root_list *)
let node_list_backup = ref ([] : cfgNode list)
let root_list_backup = ref ([] : int list)

let edge_list = ref ([] : (int * int * int) list)

(* flip choice *)
let flip_choice (c : int) : int =
	if c = 0 then
		1
	else
		0

(* create a cfgnode *)
let createCfgNode (u_name : string) (root_id : int) (i : int) (sl : int list) (pl : int list) (pcl : int list) (os : int list) (cln : int) (cce : string) (isb :int ) : cfgNode =
{
	unit_name = u_name ;
	root = root_id;
	id = i;
	succ_list = ref sl;
	prec_list = ref pl;
	prec_choice_list = ref pcl;
	operand_stack = ref os;
	is_branch = isb ;
	(*Fang*)
	cfg_line_number = cln ;
	cfg_condition_expression = cce ;
}

let nullCfgNode : cfgNode =
{
	unit_name = "";
	root = -1;
	id = -1;
	succ_list = ref [];
	prec_list = ref [];
	prec_choice_list = ref [];
	operand_stack = ref [];
	is_branch = 0;
	(*Fang*)
	cfg_line_number = -1 ;
	cfg_condition_expression = "";
}

(* compare two node's id *)
let cmp_node (tar: int) (ent : cfgNode) =
	if ent.id = tar then true else false

let cmp_edge (tar : (int * int * int)) (ent : (int * int * int)) : bool = 
	let tar_prec = match tar with (prec, next, choice) -> prec in
	let tar_next = match tar with (prec, next, choice) -> next in
	let tar_choice = match tar with (prec, next, choice) -> choice in
	let ent_prec = match ent with (prec, next, choice) -> prec in
	let ent_next = match ent with (prec, next, choice) -> next in
	let ent_choice = match ent with (prec, next, choice) -> choice in
	if debug_flag then
		ignore(E.log "\n cmp edge: edge: %d --%d-> %d with edge: %d --%d-> %d\n" tar_prec tar_choice tar_next ent_prec ent_choice ent_next);
	if (tar_prec = ent_prec) && (tar_next = ent_next) && (tar_choice = ent_choice) then 
		begin
			if debug_flag then
				ignore(E.log "\nShoot!\n");
			true
		end
	else false

(* node's succ_list *)
let addChild (prec_id : int) (child_id : int) (node : cfgNode) : unit = 
	if node.id = prec_id then begin
		node.succ_list := !(node.succ_list) @ [child_id]
	end else ()

(* node's prec_list and prec_choice_list *)
let addParent (child_id : int) (prec_id : int) (prec_choice : int) (node : cfgNode) : unit = 
	if node.id = child_id then begin
		if debug_flag then
		begin
			ignore(E.log "\nnode_id = %d , child_id = %d " node.id child_id);
			ignore(E.log "\npre prec_list length : %d\n" (List.length !(node.prec_list)))
		end;
		node.prec_list := !(node.prec_list) @ [prec_id];
		if debug_flag then
			ignore(E.log "prec_list length : %d\n" (List.length !(node.prec_list)));
		node.prec_choice_list := !(node.prec_choice_list) @ [prec_choice]
	end else ()
	
(*For printf the location of the current statement*)
let rec lineNumber (stmt_ToGetLineNumber:stmt) : int = try (match stmt_ToGetLineNumber.skind with
	| Instr (instrs) ->
		(match (List.hd instrs) with
		|Set (lv, exp, loc) -> loc.line
		|Call (lv, exp, expl, loc) -> loc.line
		|Asm (at, strl, l, l2, l3, loc) -> loc.line
		)
	| Return (e, loc) -> loc.line
	| Goto (r, loc) -> loc.line
	| Break (loc) -> loc.line
	| Continue (loc) -> loc.line
	| If (exp, bl, bl2, loc) -> loc.line
	| Switch (exp, bl, stl, loc) -> loc.line
	| Loop (bl1, loc, s, s2) -> loc.line
	| Block (block) -> 
		lineNumber (List.hd block.bstmts)
	| TryFinally (bl, bl2, loc) -> loc.line
	| TryExcept (bl, a, bl2, loc) -> loc.line) with Failure("hd") -> -1

let logic_operand_id = ref 0
	
(* this function's idea is coverting infix logic expression to postfix logic expression*)
(* logic operator priority : ! > && > || , use 1,2,3,... to tag logic operand , and use -5,-6,-7 to tag LAnd,LOr,LNot*)
let rec getOpList (e : exp) : (int list) =
	match e with
	| BinOp (op, e1, e2, t) -> 
		( match op with
			| LAnd ->
						(getOpList e2) @ (getOpList e1) @ [-5]
			| LOr ->
						(getOpList e2) @ (getOpList e1) @ [-6]
			| _ ->
						logic_operand_id := !logic_operand_id+1; 
						[!logic_operand_id]
				   
		)
	|UnOp (op, e1, t) -> 
		( match op with
			| LNot ->  
						(getOpList e1) @ [-7]
			| _ -> 
						logic_operand_id := !logic_operand_id+1; 
						[!logic_operand_id]
		)
	| _ ->
		logic_operand_id := !logic_operand_id+1; 
		[!logic_operand_id]
	
let getOperators (st: stmt) : (int list) =
	logic_operand_id := 0;
	(* here, we only need consider If stmt, "switch"/"case"/"defalut" have already been removed
	   by --domakeCFG *)
	match st.skind with
	| If (e, bl1, bl2, lc) -> getOpList e
	|_ -> []
	
(*Fang*)
let getExpression (st: stmt) : (string) =
	match st.skind with
		| If(e, bl1, bl2, lc) ->
			let tmp_buff = Buffer.create 20 in
			Buffer.add_string tmp_buff ((sprint 20 ((d_exp () e))));
			Buffer.contents tmp_buff
		| _ -> ""
	
(* u_name : unit_name *)
let rec abstract_CFG (u_name:string) (st : stmt) (prec_id : int) (choice : int ) (root_id : int)  : unit =
	 
	(*get the st's successors*)
	let succs = st.succs in
	if debug_flag then
		ignore(E.log "\nStatment Id: %d, line: %d\n" st.sid (lineNumber st));
	if (List.length st.succs) > 1 (* cfg branch node *)
	then begin
		(* store branch node id *)
		branch_node_id_list := !branch_node_id_list @ [st.sid];

		let new_node = ref nullCfgNode in
		
		if not (List.exists (cmp_node st.sid) !node_list) then begin
			if debug_flag then
			ignore(E.log "\n  !!new node: %d\n" st.sid);
			(* [suting] node_list *)
			(*Fang*)
			new_node := createCfgNode u_name root_id st.sid [] [] [] (getOperators st) (lineNumber st) (getExpression st) 1;
			node_list := !node_list @ [!new_node]
		end else begin
			if debug_flag then
			ignore(E.log "\n  !!exsiting node: %d\n" st.sid);
			new_node := List.find (cmp_node st.sid) !node_list
		end;
		
		if debug_flag then
			ignore(E.log "start finding edge: %d --%d-> %d\n" prec_id choice st.sid);

		if not (List.exists (cmp_edge (prec_id, st.sid, choice)) !edge_list) then begin
			if debug_flag then
				ignore(E.log "\n  !!new edge: %d --%d-> %d\n" prec_id choice st.sid);
			(* [suting]edge_list *)
			edge_list := !edge_list @ [(prec_id, st.sid, choice)];
			List.iter (addChild prec_id st.sid) !node_list;
			List.iter (addParent st.sid prec_id choice) !node_list;
			if debug_flag then
				ignore(E.log "!!!! num = %d " (List.length succs)) ;
			for index = 0 to (List.length succs)-1
			do 	
				if debug_flag then
					ignore(E.log "next node:\n");
				(* new branch node *)
				abstract_CFG u_name (List.nth succs index) (!new_node).id index root_id 
			done
		end
	end 
	(* else for "(List.length st.succs) > 1" *)
	else if ((List.length st.succs) = 1) 
	then begin
		if debug_flag then
		ignore(E.log "\nnode:%d has one successor\n" st.sid);
		if not (List.exists (cmp_edge (prec_id, st.sid, choice)) !edge_list) then begin
			if debug_flag then
				ignore(E.log "\n  !!new edge: %d --%d-> %d\n" prec_id choice st.sid);
			edge_list := !edge_list @ [(prec_id, st.sid, choice)];
			(* skip this sequential node *)
			abstract_CFG u_name (List.hd st.succs) prec_id choice root_id 
		end else ();
	end 
	(* else for "(List.length st.succs) = 1" *)
	else if ((List.length st.succs) = 0)  (* "return" cfg node *)
	then begin
		if debug_flag then
		ignore(E.log "\nnode:%d has no successor\n" st.sid);
		if not (List.exists (cmp_node st.sid) !node_list) then begin
			if debug_flag then
			ignore(E.log "\n  !!new end node: %d\n" st.sid);
			(* [suting]  node_list *)
			(*Fang*)
			let new_node = createCfgNode u_name root_id st.sid [] [] [] [] (lineNumber st) (getExpression st) 0 in
			node_list := !node_list @ [new_node]
		end else ();
		
		if not (List.exists (cmp_edge (prec_id, st.sid, choice)) !edge_list) then begin
			if debug_flag then
			ignore(E.log "\n  !!new edge: %d --%d-> %d\n" prec_id choice st.sid);
			(* [suting]  edge_list *)
			edge_list := !edge_list @ [(prec_id, st.sid, choice)];
			if debug_flag then
			ignore(E.log "\n  length: %d \n" (List.length !node_list));
			List.iter (addParent st.sid prec_id choice) !node_list;
			List.iter (addChild prec_id st.sid) !node_list
		end else ()
	(* end for "(List.length st.succs) = 0" *)
	end else ()
	
let convert_int_list_to_string (node_id_list:int list) : string =
	let tmp_buff = Buffer.create 20 in
	let node_id_list_cnt = (List.length node_id_list) in
	for index=0 to node_id_list_cnt-1 
	do
			Buffer.add_string tmp_buff (string_of_int (List.nth node_id_list index));
			if index < node_id_list_cnt-1 then
				Buffer.add_string tmp_buff ";"
	done
	;
	Buffer.contents tmp_buff
	
let doGlobal (f:file) (file_name:string) (db_helper:cautFrontDatabaseHelper) (db_handler:Sqlite3.db) (g : global) : unit = 
	match g with
	| GFun (fd, loc)->

		(* head stmt *)
		let head_stmt = (List.hd fd.sbody.bstmts) in
		let root_id = head_stmt.sid in

		(*match head_stmt.stmtkind with*)

		(*Fang: store root node*)
		let root_node = createCfgNode fd.svar.vname root_id root_id [] [] [] (getOperators (List.hd fd.sbody.bstmts)) (lineNumber (List.hd fd.sbody.bstmts)) (getExpression (List.hd fd.sbody.bstmts)) 0 in

		node_list := !node_list @ [root_node];
		root_list := !root_list @ [root_id];
		
		(* search root node's successors *)
		let succs = (List.hd fd.sbody.bstmts).succs in
		let succs_num = (List.length succs) in
		(* store branch node id [root] *)
		if succs_num=2 then begin
			(* is branch node *)
			root_node.is_branch <- 1;
			branch_node_id_list := !branch_node_id_list @ [root_id]
		end;

		for index = succs_num-1 downto 0 (* succs_num >=1 , success_num==0 --> no CFG *)
		do 
			if succs_num=2 then begin(* CAUTION : succs[0] --> true branch, succs[1] --> false branch 
						    but after --domakeCFG, it was reversed
						*) 
				abstract_CFG fd.svar.vname (List.nth succs (flip_choice index)) root_id (flip_choice index) root_id 
			end
			else if succs_num=1 then (* index==0 --> sequential *)
				abstract_CFG fd.svar.vname (List.nth succs index) root_id index root_id
		done
		;
		if debug_flag then
			ignore(E.log "\ncreateCFG one func Success\n") ;
		
		
		(* store root cfg node info into db file *)
		let root_list_cnt = (List.length !root_list) in
		if debug_flag then
			E.log "root_list_cnt = %d \n" root_list_cnt ;
		if root_list_cnt > 0 then
			begin
				for index = 0 to root_list_cnt-1
				do 
					ignore (db_helper#insert_caut_tb db_handler db_helper#get_caut_cfg_root_list_tb_name
							["file_name";"unit_name";"root_id"]
							[file_name ; fd.svar.vname ; (string_of_int (List.nth !root_list index)) ]
							)
				done
			end
		;

		(* store cfg nodes info into db file *)
		let node_list_cnt = (List.length !node_list) in
		if debug_flag then
		E.log "node_list_cnt = %d \n" node_list_cnt ;
		if node_list_cnt > 0 then
			begin
				for index = 0 to node_list_cnt-1
				do 
					let cfg_node = List.nth !node_list index in
					let unit_name = cfg_node.unit_name in
					let root_id = cfg_node.root in
					let node_id = cfg_node.id in

					(*Fang*)
					let cfg_line_number = cfg_node.cfg_line_number in
					let cfg_condition_expression = cfg_node.cfg_condition_expression in
					let is_branch_node = cfg_node.is_branch in
					let succ_list = convert_int_list_to_string !(cfg_node.succ_list) in
					let prec_list = convert_int_list_to_string !(cfg_node.prec_list) in
					let prec_choice_list = convert_int_list_to_string !(cfg_node.prec_choice_list) in
					let operand_stack = convert_int_list_to_string !(cfg_node.operand_stack) in
					(*Fang*)
					ignore (db_helper#insert_caut_tb db_handler db_helper#get_caut_cfg_node_list_tb_name
							["file_name";"unit_name";"root_id";"node_id";"succ_list";"prec_list";"prec_choice_list";"operand_stack" ; "line_no" ;"branch_expr";"is_branch";"cond_id_list"]
							[file_name ; unit_name  ; (string_of_int root_id) ; (string_of_int node_id) ; succ_list ; 
							prec_list ; prec_choice_list ; operand_stack ; (string_of_int cfg_line_number); cfg_condition_expression;(string_of_int is_branch_node);""]
							)
				done
			end
		;
		(* back up root_list and node_list *)
		root_list_backup := !root_list_backup @ !root_list ;
		node_list_backup := !node_list_backup @ !node_list ;
		(* clear root_list and node_list *)
		root_list := [] ;
		node_list := [] 
		
		
	| _ -> ()
		
	
let createCFG (f:file) (file_name:string) (db_name : string) : (cfgNode list ref * int list ref) = 

	let dump = false in

	if dump = true then
	ignore(E.log "\n [cautcreateCFG->createCFG]\n");
	
	
	(* create db database file to store cfg node info *)
	let db_helper = (new cautFrontDatabaseHelper) in
	let db_handler = db_helper#caut_open_database db_name in
	if debug_flag then
	E.log "%s" "**********create/open database succeed************\n";
	(* create "node_list" and "root_list" db table *)
	ignore (db_helper#caut_create_database_table db_handler db_helper#get_caut_cfg_node_list_tb_name);
	ignore (db_helper#caut_create_database_table db_handler db_helper#get_caut_cfg_root_list_tb_name);
	
	(* iterate global function *)
	(try ( iterGlobals f (doGlobal f file_name db_helper db_handler)) with _ -> ());
	 
	(* close db *)
	ignore (db_helper#caut_close_database db_handler ) ;

	if dump = true then
		ignore(E.log "\n createCFG Success\n");
	
	if dump = true then
		List.iter (function (id:int) -> E.log "%d " id) !branch_node_id_list;
	
	(node_list_backup, root_list_backup)

let output_list (out : out_channel) (l : int list) : unit =
		output_string out "[";
		for index = 0 to (List.length l) - 1
		do
			let v = List.nth l index in
			output_string out (string_of_int v);
			output_string out ","
		done;
		output_string out "]";
		output_string out ";"

	
let record2File (node_list : cfgNode list) (root_list : int list) (filepath : string) = 
	let out = open_out filepath in
	output_string out "node_list\n";
	(* print_int (List.length node_list);*)
	for index = 0 to (List.length node_list) - 1
	do
		let node = List.nth node_list index in
		output_string out (string_of_int node.root);
		output_string out ";";
		output_string out (string_of_int node.id);
		output_string out ";";
		output_list out !(node.succ_list);
		output_list out !(node.prec_list);
		output_list out !(node.prec_choice_list);
		output_list out !(node.operand_stack);

		output_string out "\n";
	done;
	output_string out "root_list\n";
	for index = 0 to (List.length root_list) - 1
	do
		let root_id = List.nth root_list index in
		output_string out (string_of_int root_id);
		output_string out ","
	done;
	if debug_flag then
	begin
		ignore(E.log "\n close output channel\n");
		ignore(close_out out);
		ignore(E.log "\n store cfg Success\n")
	end
