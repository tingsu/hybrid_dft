open Pretty
open Cil

open CautcreateCFG
open CautSqlite3Interface
module E = Errormsg

(*debug flag *)
let debug_flag = false 

(*runtime???????????cfg???*)
type branch_label_t = {
	cfg_node_id : int;
	cfg_choice_of_true_branch : int;
	cfg_choice_of_false_branch : int;
	isLastCondition : int ; 
}

(*cfg-runtime branch map*)
type cfg_rt_branch_map_t = {
	
	cfg_id : int; (*the id of a node in cfg*)
	cfg_choice : int; (*the choice of the cfg node*)
	cfg_branch_isLastCondition : int ; (**)
	rt_id : int; (*the id of a node in runtime path*) 
	rt_choice : int; (*the choice of the runtime path node*)

}

(*cfg??????if????????????????*)
type if_stmt_branch_counter_t = {
	if_stmt_id : int;
	mutable counter : int;
}

(*cfg?????runtime?????????*)
let cfg_rt_mapping_table = ref ([]: cfg_rt_branch_map_t list)
let cfg_rt_mapping_table_backup = ref ([]: cfg_rt_branch_map_t list)

(*??cfg_rt_mapping_table???浽?????*)
let record2File (filepath : string) = 
	let out = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 filepath in
	output_string out "\nmap_node_list\n";
	output_string out "rt_id rt_choice cfg_id cfg_choice\n";
	(* print_int (List.length !cfg_rt_mapping_table);*)
	for index = 0 to (List.length !cfg_rt_mapping_table) - 1
	do
		let node = List.nth !cfg_rt_mapping_table index in
		output_string out (string_of_int node.rt_id);
		output_string out ";";
		output_string out (string_of_int node.rt_choice);
		output_string out ";";
		output_string out (string_of_int node.cfg_id);
		output_string out ";";
		output_string out (string_of_int node.cfg_choice);
		output_string out ";";
		output_string out "\n";
	done;
	if debug_flag then
	begin
		ignore(E.log "\n close output channel\n");
		ignore(close_out out);
		ignore(E.log "\n store map_node_list Success\n")
	end
	
(*cfg??if??????runtime??????????б?*)
let if_stmt_branch_counter_table = ref ([] : if_stmt_branch_counter_t list)


(*make branch_label_t *)
let mk_branch_label (stmt_id : int) (true_branch_choice : int) (false_branch_choice : int) : branch_label_t = {
	cfg_node_id = stmt_id;
	cfg_choice_of_true_branch = true_branch_choice;
	cfg_choice_of_false_branch = false_branch_choice;
	isLastCondition = 1 ;
}

(*????runtime?????if????????????????cfg?????????*)
let mk_cfg_rt_branch_map (branch_label : branch_label_t) (rt_stmt_id : int) : cfg_rt_branch_map_t list =
	[
	{
		cfg_id = branch_label.cfg_node_id;
		cfg_choice = branch_label.cfg_choice_of_true_branch;
		cfg_branch_isLastCondition = branch_label.isLastCondition ;
		rt_id = rt_stmt_id;
		rt_choice = 1;
	}
	;
	{
		cfg_id = branch_label.cfg_node_id;
		cfg_choice = branch_label.cfg_choice_of_false_branch;
		cfg_branch_isLastCondition = branch_label.isLastCondition ;
		rt_id = rt_stmt_id;
		rt_choice = 0;
	}
	]

(*???cfg?????if??????????????*)
let get_if_stmt_branch_counter (stmt_id : int) : int = 
	try(
		let counter = List.find (fun (x:if_stmt_branch_counter_t)->
			if x.if_stmt_id == stmt_id then true else false) !if_stmt_branch_counter_table in
		counter.counter <- (counter.counter + 1);
		counter.counter - 1
	)with Not_found ->
	(
		if_stmt_branch_counter_table := !if_stmt_branch_counter_table @ [{if_stmt_id = stmt_id; counter = 1}];
		0
	)
	


class deconstruct_condition_visitor (isCfgNode:int->bool)= object(self)
	inherit nopCilVisitor
	
	
	
	(*
	method mk_if_stmt_1 (true_branch_block : block) (false_branch_block : block) (if_stmt_id : int) (lc : location) : (stmt * stmt) =
  	let counter = get_if_stmt_branch_counter if_stmt_id in
  	let true_branch = mkStmt (Block true_branch_block) in
  	(*true_branch.labels <- true_branch.labels @ [Label("if_"^(string_of_int if_stmt_id)^"_"^(string_of_int counter)^"_true", lc, false)];*)
  	let false_branch = mkStmt (Block false_branch_block) in
  	(*false_branch.labels <- false_branch.labels @ [Label("if_"^(string_of_int if_stmt_id)^"_"^(string_of_int counter)^"_false", lc, false)];*)
  	(true_branch, false_branch)
	*)
		
	method private mk_if_stmt (cond_expr:exp) (true_block :stmt list ) (false_block: stmt list)  : stmt =
		(*if is_first == 0 then	begin
			(List.nth true_block 0).sid <- new_sid() ;
			(List.nth false_block 0).sid <- new_sid() ;
		end; 
		*)
		let if_stmt = mkStmt (If(cond_expr, mkBlock(true_block) , mkBlock(false_block) , !currentLoc)) in
		(*if_stmt.sid <- new_sid() ; (* create a new global stmt id *) *)
		if_stmt
	
	method private slpit_if_stmt (if_stmt : stmt) (branch_label : branch_label_t)  : stmt = 
		let dump = false in
		(* take care of If stmt *)
		match if_stmt.skind with
		| If (cond_expr, true_block, false_block, location) ->
			(
			match cond_expr with
			| BinOp(LAnd, expr_1, expr_2, _) -> 	(* if(expr_1 && expr_2 ) { true_block } else{ false_block} *)
				if dump = true then
					ignore(E.log " LAnd ");
				
				if dump = true then
				begin (*print expr_1 and expr_2 *)
					ignore(E.log "expr_1: %s \n" (sprint 20 ((d_exp () expr_1))));
					ignore(E.log "expr_2: %s \n" (sprint 20 ((d_exp () expr_2))))
				end
				;
				
				(* make if_stmt_expr2 as the true block of if_stmt_expr1 "if( expr_2)  { true_block } else{ false_block}"  *)
				let if_stmt_expr2_as_exp1_true_block = 
						self#mk_if_stmt expr_2 true_block.bstmts false_block.bstmts 
				in
				
				if dump = true then
					ignore(E.log "expr_1_true_branch:\n %s" (sprint 20 (d_stmt () if_stmt_expr2_as_exp1_true_block)));
				
				let expr_1_true_branch_label = {
						cfg_node_id = branch_label.cfg_node_id;
						cfg_choice_of_true_branch = branch_label.cfg_choice_of_true_branch;
						cfg_choice_of_false_branch = branch_label.cfg_choice_of_false_branch;
						isLastCondition = 1 ;
					} in
				
				let simple_expr_1_true_branch = self#slpit_if_stmt if_stmt_expr2_as_exp1_true_block expr_1_true_branch_label  in
				(*ignore(E.log "simple_expr_1_true_branch: %s \n" (sprint 20 ((d_stmt () simple_expr_1_true_branch))));*)
				
				(* make if_stmt_expr2 as the false block of if_stmt_expr1 "if( expr_2)  { false_block } else{ false_block}"  *)
				let if_stmt_expr2_as_exp1_false_block = 
						self#mk_if_stmt expr_2 false_block.bstmts false_block.bstmts 
				in
				if dump = true then
					ignore(E.log "expr_1_false_branch:\n %s" (sprint 20 (d_stmt () if_stmt_expr2_as_exp1_false_block)));
					
				
				let expr_1_false_branch_label = {	
						cfg_node_id = branch_label.cfg_node_id;
						cfg_choice_of_true_branch = branch_label.cfg_choice_of_false_branch;
						cfg_choice_of_false_branch = branch_label.cfg_choice_of_false_branch;
						isLastCondition = 1 ;
					} in
			
				let simple_expr_1_false_branch = self#slpit_if_stmt if_stmt_expr2_as_exp1_false_block expr_1_false_branch_label  in
				(*ignore(E.log "simple_expr_1_false_branch: %s \n" (sprint 20 ((d_stmt () simple_expr_1_false_branch))));*)
				
			
				let if_stmt_expr1 = self#mk_if_stmt expr_1 [simple_expr_1_true_branch] [simple_expr_1_false_branch] in
				(*ignore(E.log "expr_1_if_stmt: %s \n" (sprint 20 ((d_stmt () expr_1_if_stmt))));*)
				if dump = true then
					ignore(E.log "expr_1_if_stmt:\n %s" (sprint 20 (d_stmt () if_stmt_expr1)));
			
				let expr_1_label = {
						cfg_node_id = branch_label.cfg_node_id;
						cfg_choice_of_true_branch = -1;
						cfg_choice_of_false_branch = branch_label.cfg_choice_of_false_branch;
						isLastCondition = 0 ;
					} in
				
				let simple_expr_1_if_stmt = self#slpit_if_stmt if_stmt_expr1 expr_1_label  in
				(*ignore(E.log "simple_expr_1_if_stmt: %s \n" (sprint 20 ((d_stmt () simple_expr_1_if_stmt))));*)
				
			
				simple_expr_1_if_stmt
				
			| BinOp(LOr, expr_1, expr_2, _) -> (* if(expr_1 || expr_2 ) { true_block } else{ false_block} *)
				
				if dump = true then
					ignore(E.log " LOr ");
					
				if dump = true then begin
  				ignore(E.log "expr_1: %s \n" (sprint 20 ((d_exp () expr_1))));
  				ignore(E.log "expr_2: %s \n" (sprint 20 ((d_exp () expr_2))));
				end
				;
				(* make if_stmt_expr2 as the true block of if_stmt_expr1 "if( expr_2)  { true_block } else{ true_block}"  *)
				let if_stmt_expr2_as_exp1_true_block =
						self#mk_if_stmt expr_2 true_block.bstmts true_block.bstmts 
				in
				if dump = true then
					ignore(E.log "expr_1_true_branch:\n %s" (sprint 20 (d_stmt () if_stmt_expr2_as_exp1_true_block)));
				
				let expr_1_true_branch_label = {
						cfg_node_id = branch_label.cfg_node_id;
						cfg_choice_of_true_branch = branch_label.cfg_choice_of_true_branch;
						cfg_choice_of_false_branch = branch_label.cfg_choice_of_true_branch;
						isLastCondition = 1 ;
					} in
				
				let simple_expr_1_true_branch = self#slpit_if_stmt if_stmt_expr2_as_exp1_true_block expr_1_true_branch_label  in
				
				(* make if_stmt_expr2 as the false block of if_stmt_expr1 "if( expr_2)  { true_block } else{ false_block}"  *)
				let if_stmt_expr2_as_exp1_false_block =
						self#mk_if_stmt expr_2 true_block.bstmts false_block.bstmts   
				in
				if dump = true then
					ignore(E.log "expr_1_false_branch:\n %s" (sprint 20 (d_stmt () if_stmt_expr2_as_exp1_false_block)));
			
				let expr_1_false_branch_label = {	
						cfg_node_id = branch_label.cfg_node_id;
						cfg_choice_of_true_branch = branch_label.cfg_choice_of_true_branch;
						cfg_choice_of_false_branch = branch_label.cfg_choice_of_false_branch;
						isLastCondition = 1 ;
					} in
				
				let simple_expr_1_false_branch = self#slpit_if_stmt if_stmt_expr2_as_exp1_false_block expr_1_false_branch_label  in
				
				let expr_1_if_stmt = self#mk_if_stmt expr_1 [simple_expr_1_true_branch] [simple_expr_1_false_branch] in
				if dump = true then
					ignore(E.log "expr_1_if_stmt:\n %s" (sprint 20 (d_stmt () expr_1_if_stmt)));
				
				let expr_1_label = {
						cfg_node_id = branch_label.cfg_node_id;
						cfg_choice_of_true_branch = branch_label.cfg_choice_of_true_branch;
						cfg_choice_of_false_branch = -1;
						isLastCondition = 0 ;
					} in
			
				let simple_expr_1_if_stmt = self#slpit_if_stmt expr_1_if_stmt expr_1_label  in
				
				
				simple_expr_1_if_stmt
			| UnOp(LNot, expr, _) ->  (* if( !expr ) { true_block } else{ false_block} *)
			
				if dump = true then
					ignore(E.log " LNot ");
					
				if dump = true then begin
  				ignore(E.log "expr_1: %s \n" (sprint 20 ((d_exp () expr))));
				end
				;
					
				let  if_stmt_expr = self#mk_if_stmt expr false_block.bstmts true_block.bstmts  in
			
				let new_if_stmt_label = {
						cfg_node_id = branch_label.cfg_node_id;
						cfg_choice_of_true_branch = branch_label.cfg_choice_of_false_branch;
						cfg_choice_of_false_branch = branch_label.cfg_choice_of_true_branch;
						isLastCondition = 1 ;
					} in
				
				let simple_new_if_stmt = self#slpit_if_stmt if_stmt_expr new_if_stmt_label  in
				
				simple_new_if_stmt
								
			| _  -> (* simple if stmt*)
				
				if dump = true then 
					ignore(E.log " %s " (sprint 20 ((d_exp () cond_expr))));
				if if_stmt.sid = -1 then begin
					if_stmt.sid <- new_sid (); 
					if dump = true then
						ignore(E.log "new_sid() = %d " if_stmt.sid);
				end
				;
				cfg_rt_mapping_table := !cfg_rt_mapping_table@ (mk_cfg_rt_branch_map branch_label if_stmt.sid);
				
				if_stmt
			)
		| _ -> 
			(* impossible to reach here *)
			if_stmt
		
	(* method vstmt *)
	method vstmt (st:stmt) =
		let dump= true in
		(* After computer CFG, only If stmt has conditional expressions,split composition of &&,||,! *)
		match st.skind with
		| If(BinOp(LAnd, exp1, exp2, _), true_block, false_block, location) ->
			if dump = true then begin
				ignore(E.log "%s && " (sprint 20 (d_exp () exp1)));
				ignore(E.log "%s\n" (sprint 20 (d_exp () exp2)))
			end;
			let branch_label = mk_branch_label st.sid 1 0  in
			(* need to slpit If stmt under these three If stmt type *)
			ChangeTo (self#slpit_if_stmt st branch_label  )
		| If(BinOp(LOr, exp1 , exp2, _), true_block, false_block, location) ->
			if dump = true then begin
				ignore(E.log "%s || " (sprint 20 (d_exp () exp1)));
				ignore(E.log "%s\n" (sprint 20 (d_exp () exp2)))
			end;
			let branch_label = mk_branch_label st.sid 1 0  in
			(* need to slpit If stmt under these three If stmt type *)
			ChangeTo (self#slpit_if_stmt st branch_label )
		| If(UnOp(LNot, exp, _), true_block, false_block, location) ->
			if dump = true then
				ignore(E.log "!%s\n" (sprint 20 (d_exp () exp)));
			let branch_label = mk_branch_label st.sid 1 0  in
			(* need to slpit If stmt under these three If stmt type *)
			ChangeTo (self#slpit_if_stmt st branch_label )
		| If(exp_t,true_block, false_block, location) -> 
			if dump = true then
				ignore(E.log "expr_t : %s \n" (sprint 20 (d_exp () exp_t)));
			(* if it is a cfg node  *)
			if (isCfgNode st.sid) then begin
				let branch_label = mk_branch_label st.sid 1 0 in
				(* store it *)
				cfg_rt_mapping_table := !cfg_rt_mapping_table 
											@ (mk_cfg_rt_branch_map branch_label st.sid);
				DoChildren
			end else DoChildren
		| _  -> 
			(* we do not care other stmt types*)
			DoChildren
end

let is_duplicate_rt_branch (rt_id:int) (rt_choice:int) : bool =
	List.exists (fun (item:cfg_rt_branch_map_t)-> if (item.rt_id = rt_id && item.rt_choice = rt_choice) then true else false)  !cfg_rt_mapping_table_backup


let do_code_change_by_function (f:file) (db_helper:cautFrontDatabaseHelper)(db_handler:Sqlite3.db) (cfgNode_list : cfgNode list)(iter:int) (g:global) :unit =
	match g with
		| GFun (fd, loc)-> (* global func *)
			
			(
			let dump = true in
			
			if dump = true then
				E.log "Enter unit : %s \n" fd.svar.vname ;
			
			match fd.svar.vname with
			| "testme" -> () (*skip test driver function "testme" *)
			| _ -> 
				
				(
  			for i=0 to iter
  			do
  				
    			visitCilFile 
    			(new deconstruct_condition_visitor (fun (nodeId:int) -> (List.exists (fun (node:cfgNode) -> if node.id=nodeId then true else false) cfgNode_list))) 
    			f;
    			
    			(* store into db *)
    			let rt_cfg_map_list_cnt = List.length !cfg_rt_mapping_table in
    			if dump = false then
    				E.log "rt_cfg_map_list_cnt = %d \n" rt_cfg_map_list_cnt;
    			if rt_cfg_map_list_cnt > 0 then
    				for index = 0 to rt_cfg_map_list_cnt-1 
    				do
    					let rt_cfg_branch_t = List.nth !cfg_rt_mapping_table index in
    					let rt_id = rt_cfg_branch_t.rt_id in
    					let rt_choice = rt_cfg_branch_t.rt_choice in
							(* eliminate duplicate record *)
							if (is_duplicate_rt_branch rt_id rt_choice)=false then
							begin
      					let cfg_id = rt_cfg_branch_t.cfg_id in
      					let cfg_choice = rt_cfg_branch_t.cfg_choice in
      					let cfg_branch_isLastCondition = rt_cfg_branch_t.cfg_branch_isLastCondition in
      					if dump = true then
      						E.log "unit_name : %s \n" fd.svar.vname ;
      					ignore (db_helper#insert_caut_tb db_handler db_helper#get_caut_cfg_rt_cfg_branch_map_list_tb_name
      									["file_name";"unit_name";"rt_branch_id";"rt_branch_choice";"cfg_branch_id";"cfg_branch_choice";"cfg_branch_isLastCondition"]
      									[f.fileName ; fd.svar.vname ; (string_of_int rt_id);(string_of_int rt_choice) ; (string_of_int cfg_id) ; (string_of_int cfg_choice);(string_of_int cfg_branch_isLastCondition)]
      									)
							end
    				done
    			;
    			(* back up table *)
    			cfg_rt_mapping_table_backup := !cfg_rt_mapping_table_backup @ !cfg_rt_mapping_table ;
    			(* clear table *)
    			cfg_rt_mapping_table := []
  				
  			done
				)
		)
			
		| _  ->  ()
	

												
let default_process (f : file) (cfgNode_list : cfgNode list) (iter :int)  (db_name :string) : cfg_rt_branch_map_t list= 
			
	if debug_flag then
		ignore(E.log "\n start deconstruct_condition_visitor\n");
	
	(* open db to store rt_cfg_branch_map info *)
	let db_helper = (new cautFrontDatabaseHelper) in
	let db_handler = db_helper#caut_open_database db_name in
	if debug_flag then
		E.log "%s" "**********create/open database succeed************\n";
	(* create "cfg_rt_cfg_branch_map_list" db table *)
	ignore (db_helper#caut_create_database_table db_handler db_helper#get_caut_cfg_rt_cfg_branch_map_list_tb_name);
		
	(* iterate global function *)
	(try ( iterGlobals f (do_code_change_by_function f db_helper db_handler cfgNode_list iter)) with _ -> ());
	
	(* close db *)
	ignore (db_helper#caut_close_database db_handler ) ;
	
	!cfg_rt_mapping_table_backup
