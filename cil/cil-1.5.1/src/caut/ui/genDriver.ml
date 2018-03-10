(** filename : genDriver
	This file generate "__CAUT_TEST" test driver function and main function.
	It also make "__CAUT_INPUT" interface for those four kinds of variables 
	of the unit under test.
*)
open Cil
open Pretty
open CautSqlite3Interface

module E = Errormsg

(** mkStmtInstrs creates stmt from instrs 
*)
let rec mkStmtInstrs (instr_list:instr list) : stmt list =
	 match List.length instr_list with
		| 0 -> []
		| _ -> [mkStmtOneInstr (List.hd instr_list)] @ mkStmtInstrs (List.tl instr_list) 
		
(** class cautInputInstrVisitor inserts "__CAUT_INPUT" interfaces in the body of units under test *)
class cautInputInstrVisitor (f:file) (unit_name:string) (db_helper : cautFrontDatabaseHelper) (db_handler : Sqlite3.db ) = 

	object (self)
	inherit nopCilVisitor
	
	(* search the formal argument list, and record the positon of ptr type argument *) 
	method private whether_has_ptr_arguments (f:file) (function_name:string) :int list =
		
		let function_call_ptr_arg_location_list = ref ([]:int list) in
		
		List.iter ( fun global_item ->
	
			match global_item with
			| GFun(fundec_t , _) ->
				if fundec_t.svar.vname == function_name then 
				begin
					for pos = 0 to ((List.length fundec_t.sformals)-1) do
							
					  let formal_at_pos = (List.nth fundec_t.sformals pos) in
						match formal_at_pos.vtype with
						| TPtr _ | TArray _ ->
							function_call_ptr_arg_location_list := !function_call_ptr_arg_location_list @ [pos]
						| _ -> ()
					done
				end
			| _ -> ()
			) f.globals ;
		(* return the list *)
		!function_call_ptr_arg_location_list
		
		(* convert a function name "exp" to "string" *)
		method private exp_to_string (e:exp) :string = 
			match e with
			| Lval(lval_t) ->
				(
				match lval_t with
				|(lhost_t , offset_t) ->
					(
					match lhost_t with
					| Var(var) ->
						var.vname
					| _ -> ""
					)
				)
			| _ -> ""
		
	(** vinst inserts "__CAUT_INPUT" for call return variables and ptr argument variables *)
	method vinst (instr_t:instr) : instr list visitAction =
		let caut_input_interface = emptyFunction "__CAUT_INPUT" in
		match instr_t with 
			| Call (lval_option_t , exp_t ,exp_list_t , location_t) ->
				(
					(* get ptr argument position from this current call instr *)
					let ptr_argument_pos_list = (db_helper#query_caut_tb db_handler db_helper#get_caut_call_argument_variable_tb_name
															["arg_no"]
															["file_name";"unit_name";"call_unit"] 
															[f.fileName ; unit_name ; (self#exp_to_string exp_t)]
												 )in
					
					match lval_option_t with
						(* return value and ptr argument *)
						| Some lval_option_t_some  -> 
								(   
								match lval_option_t_some with
									| (lhost_t,offset_t) ->
										match lhost_t with
											| Var var ->
												
												let instr_return_call = Call(None, Lval(Var caut_input_interface.svar, NoOffset), [AddrOf(lval_option_t_some)], locUnknown ) in
												let ptr_argument_pos_list_len = (List.length ptr_argument_pos_list) in
												let instr_arg_call_list = ref ([]:instr list) in
												(
												if  (ptr_argument_pos_list_len != 0) then
													begin
															for i = 0 to ptr_argument_pos_list_len-1 do
																	let pos = (int_of_string (List.nth ptr_argument_pos_list i)) in
																	let instr_t_4 = Call(None, Lval(Var caut_input_interface.svar, NoOffset), [(List.nth exp_list_t pos)], locUnknown ) in
																	instr_arg_call_list := !instr_arg_call_list @ [instr_t_4] ;
															done
													end
												);
												ChangeTo ([instr_return_call] @  !instr_arg_call_list)
																							
											| _ -> DoChildren
									)
						(* only ptr argument *)
						| None ->
							let ptr_argument_pos_list_len = List.length ptr_argument_pos_list in
							let instr_arg_call_list = ref ([]:instr list) in
							(
							if  (ptr_argument_pos_list_len != 0) then
								begin
										for i = 0 to ptr_argument_pos_list_len-1 do
												let pos = (int_of_string (List.nth ptr_argument_pos_list i)) in
												let instr_t_4 = Call(None, Lval(Var caut_input_interface.svar, NoOffset), [(List.nth exp_list_t pos)], locUnknown ) in
												instr_arg_call_list := !instr_arg_call_list @ [instr_t_4] ;
										done
								end
							);
							ChangeTo (!instr_arg_call_list)
				)
			| _ -> DoChildren
		
end;;


(** doCautInput does caut input job*)
let doCautInput (unit_name:string) (f:file) (db_helper : cautFrontDatabaseHelper) (db_handler : Sqlite3.db ) (g:global) = 
	
	match g with 
			| GFun(fundec_t ,location_t )->
					if fundec_t.svar.vname = unit_name then
						fundec_t.sbody <- visitCilBlock (new cautInputInstrVisitor f unit_name db_helper db_handler) fundec_t.sbody
			| _ -> ()

	
(** doCreateCautTestDriver creates "__CAUT_TEST" driver function 
	@param 1 the unit under test
*)
let doCreateCautTestDriver (unit_name:string) (f:file) (db_helper : cautFrontDatabaseHelper) (db_handler : Sqlite3.db )= 
	
	(* create empty test driver function *)
	let test_driver = emptyFunction "__CAUT_TEST" in
	let location_t = locUnknown in
	let global_test_driver = GFun(test_driver, location_t) in
	f.globals <- (f.globals @ [global_test_driver]) ;
	
	(* get the sformals list of unit under test *)
	let unit_sformals_list = ref ([] : varinfo list) in
	(
	for i=0 to (List.length f.globals-1) do
		match (List.nth f.globals i) with
		| GFun (fundec_t , location_t ) ->
				if fundec_t.svar.vname = unit_name then
					begin 
						unit_sformals_list := fundec_t.sformals ;
					end
		| _ -> ()
	done
	);
	
	
	match global_test_driver with
		| GFun (fundec_t , location_t ) ->
			
			let caut_input_interface = emptyFunction "__CAUT_INPUT" in
			let instr_list = ref([]:instr list) in
			
			for i=0 to (List.length !unit_sformals_list)-1 do
				let varinfo_t = List.nth !unit_sformals_list i in
				(* 1. insert formals declaration , "makeLocalVar" do not need to insert explicitly *)
				ignore (makeLocalVar fundec_t ~insert:true varinfo_t.vname varinfo_t.vtype ) ;
				let instr_t = Call(None, Lval(Var caut_input_interface.svar, NoOffset), [AddrOf(Var(varinfo_t),NoOffset)], locUnknown ) in
				instr_list := !instr_list @ [instr_t]
			done
			;
			(* 2. insert "__CAUT_INPUT" interface for formals *)
			let stmt_list = mkStmtInstrs !instr_list in
			fundec_t.sbody.bstmts <- fundec_t.sbody.bstmts @ stmt_list ; 
			
			(* find related "local" and "extern" globals in this unit under test,and store them in the varinfo list *)
			let global_var_list = ref([]:varinfo list) in
			let global_name_list = db_helper#query_caut_tb db_handler db_helper#get_caut_related_global_variable_tb_name ["glo_name"] ["file_name";"unit_name";] [f.fileName;unit_name] in
			for j=0 to (List.length global_name_list)-1 do
				let global_name = (List.nth global_name_list j) in
				for k=0 to (List.length f.globals-1) do
					match (List.nth f.globals k) with
					(* "local" global *)
					| GVar (varinfo_t ,initinfo_t , location_t ) ->
							if varinfo_t.vname = global_name then
								global_var_list := !global_var_list @ [varinfo_t]
					(* "extern" global *)
					| GVarDecl (varinfo_t,location_t)  -> 
							if varinfo_t.vname = global_name then
								global_var_list := !global_var_list @ [varinfo_t]
					| _ -> ()
				done
			done
			;
			
			(* clear instr_list for reuse *)
			instr_list := [] ;
			for i=0 to (List.length !global_var_list)-1 do
				let varinfo_t = List.nth !global_var_list i in
				let instr_t = Call(None, Lval(Var caut_input_interface.svar, NoOffset), [AddrOf(Var(varinfo_t),NoOffset)], locUnknown) in
				instr_list := !instr_list @ [instr_t]
			done
			;
			(* 3. insert "__CAUT_INPUT" interface for globals*)
			let stmt_list = mkStmtInstrs !instr_list in
			fundec_t.sbody.bstmts <- fundec_t.sbody.bstmts @ stmt_list ;
			
			(* 4. generate unit call *)
			let call_exp_list = ref([]:exp list) in
			for i=0 to (List.length !unit_sformals_list)-1 do
				let varinfo_t = List.nth !unit_sformals_list i in
				let exp_t = Lval(Var(varinfo_t),NoOffset) in
				call_exp_list := !call_exp_list @ [exp_t]
			done
			;
			for i=0 to (List.length f.globals-1) do
				match (List.nth f.globals i) with
				| GFun (fun_t , loc_t ) ->
						if fun_t.svar.vname = unit_name then
							begin 
								let call_unit_instr = Call(None, Lval(Var fun_t.svar, NoOffset), !call_exp_list , locUnknown) in
								fundec_t.sbody.bstmts <- fundec_t.sbody.bstmts @ [mkStmtOneInstr call_unit_instr]
							end
				| _ -> ()
			done
			
		| _ -> ()
		

(** doCreateMain creates main function 
*)
let doCreateMain (f:file) = 
	let fun_main = emptyFunction "main" in
	let location_t = locUnknown in
	let global_main = GFun(fun_main, location_t) in
	f.globals <- (f.globals @ [global_main]) ;
	
	for i=0 to (List.length f.globals-1) do
		match (List.nth f.globals i) with
		| GFun (fundec_t,location_t)  -> 
				if fundec_t.svar.vname = "__CAUT_TEST" then
					let call_unit_instr = Call(None, Lval(Var fundec_t.svar, NoOffset), [] , locUnknown) in
					(
					match global_main with
						| GFun (fun_t , loc_t)  -> 
								fun_t.sbody.bstmts <- fun_t.sbody.bstmts @ [mkStmtOneInstr call_unit_instr];
						| _ -> ()
					)
		| _ -> ()
	done

(** main job *)
let main_job (f:file) (db_name:string) (unit_name:string) = 

	(* open database *)
	let db_helper = (new cautFrontDatabaseHelper) in
	let db_handler = db_helper#caut_open_database  db_name in
	E.log "%s" "**********create/open database succeed************\n";

	(* The following steps are done for the unit under test every time *)
	(* 1.generate test driver *)
	doCreateCautTestDriver  unit_name f db_helper db_handler;
	
	(* 2.insert "__CAUT_INPUT" interfaces for the unit under test *)
	iterGlobals f (doCautInput unit_name f db_helper db_handler) ;
	
	(* 3. create main *)
	doCreateMain f ;
	
	(* close db *)
	ignore (db_helper#caut_close_database db_handler )
	