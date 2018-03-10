(** filename : parseVar
	This file collects var info (including four kinds of variables about the unit under test : 
	call return variables and ptr argument varibales of inner function call
	related global variables and the parameters of the unit )
	from source files.
*)
open Cil
open Pretty
open CautSqlite3Interface

module E = Errormsg


let debug = ref true ;

(*********************************************Debugging Utilities**********************************)
 
(** class printStmtIdVisitor prints statement id.
	This function is just for debugging
*)
class printStmtIdVisitor = object (self)
	inherit nopCilVisitor
	
	method vstmt (st:stmt) : stmt visitAction =
		E.log "\nstmt = %a , stmt.id= %d " d_stmt st st.sid ;
		DoChildren
end;;

(** doprintFunctionStmt prints stmts in a specified function 
	This function is just for debugging
*)
let doPrintFunctionStmt (function_name:string) (g:global)=
	
	match g with 
		| GFun(fundec_t ,location_t )->
			if fundec_t.svar.vname = function_name then
				fundec_t.sbody <- visitCilBlock (new printStmtIdVisitor) fundec_t.sbody
		| _ -> ()

		
		
		
(**********************************************************************************)


(**	class testableUnitVisitor finds out all testable units/functions.
	@param 1 cautFrontDatabaseHelper class instance
	@param 2 Sqlite3 database handler
*)
class testableUnitVisitor (db_helper:cautFrontDatabaseHelper) (db_handler:Sqlite3.db) (f:file) =
	
	object (self)

	inherit nopCilVisitor
	
	(** convert_formal_list_to_string converts the argument list with item type (string*typ*attributes)
 		into a string like "type1;type2"
		@param 1 formal parameter list
	*)
	method private convert_formal_list_to_string (list_t : (string*typ*attributes) list) : string =
		let tmp_buf = Buffer.create 20 in
		let list_len = (List.length list_t) in
		if list_len = 0 then 
			begin
				(* use "void" to represent no argument *)
				Buffer.add_string tmp_buf "void";
				Buffer.contents tmp_buf
			end
		else 
			begin
				for i=0 to (list_len-1) do
					let list_item = List.nth list_t i in
					match list_item with
					| ((sname:string), (t:typ), (attr:attributes)) ->
							Buffer.add_string tmp_buf (sprint 20 (d_type () t) );
							if i < (list_len - 1 ) then
								Buffer.add_string tmp_buf ";"
				done;
				Buffer.contents tmp_buf
			end
			
	(** get parameter name and type *)
	method private get_par_name (x,_,_) = x
	method private get_par_type (_,y,_) = sprint 20 (d_type () y) 

	(** vfunc finds function definition not declaration *)
	method vfunc (fu:fundec) : fundec visitAction =
		
		(* get file name and unit name *)
		let file_name = f.fileName in
		let unit_name = fu.svar.vname in
		
		(match fu.svar.vtype with 
		| TFun( typ_t , list_t , bool_t , attributes_t) ->
		
			(* get result type *)
			let result_type = (sprint 20 (d_type () typ_t) )  in
			
			let arguments_list = argsToList (list_t) in
			(* get formal list string *)
			let parameter_list = (self#convert_formal_list_to_string arguments_list) in
			
			let line_number = !currentLoc.line in
			
			(* insert unit_tb record into database *)
			ignore (db_helper#insert_caut_tb db_handler db_helper#get_caut_testable_unit_tb_name
						["file_name";"unit_name";"result_type";"parameter_list";"line_number"]
						[file_name ; unit_name ; result_type ; parameter_list ; (string_of_int line_number) ]
					);
			
			let arguments_list_len = List.length arguments_list in
			(* if it has parameters *)
			if arguments_list_len > 0 then
				for i = 0 to (arguments_list_len -1) do 
					let list_item = List.nth arguments_list i in
					let par_name = self#get_par_name list_item in
					let par_type = self#get_par_type list_item in
					ignore (db_helper#insert_caut_tb db_handler db_helper#get_caut_unit_parameter_variable_tb_name
						["file_name";"unit_name";"par_name";"par_type";"par_no";"line_number"]
						[file_name ; unit_name ; par_name ; par_type ; (string_of_int i) ; (string_of_int line_number) ]
					);
				done
			
			
		| _ -> ()		
		);
		E.log "\n\n";
		DoChildren
		
	(* create testable_unit table and unit_parameter_variable table after class object was created *)
	initializer ignore (db_helper#caut_create_database_table db_handler db_helper#get_caut_testable_unit_tb_name) ;
				ignore (db_helper#caut_create_database_table db_handler db_helper#get_caut_unit_parameter_variable_tb_name) 
			
		
	end;;

	
(** class unitBodyVisitor collects variable info from the specified unit under test 
	@param 4 function name of the unit under test
*)
class unitBodyVisitor (f:file) (db_helper:cautFrontDatabaseHelper) (db_handler:Sqlite3.db) (function_name:string)=
	 
	object (self)
	
	inherit nopCilVisitor
	
	val file_name = f.fileName
	val unit_name = function_name
		
	(** exp_to_string converts a function name "exp" to a string *)
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
	
	(** whether_has_ptr_arguments searchs the formal argument list, and record the positon of ptr type argument *) 
	method private whether_has_ptr_arguments (f:file) (function_name:string) : int list =
		
		let function_call_ptr_arg_location_list = ref ([]:int list) in
		
		List.iter ( fun global_item ->
	
			match global_item with
			| GFun(fundec_t , _) ->
				if fundec_t.svar.vname == function_name then 
				begin
					for pos = 0 to ((List.length fundec_t.sformals)-1) do
												
					  let formal_at_pos = (List.nth fundec_t.sformals pos) in
						match formal_at_pos.vtype with
						| TPtr(_,_) ->
							function_call_ptr_arg_location_list := !function_call_ptr_arg_location_list @ [pos]
						| _ -> ()
					done
				end
			| _ -> ()
			) f.globals ;
		!function_call_ptr_arg_location_list

	
	(** find out call return variables and ptr agument variables *)
	method vinst (instr_t : instr) : instr list visitAction =
	
		(
		match instr_t with 
		(* find out the variable assigned by function call in the body of the specified function *)
		| Call(lval_option_t,exp_t,exp_list_t,location_t) ->					
			(
			match lval_option_t with
			| Some(lval_option_tt) -> 
					(match lval_option_tt with 
					| (lhost_t ,offset_t) ->
						(match lhost_t with
						| Var(v) ->
							let return_name = v.vname in
							let return_type = (sprint 20 (d_type () v.vtype)) in
							let call_unit = (self#exp_to_string exp_t) in
							let line_number = !currentLoc.line in
							ignore (db_helper#insert_caut_tb db_handler db_helper#get_caut_call_return_variable_tb_name
										["file_name";"unit_name";"return_name";"return_type";"call_unit";"line_number"]
										[file_name ; unit_name ; return_name ; return_type ; call_unit ; (string_of_int line_number) ]
									)
						| _ -> ()
						)
					)
			| None -> ()
			);
			(* find out Ptr type argument in function call statement *)
			let fun_name = self#exp_to_string(exp_t) in
			let pos_list = (self#whether_has_ptr_arguments f fun_name) in
			(
			if (List.length pos_list) = 0 then
				E.log "\n%s : [No Ptr Type Arguments]" fun_name
			else
				begin
					for i=0 to ((List.length pos_list)-1) do
						let pos = (List.nth pos_list i) in
						let exp_at_pos = List.nth exp_list_t pos in
						match exp_at_pos with
						| Lval(lval_t) | AddrOf(lval_t) | StartOf(lval_t)  ->
							(
							match lval_t with
							| (lhost_t , offset_t) ->
								(
								match lhost_t with
								| Var(v) ->
									let arg_name = v.vname in
									let arg_type = (sprint 20 (d_type () v.vtype)) in
									let call_unit = (self#exp_to_string exp_t) in
									let line_number = !currentLoc.line in
									ignore (db_helper#insert_caut_tb db_handler db_helper#get_caut_call_argument_variable_tb_name
											["file_name";"unit_name";"arg_name";"arg_type";"call_unit";"line_number"]
											[file_name ; unit_name ; arg_name ; arg_type ; call_unit ; (string_of_int line_number) ]
											)
								| _ -> ()
								)
							)
						| _ -> ()
					done
				end
				);
		| _ -> ()
		);
		DoChildren
	
	(** find out related global variables in the body of the specified function *)
	method vvrbl (v:varinfo) : varinfo visitAction =
		
		(* Here including extern globals and globals *)
		if v.vglob = true then
			begin
				match v.vtype with
				| TVoid _ | TInt _ | TFloat _ | TPtr _ | TArray _ | TNamed _ | TComp _ | TEnum _ ->
					let glo_name = v.vname in
					let glo_type = (sprint 20 (d_type () v.vtype)) in
					let line_number = !currentLoc.line in
					ignore (db_helper#insert_caut_tb db_handler db_helper#get_caut_related_global_variable_tb_name
							["file_name";"unit_name";"glo_name";"glo_type";"line_number"]
							[file_name ; unit_name ; glo_name ; glo_type ; (string_of_int line_number) ]
							)
				| _ -> ()
			end;
		DoChildren
		
	(* create call_argument_variable , call_return_variable and related_global_variable table *)
	initializer ignore (db_helper#caut_create_database_table db_handler db_helper#get_caut_call_argument_variable_tb_name) ;
				ignore (db_helper#caut_create_database_table db_handler db_helper#get_caut_call_return_variable_tb_name) ;
				ignore (db_helper#caut_create_database_table db_handler db_helper#get_caut_related_global_variable_tb_name)

	end;;


(** doGlobalCFG creates CFG based on GFun *)
let doGlobalCFG (g:global) =
	match g with 
	| GFun(fundec,_) ->
		prepareCFG fundec;
		computeCFGInfo fundec true
	| _ -> ()
	
	
(** doChangeMainName changes function "main" to "__CAUT_main", just the name 
	@param 1 global in this file
*)
let doChangeMainName (g:global) =
	match g with 
	| GFun(fi,_) ->
		if fi.svar.vname = "main" then 
			begin
				let new_name = "__CAUT_main" in
				fi.svar.vname <- new_name
			end
	| _ -> ()

	
(** doScanUnitBody collects var info from all units body *)
let doScanUnitBody (f: file) (db_helper:cautFrontDatabaseHelper)(db_handler:Sqlite3.db) (g:global) =
	
	let units_under_test = db_helper#query_caut_tb db_handler db_helper#get_caut_testable_unit_tb_name ["unit_name"] [] [] in
	let list_len = (List.length units_under_test) in
	for i=0 to (list_len - 1) do
		(* scan all units *)
		let unit_name = (List.nth units_under_test i) in
		match g with
		| GFun(fundec_t , location_t) ->
			if fundec_t.svar.vname = unit_name then
				fundec_t.sbody <- visitCilBlock (new unitBodyVisitor f db_helper db_handler unit_name) fundec_t.sbody
		| _ -> ()
	done
	
	
(** main_job does main job 
*)
let main_job (f:file) (db_name:string)=
	
	(*1. change function "main" to "__CAUT_main" *)
	iterGlobals f doChangeMainName ;
	
	(* open database *)
	let db_helper = (new cautFrontDatabaseHelper) in
	let db_handler = db_helper#caut_open_database db_name in
	E.log "%s" "**********create/open database succeed************\n";
	
	(*2. generate global cfg based on functions *)
	iterGlobals f (doGlobalCFG) ;
	
	
	(*3. collect testable units from source files or header files *)
	visitCilFile (new testableUnitVisitor db_helper db_handler f ) f ;
	
	(*4. scan all testable units for their var infos *)
	iterGlobals f (doScanUnitBody f db_helper db_handler) ;
	
	(* close db *)
	ignore (db_helper#caut_close_database db_handler )
;;
