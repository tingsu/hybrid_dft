(**
 This file is used to prepare the program under test for data flow testing by blast (also for cpachecker).
 
 we set a flag immediately following the def stmt, and check this flag with an if stmt immediately precede the use stmt.
 In this if stmt, we put an ERROR label, like "ERROR: goto ERROR".
 We also negate the flag after a redef stmt.

 If this ERROR label is reachable, then we actually find a path which could probably cover this def-use pair.
 
 TODO:
 We need to find out all possible redef stmts beforehand (e.g. redefs from variable aliases).
*)

open Cil
open MyUseDefAssocByHand

module E = Errormsg 

let cegar_dua_id = ref 0
(* the name of the cegar-based model checker *)
let cegar_model_checker = ref ""
(* if true, read the duas and var defs from previously analyzed data to speed up the process of program instrumentation (especially for large programs; if false, use the analyzed data on line *)
let offline_instrumentation = ref false
let duas_data_file = ref ""
let var_defs_data_file = ref ""

class transProgramVisitor (dua: MyUseDefAssocByHand.myDua) (redef_stmt_ids: int list) (blast_check_flag_var: Cil.varinfo) = object(self)
	inherit nopCilVisitor
	
	(** create blast if stmt 
		if( blast_df_flag == 1 ) { ERROR: goto ERROR; }
	*)
	method private mkBlastIfStmt () : Cil.stmt = 
		(* create the "ERROR" label *)
		let error_label = Label("ERROR", Cil.locUnknown, false) in
		
		(* create the goto stmt *)
		let goto_stmt = ref(Cil.mkEmptyStmt ()) in (* an empty stmt (of kind Instr) *)
		!goto_stmt.labels <- [error_label];
		!goto_stmt.skind <- Goto(goto_stmt, Cil.locUnknown);
		let tb = Cil.mkBlock [!goto_stmt] in
		let fb = Cil.mkBlock [] in
		
		let v = Cil.one in
		let lv = ((Var(blast_check_flag_var)), NoOffset) in
		let e = BinOp(Eq, Lval(lv), v, Cil.intType) in
		(* create the if stmt *)
		let if_stmt = Cil.mkEmptyStmt () in
		if_stmt.skind <- If(e, tb, fb,  Cil.locUnknown);
		(* E.log "if stmt: %a" d_stmt if_stmt; *)
		if_stmt
	
	method vstmt (st: Cil.stmt) = 
		let action s =
			match s.skind with
			| Instr (il) ->
				if s.sid == dua.dua_def.stmt_id then begin
					let v = Cil.one in (* set the check flag *)
					let lv = ((Var(blast_check_flag_var)), NoOffset) in
					let blast_instr = Set( lv, v, Cil.locUnknown ) in
					let new_instrs = il @ [blast_instr] in
					let new_stmt = Cil.mkStmt (Instr(new_instrs)) in
					E.log "add check after DEF!\n";
					new_stmt
				end
				else if s.sid == dua.dua_use.stmt_id && (dua.dua_kind == 0) then begin
					let blast_if_stmt = self#mkBlastIfStmt () in
					let new_block = mkBlock [blast_if_stmt; s] in
					let comp_stmt = Cil.mkEmptyStmt () in
					comp_stmt.skind <- Block new_block;
					E.log "add check before USE!\n";
					comp_stmt
				end else if (MyCilUtility.isIdInList s.sid redef_stmt_ids) then begin
					let v = Cil.zero in (* clear the check flag *)
					let lv = ((Var(blast_check_flag_var)), NoOffset) in
					let blast_instr = Set( lv, v, Cil.locUnknown ) in
					let new_instrs = il @ [blast_instr] in
					let new_stmt = Cil.mkStmt (Instr(new_instrs)) in
					new_stmt.labels <- s.labels; (* remember to add labels *)
					E.log "add check after REDEF!\n";
					new_stmt
				end else begin
					s
				end
			| If (e, b1, b2, loc) ->
				if (s.sid == dua.dua_use.stmt_id) && (dua.dua_kind == 1) then begin
					let blast_if_stmt = self#mkBlastIfStmt () in
					b1.bstmts <- blast_if_stmt :: b1.bstmts; 
					E.log "add check before USE!\n";
    				s
				end else if (s.sid == dua.dua_use.stmt_id) && (dua.dua_kind == 2) then begin
					let blast_if_stmt = self#mkBlastIfStmt () in
					b2.bstmts <- blast_if_stmt :: b2.bstmts;
					E.log "add check before USE!\n";
					s
				end else begin
					s
				end
			| _ -> s
		in
		(*E.log "s: %a\n" d_stmt st;
		E.log "sid: %d\n" st.sid;
		E.log "------\n";*)
		(** Use "ChangeDoChildrenPost" to change stmt *)
		ChangeDoChildrenPost(st, action)
		
	
end;;

let set_cegar_model_checker (model_checker: string) =

    cegar_model_checker := model_checker
;;

(* set dua data file *)
let set_duas_data_file (file_name: string) =

    duas_data_file := file_name
;;

(* set var defs data file *)
let set_var_defs_data_file (file_name: string) =

    var_defs_data_file := file_name
;;

let set_offline_instrumentation (yes: bool) =

    offline_instrumentation := yes
;;

let transform_prgoram_for_blast (file: Cil.file) (dua_id: int) (duas_list: MyUseDefAssocByHand.myDua list) (var_defs_list: MyUseDefAssocByHand.var_defs list) = 

	(* get the target dua *)
	let dua = List.nth duas_list dua_id in
	E.log "def line: %d, use line: %d\n" dua.dua_def.var_line dua.dua_use.var_line;
	E.log "def sid: %d, use sid: %d\n" dua.dua_def.stmt_id dua.dua_use.stmt_id;
	E.log "dua kind: %d\n" dua.dua_kind;
	
	(* get the redef on the var of this dua.
	   Here, we currently only consider redef without var alias.
	*)
	let redef_stmt_ids = ref([]: int list) in 
	List.iter
		begin fun var_def ->
			if (var_def.var_defs_func_id == dua.dua_def.fun_id) && 
				(var_def.var_defs_var_stmt_id != dua.dua_def.stmt_id) &&
				(var_def.var_defs_var_id == dua.dua_def.var_id) then begin
				(* in the same function and def on the same var, exclude the def stmt itself *)
				redef_stmt_ids := !redef_stmt_ids @ [var_def.var_defs_var_stmt_id]
			end 
		end
	var_defs_list;
    (* E.log "func id: %d, stmt id: %d, var id: %d\n" dua.dua_def.fun_id dua.dua_def.stmt_id dua.dua_def.var_id; 
    E.log "redef stmt ids: %d redefs\n" (List.length !redef_stmt_ids);*)
	
	(* find the function where the dua locates *)
	let fn = FindCil.fundec_by_name file dua.dua_def.fun_name in
	(* create the blast check flag var *)
	let blast_check_flag_var = Cil.makeLocalVar fn ~insert:true "blast_df_flag" Cil.intType in
	ignore (Cil.visitCilFunction (new transProgramVisitor dua !redef_stmt_ids blast_check_flag_var) fn);
	redef_stmt_ids := []
;;

let do_cegar_job (file: Cil.file) = 

    let dua_cnt = ref 0 in
    let duas_list = ref([]: MyUseDefAssocByHand.myDua list) in
    let var_defs_list = ref([]: MyUseDefAssocByHand.var_defs list) in

    if not !offline_instrumentation then begin
	    (* find out all var defs and uses *)
	    MyUseDefAssocByHand.find_var_defs_and_uses file;
        

        (* get the dir and base name of the program under test *)
        let file_dir = Filename.dirname file.fileName in
        let file_base_name = Filename.basename file.fileName in

	    (* dump var defs *)
	    let dump_file_name = file_dir ^ "/" ^ "obj-" ^ !cegar_model_checker ^ "/" ^ file_base_name ^ "." ^ !MyDfSetting.caut_var_defs_dump_file_name in
        var_defs_list := !my_var_defs_list;
	    dump_var_defs dump_file_name !var_defs_list;

        E.log "[DF] find duas by the RD computation ...\n";
        MyUseDefAssoc.find_intrap_dua_by_RD file;
        
        (* find pointer alaises *)
        MyPointerAnalysis.find_pointer_alias file;
        
        duas_list := !g_dua_list;
        dua_cnt := List.length !duas_list;
        E.log "dua cnts: %d\n" !dua_cnt;
        (* dump def-use pairs *)
        let dump_file_name = file_dir ^ "/" ^ "obj-" ^ !cegar_model_checker ^ "/" ^ file_base_name ^ "." ^ !MyDfSetting.caut_usedef_dump_file_name in
        dump_dua_list dump_file_name !duas_list; 

    end else begin

        (* read the var defs and duas from previously analyzed files *)
        var_defs_list := MyUseDefAssocByHand.read_var_defs !var_defs_data_file;
        E.log "var defs list: %d lines\n" (List.length !var_defs_list);
        duas_list := MyUseDefAssocByHand.read_duas !duas_data_file;
        dua_cnt := List.length !duas_list;
        E.log "duas list: %d pairs\n" !dua_cnt
        
    end;
	
	
	if (!cegar_dua_id <= 0) or (!cegar_dua_id > !dua_cnt) then begin
		E.log "dua id is incorrect or not specified !\n";
		exit 2
	end else begin
		E.log "transform prgoram for blast, dua id: %d\n" !cegar_dua_id;
		(* internal dua id starts from ZERO *)
		transform_prgoram_for_blast file (!cegar_dua_id-1) !duas_list !var_defs_list
	end
		
;;
	

let feature : featureDescr = 
  { fd_name = "blast";              
    fd_enabled = ref false;
    fd_description = "reachability checking by cegar-based model checkers";
    fd_extraopt = [ 
    	];
    fd_doit = 
    (function (f: file) -> 
      do_cegar_job f );
    fd_post_check = true
  }
