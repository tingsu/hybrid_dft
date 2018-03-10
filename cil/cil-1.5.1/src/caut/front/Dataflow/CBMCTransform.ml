(**
 This file is used to prepare the program under test for data flow testing by cbmc.
 
For a def-use pair, we set a flag immediately following the def stmt, and check this flag with an if stmt immediately precede the use stmt.  In this if stmt, we put an assert statement, e.g., assert(FALSE). We also negate the flag after a redef stmt.
If this assert statement is reachable, then we actually find a path which could cover this def-use pair.
 
 TODO:
 We need to find out all possible redef stmts beforehand (e.g. redefs from variable aliases).
*)

open Cil
open MyUseDefAssocByHand

module E = Errormsg 

let cbmc_dua_id = ref 0
let cbmc_model_checker = ref ""
(** true: instrument all duas, false: instrument one dua*)
let cbmc_instrumentation_all = ref false
(* if true, read the duas and var defs from previously analyzed data to speed up the process of program instrumentation (especially for      large programs; if false, use the analyzed data on line *)
let offline_instrumentation = ref false
let duas_data_file = ref ""
let var_defs_data_file = ref ""

class transProgramVisitor (dua: MyUseDefAssocByHand.myDua) (redef_stmt_ids: int list) (cbmc_check_flag_var: Cil.varinfo) = object(self)
	inherit nopCilVisitor

    (* create cbmc if stmt for assertion checking
     * false: create for one pair testing
     * true: create for all pair testing
     * *)
	method private mkCBMCIfStmt (instrumentation_all: bool) : Cil.stmt = 

        (** create cbmc if stmt
         * if( cbmc_df_flag == 1 ) { assert(FALSE); }
         *)

        if instrumentation_all == false then begin
  		    (* create the assert statement *)
		    let assert_fun = emptyFunction "assert" in
		    let assert_fun_stmt = [mkStmtOneInstr(Call(None,Lval((Var(assert_fun.svar)),NoOffset),[Cil.integer 0],!currentLoc))] in
		
		    let tb = Cil.mkBlock assert_fun_stmt in
		    let fb = Cil.mkBlock [] in
		
		    let v = Cil.one in
		    let lv = ((Var(cbmc_check_flag_var)), NoOffset) in
		    let e = BinOp(Eq, Lval(lv), v, Cil.intType) in
		    (* create the if stmt *)
		    let if_stmt = Cil.mkEmptyStmt () in
		    if_stmt.skind <- If(e, tb, fb,  Cil.locUnknown);
		    (* E.log "if stmt: %a" d_stmt if_stmt; *)
		    if_stmt
        end else begin

        (** create cbmc if stmt for all-pair testing to differential different pairs 
        *   if( cbmc_df_flag_X == 1 ) { assert( cbmc_df_flag_X == 0); }
        * *)
            (* create the assert statement *)
            let assert_fun = emptyFunction "assert" in
            let v1 = Cil.zero in
            let lv1 = ((Var(cbmc_check_flag_var)), NoOffset) in
            let e1 = BinOp(Eq, Lval(lv1), v1, Cil.intType) in
            let assert_fun_stmt = [mkStmtOneInstr(Call(None,Lval((Var(assert_fun.svar)),NoOffset),[e1],!currentLoc))] in

            let tb = Cil.mkBlock assert_fun_stmt in
            let fb = Cil.mkBlock [] in

            let v2 = Cil.one in
            let lv2 = ((Var(cbmc_check_flag_var)), NoOffset) in
            let e2 = BinOp(Eq, Lval(lv2), v2, Cil.intType) in
            (* create the if stmt *)
            let if_stmt = Cil.mkEmptyStmt () in
            if_stmt.skind <- If(e2, tb, fb,  Cil.locUnknown);
            (* E.log "if stmt: %a" d_stmt if_stmt; *)
            if_stmt

        end

	
	method vstmt (st: Cil.stmt) = 
		let action s =
			match s.skind with
			| Instr (il) ->
                (* set the check flag after the DEF stmt *)
				if s.sid == dua.dua_def.stmt_id then begin
					let v = Cil.one in
					let lv = ((Var(cbmc_check_flag_var)), NoOffset) in
					let cbmc_instr = Set( lv, v, Cil.locUnknown ) in
					let new_instrs = il @ [cbmc_instr] in
					let new_stmt = Cil.mkStmt (Instr(new_instrs)) in

                    (* copy the sid, succs, preds and labels of the original stmt to the new created stmt!! *)
                    new_stmt.sid <- s.sid;
                    new_stmt.succs <- s.succs;
                    new_stmt.preds <- s.preds;
                    new_stmt.labels <- s.labels;

					E.log "add check after DEF!\n";
                    E.log "the DEF stmt: %a\n" d_stmt s;
					new_stmt
				end

                (* set the flag before the COMPUTATION use *)
				else if s.sid == dua.dua_use.stmt_id && (dua.dua_kind == 0) then begin
					let cbmc_if_stmt = self#mkCBMCIfStmt (!cbmc_instrumentation_all) in

                    (* update the cfg relation only for preds and succs *)
                    cbmc_if_stmt.succs <- [s];
                    cbmc_if_stmt.preds <- s.preds;

					let new_block = mkBlock [cbmc_if_stmt; s] in
					let comp_stmt = Cil.mkEmptyStmt () in
					comp_stmt.skind <- Block new_block;

					E.log "add check before USE!\n";
                    E.log "the USE stmt: %a\n" d_stmt s;
                    comp_stmt

                (* clear the flag after the REDEF stmt *)
				end else if (MyCilUtility.isIdInList s.sid redef_stmt_ids) then begin
					let v = Cil.zero in (* clear the check flag *)
					let lv = ((Var(cbmc_check_flag_var)), NoOffset) in
					let cbmc_instr = Set( lv, v, Cil.locUnknown ) in
					let new_instrs = il @ [cbmc_instr] in
					let new_stmt = Cil.mkStmt (Instr(new_instrs)) in

                    (* copy the sid, succs, preds and labels of the original stmt to the new created stmt!! *)
                    new_stmt.sid <- s.sid;
                    new_stmt.succs <- s.succs;
                    new_stmt.preds <- s.preds;
                    new_stmt.labels <- s.labels;
					new_stmt.labels <- s.labels; (* remember to add labels *)

					E.log "add check after REDEF!\n";
                    E.log "the REDEF stmt: %a\n" d_stmt s;
					new_stmt
				end else begin
					s
				end
			| If (e, b1, b2, loc) ->
				if (s.sid == dua.dua_use.stmt_id) && (dua.dua_kind == 1) then begin
					let cbmc_if_stmt = self#mkCBMCIfStmt (!cbmc_instrumentation_all) in

                    (* update the cfg relation only for preds and succs *)
                    cbmc_if_stmt.succs <- b1.bstmts;
                    cbmc_if_stmt.preds <- [s];

					b1.bstmts <- cbmc_if_stmt :: b1.bstmts; 
                    
					E.log "add check before USE!\n";
    				s
				end else if (s.sid == dua.dua_use.stmt_id) && (dua.dua_kind == 2) then begin
					let cbmc_if_stmt = self#mkCBMCIfStmt (!cbmc_instrumentation_all) in

                    (* update the cfg relation only for preds and succs *)
                    cbmc_if_stmt.succs <- b2.bstmts;
                    cbmc_if_stmt.preds <- [s];

					b2.bstmts <- cbmc_if_stmt :: b2.bstmts;
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

let set_cbmc_model_checker (model_checker: string) =

    cbmc_model_checker := model_checker
;;

let set_cbmc_instrumentation_all_mode (all: bool)=

    cbmc_instrumentation_all := all
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

let transform_prgoram_for_cbmc (file: Cil.file) (dua_id: int) (duas_list: MyUseDefAssocByHand.myDua list) (var_defs_list:                    MyUseDefAssocByHand.var_defs list)  = 

	(* get the target dua *)
	let dua = List.nth duas_list dua_id in
	E.log "def line: %d, use line: %d\n" dua.dua_def.var_line dua.dua_use.var_line;
	E.log "def sid: %d, use sid: %d\n" dua.dua_def.stmt_id dua.dua_use.stmt_id;
	E.log "dua kind: %d\n" dua.dua_kind;
	
	(* get the redef on the var of this dua.
	   Here, we currently only consider redef without var alias. One possible way to improve is to use must-alias analysis or other more precise alias analysis techniques
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
	

	(* find the function where the dua locates *)
	let fn = FindCil.fundec_by_name file dua.dua_def.fun_name in
	(* create the cbmc check flag var *)
	let cbmc_check_flag_var = Cil.makeLocalVar fn ~insert:true ("cbmc_df_flag_" ^ string_of_int(dua_id) ) Cil.intType in
	ignore (Cil.visitCilFunction (new transProgramVisitor dua !redef_stmt_ids cbmc_check_flag_var) fn);
	redef_stmt_ids := []
;;


let do_cbmc_job (file: Cil.file) = 

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
	    let dump_file_name = file_dir ^ "/" ^ "obj-" ^ !cbmc_model_checker ^ "/" ^ file_base_name ^ "." ^ !MyDfSetting.caut_var_defs_dump_file_name in
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
	    let dump_file_name = file_dir ^ "/" ^ "obj-" ^ !cbmc_model_checker ^ "/" ^ file_base_name ^ "." ^ !MyDfSetting.caut_usedef_dump_file_name in
	    dump_dua_list dump_file_name !duas_list;

    end else begin
        
        (* read the var defs and duas from previously analyzed files *)
        var_defs_list := MyUseDefAssocByHand.read_var_defs !var_defs_data_file;
        E.log "var defs list: %d lines\n" (List.length !var_defs_list);
        duas_list := MyUseDefAssocByHand.read_duas !duas_data_file;
        dua_cnt := List.length !duas_list;
        E.log "duas list: %d pairs\n" !dua_cnt

    end;

    if !cbmc_instrumentation_all = false then begin
        E.log "instrument one pair\n";

	    if (!cbmc_dua_id <= 0) or (!cbmc_dua_id > !dua_cnt) then begin
            E.log "dua id is incorrect or not specified !\n";
		    exit 2
	    end else begin
		    E.log "transform prgoram for cbmc, dua id: %d\n" !cbmc_dua_id;
		    (* internal dua id starts from ZERO *)
		    transform_prgoram_for_cbmc file (!cbmc_dua_id-1) !duas_list !var_defs_list
	    end

    end else if !cbmc_instrumentation_all = true then begin

        E.log "transform prgoram for cbmc with all duas\n";
        let instrument_one_dua dua =
            let dua_id = dua.dua_id in
            transform_prgoram_for_cbmc file (dua_id-1) !duas_list !var_defs_list
        in
        (* instrument all duas *)
        List.iter instrument_one_dua !duas_list

    end
		
;;


let feature : featureDescr = 
  { fd_name = "cbmc";              
    fd_enabled = ref false;
    fd_description = "reachability checking by cbmc";
    fd_extraopt = [ 
    	];
    fd_doit = 
    (function (f: file) -> 
      do_cbmc_job f);
    fd_post_check = true
  }
