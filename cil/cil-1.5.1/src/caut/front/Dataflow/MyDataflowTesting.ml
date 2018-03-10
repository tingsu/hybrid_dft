open Cil

module E = Errormsg 

let setup_dataflow_testing_environment (f: Cil.file) = 
	E.log "\nSetup Data Flow Testing Environment ...\n";
	(* find out all var defs and uses in the original code by "--domakeCFG" in data flow testing *)
	MyUseDefAssocByHand.find_var_defs_and_uses f;
	
	if !MyDfSetting.caut_df_dua_by_hand = true then begin 
		E.log "[DF] find duas by hand ...\n";
		(* find out all var names in mark functions "CAUT_DEF_USE" in data flow testing *)
		MyUseDefAssocByHand.find_dua_var_name f;
		(* find context points for dua *)
		MyUseDefAssocByHand.find_df_context_point f;
		(* find dua *)
		MyUseDefAssocByHand.find_dua_by_hand f;
	end else begin
		E.log "[DF] find duas by the RD computation ...\n";
		MyUseDefAssoc.find_intrap_dua_by_RD f;
	end;

	(* transform var defs and uses from the original code to the simpified code by "--dosimplify" *)
	MyUseDefAssocByHand.transform_var_uses_and_defs_into_simplified f 0;

	(* transform duas from the original code to the simpified code by "--dosimplify" *)
	MyUseDefAssocByHand.transform_dua_to_simplified f 0
	
	(* find pointer alaises *)
	(* MyPointerAnalysis.find_pointer_alias f; *)

	(* setup if condition mapping before code simplification *)
	(* MyIfConditionMap.setup_if_condition_mapping f; *)

	(* find intermediate goals for dua's crieds by reaching definition *)
	(* MyIntermediateGoals.find_intermediate_goals_for_crieds f !(MyUseDefAssocByHand.g_dua_list) *)
;;

(** this function has not been called *)
let update_dataflow_testing_environment (f: Cil.file) =
	E.log "\nUpdate Data Flow Testing Environment ...\n";
	
	(* update if condition mapping after code simplification *)
	MyIfConditionMap.update_if_condition_mapping f;
	E.log "[finish] update if condition mapping\n";
	
	(* transform var defs and uses from the original code to the simpified code by "--dosimplify" *)
	MyUseDefAssocByHand.transform_var_uses_and_defs_into_simplified f 1;
	
	if !MyDfSetting.caut_df_dua_by_hand = true then begin 
		(* transform context points from the original code to the simpified code by "--dosimplify" *)
		MyUseDefAssocByHand.transform_df_context_point_into_simplified f 1;
	end;
	
	(* transform pointer alaises from the original code to the simpified code by "--dosimplify" *)
	MyPointerAnalysis.transform_pointer_alias_into_simplified f 1;
        (* transform dua's var def or use from the original code to the simpified code by "--dosimplify" *)
	MyUseDefAssocByHand.transform_dua_deforuse_to_simplified f 1;
	
	(* transform duas from the original code to the simpified code by "--dosimplify" *)
	MyUseDefAssocByHand.transform_dua_to_simplified f 1;
	(* transform intermediate goals from the original code version to the simpified code version *)
	MyIntermediateGoals.transform_intermediate_goals_to_simplified f 1;

	(* find funcall alias on duas *)
	MyPointerAnalysis.find_funcall_alias f
;;

(******************************************)
(**Since CAUT works on simplified code, we call this function to operate data flow testing on the simplified code version *) 
let operate_data_flow_testing_on_simplified_code (f: Cil.file) = 
	
	E.log "\nSetup Data Flow Testing Environment ...\n";
	(* find out all var defs and uses in the original code by "--domakeCFG" in data flow testing *)
	MyUseDefAssocByHand.find_var_defs_and_uses f;
	
	if !MyDfSetting.caut_df_dua_by_hand = true then begin 
		E.log "[DF] find duas by hand ...\n";
		(* find out all var names in mark functions "CAUT_DEF_USE" in data flow testing *)
		MyUseDefAssocByHand.find_dua_var_name f;
		(* find context points for dua *)
		MyUseDefAssocByHand.find_df_context_point f;
		(* find dua *)
		MyUseDefAssocByHand.find_dua_by_hand f;
	end else begin
		E.log "[DF] find duas by the RD computation ...\n";
		MyUseDefAssoc.find_intrap_dua_by_RD f
	end;
	
	(* find pointer alaises *)
	MyPointerAnalysis.find_pointer_alias f;
	
	(* find intermediate goals for dua's crieds by reaching definition *)
	MyIntermediateGoals.find_intermediate_goals_for_crieds f !(MyUseDefAssocByHand.g_dua_list);
	
	(***********set no transform  **********)
	
	(* transform var defs and uses from the original code to the simpified code by "--dosimplify" *)
	MyUseDefAssocByHand.transform_var_uses_and_defs_into_simplified f 0;
	
	if !MyDfSetting.caut_df_dua_by_hand = true then begin 
		(* transform context points from the original code to the simpified code by "--dosimplify" *)
		MyUseDefAssocByHand.transform_df_context_point_into_simplified f 0;
	end;
	
	(* transform pointer alaises from the original code to the simpified code by "--dosimplify" *)
	MyPointerAnalysis.transform_pointer_alias_into_simplified f 0;
        (* transform dua's var def or use from the original code to the simpified code by "--dosimplify" *)
	MyUseDefAssocByHand.transform_dua_deforuse_to_simplified f 0;
	
	(* transform duas from the original code to the simpified code by "--dosimplify" *)
	MyUseDefAssocByHand.transform_dua_to_simplified f 0;
	(* transform intermediate goals from the original code version to the simpified code version *)
	MyIntermediateGoals.transform_intermediate_goals_to_simplified f 0;

	(* find funcall alias on duas *)
	MyPointerAnalysis.find_funcall_alias f
;;
