open Cil

module UD = Usedef
module E = Errormsg

(* the dump file name for cried's intermediate goals *)
let caut_cried_intermediate_goals_dump_file_name = ref "intergoal.txt"

(** an intermediate goal 

	Here, we regard the statement which has the data affection on some variable of CRIED_i as an intermediate goals.
	We only consider intermediate goals which locates between CRIED_i-1 and CRIED_i.
	It's demanded by our path search algorithm.
	Limitations:
		We do not consider recursive data affection and pointer aliases.
		And we only consider intermediate goals in an intra-procedural range.
		It aims to make the algorithm simple.
		
		Otherwise we have to consider intra-procedural and inter-procedural reaching definitions.
		It's complicate and do not scalable to large program.
		
*)
type interGoal = {
	
    mutable intergoal_file_name: string;
    mutable intergoal_fun_name: string;
    mutable intergoal_fun_id: int;
    
    mutable intergoal_cried: MyCriticalEdge.criticalEdge list; (* the nearest cried for the intermediate goal *)
    mutable intergoal_stmt_id: int; (* the stmt id the intermediate goal *)
    mutable intergoal_stmt_line: int;
    
    mutable intergoal_var_name: string;
    mutable intergoal_var_id: int; (* which var is under data affection ? *)
    
    mutable intergoal_dua_id: int; (* belong to which dua ? *)
    mutable intergoal_dua_cried_id: int; (* belong to which cried ? *)
}

(** the goal list to store crieds' intermediate goals *)
let g_interGoalList = ref ([]: interGoal list)


(** dump intermediate goals 
	#dua_id #dua_cried_id #fun_id #cried_var_id #cried_var_name #interm_goal_stmt_id #interm_goal_line #interm_goal_cried
*)
let dump_intermediate_goals_list (filename:string) (mylist: interGoal list) = 
	let dump_channel = open_out filename in
    List.iter
    	begin fun item ->
    	  output_string dump_channel (string_of_int item.intergoal_dua_id); (* dua_id, start with 1 *)
		  output_string dump_channel " ";
		  output_string dump_channel (string_of_int item.intergoal_dua_cried_id); (* dua_cried_id, start with 1 *)
		  output_string dump_channel " ";
		  output_string dump_channel (string_of_int item.intergoal_fun_id); (* fun_id *)
		  output_string dump_channel " ";
		  output_string dump_channel (string_of_int item.intergoal_var_id); (* var_id *)
		  output_string dump_channel " ";
		  output_string dump_channel (item.intergoal_var_name); (* var_name *)
		  output_string dump_channel " ";
		  output_string dump_channel (string_of_int item.intergoal_stmt_id); (* stmt_id *)
		  output_string dump_channel " ";
		  output_string dump_channel (string_of_int item.intergoal_stmt_line); (* stmt_line *)
		  output_string dump_channel " ";
		  
		  (* print the nearest cried *)
		  let crieds_cnt = List.length item.intergoal_cried in
		  if crieds_cnt = 0 then
			 output_string dump_channel "no#"
		  else begin
			 for i=0 to crieds_cnt-1 do
				let cried = List.nth item.intergoal_cried i in
				output_string dump_channel (string_of_int cried.MyCriticalEdge.funId);
				output_string dump_channel ":";
				output_string dump_channel (string_of_int cried.MyCriticalEdge.criStmtId);
				output_string dump_channel ":";
				output_string dump_channel (string_of_int cried.MyCriticalEdge.criStmtBranch);
				output_string dump_channel ":";
				output_string dump_channel (string_of_int cried.MyCriticalEdge.criLine);
				if i = crieds_cnt-1 then
				   output_string dump_channel "#"
				else
				   output_string dump_channel ";"
			 done
		  end;
		  output_string dump_channel "\n";
		  flush dump_channel
    	end
      mylist
;;

(** is the interm goal for the target cried ?
	Here, we only consider interm goals between the target cried and the upper cried.
	Otherwise, we will consider interm goals in the function where the target cried locates at.
	Global: MyUseDefAssocByHand.g_dua_list
*)
let is_interm_goal_for_cried (file: Cil.file) (dua_id: int) (target_cried_id: int) (interm_goal_sid: int) : bool =
	let dua_list = (* find the target dua *)
		List.filter
			begin fun item ->
				if item.MyUseDefAssocByHand.dua_id = dua_id then
					true
				else
					false
			end
		  !(MyUseDefAssocByHand.g_dua_list)
	in
	let target_dua = List.hd dua_list in (* Only one dua *)
	let cried_list = ref([]: MyCriticalEdge.criticalEdge list) in
	cried_list := !cried_list @ target_dua.MyUseDefAssocByHand.dua_def_crieds @ target_dua.MyUseDefAssocByHand.dua_use_crieds; (* collect all crieds of the target dua *)
	let target_cried = (List.nth !cried_list target_cried_id) in
	if target_cried_id = 0 then begin (* the target cried is the first cried *)
		true
	end else begin (* the target cried is not the first cried *)
		let upper_cried = (List.nth !cried_list (target_cried_id-1)) in
		if (MyCilUtility.is_stmtA_a_pred_of_stmtB (* check whether the interm goal is between the upper cried and the target cried *)
						file 
						target_cried.MyCriticalEdge.funId 
						target_cried.MyCriticalEdge.criStmtId 
						target_cried.MyCriticalEdge.funId 
						interm_goal_sid
			) &&
			(MyCilUtility.is_stmtA_a_succ_of_stmtB 
						file 
						upper_cried.MyCriticalEdge.funId 
						upper_cried.MyCriticalEdge.criStmtId 
						target_cried.MyCriticalEdge.funId 
						interm_goal_sid) 
		then 
			true (* return true *)
		else
			false (* return false *)
	end
	

class myCriedStmtVisitor (file: Cil.file) (func: Cil.fundec) (dua_id: int) (dua_cried_id: int) (cried: MyCriticalEdge.criticalEdge) = object
   inherit nopCilVisitor
   
   method vstmt st = 
     match st.skind with
     | If(_,_,_,_) ->
		if st.sid == cried.MyCriticalEdge.criStmtId then begin (* find the target cried stmt *)
          
          (* make an Instruction from this cired statement *)
		  let cried_instr = Instruction.of_stmt_first file func st in
		  
		  (* find this cried stmt's intermediate goals*)
		  let data_affection_list = IntermediateGoals.find_direct_reaching_definition cried_instr in 
		  
		  match data_affection_list with
		  | Some(dal) -> (* has intermediate goals *)
		  	let list_len = List.length dal in
		  	for i=0 to list_len-1 do
		  	
		  		let da = List.nth dal i in 
		  		(* check whether the interm goal is between the upper cried and the target cried *)
		  		(* E.log "Checking Intermediate Goals ... \n"; *)
		  		let isIntermGoal = is_interm_goal_for_cried file dua_id dua_cried_id  da.IntermediateGoals.data_affection_stmt.sid in
		  		(* let isIntermGoal = true in *)
		  		
		  		(** We only consider intermmediate goals between the target cried and the upper cried*)
		  		if isIntermGoal = true then begin
			  		(* find the data affection stmt's nearest cried *)
			  		let da_cried = MyCriticalEdge.findNearestCriticalEdge da.IntermediateGoals.data_affection_stmt in
			  		
			  		match da_cried with
			  		| Some( res ) -> (* has cried *)
			  			let cried_stmt_id, cried_stmt_branch, cried_stmt_line = res in
			  			let cried_item = {
			  			
			  				MyCriticalEdge.funName = func.svar.vname;
			  				MyCriticalEdge.funId = func.svar.vid;
			  				MyCriticalEdge.criStmtId = cried_stmt_id;
			  				MyCriticalEdge.criStmtBranch = cried_stmt_branch;
			  				MyCriticalEdge.criLine = cried_stmt_line;
			  				
			  			} in
				  		let item = {
				  				intergoal_file_name = file.fileName;
				  				intergoal_fun_name = func.svar.vname;
				  				intergoal_fun_id = func.svar.vid;
				  				intergoal_dua_id = dua_id;
				  				intergoal_dua_cried_id = dua_cried_id+1;
				  				
				  				intergoal_stmt_id = da.IntermediateGoals.data_affection_stmt.sid;
				  				(* get the stmt's line *)
				  				intergoal_stmt_line = da.IntermediateGoals.data_affection_stmt_line;
				  				intergoal_var_id = da.IntermediateGoals.data_affection_var_id;
				  				intergoal_var_name = da.IntermediateGoals.data_affection_var_name;
				  				
				  				intergoal_cried = [cried_item] (* the nearest cried *)
				  			}
				  		in
				  		g_interGoalList := !g_interGoalList @ [item]
				  	| None -> (* has no cried *)
				  		let item = {
				  				intergoal_file_name = file.fileName;
				  				intergoal_fun_name = func.svar.vname;
				  				intergoal_fun_id = func.svar.vid;
				  				intergoal_dua_id = dua_id;
				  				intergoal_dua_cried_id = dua_cried_id+1;
				  				
				  				intergoal_stmt_id = da.IntermediateGoals.data_affection_stmt.sid;
				  				(* get the stmt's line *)
				  				intergoal_stmt_line = da.IntermediateGoals.data_affection_stmt_line;
				  				intergoal_var_id = da.IntermediateGoals.data_affection_var_id;
				  				intergoal_var_name = da.IntermediateGoals.data_affection_var_name;
				  				
				  				intergoal_cried = [] (* no cried *)
				  			}
				  		in
				  		g_interGoalList := !g_interGoalList @ [item]
				 end
		  	done
		  | None -> ()
		 end;
		 DoChildren
     | _ -> DoChildren

end
;;



(** find intermediate goals for vars in duas' crieds *)
let find_intermediate_goals_for_crieds (file: Cil.file) (mydua_list: MyUseDefAssocByHand.myDua list) =
	let debug = false in
	
	let list_len = List.length mydua_list in
	
	if debug = true then begin
		E.log "[intergoal]dua count: %d\n" list_len
	end;
	
	for i=0 to list_len-1 do (* iterate all duas *)
		let dua = List.nth mydua_list i in
		
		if debug = true then begin
			E.log "%dth dua\n" (i+1)
		end;
		
		(** handle dua's def crieds *)
		let def_crieds = dua.MyUseDefAssocByHand.dua_def_crieds in 
		let def_cried_cnt = List.length def_crieds in
		for j=0 to (def_cried_cnt-1) do (* iterate a dua's var def's crieds *)
		
			let id = dua.MyUseDefAssocByHand.dua_id in (* dua id *)
			let cried_id = j in (* cried id *)
			
			if debug = true then begin
				E.log "%dth cried\n" cried_id
			end;
			
			let def_cried = List.nth def_crieds j in (* var def's cried *)
			
			List.iter 
       			begin fun g ->
				  match g with
			 	  | GFun(func, loc) ->
			 	  	 (* find the target function where the cried locates *)
					 if func.svar.vid = def_cried.MyCriticalEdge.funId then
					 	ignore (Cil.visitCilFunction (new myCriedStmtVisitor file func id cried_id def_cried) func)
					 else
					 	()
				  | _ -> ()
      			end
      		file.globals
			
		done;
		
		(** handle dua's use crieds *)
		let use_crieds = dua.MyUseDefAssocByHand.dua_use_crieds in
		let use_cried_cnt = List.length use_crieds in
		for j=0 to use_cried_cnt-1 do  (* iterate a dua's var use's crieds *)
			
			let id = dua.MyUseDefAssocByHand.dua_id in (* dua id *)
			let cried_id = j + def_cried_cnt in (* cried id *)
			
			if debug = true then begin
				E.log "%dth cried\n" (j + def_cried_cnt)
			end;
			
			let use_cried = List.nth use_crieds j in (* var use's cried *)
			
			List.iter 
       			begin fun g ->
				  match g with
			 	  | GFun(func, loc) ->
			 	  	 (* find the target function where the cried locates *)
					 if func.svar.vid = use_cried.MyCriticalEdge.funId then begin
					 	ignore (Cil.visitCilFunction (new myCriedStmtVisitor file func id cried_id use_cried) func)
					 end
				  | _ -> ()
      			end
      		file.globals
		done
	done
;;

(** transform intermediate goals from the original code version to the simpified code version.
    Here, we need to update an intermediate goal's cried stmt id and its own stmt id.
*)
let transform_intermediate_goals_to_simplified (file: Cil.file) (to_transform: int) = 

	let find_intermediate_goal_stmt func line = (* find the stmt where the intermediate goal stmt locates at by line in func *)
		List.find
			begin fun st ->
				match st.skind with
				| Instr (instrl) ->
					List.exists (* does it exist ? *)
						begin fun ins ->
							match ins with
							| Set (_, _, loc) | Call(_, _, _, loc) -> 
								if loc.line = line then
									true
								else
									false
							| _ -> false
						end
					  instrl
				| _ -> false
			end
		 func.sallstmts
	in
	
	(* let flip_branch_choice (orig_branch: int) = (* flip branch choice *)
				if orig_branch = 1 then
					0
				else
					1
	in*)
	if to_transform = 1 then begin
	List.iter
		begin fun intergoal ->
		
			List.iter (* update the intermediate goal's cried *)
				begin fun cried ->
					let updated_cried_sid = MyIfConditionMap.search_cried_stmt_id_after_simplified cried.MyCriticalEdge.funId cried.MyCriticalEdge.criLine cried.MyCriticalEdge.criStmtId in
					cried.MyCriticalEdge.criStmtId <- updated_cried_sid;
					(* TODO do not need to flip choice ?? *)
					cried.MyCriticalEdge.criStmtBranch <- cried.MyCriticalEdge.criStmtBranch
				end
			  intergoal.intergoal_cried;
			
			(* update the intermediate goal's stmt id *)
			let func = FindCil.fundec_by_name file intergoal.intergoal_fun_name in
			let item_stmt = find_intermediate_goal_stmt func intergoal.intergoal_stmt_line in
			intergoal.intergoal_stmt_id <- item_stmt.sid
		end
	  !g_interGoalList
	end;
    let dump_file_name = file.fileName ^ "." ^ !caut_cried_intermediate_goals_dump_file_name in 
    (* dump intermediate goals *)
    dump_intermediate_goals_list dump_file_name !g_interGoalList
;;
