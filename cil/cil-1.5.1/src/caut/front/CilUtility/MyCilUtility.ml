open Cil

module E = Errormsg

(** Get the function name from an exp (from Instr(Call)) *)
let getfunNameFromExp (e': exp) : string= 
   match e' with
   | Lval (lh, off) ->
      (
      match lh with
      | Var (vi) ->
	 vi.vname
      | _ -> ""
      )
   | _ -> ""
;;

(** find a stmt in @fundec by a @sid *)
let findStmtbyId (func: Cil.fundec) (sid: int) : Cil.stmt option = 
   try
     Some(List.find
       begin fun stmt ->
	  if stmt.sid == sid then
	     true
	  else
	     false
       end
      func.sallstmts)
   with
   | Not_found -> 
   	  E.log "MyCilUtility@findStmtbyId error!\n";
   	  None (* Not_found *)
;;

(** get the line of a Cil.stmt *)
let getStmtLoc (st: Cil.stmt) : int= 
	match st.skind with
	| Instr (insl) ->
		let loc = Cil.get_instrLoc (List.hd insl) in
		loc.line
	| _ -> 
		let loc = Cil.get_stmtLoc st.skind in
		loc.line

(** check whether @id' is in @idList *)
let isIdInList (id': int) (idList: int list) : bool = 
    List.exists 
      begin fun id ->
        if id' == id then
          true
        else
          false
      end
   idList
;;

(** find the var id according to its name 
	file: the file
	func: the func where the var name locates at
	var_name: the var name
	Return: (true/false, var_id/0)
*)
let findVarIdfromVarName (file:Cil.file) (func: Cil.fundec) (var_name: string) = 
    
	let sformal_var_list = 
		List.filter
			begin fun var ->
				if var.vname = var_name then
					true
				else
					false
			end
		 func.sformals (* is it a formal var ? *)
	in
	if sformal_var_list != [] then begin (* find it *)
		let target_var = List.hd sformal_var_list in
		target_var.vid

	end else begin (* not find, continue search slocals *)
	
		let local_var_list =  
			List.filter
				begin fun var ->
					if var.vname = var_name then
						true
					else
						false
				end
			  func.slocals (* is it a local var ? *)
		in
		if local_var_list != [] then begin (* find it *)
			let target_var = List.hd local_var_list in
			target_var.vid

		end else begin (* not find, continue search globals *)

			let global_var_list = ref([]: Cil.varinfo list) in
			List.iter
				begin fun g ->
					match g with
					| GVar (v,_,loc) | GVarDecl(v,loc) -> 
						(match v.vtype with
						| TFun _ -> ()  (* skip variables of function type *)
						| _ -> 
							if v.vname = var_name then
								global_var_list := !global_var_list @ [v]
							else
								()
						)
					| _ -> ()
				end
	  		 file.globals; (* is it a global var ? *)
			if !global_var_list != [] then begin (* find it *)
				let target_var = List.hd !global_var_list in
				target_var.vid
			end else begin (* not find ?? *)
				ignore (E.log "**** Find Var Id Failed ?? ****\n");
				ignore (E.log "var_name = %s\n" var_name);
				ignore (E.log "****************\n");
				exit 2 (* terminate the process *)
			end
		end
	end
;;

(** is a stmt (A) a predecessor of another stmt (B) *)
let is_stmtA_a_pred_of_stmtB (file: Cil.file) (func_id_of_stmtB: int) (stmt_id_of_stmtB: int) (func_id_of_stmtA: int) (stmt_id_of_stmtA: int) : bool =
	if func_id_of_stmtB != func_id_of_stmtA then (* if these two stmts are not in the same function, return true *)
		true
	else begin
		let ret = ref false in
		List.iter
			begin fun g ->
				match g with
				| GFun(func, loc) -> (* find the target func *)
					if func.svar.vid == func_id_of_stmtA then begin
						let stmt_of_stmtB = findStmtbyId func stmt_id_of_stmtB in
						match stmt_of_stmtB with
						| Some(st) ->
							let predQueue = Queue.create () in (* create a queue *)
							let idList = ref([]: int list) in (*a list to store stmt id *)
							
							for i=0 to ((List.length st.preds)-1) do
								let pred_stmt = (List.nth st.preds i) in
								Queue.add pred_stmt predQueue;
								idList := !idList @ [pred_stmt.sid]
							done;
							while not (Queue.is_empty predQueue) do
								let sm = Queue.take predQueue in
								if sm.sid = stmt_id_of_stmtA then begin
									ret := true;
									(* find the stmtA, clear the Queue *)
									Queue.clear predQueue
								end else begin
									for j=0 to ((List.length sm.preds)-1) do
										let pred_stmt = (List.nth sm.preds j) in
										(* only add unvisited preds *)
										if not (isIdInList pred_stmt.sid !idList) then begin
											idList := !idList @ [pred_stmt.sid];
											Queue.add pred_stmt predQueue
										end	else
											()
									done
								end
							done;
							idList := []
						| None -> ()
					end
				| _ -> ()
			end
		  file.globals;
		!ret (* return the result *)
	end
;;

(** is a stmt (A) a successor of another stmt (B) *)
let is_stmtA_a_succ_of_stmtB (file: Cil.file) (func_id_of_stmtB: int) (stmt_id_of_stmtB: int) (func_id_of_stmtA: int) (stmt_id_of_stmtA: int) : bool =
	if func_id_of_stmtB != func_id_of_stmtA then (* if these two stmts are not in the same function, return true *)
		true
	else begin
		let ret = ref false in
		List.iter
			begin fun g ->
				match g with
				| GFun(func, loc) -> (* find the target func *)
					if func.svar.vid = func_id_of_stmtA then begin
						let stmt_of_stmtB = findStmtbyId func stmt_id_of_stmtB in
						match stmt_of_stmtB with
						| Some(st) ->
							let succQueue = Queue.create () in
							let idList = ref([]: int list) in (*a list to store stmt id *)
							
							for i=0 to ((List.length st.succs)-1) do
								let succ_stmt = (List.nth st.succs i) in
								Queue.add succ_stmt succQueue;
								idList := !idList @ [succ_stmt.sid]
							done;
							while not (Queue.is_empty succQueue) do
								let sm = Queue.take succQueue in
								if sm.sid = stmt_id_of_stmtA then begin
									ret := true;
									(* find the stmtA, clear the Queue *)
									Queue.clear succQueue
								end else begin
									for j=0 to ((List.length sm.succs)-1) do
										let succ_stmt = (List.nth sm.succs j) in
										(* only add unvisited succs *)
										if not (isIdInList succ_stmt.sid !idList) then begin
											idList := !idList @ [succ_stmt.sid];
											Queue.add succ_stmt succQueue
										end
										else
											()
									done
								end
							done;
							idList := []
						| None -> ()
					end
				| _ -> ()
			end
		  file.globals;
		!ret (* return the result *)
	end
;;
