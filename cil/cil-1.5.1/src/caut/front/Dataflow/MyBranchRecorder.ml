(** 
	This module is to record all branches in the file.
*)
open Cil

let dump_branches_file_name = ref "branches.txt"

(** one if stmt has two branches *)
type my_branch = {

	mutable my_branch_fun_name: string;
	mutable my_branch_fun_id: int;
	mutable my_branch_if_stmt_id: int;
	mutable my_branch_choice: int; (* branch choice *)
	mutable my_branch_if_stmt_line: int;
	mutable my_branch_if_succs_list: int list; (* this field is used to record all succs with IF stmt type *)
}

let g_my_branch_list = ref([]: my_branch list)

(* file format 
  fun_name fun_id if_stmt_id if_stmt_choice if_stmt_line if_succs_list(id1;id2;id3#)
*)
let dump_my_branch_list (branch_list: my_branch list) (filename: string) =

	let dump_channel = open_out filename in
    List.iter 
		begin fun item -> 
			output_string dump_channel item.my_branch_fun_name;
			output_string dump_channel " ";
			output_string dump_channel (string_of_int item.my_branch_fun_id);
			output_string dump_channel " ";
			output_string dump_channel (string_of_int item.my_branch_if_stmt_id);
			output_string dump_channel " ";
			output_string dump_channel (string_of_int item.my_branch_choice);
			output_string dump_channel " ";
			output_string dump_channel (string_of_int item.my_branch_if_stmt_line);
			output_string dump_channel " ";
			let succ_cnt = List.length item.my_branch_if_succs_list in
			if succ_cnt = 0 then begin (* if no succs, output 'no' *)
				output_string dump_channel "no\n";
			end else begin
				for i=0 to succ_cnt-1 do
					let succ_id = List.nth item.my_branch_if_succs_list i in
					output_string dump_channel (string_of_int succ_id);
					if i = succ_cnt-1 then begin
						output_string dump_channel "#\n";
					end else begin
						output_string dump_channel ";";
					end
				done
			end;
			(* flush *)
		    flush dump_channel
		end
	  branch_list
;;

(** find all succs with if stmt type for a specified stmt 
    NOTE we currently only compute if succs in a single function.
    BUT it is easy to extend it to the whole program by constructing ICFG.
*)
let find_all_if_succs (tarIfStmt: Cil.stmt): int list = 
   (* record all visited successors statements when finding all succs for "tarIfStmt" *)
   let visitedSuccStmtIdList = ref ([]:int list) in
   (* record all if succs ids *)
   let if_succs_id_list = ref([]:int list) in
   
   (* put the target stmt id into the visited list *)
   visitedSuccStmtIdList := !visitedSuccStmtIdList @ [tarIfStmt.sid];
   (* put the target stmt into the stmt queue *)
   let stmtQueue = Queue.create () in
   Queue.add tarIfStmt stmtQueue;
   while not (Queue.is_empty stmtQueue) do
      (* take the head stmt *)
      let sm = Queue.take stmtQueue in
      let smSuccCount = List.length sm.succs in
      (* traverse its succs *)
      for i=0 to smSuccCount-1 do
		let smSucc = List.nth sm.succs i in
		if MyCilUtility.isIdInList smSucc.sid !visitedSuccStmtIdList then
		   () (* exists, skip it *)
		else begin
			(match smSucc.skind with
			| If(e, b1, b2, loc) ->
				if_succs_id_list := !if_succs_id_list @ [smSucc.sid]
			| _ -> ()
			);
		   (* otherwise, keep it *)
		   Queue.add smSucc stmtQueue;
		   visitedSuccStmtIdList := !visitedSuccStmtIdList @ [smSucc.sid]
		end
      done
   done;
   !if_succs_id_list

class myBranchVisitor (func: Cil.fundec) = object(self)
	inherit nopCilVisitor
	
	method vstmt st = 
		match st.skind with
		| If(e,b1,b2,loc) ->
			let branch_true = {my_branch_fun_name=func.svar.vname;
					    my_branch_fun_id = func.svar.vid;
					   	my_branch_if_stmt_id = st.sid;
					   	my_branch_if_stmt_line = loc.line;
					   	my_branch_choice = 1; 
					   	(* NOTE, succs[0] for true branch, succs[1] for false branch, they are opposite against the result of "--domakeCFG" *)
					   	my_branch_if_succs_list = (find_all_if_succs (List.nth st.succs 0))
					   	} in 	
			 let branch_false = {my_branch_fun_name=func.svar.vname;
					    my_branch_fun_id = func.svar.vid;
					   	my_branch_if_stmt_id = st.sid;
					   	my_branch_if_stmt_line = loc.line;
					   	my_branch_choice = 0;
					   	my_branch_if_succs_list = (find_all_if_succs (List.nth st.succs 1))
					   	} in 	
		    g_my_branch_list := !g_my_branch_list @ [branch_true] @ [branch_false] ;
		    DoChildren
		| _ -> DoChildren
end;;

let do_record_branch (file: Cil.file) =
	List.iter
		begin fun g ->
			match g with
			| GFun(func, loc) ->	
				ignore (Cil.visitCilFunction (new myBranchVisitor func) func);
			| _ -> ()
		end
	  file.globals;
    let dump_file_name = file.fileName ^ "." ^ !dump_branches_file_name in 
	dump_my_branch_list !g_my_branch_list dump_file_name
;;

let feature : featureDescr = 
  { fd_name = "branch";              
    fd_enabled = ref false;
    fd_description = "recording all branches in the file";
    fd_extraopt = [ ];
    fd_doit = 
    (function (f: file) -> 
      do_record_branch f);
    fd_post_check = true
  }
