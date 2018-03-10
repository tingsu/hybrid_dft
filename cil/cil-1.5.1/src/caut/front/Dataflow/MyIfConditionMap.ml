open Cil

module E = Errormsg

(** 
	Maintain the mapping of conditions of if/while statements between the original code version
	and the simplified code version.

	The mapping is based on such an observation:
		The line and the code position of a condition will not change after code simplification.
		So the CIL pass of the orginal code will definitely have the same visiting sequence with the CIL pass
		of the simplified code.
*)
type if_condition_mapping = {

	mutable if_condition_file_name: string;
	mutable if_condition_fun_id: int;
	mutable if_condition_line: int; (* <if_condition_line, if_condition_id> is the partial KEY *)
	(* the stmt ids in the following two list are corresponding to each other *)
	mutable if_condition_original_stmt_ids: int list; (* the stmt id in the original code version *)
	mutable if_condition_simplified_stmt_ids: int list; (* the stmt id in the simplified code version *)
}

(** the global if condition mapping list *)
let g_if_condition_mapping_list = ref ([]: if_condition_mapping list)

(** print the if condition mapping list *)
let print_if_condition_mapping_list (mylist: if_condition_mapping list) = 
	E.log "======= Condition Mapping List =======\n";
	E.log "file fun line orig_sid simp_sid\n";
	let list_len = List.length mylist in
	for i=0 to list_len-1 do
		let item = List.nth mylist i in
		E.log "%s %d %d " item.if_condition_file_name item.if_condition_fun_id item.if_condition_line;
		E.log "[";
		List.iter
			begin fun id ->
				E.log "%d " id
			end
		 item.if_condition_original_stmt_ids;
		E.log "] [";
		List.iter
			begin fun id ->
				E.log "%d " id
			end
		 item.if_condition_simplified_stmt_ids;
		E.log "]\n"
	done
;;

(** Search the updated stmt id of a cried after code simplification 
		func_id: the id of the function where the cried locates at
		line: the line of the cried
		orig_stmt_id: the original stmt id of the cried
	Return
		the updated stmt id of the cried
*)
let search_cried_stmt_id_after_simplified (func_id: int) (line: int) (orig_stmt_id: int) : int =
	let target_map_list = 
		List.filter 
			begin fun map ->
				if map.if_condition_fun_id = func_id && map.if_condition_line = line then
					true
				else
					false
			end
		 !g_if_condition_mapping_list
	in
	let target_map = List.hd target_map_list in (* Here, only one element *)
	
	let pos = ref 0 in
	let list_len = List.length target_map.if_condition_original_stmt_ids in
	for i=0 to (list_len-1) do
		let id = List.nth target_map.if_condition_original_stmt_ids i in
		if id = orig_stmt_id then (* find the position of the original stmt id in the list *)
			pos := i
	done;
	(List.nth target_map.if_condition_simplified_stmt_ids !pos)

(** 
	#line (#id) #stmt_id --> #line (#id) #stmt_id' 
*)
class myIfConiditionVisitor (file: Cil.file) (func: Cil.fundec) (cilpass: int)= object(self)
	inherit nopCilVisitor

	method vstmt st = 
		match st.skind with
		| If (e,bt,bf,loc) ->
			if cilpass = 1 then begin  (* the first CIL pass *)
				let map_list = 
					List.filter (* does this line already exist ? *)
						begin fun item ->
							if item.if_condition_line = loc.line then
								true
							else 
								false
						end
					 !g_if_condition_mapping_list
				in
				if (List.length map_list) = 0 then begin (* is it the condition with a new line ? *)
					let item = {
							if_condition_file_name = file.fileName;
							if_condition_fun_id = func.svar.vid;
							if_condition_line = loc.line;
							if_condition_original_stmt_ids = [st.sid];
							if_condition_simplified_stmt_ids = []
							}
					in
					g_if_condition_mapping_list := !g_if_condition_mapping_list @ [item]
				end else begin (* if this line already exists, update item *)
					List.iter
						begin fun item ->
							if item.if_condition_line = loc.line then begin
								item.if_condition_original_stmt_ids <- item.if_condition_original_stmt_ids @ [st.sid]
							end
						end
					 !g_if_condition_mapping_list
				end
			end else if cilpass = 2 then begin (* the second CIL pass *)
				List.iter
					begin fun item ->
						if item.if_condition_line = loc.line then begin
							item.if_condition_simplified_stmt_ids <- item.if_condition_simplified_stmt_ids @ [st.sid]
						end
					end
				  !g_if_condition_mapping_list
				
			end;
			DoChildren
		| _ -> DoChildren
end
;;

(** set up if condition mapping in the first CIL pass *)
let setup_if_condition_mapping (file: Cil.file) = 
	List.iter
		begin fun g ->
			match g with
			| GFun (func, loc) ->
				(* the first CIL pass: 1 *)
				ignore (Cil.visitCilFunction (new myIfConiditionVisitor file func 1) func) 
			| _ -> ()
		end
	 file.globals
;;

(** update if condition mapping in the second CIL pass *)
let update_if_condition_mapping (file: Cil.file) =
	List.iter
		begin fun g ->
			match g with
			| GFun (func, loc) ->
				(* the second CIL pass: 2 *)
				ignore (Cil.visitCilFunction (new myIfConiditionVisitor file func 2) func)
			| _ -> ()
		end
	 file.globals;
	print_if_condition_mapping_list !g_if_condition_mapping_list
;;


(*****************************************************************************************)

(** cil stmt mapping relation between the orginal code and the simpilied code *)
type cil_stmt_mapping = {

	mutable cil_stmt_file_name: string;
	mutable cil_stmt_fun_id: int;
	mutable cil_stmt_line: int; (* <cil_stmt_line, cil_stmt_id> is the partial KEY *)
	(* the stmt ids in the following two list are corresponding to each other
	   NOTE: i am not very sure about this mapping relation.
	*)
	mutable cil_stmt_original_stmt_ids: int list; (* the stmt id in the original code version *)
	mutable cil_stmt_simplified_stmt_ids: int list; (* the stmt id in the simplified code version *)
}

(** the global cil stmt mapping list *)
let g_cil_stmt_mapping_list = ref ([]: cil_stmt_mapping list)

(** print the cil stmt mapping list *)
let print_cil_stmt_mapping_list (mylist: cil_stmt_mapping list) = 
	E.log "======= cil stmt Mapping List =======\n";
	E.log "file fun line orig_sid simp_sid\n";
	let list_len = List.length mylist in
	for i=0 to list_len-1 do
		let item = List.nth mylist i in
		E.log "%s %d %d " item.cil_stmt_file_name item.cil_stmt_fun_id item.cil_stmt_line;
		E.log "[";
		List.iter
			begin fun id ->
				E.log "%d " id
			end
		 item.cil_stmt_original_stmt_ids;
		E.log "] [";
		List.iter
			begin fun id ->
				E.log "%d " id
			end
		 item.cil_stmt_simplified_stmt_ids;
		E.log "]\n"
	done
;;



(** Search the updated stmt id of a cil stmt after code simplification 
		func_id: the id of the function where the cried locates at
		line: the line of the cried
		orig_stmt_id: the original stmt id of the cil stmt
	Return
		the updated stmt id of the cil stmt
*)
let search_cil_stmt_id_after_simplified (func_id: int) (line: int) (orig_stmt_id: int) : int =
	let target_map_list = 
		List.filter 
			begin fun map ->
				if map.cil_stmt_fun_id = func_id && map.cil_stmt_line = line then
					true
				else
					false
			end
		 !g_cil_stmt_mapping_list
	in
	let target_map = List.hd target_map_list in (* Here, only one element *)
	
	let pos = ref 0 in
	let list_len = List.length target_map.cil_stmt_original_stmt_ids in
	for i=0 to (list_len-1) do
		let id = List.nth target_map.cil_stmt_original_stmt_ids i in
		if id = orig_stmt_id then (* find the position of the original stmt id in the list *)
			pos := i
	done;
	(List.nth target_map.cil_stmt_simplified_stmt_ids !pos)
	
(** 
	#line (#id) #stmt_id --> #line (#id) #stmt_id' 
*)
class myCilStmtVisitor (file: Cil.file) (func: Cil.fundec) (cilpass: int)= object(self)
	inherit nopCilVisitor

	method vstmt st = 
		let action (sid:int) (line: int) = 
			if cilpass = 1 then begin  (* the first CIL pass *)
				let map_list = 
					List.filter (* does this line already exist ? *)
						begin fun item ->
							if item.cil_stmt_line = line then
								true
							else 
								false
						end
					 !g_cil_stmt_mapping_list
				in
				if (List.length map_list) = 0 then begin (* is it the condition with a new line ? *)
					let item = {
							cil_stmt_file_name = file.fileName;
							cil_stmt_fun_id = func.svar.vid;
							cil_stmt_line = line;
							cil_stmt_original_stmt_ids = [sid];
							cil_stmt_simplified_stmt_ids = []
							}
					in
					g_cil_stmt_mapping_list := !g_cil_stmt_mapping_list @ [item]
				end else begin (* if this line already exists, update item *)
					List.iter
						begin fun item ->
							if item.cil_stmt_line = line then begin
								item.cil_stmt_original_stmt_ids <- item.cil_stmt_original_stmt_ids @ [sid]
							end
						end
					 !g_cil_stmt_mapping_list
				end
			end else if cilpass = 2 then begin (* the second CIL pass *)
				List.iter
					begin fun item ->
						if item.cil_stmt_line = line then begin
							item.cil_stmt_simplified_stmt_ids <- item.cil_stmt_simplified_stmt_ids @ [sid]
						end
					end
				  !g_cil_stmt_mapping_list
				
			end
		in
		(* Here, we only consider If, Instr stmts *)
		match st.skind with
		| If(e,b1,b2,loc) ->
			action st.sid loc.line;
			DoChildren
		| Instr (isl) ->
			let head_instr = List.hd isl in
			(match head_instr with
			| Set (lv,e,loc) ->
				action st.sid loc.line
			| Call (lvo,e, el, loc) ->
				action st.sid loc.line
			| _ ->
				()
			);
			DoChildren
		| _ ->
			DoChildren
end
;;

(** set up cil stmt mapping in the first CIL pass *)
let setup_cil_stmt_mapping (file: Cil.file) = 
	List.iter
		begin fun g ->
			match g with
			| GFun (func, loc) ->
				(* the first CIL pass: 1 *)
				ignore (Cil.visitCilFunction (new myCilStmtVisitor file func 1) func) 
			| _ -> ()
		end
	 file.globals
;;

(** update if condition mapping in the second CIL pass *)
let update_cil_stmt_mapping (file: Cil.file) =
	List.iter
		begin fun g ->
			match g with
			| GFun (func, loc) ->
				(* the second CIL pass: 2 *)
				ignore (Cil.visitCilFunction (new myCilStmtVisitor file func 2) func)
			| _ -> ()
		end
	 file.globals;
	print_cil_stmt_mapping_list !g_cil_stmt_mapping_list
;;

