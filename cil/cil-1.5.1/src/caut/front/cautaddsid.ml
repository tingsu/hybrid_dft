open Pretty
open Cil
module E = Errormsg

(* debug 
let rec get_succs (st:stmt) (out:out_channel)= 
	for index = 0 to (List.length st.succs) - 1
		do
			let succ_stmt = List.nth st.succs index in
				fprint out 20 (d_stmt () succ_stmt) ;
				output_string out "\tId:";
				output_string out (string_of_int succ_stmt.sid);
				output_string out "\n\n" ;
				get_succs (succ_stmt out) 
		done ;
*)

class vstStmt (out : out_channel) = object (self)
	inherit nopCilVisitor
	
	
	method vstmt (st : stmt) =
		output_string out "\nstmt:\n";
		ignore(fprint out 20 (d_stmt () st));
		output_string out "\tId:";
		output_string out (string_of_int st.sid);
		output_string out "\n\n";
		
		(* print stmt's succs *)
		output_string out "Succ_list : \n" ;
		let succ_cnt = List.length st.succs in
		for index=0 to succ_cnt -1 
			do
				let stmt_t = List.nth st.succs index in
				ignore(fprint out 20 (d_stmt () stmt_t)) ;
				output_string out "\tId:";
				output_string out (string_of_int stmt_t.sid);
				output_string out "\n"
			done
		;
		
		DoChildren
	
	
	(*debug
	method vstmt (st:stmt) = 
		match st.skind with
			| If(_,_,_,_) -> 
					output_string out "stmt:\n";
					ignore(fprint out 20 (d_stmt () st));
					output_string out "\tId:";
					output_string out (string_of_int st.sid);
					output_string out "\n\n";
					get_succs st out;
					DoChildren
			| _  -> DoChildren
	*)
		
end

let printSid2File (f:file) : unit =
	let out = open_out "stid_list.txt" in
	visitCilFile (new vstStmt out) f;
	close_out out