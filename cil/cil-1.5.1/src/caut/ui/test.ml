open Cil
open Cfg

module E = Errormsg
module IH = Inthash


(** definition of a SET with simple element type *)
module MY_SET = Set.Make (struct
			type t = int
			let compare x y = Pervasives.compare x y

		end)

let do_set_difference () = 
	E.log "I am here!!!\n";
	let myset_1 = ref MY_SET.empty in
	let myset_2 = ref MY_SET.empty in
	let list_1 = [1;2;3] in
	List.iter
		begin fun it -> 
			E.log "ele: %d\n" it;
			myset_1	:= MY_SET.add it !myset_1 (* MY_SET.add returns my_set_1 *)
		end
	  list_1;
	if MY_SET.is_empty !myset_1 then
		E.log "empty ??\n";
	E.log "set_1 size = %d\n" (MY_SET.cardinal !myset_1);

	myset_2 := List.fold_right MY_SET.add [2;4;5] !myset_2; (* List.fold_right *)
	
	E.log "set_2 size = %d\n" (MY_SET.cardinal !myset_2);
	let diff_set = MY_SET.diff !myset_1 !myset_2 in
	E.log "set_diff size = %d\n" (MY_SET.cardinal diff_set);
	MY_SET.iter
		begin fun it ->
			E.log "%d " it
		end
	  diff_set
;;

	

class varVisitor = object(self)
inherit nopCilVisitor

	method vvdec var = 
		E.log "var name: %s\n" var.vname;
		E.log "var vid: %d\n" var.vid;
		DoChildren

end

(*
class stmtVisitor = object(self)
inherit nopCilVisitor
		
	
	method vstmt st = 
		(* print stmt and its id *)
		E.log "stmt id: %d\n" st.sid;
		E.log "stmt: %a\n" d_stmt st;
		E.log "def? ";
		let reach = Reachingdefs.getRDs st.sid in
		(
		match reach with
		| Some(_,_, def_id_stmt_hash) -> 
			E.log "yes def\n";
			E.log "len: %d\n" (IH.length def_id_stmt_hash);
			Inthash.iter (fun def_var_id def_stmt_id_set ->
					E.log "def_id: %d\n" def_var_id;
					(
					match (IH.find def_stmt_id_set 0) with
					| Some(i) -> ignore(E.log "def stmt id: %d\n" i) 
					| None -> ()
					)
				     )def_id_stmt_hash;
			DoChildren
		| None -> 
			E.log "no def\n";
			DoChildren
		)
		0
end
*)


let myComputeRD  (f:file) (g:global) = 
	match g with
	| GFun(fd,lc)	->
		ignore (visitCilFunction (new varVisitor) fd);
		Reachingdefs.computeRDs fd
		(*visitCilFile (new stmtVisitor) f*)
		(*ignore (Reachingdefs.ppFdec fd)*)
	| _ -> ()

let domain (f:file) = 
	do_set_difference ()
	(* computeFileCFG f;
	iterGlobals f (myComputeRD f) *)

	
let feature : featureDescr = 
{
	fd_name = "test";
	fd_enabled = ref false;
	fd_description = "data flow test";

	fd_extraopt = [
		      ];
	fd_doit =  (function (f: file) -> 
			domain f);
	fd_post_check = true
}
