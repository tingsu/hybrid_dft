(*
 * Simple instrumentation
 * 1. exclude all the extern declarations
 *)
module E = Errormsg

open Cil
 
let exclude_extern_declartions (f: file) : unit =
	let rec get_none_extern_declartions (none_decs : global list) (decs : global list) : (global list) =
		try
			let g = List.hd decs in
			match g with
			|GVarDecl (vi, l) ->
				(match vi.vtype with
				| TFun(t, arg, b, attrs) ->
					(*ignore(E.log "\nattr:%a\n" d_attrlist attrs);*)
					if List.exists 
						(fun (Attr(an, args): attribute)-> match an, args with
							| "missingproto", [] -> true
							| _ -> false ) attrs
					then get_none_extern_declartions (none_decs) (List.tl decs)
					else get_none_extern_declartions (none_decs @ [g]) (List.tl decs)
				| _ ->
					get_none_extern_declartions (none_decs @ [g]) (List.tl decs)		
				)
			|_ -> 
				get_none_extern_declartions (none_decs @ [g]) (List.tl decs) 
		with Failure "hd" -> none_decs
			| Failure "tl" -> none_decs
	in
	f.globals <- (get_none_extern_declartions [] f.globals)
 
let defaut_process (f : file) : unit =
	exclude_extern_declartions f