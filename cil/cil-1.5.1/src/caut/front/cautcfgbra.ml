(** cautprep.ml
	Author: Jiang Siyuan
	Date:4/25/2010
	Description: collecting original branch-info(have to make a CFG first)
*)

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let bra_sids = ref ([] : int list)

class colbraVisitor = object (self)
	inherit nopCilVisitor

(**)	
	method vstmt (st : stmt) =
		match st.skind with
			| Instr _ | Return _ | Goto _ | Block _ | TryFinally _ | TryExcept _ -> 
				DoChildren
			| Break _ | Continue _ | Switch _ -> 
				ignore(E.error ("error number : shall not occure \"break, continue or switch\"")); 
				DoChildren
			| If _ ->
				(* branch id is the statement id *)
				bra_sids := (!bra_sids) @ [st.sid]; 
				DoChildren
			| Loop (bl1, loc, Some(st_continue), Some(st_break)) -> 
				bra_sids := (!bra_sids) @ [st_continue.sid] @ [st_break.sid];
				DoChildren
			| _ ->
				DoChildren

end

let doGlobal = function 
	GFun(fi, _) ->
		(* compute CFG *)
		prepareCFG fi;
		computeCFGInfo fi true;
	| _ -> ()

let intarr2str (l : int list) : string =
	let str_hd = "/*CAUT branches:\n" in
	let rec myIter (myList : int list) : string =
		let str = ref "" in
		ignore(
			try 
				str := "#" ^ (string_of_int (List.hd myList) );
			with Failure "hd" -> str := "";
		);
		try
			(!str) ^ ( myIter (List.tl myList) )
		with Failure "tl" -> ""
	in
	str_hd ^ (myIter l) ^ "*/"
	
let default_process (f: file) = 
	(* compute CFG : after this, there will be no:
	 * "Break, Switch, Default, or Continue" *)
	iterGlobals f doGlobal;
	(* collecting branches' information *)
	let myVst = new colbraVisitor in
	visitCilFile myVst f;
	let brainf = GText (intarr2str !bra_sids) in
	f.globals <- (f.globals) @ [brainf]
	
let computeCfgAllGlobal (f: file) = 
	(* compute CFG : after this, there will be no:
	 * "Break, Switch, Default, or Continue" *)
	iterGlobals f doGlobal
	
let collectingBranches (f: file) = 
	(* collecting branches' information *)
	let myVst = new colbraVisitor in
	visitCilFile myVst f;
	let brainf = GText (intarr2str !bra_sids) in
	f.globals <- (f.globals) @ [brainf]

let feature : featureDescr = 
{
	  fd_name = "cautcfgbra";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : Branches Collection : making CFG and then collecting branches' information";
    fd_extraopt = [];
    fd_doit = (function (f: file) -> default_process f);
    fd_post_check = true;
}
