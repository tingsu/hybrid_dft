(** cautprep.ml
	Author: Jiang Siyuan
	Date:4/25/2010
	Description: pre-process, including: const-fold
*)

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let default_process (f: file) = 
	(* doing process of const folding which means no longer 'sizeof' or 'alignof' *)
	let myVst = constFoldVisitor true in
	visitCilFile myVst f

let feature : featureDescr = 
{
	  fd_name = "cautprep";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : Proprocess : Eliminate all 'sizeof' or 'alignof'";
    fd_extraopt = [];
    fd_doit = (function (f: file) -> default_process f);
    fd_post_check = true;
}
