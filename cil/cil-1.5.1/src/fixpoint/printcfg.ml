open Cil
open Pretty
open Cfg
module E = Errormsg

let printcfg (g:global) =
	match g with
	| GFun(fd,lc) -> 
		printCfgFilename "cfg.txt" fd
	| _ -> ()

let domain (f:file) =
	computeFileCFG f;
	iterGlobals f printcfg 

let feature : featureDescr = 
{   fd_name = "printcfg";              
    fd_enabled = ref false;
    fd_description = "print the control flow graph";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
      domain f);
    fd_post_check = true
}
