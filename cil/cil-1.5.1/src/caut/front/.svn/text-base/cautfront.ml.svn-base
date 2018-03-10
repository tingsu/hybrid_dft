(** cautfront.ml
	Author: Jiang Siyuan (stacy_j@acm.org)
	Date:4/25/2010
	Description: Managing the whole process of caut front module
*)
module E = Errormsg

open Cil
open String
open CautcreateCFG
open Caut_store_branch_info_test
(* cf is old caut cil front *)
open Cf

(*debug flag*)
let debug_flag = false 

let choice = ref (-1)
let iter = ref (0)
let test_unit_name = ref ""
let mylibs = ref ""
let printSid = false 

let cfgNode_list = ref ([] : cfgNode list)
let cfgRoot_list = ref ([] : int list)

let cfg_rt_mapping_table = ref ([]: cfg_rt_branch_map_t list)

(* a Visitor Class to print statement kind and its id(sid) *)
class printSidVst = object(self)
	inherit nopCilVisitor
	method vstmt (st:stmt) = 
		let kindName = (match st.skind with
			| Instr _ ->"instr : "
			| Return _ ->"ret : "
			| Goto _ ->"goto : "
			| Break _ ->"break : "
			| Continue _ ->"continue : "
			| If _->"if : "
			| Switch _ ->"switch : "
			| Loop _ ->"loop : "
			| Block _-> "block : "
			| TryFinally _ -> "TryFinally : "
			| TryExcept _ -> "TryExcept : ")
		in
		DoChildren
end

(*************************************** default process ************************************************)
let process (f : file) = 
	let printSidP = match printSid with 
		|true -> 
			let myPrintSidVst = new printSidVst in
			visitCilFile myPrintSidVst f 
		|_ -> () in

	let db_name = Buffer.create 20 in
	Buffer.add_string db_name f.fileName;
	Buffer.add_string db_name ".db";
	
	let rec one_process (process_number : int) : unit =
		if process_number <= !choice then
			match process_number with
			| 0 -> 
				if debug_flag then
					ignore(E.log "\nCaut prep\n");
				Cautprep.default_process f;
				printSidP;
				one_process (process_number + 1)
			| 1 ->
				
				if debug_flag then
					ignore(E.log "\nCaut cfgbra\n");
				Cautcfgbra.computeCfgAllGlobal f;
				(* print stmt id to file *)
				Cautaddsid.printSid2File f;
				
				if debug_flag then
					ignore(E.log "\nCaut create cfg\n");
				let (cfgNode_list, cfgRoot_list) = CautcreateCFG.createCFG f (Buffer.contents db_name) in
				if debug_flag then
					ignore(E.log "\nCaut record cfg\n");
				(* store cfg node/root list to "cfg.txt"*)
				CautcreateCFG.record2File !cfgNode_list !cfgRoot_list "cfg.txt";
				
				
				if debug_flag then
					ignore(E.log "\nCaut store branch infomation in CFG(test)\n");
				cfg_rt_mapping_table := Caut_store_branch_info_test.default_process f !cfgNode_list !iter (Buffer.contents db_name);
				Caut_store_branch_info_test.record2File "cfg.txt";
				
				(* write branch info to the end of source file *)
				Cautcfgbra.collectingBranches f;
				(*Cautaddsid.printSid2File f;*)
				printSidP;
				
				one_process (process_number + 1)
			| 2 -> 
				if debug_flag then
					ignore(E.log "\nCaut cf\n");
				Cf.default_tcg f;
				one_process (process_number + 1) 
				
			| 3 ->
				ignore(E.log "\nCaut exco\n");
				Cautexco.default_process f;
				printSidP;
				one_process (process_number + 1)
			| 4 ->
				ignore(E.log "\nCaut tra\n");
				Cauttra.default_process f;
				printSidP;
				one_process (process_number + 1)
			| 5 ->
				ignore(E.log "\nCaut decom\n");
				iterGlobals f Cautdecom.doGlobal;
				printSidP;
				one_process (process_number + 1)
			| 6 ->
				ignore(E.log "\nCaut simplify\n");
				iterGlobals f Cautsimplify.doGlobal;
				printSidP;
				one_process (process_number + 1)
			| 7 ->
				ignore(E.log "\nCaut types\n");
				Cauttyps.default_process f;
				printSidP;
				one_process (process_number + 1)
			| 8 ->
				ignore(E.log "\nCaut inin\n");
				ignore(E.log "   test unit: %s\n" !test_unit_name);
				ignore(E.log "   libs:%s\n" !mylibs);
				Cautinin.default_process f !test_unit_name !mylibs;
				printSidP;
				one_process (process_number + 1)
			| 9 ->
				ignore(E.log "\nCaut Instrumentation\n");
				Cautinstrument.defaut_process f;
				printSidP;
				one_process (process_number + 1)
			| _ -> ()
	in
	one_process 0

let feature : featureDescr = 
  {
    fd_name = "cautfront";
    fd_enabled = ref false;
    fd_description = "cautfront module, default : including all the stuff";

	fd_extraopt = [
			("-choice", Arg.Set_int choice, " set the number of modules that processes, started at 0-9");
			("-iter", Arg.Set_int iter, "set the iterations to split composition if stmt");
			("-testunit", Arg.Set_string test_unit_name, " set the test unit, default is \"main\"");
			("-mylib", Arg.Set_string mylibs, " set the standard libs used");
	];
    fd_doit =  (function (f: file) -> 
      process f);
    fd_post_check = true
  } 
