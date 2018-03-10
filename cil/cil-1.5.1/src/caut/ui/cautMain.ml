open Cil
open Pretty
open ParseVar
open GenDriver

module E = Errormsg

exception Step_error of string

let _STEP = ref "" 
let _UNIT = ref ""
let _DB = ref ""

let default_process (f:file) = 
	match !_STEP with
	| "parseVar" -> 	
		ParseVar.main_job f !_DB
	| "genDriver" ->
		GenDriver.main_job f !_DB !_UNIT 
	| _ -> 
			raise (Step_error "step error. ")
	

let feature : featureDescr = 
	{
		fd_name = "cautMain";
		fd_enabled = ref false ;
		fd_description = "caut default process";
		fd_extraopt = [
				"-step",Arg.Set_string _STEP, "caut front step : 1.parseVar 2.genDriver" ;
				"-unit",Arg.Set_string _UNIT, "unit under test ";
				"-db",Arg.Set_string _DB , "database name using project name ";
			];
		fd_doit = 
			(
				function (f : file) ->
					default_process f
			);
		fd_post_check = true 
	}
