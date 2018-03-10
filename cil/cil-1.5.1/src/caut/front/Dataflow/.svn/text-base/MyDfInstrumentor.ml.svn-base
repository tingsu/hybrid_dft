(** This module insert monitoring interfaces before each statement in every function.
    Get the help from http://blog.gmane.org/gmane.comp.compilers.cil.user 

The intent of queueInstr is to be called from functions visiting expressions, types, etc.,
from vstmt and vinstr.  You do not need it in vstmt indeed: you can insert
by yourself since you have a direct access to the current statement.

*)
open Cil
 
let function_id = ref 0

class myVisitor = object(self)
   inherit nopCilVisitor
 
   method vglob = function (* visit all functions *)
   | GFun (fd, _) ->
      function_id := fd.svar.vid;
      DoChildren
   | _ -> SkipChildren
 
   method vstmt st = 
     let df_fun = Cil.emptyFunction "df_stmt_monitor" in
     let df_monitor_call = Call(None,Lval(Var df_fun.svar,NoOffset),
          [Cil.integer !function_id; Cil.integer st.sid; Cil.integer (MyCilUtility.getStmtLoc st)], !currentLoc) in
     ChangeDoChildrenPost(st, fun s -> self#queueInstr [df_monitor_call]; s)
 
end;;

 
(** instrument data flow testing interface by visiting all funs in a file, ok !!!*)
let do_df_instrumentation (file: Cil.file) =
   visitCilFileSameGlobals (new myVisitor) file

	

(** instrument data flow testing interface by visiting stmt list in a fun, failed !!! *)
let do_df_instrumentation_2 (file: Cil.file) = 
	let stmtl = ref ([]: Cil.stmt list) in
	let df_fun = Cil.emptyFunction "df_stmt_monitor" in
	List.iter
		begin fun g ->
		 match g with 
		 | GFun (fd, _) ->
			List.iter
				begin fun st ->
					let df_monitor_call = Call(None,Lval(Var df_fun.svar,NoOffset),
          [Cil.integer fd.svar.vid; Cil.integer st.sid], !currentLoc) in
					let df_stmt = Cil.mkStmtOneInstr df_monitor_call in	
					stmtl := !stmtl @ [df_stmt] @ [st]
				end
			  fd.sallstmts;
			fd.sallstmts <- !stmtl;
			stmtl := []
		 | _ -> ()
		end
	 file.globals
	
   
let feature : featureDescr = 
  { fd_name = "df";              
    fd_enabled = ref false;
    fd_description = "data flow testing instrumentor";
    fd_extraopt = [ ];
    fd_doit = 
    (function (f: file) -> 
      do_df_instrumentation f);
    fd_post_check = true
  }

