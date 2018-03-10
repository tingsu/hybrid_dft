(** Find the callers and callees of functions. *)

(**/**) (* various helper modules *)
open Cil

module Callgraph = Graph.Persistent.Digraph.ConcreteBidirectional (CilData.CilFundec)
module FundecSet = Set.Make (CilData.CilFundec)
module E = Errormsg

module DotCallgraph = Graph.Graphviz.Dot (struct
    include Callgraph
    let quote v = Str.global_replace (Str.regexp "\"") "\\\"" v.Cil.svar.Cil.vname

    (* Graphviz attributes *)
    let vertex_name v = "\""^(quote v)^"\""
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes v = [ `Label (quote v) ]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
end)

let output_dot_file = ref "cilcallgraph.dot"

(**/**)


(** Resolve a {!Cil.exp} to a list of {!Cil.fundec}, using {!CilPtranal.points_to_fundec} to resolve function pointers. *)
let resolve_exp_to_fundecs file = function
    | Cil.Lval (Cil.Var fn, Cil.NoOffset) -> (try [ FindCil.fundec_by_varinfo file fn ] with Not_found -> [])
    (*| Cil.Lval (Cil.Mem ptrexp, Cil.NoOffset) -> CilPtranal.points_to_fundec file ptrexp*)
	| Cil.Lval (Cil.Mem ptrexp, Cil.NoOffset) -> failwith "call by function pointers?"
    | _ -> failwith "Does Cil generate other variations of function call expressions?"


(** Compute the callgraph of a file. *)
let compute_callgraph =
    let module Memo = Memo.Make (CilData.CilFile) in
    Memo.memo "CilCallGraph.compute_callgraph"
        begin fun file ->
            let callgraph = ref Callgraph.empty in
            Cil.visitCilFileSameGlobals begin object
                inherit Cil.nopCilVisitor
                method vfunc fundec =
                    callgraph := Callgraph.add_vertex !callgraph fundec;
                    ignore begin Cil.visitCilFunction begin object
                        inherit Cil.nopCilVisitor
                        method vinst = function
                            | Cil.Call (_, fexp, _, _) ->
                                let targets = resolve_exp_to_fundecs file fexp in
                                List.iter (fun target -> callgraph := Callgraph.add_edge !callgraph fundec target) targets;
                                Cil.SkipChildren
                            | _ ->
                                Cil.SkipChildren
                    end end fundec end;
                    Cil.SkipChildren
            end end file;
            !callgraph
        end


(** Find the {!Cil.fundecs} of callers of a function. *)
let find_callers file fundec = Callgraph.pred (compute_callgraph file) fundec


(** Find the {!Cil.fundecs} of callees of a function. *)
let find_callees file fundec = Callgraph.succ (compute_callgraph file) fundec


(**/**) (* helper function for transitive queries *)
let find_transitive_worker dir file fundec =
    let callgraph = compute_callgraph file in
    let rec find_transitive_worker result = function
        | fundec::worklist when not (FundecSet.mem fundec result) ->
            find_transitive_worker (FundecSet.add fundec result) (List.rev_append (dir callgraph fundec) worklist)
        | _::worklist ->
            find_transitive_worker result worklist
        | [] ->
            FundecSet.elements result
    in
    find_transitive_worker FundecSet.empty (dir callgraph fundec)
(**/**)


(** Find the {!Cil.fundecs} of transitive callers of a function. *)
let find_transitive_callers =
    find_transitive_worker Callgraph.pred


(** Find the {!Cil.fundecs} of transitive callees of a function. *)
let find_transitive_callees =
    find_transitive_worker Callgraph.succ


(** Find the distance between two {!Cil.fundecs}. *)
let get_distance = (* calculate call chain distance between two functions *)
    let module Memo = Memo.Make (Module.CombineHashedTypes3 (CilData.CilFile) (CilData.CilFundec) (CilData.CilFundec)) in
    let get_distance = Memo.memo "CilCallgraph.get_distance"
        begin fun (file, f1, f2) ->
            let rec bfs = function (* a bfs calculation style, so the shortest distance *)
                | [] -> max_int
                | (f, d) :: tail ->
                    if f == f2 then d else
                    let callees = find_callees file f in
                    let tail = List.fold_left (fun tail callee ->
                        if List.exists (fun (k,_) -> k == callee) tail then tail
                        else tail @ [(callee, d+1)]
                    ) tail callees in
                    bfs tail
            in
            bfs [(f1, 0)]
        end
    in
    fun file f1 f2 -> Profiler.global#call "CilCallgraph.get_distance" begin fun () ->
        get_distance (file, f1, f2)
    end

(*******************************************)

(** output functon call relations for a specified function *)
let output_call_relation (file: Cil.file) (func: Cil.fundec) = 
	E.log "%s's callers: \n" func.svar.vname;
	let callerl = find_callers file func in
	List.iter
		begin fun fn ->
			E.log "%s " fn.svar.vname
		end
	  callerl;
	E.log "\n";
	E.log "%s's callees: \n" func.svar.vname;
	let calleel = find_callees file func in
	List.iter
		begin fun fn ->
			E.log "%s " fn.svar.vname
		end
	  calleel;
	E.log "\n"

(** output transitive calls (caller/callee) for a specified function *)
let output_transitive_call (file: Cil.file) (func: Cil.fundec) = 
	
	E.log "%s's transitive callers: \n" func.svar.vname;
	let transitive_callers = find_transitive_callers file func in
	List.iter
		begin fun fn ->
			E.log "%s " fn.svar.vname
		end
	  transitive_callers;
	E.log "\n";
	
	E.log "%s's transitive callee: \n" func.svar.vname;
	let transitive_callees = find_transitive_callees file func in
	List.iter
		begin fun fn ->
			E.log "%s " fn.svar.vname
		end
	  transitive_callees;
	E.log "\n"

let output_callgraph (file: Cil.file) = 
	List.iter
		begin fun g ->
			match g with
			| GFun (func, loc) ->
				output_call_relation file func; (* function call relation *)
				output_transitive_call file func; (* transitive function call relation *)
			| _ -> ()
		end
	  file.globals;
	let entry_fn = FindCil.fundec_by_name file "testme" in
	let bar_fn = FindCil.fundec_by_name file "CAUT_DEF_USE" in
	let dist_fn = get_distance file entry_fn bar_fn in
	E.log "\ndist between %s and %s = %d \n" entry_fn.svar.vname bar_fn.svar.vname dist_fn

(*************************************************)

(** Save the callgraph of a file to a GraphViz dot file. *)
let save_to_dot file dot_file =
    let dot_out = open_out dot_file in
    let callgraph = compute_callgraph file in (* compute call graph *)
	(* output_callgraph file; *) (* debug : output function call relations *)
    try
        DotCallgraph.output_graph dot_out callgraph;
        close_out dot_out
    with e ->
        close_out dot_out;
        raise e

(** Cil feature description. *)
let feature : Cil.featureDescr = {
    Cil.fd_enabled = ref false;
    Cil.fd_name = "cilcallgraph";
    Cil.fd_description = "saving the callgraph of the program to a GraphViz dot file";
    Cil.fd_extraopt = [
        ("--cilcallgraph-dot-file",
        Arg.Set_string output_dot_file,
        "<filename> The file to which to write the callgraph (default is " ^ !output_dot_file ^ ")")
    ];
    Cil.fd_doit = (fun file -> save_to_dot file !output_dot_file);
    Cil.fd_post_check = false;
}
