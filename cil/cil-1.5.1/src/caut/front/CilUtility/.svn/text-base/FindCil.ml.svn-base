(** Functions for searching {!Cil.file}. *)


(**/**)
module VarinfoSet = Set.Make (CilData.CilVar)
module TypeSet = Set.Make (CilData.CilType)
(**/**)


(** Get the search tables for a file, initializing them if necessary.
        @param file the {!Cil.file} to get the search tables for
        @return [searchtables] the object containing the search tables for the file
*)
let searchtables =
    let module Memo = Memo.Make (CilData.CilFile) in
    Memo.memo "FindCil.searchtables"
        begin fun file ->
            let varinfo_to_varinit = Hashtbl.create 100 in
            let name_to_fundec = Hashtbl.create 100 in
            let name_to_global_varinfo = Hashtbl.create 100 in
            let line_to_stmts = Hashtbl.create 100 in
            let line_to_instrs = Hashtbl.create 100 in
            let all_varinfos = ref VarinfoSet.empty in
            let all_types = ref TypeSet.empty in

            Cil.visitCilFile begin object
                inherit Cil.nopCilVisitor
                val mutable current_fundec = Cil.dummyFunDec
                val mutable current_stmt = Cil.dummyStmt
                method vtype typ =
                    all_types := TypeSet.add typ !all_types;
                    Cil.DoChildren
                method vvdec varinfo =
                    if not (Hashtbl.mem varinfo_to_varinit varinfo) then Hashtbl.replace varinfo_to_varinit varinfo { Cil.init = None };
                    if varinfo.Cil.vglob then Hashtbl.replace name_to_global_varinfo varinfo.Cil.vname varinfo;
                    all_varinfos := VarinfoSet.add varinfo !all_varinfos;
                    Cil.DoChildren
                method vfunc fundec =
                    current_fundec <- fundec;
                    Hashtbl.replace name_to_fundec fundec.Cil.svar.Cil.vname fundec;
                    List.iter (fun varinfo -> all_varinfos := VarinfoSet.add varinfo !all_varinfos) fundec.Cil.sformals;
                    List.iter (fun varinfo -> all_varinfos := VarinfoSet.add varinfo !all_varinfos) fundec.Cil.slocals;
                    Cil.DoChildren
                method vstmt stmt =
                    current_stmt <- stmt;
                    let loc = Cil.get_stmtLoc stmt.Cil.skind in
                    let line = (loc.Cil.file, loc.Cil.line) in
                    let stmts = try Hashtbl.find line_to_stmts line with Not_found -> [] in
                    Hashtbl.replace line_to_stmts line ((current_fundec, stmt)::stmts);
                    Cil.DoChildren
                method vinst instr =
                    let loc = Cil.get_instrLoc instr in
                    let line = (loc.Cil.file, loc.Cil.line) in
                    let instrs = try Hashtbl.find line_to_instrs line with Not_found -> [] in
                    Hashtbl.replace line_to_instrs line ((current_fundec, current_stmt, instr)::instrs);
                    Cil.DoChildren
                method vinit varinfo _ init =
                    Hashtbl.replace varinfo_to_varinit varinfo { Cil.init = Some init };
                    Cil.SkipChildren
            end end file;

            let all_varinfos = VarinfoSet.elements !all_varinfos in
            let all_types = TypeSet.elements !all_types in

            object
                method varinfo_to_varinit = Hashtbl.find varinfo_to_varinit
                method name_to_fundec = Hashtbl.find name_to_fundec
                method name_to_global_varinfo = Hashtbl.find name_to_global_varinfo
                method line_to_stmts = Hashtbl.find line_to_stmts
                method line_to_instrs = Hashtbl.find line_to_instrs
                method all_varinfos = all_varinfos
                method all_types = all_types
            end
        end

(** Find a {!Cil.fundec} by {!Cil.varinfo} from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.fundec} in
        @param varinfo the {!Cil.varinfo} of the {!Cil.fundec} to find
        @return the {!Cil.fundec}
        @raise Not_found if a {!Cil.fundec} for [varinfo] does not exist in [file]
*)
let fundec_by_varinfo file varinfo =
    (searchtables file)#name_to_fundec varinfo.Cil.vname


(** Find the {!Cil.initinfo} for a global {!Cil.varinfo} from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.initinfo} in
        @param varinfo the {!Cil.varinfo} of the {!Cil.initinfo} to find
        @return the {!Cil.initinfo}
        @raise Not_found if the global [varinfo] does not exist in [file]
*)
let global_varinit_by_varinfo file varinfo =
    (searchtables file)#varinfo_to_varinit varinfo


(** Find a {!Cil.fundec} by name from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.fundec} in
        @param name the name of the {!Cil.fundec} to find
        @return the {!Cil.fundec}
        @raise Not_found if a {!Cil.fundec} named [name] does not exist in [file]
*)
let fundec_by_name file name =
    (searchtables file)#name_to_fundec name


(** Find a global {!Cil.varinfo} by name from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.varinfo} in
        @param name the name of the {!Cil.varinfo} to find
        @return the {!Cil.varinfo}
        @raise Not_found if a global {!Cil.varinfo} named [name] does not exist in [file]
*)
let global_varinfo_by_name file name =
    (searchtables file)#name_to_global_varinfo name


(** Find a list of statements by file name and line number from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.stmt} in
        @param line the line to find the statements, as a [(filename, line)] pair
        @return the list of statements as {!Cil.fundec}-by-{!Cil.stmt} pairs
        @raise Not_found if no statements can be found at [line] in [file]
*)
let stmts_by_line file line =
    (searchtables file)#line_to_stmts line


(** Find a list of instructions by file name and line number from a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.instr} in
        @param line the line to find the instructions, as a [(filename, line)] pair
        @return the list of instructions as {!Cil.fundec}-by-{!Cil.stmt}-by-{!Cil.instr} triples
        @raise Not_found if no instructions can be found at [line] in [file]
*)
let instrs_by_line file line =
    (searchtables file)#line_to_instrs line


(** Return a list of all {!Cil.varinfo} in a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.varinfo} in
        @return the list of {!Cil.varinfo}
*)
let all_varinfos file =
    (searchtables file)#all_varinfos


(** Return a list of all {!Cil.type} in a {!Cil.file}.
        @param file the {!Cil.file} to find the {!Cil.type} in
        @return the list of {!Cil.type}
*)
let all_types file =
    (searchtables file)#all_types

