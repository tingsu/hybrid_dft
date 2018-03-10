(** Wrapper for CIL's Ptranal module additional resolves to field offsets, filtered by type. *)


(** Table to track initialized files. *)
let initialized = Hashtbl.create 0


(** Initialize a file for Cil's pointer analysis.
    @param file the Cil file to initialize
*)
let init_file file =
    (* set up to use Cil's pointer analysis *)
    if not (Hashtbl.mem initialized file) then begin
        if Hashtbl.length initialized > 0 then
            (* I don't know if Cil's pointer analysis works reliably for multiple files, e.g.:
             * - it appears to conflate some facts between files, such as in Cil.Ptranal.hose_global;
             * - it has several global hashtables keyed on Cil.varinfo, Cil.exp, Cil.lval, etc.
             *)
            Format.eprintf "Warning: Cil's pointer analysis may not work reliably for multiple files@\n";
        Ptranal.analyze_file file;
        Ptranal.compute_results false;
        Hashtbl.add initialized file true
    end


(** Fold helper for Cil.TComp. *)
let fold_struct f acc compinfo =
    List.fold_left f acc compinfo.Cil.cfields


(** Fold helper for Cil.TArray. *)
let fold_array f acc len_opt =
    begin match len_opt with
        | Some len ->
            begin match Cil.isInteger len with
                | Some n ->
                    let rec fold_array acc i index =
                        if Int64.compare i n == 0 then
                            Some acc
                        else
                            let acc = f acc index in
                            fold_array acc (Int64.succ i) (Cil.increm index 1)
                    in
                    fold_array acc Int64.zero Cil.zero
                | None ->
                    (* non-constant length *)
                    None
            end
        | None ->
            (* no length *)
            None
    end


(** Helper that tests if a pointer can point to a target.
        @param pointer_type is the pointer type
        @param target_type is the target type
        @return whether the pointer can point to the target
*)
let accept_points_to pointer_type = Profiler.global#call "accept_points_to" begin fun () ->
    (* allow pointers to point only to certain types *)
    let canonicalize_type t = Cil.typeSigWithAttrs (fun _ -> []) t in
    let pointer_typesig = canonicalize_type pointer_type in
    fun target_type ->
        let rec accept_points_to x y = match x, y with
            | Cil.TSPtr (x, _), y when x = y -> (* pointer matches target *)
                true
            | Cil.TSPtr (Cil.TSBase (Cil.TVoid _), _), _ -> (* void * points to anything *)
                true
            | Cil.TSPtr (x, _), Cil.TSArray (y, _, _) when x = y -> (* pointers may point to arrays *)
                true
            | Cil.TSPtr (x, _), Cil.TSPtr (y, _) -> (* peel off a level and recurse *)
                accept_points_to x y
            (*
            | Cil.TInt _, _ -> (* allow int types to be pointers too *)
                true
            *)
            (* TODO: what else? enums with ints? structs to sub-structs? *)
            | _, _ ->
                false
        in
        accept_points_to pointer_typesig (canonicalize_type target_type)
end


(** Helper to enumerate all compatible pointer target field/array offsets in a target type.
        @param pointer_type is the pointer type
        @param target_type is the target type
        @return all compatible field/array offsets
*)
let to_offsets =
    let module Memo = Memo.Make (struct
        type t = CilData.CilCanonicalType.t * CilData.CilCanonicalType.t
        let hash (p, t) = Hashtbl.hash (CilData.CilCanonicalType.hash p, CilData.CilCanonicalType.hash t)
        let equal (xp, xt) (yp, yt) = CilData.CilCanonicalType.equal xp yp && CilData.CilCanonicalType.equal xt yt
    end) in
    let to_offsets = Memo.memo "CilPtranal.to_offsets" begin fun (pointer_type, target_type) ->
        Profiler.global#call "to_offsets (uncached)" begin fun () ->
            let accept_type = accept_points_to pointer_type in

            let rec to_offsets offsets typ base =
                let offset_type = Cil.unrollType (Cil.typeOffset typ base) in
                (* collect offsets that matches the target type *)
                let offsets = if accept_type offset_type then
                    base::offsets
                else
                    offsets
                in
                (* recurse over all field, offsets *)
                match offset_type with
                    | Cil.TComp (compinfo, _) ->
                        fold_struct begin fun offsets field ->
                            to_offsets offsets typ (Cil.addOffset (Cil.Field (field, Cil.NoOffset)) base)
                        end offsets compinfo
                    | Cil.TArray (_, len_opt, _) ->
                        let offsets_opt = fold_array begin fun offsets index ->
                            to_offsets offsets typ (Cil.addOffset (Cil.Index (index, Cil.NoOffset)) base)
                        end offsets len_opt in
                        begin match offsets_opt with
                            | Some offsets -> offsets
                            | None -> offsets
                        end
                    | _ ->
                        offsets
            in
            to_offsets [] (Cil.unrollType target_type) Cil.NoOffset
        end
    end in
    fun pointer_type target_type ->
        to_offsets (pointer_type, target_type)


(** Wrapper that converts a points-to function that resolves expressions to variables, to a function that resolves
    expressions to fields in variables as well, conservatively and filtered by type.
        @param points_to_varinfo is the points-to function to wrap
        @param exp is the expression to resolve
        @return [(target_varinfos, target_mallocs)] where [target_varinfos] contains the points to target varinfos
                and offsets; and [target_mallocs] contains a list of dynamic allocation sites and offsets
*)
let wrap_points_to_varinfo points_to_varinfo exp = Profiler.global#call "wrap_points_to_varinfo" begin fun () ->
    let varinfos, mallocs = points_to_varinfo exp in

    let pointer_type = Cil.unrollType (Cil.typeOf exp) in
    let to_offsets = to_offsets pointer_type in

    (* combine the target varinfos with offsets *)
    let target_varinfos = List.fold_left begin fun target_varinfos varinfo ->
        List.fold_left begin fun target_varinfos offset ->
            (varinfo, offset)::target_varinfos
        end target_varinfos (to_offsets varinfo.Cil.vtype)
    end [] varinfos in

    (* combine the target mallocs with offsets *)
    let target_mallocs = List.fold_left begin fun target_mallocs ((_, _, typ as malloc), lhosts) ->
        List.fold_left begin fun target_mallocs offset ->
            (malloc, lhosts, offset)::target_mallocs
        end target_mallocs (to_offsets typ)
    end [] mallocs in

    (target_varinfos, target_mallocs)
end


(** Helper that generates a map from untyped dynamic allocation sites ([Cil.varinfo * string] tuple) to typed dynamic
    allocation sites ([(CilData.Malloc.t * CilData.CilLhost.t list) list]), using a points-to analysis to find the types and
    pointer expressions that refer to those sites.
        @param file is the file for which to generate the mapping
        @param points_to_varinfo is the points-to function which is used to infer the types
        @return [malloc_map] which is a function that maps a list of untyped dynamic allocation sites to typed dynamic
                allocation sites
*)
let mallocs_of_sites =
    let module MallocSet = Set.Make (CilData.Malloc) in
    let module MallocMap = Map.Make (CilData.Malloc) in
    let module LhostSet = Set.Make (CilData.CilLhost) in
    let module LhostMallocSet = Set.Make (struct
        type t = CilData.CilLhost.t * CilData.Malloc.t
        let compare (xl, xm as x) (yl, ym as y) = if x == y then 0 else
            match CilData.CilLhost.compare xl yl with
                | 0 -> CilData.Malloc.compare xm ym
                | i -> i
    end) in
    let module SiteMap = Map.Make (struct
        type t = CilData.CilVar.t * string
        let compare (xv, xs as x) (yv, ys as y) = if x == y then 0 else
            match CilData.CilVar.compare xv yv with
                | 0 -> String.compare xs ys
                | i -> i
    end) in

    let module Memo = Memo.Make (struct
        type t = CilData.CilFile.t * (Cil.exp -> Cil.varinfo list * (Cil.varinfo * string) list)
        let hash (f, p) = Hashtbl.hash (CilData.CilFile.hash f, p)
        let equal (xf, xp) (yf, yp) = CilData.CilFile.equal xf yf && xp == yp
    end) in
    let malloc_of_sites = Memo.memo "CilPtranal.malloc_of_sites" begin fun (file, points_to_varinfo) ->
        let site_to_mallocs =
            Profiler.global#call "mallocs_of_sites (uncached)" begin fun () ->
                let points_to_varinfo exp = Profiler.global#call "points_to_varinfo" (fun () -> points_to_varinfo exp) in

                (* build a map from malloc sites to malloc types (Cil.fundec * string => MallocSet.t MallocMap.t) *)
                let site_to_mallocs = SiteMap.empty in

                (* for each varinfo or malloc pointer given as an lhost, find all malloc targets and infer their types
                 * from the pointer lhost type *)
                let process_lhost site_to_mallocs updated lhost malloc_source_opt = Profiler.global#call "process_lhost" begin fun () ->
                    let lhost_type = Cil.typeOf (Cil.Lval (lhost, Cil.NoOffset)) in

                    (* process each offset in the lhost that is a pointer *)
                    let process_offset_points_to site_to_mallocs updated offset target_type =
                        let pointer = Cil.Lval (lhost, offset) in
                        let _, malloc_sites = points_to_varinfo pointer in

                        List.fold_left begin fun (site_to_mallocs, updated) (malloc_varinfo, malloc_name as site) ->
                            let malloc = (malloc_varinfo, malloc_name, target_type) in
                            let malloc_lhost = Cil.Mem pointer in (* an lhost that aliases this malloc *)
                            let malloc_meta = try SiteMap.find site site_to_mallocs with Not_found -> MallocMap.empty in
                            let sources, malloc_lhosts = try MallocMap.find malloc malloc_meta with Not_found -> (MallocSet.empty, LhostSet.empty) in
                            let updated, sources, malloc_lhosts = begin match malloc_source_opt with
                                | Some source when not (MallocSet.mem source sources) -> (* from a malloc'ed pointer that had not been seen previously *)
                                    (LhostMallocSet.add (malloc_lhost, malloc) updated, MallocSet.add source sources, LhostSet.add malloc_lhost malloc_lhosts)
                                | Some _ ->
                                    (updated, sources, malloc_lhosts)
                                | None -> (* from a varinfo *)
                                    (LhostMallocSet.add (malloc_lhost, malloc) updated, sources, LhostSet.add malloc_lhost malloc_lhosts)
                            end in
                            let malloc_meta = MallocMap.add malloc (sources, malloc_lhosts) malloc_meta in
                            let site_to_mallocs = SiteMap.add site malloc_meta site_to_mallocs in
                            (site_to_mallocs, updated)
                        end (site_to_mallocs, updated) malloc_sites
                    in

                    (* find all offsets in the lhost that is a pointer *)
                    let rec fold_pointer_offsets site_to_mallocs updated offset = match Cil.typeOffset lhost_type offset with
                        | Cil.TPtr (target_type, _) ->
                            process_offset_points_to site_to_mallocs updated offset target_type
                        | Cil.TComp (compinfo, _) ->
                            fold_struct begin fun (site_to_mallocs, updated) field ->
                                fold_pointer_offsets site_to_mallocs updated (Cil.addOffset (Cil.Field (field, Cil.NoOffset)) offset)
                            end (site_to_mallocs, updated) compinfo
                        | Cil.TArray (target_type, len_opt, _) ->
                            fold_pointer_offsets site_to_mallocs updated (Cil.addOffset (Cil.Index (Cil.zero, Cil.NoOffset)) offset)
                        | _ ->
                            (site_to_mallocs, updated)
                    in
                    fold_pointer_offsets site_to_mallocs updated Cil.NoOffset
                end in

                (* first infer malloc types from varinfo pointers *)
                let site_to_mallocs, updated = List.fold_left begin fun (site_to_mallocs, updated) varinfo ->
                    process_lhost site_to_mallocs updated (Cil.Var varinfo) None
                end (site_to_mallocs, LhostMallocSet.empty) (FindCil.all_varinfos file) in

                (* then infer malloc types from malloc pointers, iteratively to a fixpoint *)
                let rec do_fixpoint site_to_mallocs updated =
                    if LhostMallocSet.is_empty updated then
                        site_to_mallocs
                    else
                        let site_to_mallocs, updated = LhostMallocSet.fold begin fun (lhost, source) (site_to_mallocs, updated) ->
                            process_lhost site_to_mallocs updated lhost (Some source)
                        end updated (site_to_mallocs, LhostMallocSet.empty) in
                        do_fixpoint site_to_mallocs updated
                in
                let site_to_mallocs = do_fixpoint site_to_mallocs updated in

                (* finally, transform the map to a suitable form *)
                let site_to_mallocs = SiteMap.map begin fun malloc_meta ->
                    MallocMap.fold begin fun malloc (_, malloc_lhosts) mallocs ->
                        (malloc, LhostSet.elements malloc_lhosts)::mallocs
                    end malloc_meta []
                end site_to_mallocs in

                site_to_mallocs
            end
        in
        fun sites ->
            List.concat (List.map (fun site -> SiteMap.find site site_to_mallocs) sites)
    end in
    fun file points_to_varinfo ->
        malloc_of_sites (file, points_to_varinfo)


(** Wrapper for Cil's {!Ptranal.resolve_exp} that resolves to fields in variables as well, conservatively and
    filtered by type.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(target_varinfos, target_mallocs)] where [target_varinfos] contains the points to target varinfos
                and offsets; and [target_mallocs] contains a list of dynamic allocation sites and offsets
*)
let points_to file exp = Profiler.global#call "CilPtranal.points_to" begin fun () ->
   init_file file;
    let resolve_exp exp = Profiler.global#call "Ptranal.resolve_exp" (fun () -> Ptranal.resolve_exp exp) in
    let mallocs_of_sites = mallocs_of_sites file resolve_exp in
    let points_to exp =
        let varinfos, sites = resolve_exp exp in
        (varinfos, mallocs_of_sites sites)
    in
    wrap_points_to_varinfo points_to exp
end


(** Wrapper for Cil's {!Ptranal.resolve_fundec} that filters by type.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [fundec_list] which is a list of target functions
*)
let points_to_fundec file exp = Profiler.global#call "CilPtranal.points_to_fundec" begin fun () ->
    init_file file;
    let pointer_type = Cil.typeOf exp in
    List.filter (fun f -> accept_points_to pointer_type f.Cil.svar.Cil.vtype) (Ptranal.resolve_funptr exp)
end


(**/**)
(* A dummy lhost to refer to malloc'ed locations. *)
let malloc_host = Cil.makeGlobalVar "__cilptranal_points_to_host" Cil.voidPtrType

(* Helper for naive_points_to/unsound_points_to to generate a dummy lhost to refer to malloc'ed locations. *)
let make_malloc_lhost typ =
    Cil.Mem (Cil.mkCast (Cil.Lval (Cil.var malloc_host)) (Cil.TPtr (typ, [])))

(* Helper for naive_points_to/unsound_points_to to test if an lhost is a dummy lhost that refers to malloc'ed locations. *)
let is_malloc_lhost_exp = function
    | Cil.Lval (Cil.Mem (Cil.Lval (Cil.Var lhost, Cil.NoOffset)), _)
    | Cil.Lval (Cil.Mem (Cil.CastE (_, Cil.Lval (Cil.Var lhost, Cil.NoOffset))), _) ->
        lhost == malloc_host
    | _ ->
        false

(* Helper for naive_points_to/unsound_points_to to find the varinfo for malloc. *)
let find_malloc =
    let dummy_malloc = Cil.makeGlobalVar "malloc" (Cil.TFun (Cil.voidPtrType, Some [ ("", !Cil.typeOfSizeOf, []) ], false, [])) in
    fun file ->
        try
            FindCil.global_varinfo_by_name file "malloc"
        with Not_found ->
            dummy_malloc
(**/**)


(** Naive point-to that maps anything to everything (including [malloc]), filtered by type.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(targets_list, target_mallocs)] where [target_list] contains the points to target lvals and offset;
                and [target_mallocs] contains a list of dynamic allocation sites and offsets
*)
let naive_points_to =
    let module Memo = Memo.Make (struct
        type t = CilData.CilFile.t * CilData.CilExp.t
        let hash (f, e) = Hashtbl.hash (CilData.CilFile.hash f, CilData.CilExp.hash e)
        let equal (xf, xe) (yf, ye) = CilData.CilFile.equal xf yf && CilData.CilExp.equal xe ye
    end) in
    let naive_points_to = Memo.memo "CilPtranal.naive_points_to" begin fun (file, exp) ->
        Profiler.global#call "CilPtranal.naive_points_to" begin fun () ->
            let varinfos = FindCil.all_varinfos file in
            let malloc_varinfo = find_malloc file in
            let mallocs = List.map begin fun typ ->
                let malloc = (malloc_varinfo, "malloc", typ) in
                let malloc_lhost = make_malloc_lhost typ in
                (malloc, [ malloc_lhost ])
            end (FindCil.all_types file) in

            wrap_points_to_varinfo (fun _ -> (varinfos, mallocs)) exp
        end
    end in
    fun file exp ->
        naive_points_to (file, exp)


(** Unsound point-to that maps each pointer to one or two distinct [malloc]s: one of the pointer target type, and if
    the pointer points to base (numeric) type, another of an array of size 4 of the base type; and if the pointer is
    a function pointer, all functions returned by {!Ptranal.resolve_exp} filtered by type.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(targets_list, target_mallocs)] where [target_list] is empty and [target_mallocs] contains a single
                dynamic allocation site
*)
let unsound_points_to =
    let array_size = Some (Cil.integer 4) in
    let counter = Counter.make () in
    let resolve_exp exp = Profiler.global#call "Ptranal.resolve_exp" (fun () -> Ptranal.resolve_exp exp) in
    fun file exp -> Profiler.global#call "CilPtranal.unsound_points_to" begin fun () ->
        match Cil.unrollType (Cil.typeOf exp) with
            | Cil.TPtr (typ, _) as pointer_type ->
                let malloc_varinfo = find_malloc file in
                let name = "malloc" ^ string_of_int (Counter.next counter) in

                let varinfo_targets, malloc_targets =
                    if is_malloc_lhost_exp exp then
                        (* since two lhosts are returned every time below, only provide targets for the non-malloc lhost *)
                        ([], [])
                    else if Cil.isFunctionType typ then
                        (* resolve function pointers *)
                        let varinfo_targets, _ = resolve_exp exp in
                        let varinfo_targets = List.filter (fun v -> accept_points_to pointer_type v.Cil.vtype) varinfo_targets in
                        (varinfo_targets, [])
                    else
                        let deref_lhost = Cil.Mem exp in

                        let malloc = (malloc_varinfo, name, typ) in
                        let malloc_lhost = make_malloc_lhost typ in
                        let malloc_targets = [ (malloc, [ deref_lhost; malloc_lhost ]) ] in

                        let malloc_targets =
                            if Cil.isArithmeticType typ then
                                let array_typ = Cil.TArray (typ, array_size, []) in
                                let deref_array_lhost = Cil.Mem (Cil.mkCast exp (Cil.TPtr (array_typ, []))) in
                                let malloc_array = (malloc_varinfo, name, array_typ) in
                                let malloc_array_lhost = make_malloc_lhost array_typ in
                                (malloc_array, [ deref_array_lhost; malloc_array_lhost ])::malloc_targets
                            else
                                malloc_targets
                        in
                        ([], malloc_targets)
                in
                wrap_points_to_varinfo (fun _ -> (varinfo_targets, malloc_targets)) exp
            | _ ->
                ([], [])
    end


(** Unsound point-to that maps each void pointer variable to zero or more distinct [malloc]s of types partially
    determined from a pointer analysis, and every other pointer (variable or malloc'ed) to one or two distinct
    [malloc]s: one of the pointer target type, and if the pointer points to base (numeric) type, another of an array
    of size 4 of the base type; and if the pointer is a function pointer, all functions returned by {!Ptranal.resolve_exp}
    filtered by type.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(targets_list, target_mallocs)] where [target_list] is empty and [target_mallocs] contains a single
                dynamic allocation site
*)
let unsound_typed_void_points_to =
    let counter = Counter.make () in
    fun file exp -> Profiler.global#call "CilPtranal.unsound_typed_void_points_to" begin fun () ->
        init_file file;
        match Cil.unrollType (Cil.typeOf exp) with
            | Cil.TPtr (typ, _) when Cil.isVoidType typ ->
                let malloc_varinfo = find_malloc file in
                let make_malloc typ = ((malloc_varinfo, "malloc" ^ string_of_int (Counter.next counter), typ), [ make_malloc_lhost typ ]) in

                let module TypeSet = Set.Make (CilData.CilCanonicalType) in
                let target_vars, _ = Profiler.global#call "Ptranal.resolve_exp" (fun () -> Ptranal.resolve_exp exp) in
                let target_types = List.fold_left begin fun target_types target_var ->
                    TypeSet.add target_var.Cil.vtype target_types
                end TypeSet.empty target_vars in
                let targets = TypeSet.fold begin fun typ targets ->
                    (make_malloc typ)::targets
                end target_types [] in

                wrap_points_to_varinfo (fun _ -> ([], targets)) exp
            | _ ->
                unsound_points_to file exp
    end


(** Similar to wrap_points_to_varinfo, but not field sensitive (i.e., no offset)
        @param points_to_varinfo is the points-to function to wrap
        @param exp is the expression to resolve
        @return [(target_varinfos, target_mallocs)] where [target_varinfos] contains the points to target varinfos
                and no offset; and [target_mallocs] contains a list of dynamic allocation sites and no offset
*)
let unsound_wrap_points_to_varinfo points_to_varinfo exp = Profiler.global#call "unsound_wrap_points_to_varinfo" begin fun () ->
    let varinfos, mallocs = points_to_varinfo exp in

    (* combine the target varinfos with no offset *)
    let target_varinfos = List.fold_left begin fun target_varinfos varinfo ->
        (varinfo, Cil.NoOffset)::target_varinfos
    end [] varinfos in

    (* combine the target mallocs with no offset *)
    let target_mallocs = List.fold_left begin fun target_mallocs ((_, _, typ as malloc), lhosts) ->
        (malloc, lhosts, Cil.NoOffset)::target_mallocs
    end [] mallocs in

    (target_varinfos, target_mallocs)
end

let unsound_array_size = ref 8
let unsound_array_for_all_types = ref false
let unsound_array_replace_single_element = ref false

(** Unsound point-to that maps each pointer to one or two distinct [malloc]s: one of the pointer target type, and
    another of an array of size [!unsound_array_size] of the base type; and if the pointer is a function pointer, all functions 
    returned by {!Ptranal.resolve_exp} filtered by type.
    Unlike unsound_points_to, this points_to calls unsound_wrap_points_to_varinfo which does not conservatively
    assumes a pointer points to every field of its struct/array target.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(targets_list, target_mallocs)] where [target_list] is empty and [target_mallocs] contains a single
                dynamic allocation site
*)
let really_unsound_points_to =
    let array_size = Some (Cil.integer (!unsound_array_size)) in
    let counter = Counter.make () in
    let resolve_exp exp = Profiler.global#call "Ptranal.resolve_exp" (fun () -> Ptranal.resolve_exp exp) in
    fun file exp -> Profiler.global#call "CilPtranal.really_unsound_points_to" begin fun () ->
        match Cil.unrollType (Cil.typeOf exp) with
            | Cil.TPtr (typ, _) as pointer_type ->
                let malloc_varinfo = find_malloc file in
                let name = "malloc" ^ string_of_int (Counter.next counter) in

                let varinfo_targets, malloc_targets =
                    if is_malloc_lhost_exp exp then
                        (* since two lhosts are returned every time below, only provide targets for the non-malloc lhost *)
                        ([], [])
                    else if Cil.isFunctionType typ then
                        (* resolve function pointers *)
                        let varinfo_targets, _ = resolve_exp exp in
                        let varinfo_targets = List.filter (fun v -> accept_points_to pointer_type v.Cil.vtype) varinfo_targets in
                        (varinfo_targets, [])
                    else
                        let single_malloc_target () =
                            let deref_lhost = Cil.Mem exp in
                            let malloc = (malloc_varinfo, name, typ) in
                            let malloc_lhost = make_malloc_lhost typ in
                            [ (malloc, [ deref_lhost; malloc_lhost ]) ] 
                        in
                        let malloc_targets =
                            if (!unsound_array_for_all_types) || Cil.isArithmeticType typ then
                                let array_typ = Cil.TArray (typ, array_size, []) in
                                let deref_array_lhost = Cil.Mem (Cil.mkCast exp (Cil.TPtr (array_typ, []))) in
                                let malloc_array = (malloc_varinfo, name, array_typ) in
                                let malloc_array_lhost = make_malloc_lhost array_typ in
                                let malloc_targets = if (!unsound_array_replace_single_element) then [] else single_malloc_target () in
                                (malloc_array, [ deref_array_lhost; malloc_array_lhost ])::malloc_targets
                            else single_malloc_target ()
                        in
                        ([], malloc_targets)
                in
                unsound_wrap_points_to_varinfo (fun _ -> (varinfo_targets, malloc_targets)) exp
            | _ ->
                ([], [])
    end


(** Unsound point-to that maps each void pointer variable to zero or more distinct [malloc]s of types partially
    determined from a pointer analysis, and every other pointer (variable or malloc'ed) to one or two distinct
    [malloc]s: one of the pointer target type, and another of an array of size [!unsound_array_size] of the base type; and if
    the pointer is a function pointer, all functions returned by {!Ptranal.resolve_exp} filtered by type.
    Unlike unsound_points_to, this points_to calls unsound_wrap_points_to_varinfo which does not conservatively
    assumes a pointer points to every field of its struct/array target.
        @param file is the file being analyzed
        @param exp is the expression to resolve
        @return [(targets_list, target_mallocs)] where [target_list] is empty and [target_mallocs] contains a single
                dynamic allocation site
*)
let really_unsound_typed_void_points_to =
    let counter = Counter.make () in
    fun file exp -> Profiler.global#call "CilPtranal.really_unsound_typed_void_points_to" begin fun () ->
        init_file file;
        match Cil.unrollType (Cil.typeOf exp) with
            | Cil.TPtr (typ, _) when Cil.isVoidType typ ->
                let malloc_varinfo = find_malloc file in
                let make_malloc typ = ((malloc_varinfo, "malloc" ^ string_of_int (Counter.next counter), typ), [ make_malloc_lhost typ ]) in

                let module TypeSet = Set.Make (CilData.CilCanonicalType) in
                let target_vars, _ = Profiler.global#call "Ptranal.resolve_exp" (fun () -> Ptranal.resolve_exp exp) in
                let target_types = List.fold_left begin fun target_types target_var ->
                    TypeSet.add target_var.Cil.vtype target_types
                end TypeSet.empty target_vars in
                let targets = TypeSet.fold begin fun typ targets ->
                    (make_malloc typ)::targets
                end target_types [] in

                unsound_wrap_points_to_varinfo (fun _ -> ([], targets)) exp
            | _ ->
                really_unsound_points_to file exp
    end

let options = [
    "--unsound-array-size",
        Arg.Set_int unsound_array_size,
        Printf.sprintf "<size> Set the unsound_array_size (default: %d)" (!unsound_array_size);
    "--unsound-array-for-all-types",
        Arg.Set unsound_array_for_all_types,
        Printf.sprintf " Set up unsound array for all types (default: %B)" (!unsound_array_for_all_types);
    "--unsound-array-replace-single-element",
        Arg.Set unsound_array_replace_single_element,
        Printf.sprintf " When unsound array is created, the pointer will point to either null of the array (default: %B)" (!unsound_array_replace_single_element);
]

