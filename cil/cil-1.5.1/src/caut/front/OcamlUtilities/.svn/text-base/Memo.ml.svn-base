(** Generic memoization and hashconsing wrappers. *)

(* TODO: weak sets (hashconsing), weak-key maps (property map), weak-key-and-value maps (caching);
 * is there a need for weak-value maps or weak-key-or-value maps? *)

(**/**) (* helpers *)
type record = {
    mutable hits : int;
    mutable misses : int;
    mutable hit_time : float;
    mutable miss_time : float;
}
let initial_size = 128
let statistics = Hashtbl.create 8
(**/**)


(** Functorized memoization functions. Polymorphic versions using Ocaml's polymorphic {!Pervasives.(=)} and
 *  {!Hashtbl.hash} are not provided as memoization is sensitive to the correctness of the equality and hash
 *  functions: the invariant that [T.equal x y] implies [T.hash x = T.hash y] must hold, otherwise, performance
 *  will be adversely impacted memory-wise and timing-wise due to duplication in the underlying hashtable. *)
module Make (T : Hashtbl.HashedType) = struct
    module H = Hashtbl.Make (T)

    (** Make a memoization wrapper that can be used to memoized several functions of the same type *)
    let make label =
        if Hashtbl.mem statistics label then Format.ksprintf invalid_arg "Label %s already exists!" label;
        let record = { hits = 0; misses = 0; hit_time = 0.; miss_time = 0. } in
        Hashtbl.add statistics label record;
        let memotable = H.create initial_size in
        let nested = ref 0. in
        fun f x ->
            let elapsed =
                let start = Sys.time () in
                let nested' = !nested in
                nested := 0.;
                fun () ->
                    let elapsed = Sys.time () -. start in
                    (* avoid multi-counting recursion by subtracting nested calls *)
                    let used = elapsed -. !nested in
                    nested := nested' +. elapsed;
                    used
            in
            try
                let y = H.find memotable x in
                record.hits <- record.hits + 1;
                record.hit_time <- record.hit_time +. elapsed ();
                y
            with Not_found ->
                let y = f x in
                (* don't need Hashtbl.replace, which is slower, as x guaranteed to be unique *)
                H.add memotable x y;
                record.misses <- record.misses + 1;
                record.miss_time <- record.miss_time +. elapsed ();
                y


    (** Make a hashconsing wrapper *)
    let make_hashcons label =
        (* TODO: use Weak? *)
        make label (fun x -> x)


    (** Memoize a function *)
    let memo label f =
        make label f


    (** Memoize a recursive function; the function will be given the memoized version of itself to recurse with *)
    let memo_rec label f =
        let wrap = make label in
        let rec g x = wrap (f g) x in
        g
end


(** Print the statistics of all memoized functions *)
let statistics_printer ff =
    let table, width = Hashtbl.fold begin fun label { hits; misses; hit_time; miss_time } (table, width) ->
        if misses > 0 then
            let hits' = float_of_int hits in
            let misses' = float_of_int misses in
            let total' = hits' +. misses' in
            let miss_rate' = misses' /. total' in
            let hit_cost' = hit_time /. hits' in
            let miss_cost' = miss_time /. misses' in
            let speedup' = miss_cost' /. hit_cost' in
            let table = (label, hits, misses, miss_rate', miss_cost', speedup')::table in
            let width = max (String.length label) width in
            (table, width)
        else
            (table, width)
    end statistics ([], 16) in
    let table = List.fast_sort (fun (_, _, _, x, _, _) (_, _, _, y, _, _) -> Pervasives.compare x y) table in
    let width = width + 2 in

    Format.pp_open_tbox ff ();

    Format.fprintf ff "%-*s" width "Memo statistics:";
    Format.pp_set_tab ff ();
    Format.fprintf ff "%7s %7s %9s %9s %12s@\n" "hits" "misses" "miss rate" "time/miss" "speedup";

    List.iter begin fun (label, hits, misses, rate, miss_cost, speedup) ->
        Format.pp_print_string ff label;
        Format.pp_print_tab ff ();
        Format.fprintf ff "%7d %7d %9.5f %9.5f %12.2f@\n" hits misses rate miss_cost speedup;
    end table;

    Format.pp_close_tbox ff ()

