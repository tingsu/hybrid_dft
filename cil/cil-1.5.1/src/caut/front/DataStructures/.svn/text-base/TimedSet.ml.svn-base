(* TODO: rename this module to RecordedSet *)

module Record = struct

    let time_elapsed =
        let time_start = Unix.gettimeofday () in
        function () -> Unix.gettimeofday () -. time_start

    type t = {
        first_seen: float; (* The first time an element is added to the set *)
        add_count: int;    (* How many times an element is added to the set *)
    }

    let create () = {
        first_seen = time_elapsed ();
        add_count = 1;
    }

    let touch r = {r with add_count = r.add_count + 1}
    
    let merge r1 r2 = {
        first_seen = Pervasives.min r1.first_seen r2.first_seen;
        add_count = r1.add_count + r2.add_count;
    }

    let print ff r = Format.fprintf ff "[%.2f,%d]" r.first_seen r.add_count
end

(** 
 * A set module that keeps track of times when elements are added.
 * The underlying data-structure is a map from the set elements to times as floats.
 *
 * For intersection/union, when an element appears in both sets, the earlier time is used.
 *)
module Make (Ord: Set.OrderedType) = struct
    
    module M = Map.Make (Ord)

    type t = Record.t M.t

    let empty = M.empty
    let is_empty = M.is_empty
    let cardinal = M.cardinal
    let iter f s = M.iter (fun elt _ -> f elt) s
    let elements s = M.fold (fun elt t lst -> (elt, t)::lst) s []
    let mem = M.mem

    let add elt s = 
        if M.mem elt s then 
            let r = M.find elt s in
            M.add elt (Record.touch r) s
        else M.add elt (Record.create ()) s

    let union s1 s2 = M.merge (
        fun _ r1opt r2opt -> match r1opt, r2opt with
        | Some r, None
        | None, Some r -> Some r
        | Some r1, Some r2 -> Some (Record.merge r1 r2)
        | None, None -> None (* Can't happen *)
    ) s1 s2

    let inter s1 s2 = M.merge (
        fun _ r1opt r2opt -> match r1opt, r2opt with
        | Some r1, Some r2 -> Some (Record.merge r1 r2)
        | _, _ -> None 
    ) s1 s2

    let diff s1 s2 =  M.merge (
        fun _ r1opt r2opt -> match r1opt, r2opt with
        | Some r, None -> Some r
        | _, _ -> None 
    ) s1 s2

end
