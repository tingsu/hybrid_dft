(** Sparse immutable array *)

module type ElementType = sig
    type t
    val default : t Lazy.t (* lazy to enable use of recursive modules *)
    val equal : t -> t -> bool
    val hash : t -> int
end

module type S = sig
    exception Out_of_bounds
    type elt
    type t
    val length : t -> int
    val get : t -> int -> elt
    val set : t -> int -> elt -> t
    val sub : t -> int -> int -> t
    val make : int -> elt ->t
    val of_list : elt list -> t
    val equal : t -> t -> bool
    val hash : t -> int
    val foldi : ('acc -> int -> elt -> 'acc) -> 'acc -> t -> 'acc
    val fold_left : ('acc -> elt -> 'acc) -> 'acc -> t -> 'acc
    val map : (elt -> elt) -> t -> t
    val exists : (elt -> bool) -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
end

module Make (Elt : ElementType) : S with type elt = Elt.t = struct
    module IndexMap = Map.Make (struct
        type t = int
        let compare (a : int) (b : int) = Pervasives.compare a b
    end)


    exception Out_of_bounds


    type elt = Elt.t


    type t = {
        length : int;
        map : Elt.t IndexMap.t; (* map must never contain Elt.default *)
        hash : int; (* hash is kept up-to-date with a commutative, invertible hash (addition/negation) of length and elements in map *)
    }


    let length array = array.length


    let get array i =
        if i < 0 || i >= array.length then raise Out_of_bounds;
        try
            IndexMap.find i array.map
        with Not_found ->
            Lazy.force Elt.default


    let set array i x =
        if i < 0 || i >= array.length then raise Out_of_bounds;
        let map, hash =
            let hash = array.hash - try Hashtbl.hash (i, Elt.hash (IndexMap.find i array.map)) with Not_found -> 0 in
            if Elt.equal x (Lazy.force Elt.default) then
                (IndexMap.remove i array.map, hash)
            else
                (IndexMap.add i x array.map, hash + Hashtbl.hash (i, Elt.hash x))
        in
        { array with map; hash }


    let sub array offset length =
        if offset < 0 then invalid_arg "negative offset";
        if length <= 0 then invalid_arg "negative or zero length";
        if length > array.length || offset > array.length - length then raise Out_of_bounds;
        let rec sub i map hash =
            (* copy into a new map, so this is optimized for small slices; the alternative would be to store an offset
             * as part of the type to optimize sub for big slices, but that in turn is ill-optimized for the equal function *)
            if i < length then
                let map, hash =
                    try
                        let x = IndexMap.find (i + offset) array.map in
                        let map = IndexMap.add i x map in
                        let hash = hash + Hashtbl.hash (i, Elt.hash x) in
                        (map, hash)
                    with Not_found ->
                        (map, hash)
                in
                sub (i + 1) map hash
            else
                (map, hash)
        in
        let map, hash = sub 0 IndexMap.empty 0 in
        { length; map; hash = length + hash }


    let make length x =
        let map, hash =
            if Elt.equal x (Lazy.force Elt.default) then
                (IndexMap.empty, 0)
            else
                let h = Elt.hash x in
                let rec make i map hash =
                    if i < length then
                        make (i + 1) (IndexMap.add i x map) (hash + Hashtbl.hash (i, h))
                    else
                        (map, hash)
                in
                make 0 IndexMap.empty 0
        in
        { length; map; hash }


    let of_list xs =
        let default = Lazy.force Elt.default in
        let length, map, hash = List.fold_left begin fun (i, map, hash) x ->
            if Elt.equal x default then
                (i + 1, map, hash)
            else
                (i + 1, IndexMap.add i x map, hash + Hashtbl.hash (i, Elt.hash x))
        end (0, IndexMap.empty, 0) xs in
        { length; map; hash = length + hash }


    let equal xs ys =
        xs == ys || xs.length = ys.length && xs.hash = ys.hash && (xs.map == ys.map || IndexMap.equal Elt.equal xs.map ys.map)


    let hash xs = xs.hash


    (* TODO: remove everything below, as uses of the below cannot take advantage of the sparsity of ImmutableArrays *)

    let foldi f acc array =
        let rec foldi acc index =
            if index < array.length then
                foldi (f acc index (get array index)) (index + 1)
            else
                acc
        in
        foldi acc 0

    let fold_left ff a bs =
        let len = length bs in
        let rec impl ff a bs i =
            if i>=len then a
            else let aa = ff a (get bs i) in
                impl ff aa bs (i+1)
        in
            impl ff a bs 0

    let map ff bs =
        let fff lst bb =
            (ff bb)::lst
        in
        let l1 = fold_left fff [] bs in
        let l2 = List.rev_append l1 [] in
            of_list l2

    let exists p arr =
        let len = length arr in
        let rec exists_aux i =
            if i >= len
            then false
            else p (get arr i) || exists_aux (succ i)
        in
        exists_aux 0

    let for_all p arr =
        let len = length arr in
        let rec for_all_aux i =
            if i >= len
            then true
            else p (get arr i) && for_all_aux (succ i)
        in
        for_all_aux 0
end
