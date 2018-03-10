(** Utilities for module operations *)

module Int = struct
    type t = int
    let hash i = i
    let equal = Pervasives.(=)
    let compare = Pervasives.compare
end

module CombineHashedTypes (T1 : Hashtbl.HashedType) (T2 : Hashtbl.HashedType) : Hashtbl.HashedType with type t = T1.t * T2.t = struct
    type t = T1.t * T2.t
    let hash = Tuple.combine_hashes T1.hash T2.hash
    let equal = Tuple.combine_equals T1.equal T2.equal
end

module CombineHashedTypes3 (T1 : Hashtbl.HashedType) (T2 : Hashtbl.HashedType) (T3 : Hashtbl.HashedType) : Hashtbl.HashedType with type t = T1.t * T2.t * T3.t = struct
    type t = T1.t * T2.t * T3.t
    let hash = Tuple.combine_hashes3 T1.hash T2.hash T3.hash
    let equal = Tuple.combine_equals3 T1.equal T2.equal T3.equal
end

module CombineOrderedTypes (T1 : Map.OrderedType) (T2 : Map.OrderedType) : Map.OrderedType with type t = T1.t * T2.t = struct
    type t = T1.t * T2.t
    let compare = Tuple.combine_compares T1.compare T2.compare
end

module CombineOrderedTypes3 (T1 : Map.OrderedType) (T2 : Map.OrderedType) (T3 : Map.OrderedType) : Map.OrderedType with type t = T1.t * T2.t * T3.t = struct
    type t = T1.t * T2.t * T3.t
    let compare = Tuple.combine_compares3 T1.compare T2.compare T3.compare
end
