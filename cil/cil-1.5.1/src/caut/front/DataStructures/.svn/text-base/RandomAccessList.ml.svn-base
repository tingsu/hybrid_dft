(** Purely functional random-access list based on Okazaki (1995).

    Summary of operations:
        - cons/head/tail: O(1)
        - lookup/update: O(log n); for the i-th element: O(min{i, log n}) worst time, O(log i) expected case
        - length: O(log n)

    Okasaki, C. 1995. Purely functional random-access lists. In Proceedings of the Seventh international Conference on
    Functional Programming Languages and Computer Architecture (La Jolla, California, United States, June 26 - 28,
    1995). FPCA '95. ACM, New York, NY, 86-95.

    @see <http://doi.acm.org/10.1145/224164.224187> http://doi.acm.org/10.1145/224164.224187
*)

exception Subscript
exception Empty

module Tree = struct
    type 'a t = Leaf of 'a | Node of 'a * 'a t * 'a t

    let rec lookup index size tree = match tree with
        | Leaf x when index = 0 -> x
        | Leaf _ -> raise Subscript
        | Node (x, _, _) when index = 0 -> x
        | Node (x, t1, t2) ->
            let size' = size / 2 in
            if index <= size'
                then lookup (index - 1) size' t1
                else lookup (index - 1 - size') size' t2

    let rec update index value size tree = match tree with
        | Leaf _ when index = 0 -> Leaf value
        | Leaf _ -> raise Subscript
        | Node (x, t1, t2) when index = 0 -> Node (value, t1, t2)
        | Node (x, t1, t2) ->
            let size' = size / 2 in
            if index <= size'
                then Node (x, update (index - 1) value size' t1, t2)
                else Node (x, t1, update (index - 1 - size') value size' t2)

    let fold_left f acc tree =
        let rec fold_left acc tree k = match tree with
            | Leaf x -> k (f acc x)
            | Node (x, t1, t2) -> let acc = f acc x in fold_left acc t1 (fun acc -> fold_left acc t2 k)
        in
        fold_left acc tree (fun x -> x)

    let map f tree =
        let rec map tree k = match tree with
            | Leaf x -> k (Leaf (f x))
            | Node (x, t1, t2) -> let x = f x in map t1 (fun t1 -> map t2 (fun t2 -> Node (x, t1, t2)))
        in
        map tree (fun x -> x)
end

type 'a t = (int * 'a Tree.t) list

(* views *)
let list_view (list : 'a t) = match list with
    | (size, Tree.Node (x, t1, t2))::rest -> `Cons (x, let size' = size / 2 in (size', t1)::(size', t2)::rest)
    | (size, Tree.Leaf x)::rest -> `Cons (x, rest)
    | [] -> `Nil

(* constructors *)
let empty : 'a t = []

let singleton value : 'a t = [ (1, Tree.Leaf value) ]

(* size queries *)
let is_empty (list : 'a t) = list = []

let length (list : 'a t) = List.fold_left (fun n (size, _) -> n + size) 0 list

(* list operations *)
let cons x (list : 'a t) = match list with
    | (size1, t1)::(size2, t2)::rest when size1 = size2 -> (1 + size1 + size2, Tree.Node (x, t1, t2))::rest
    | xs -> (1, Tree.Leaf x)::xs

let head (list : 'a t) = match list_view list with
    | `Cons (x, _) -> x
    | `Nil -> raise Empty

let tail (list : 'a t) = match list_view list with
    | `Cons (_, rest) -> rest
    | `Nil -> raise Empty

(* random-access operations *)
let rec lookup index (list : 'a t) = match list with
    | (size, tree)::rest ->
        if index < size
        then Tree.lookup index size tree
        else lookup (index - size) rest
    | [] -> raise Subscript

let rec update index value (list : 'a t) = match list with
    | (size, tree)::rest ->
        if index < size
        then (size, Tree.update index value size tree)::rest
        else (size, tree)::update (index - size) value rest
    | [] -> raise Subscript

(* iterators *)
let fold_left f acc (list : 'a t) =
    List.fold_left (fun acc (_, tree) -> Tree.fold_left f acc tree) acc list

let map f (list : 'a t) =
    List.map (fun (_, tree) -> Tree.map f tree) list

