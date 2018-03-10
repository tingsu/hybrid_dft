(** Useful extensions to the {!List} module. *)

(** Compare two lists, given a comparison function for the elements. *)
let compare co xs ys =
    let rec compare xs ys = if xs == ys then 0 else match xs, ys with
        | x::xs, y::ys ->
            begin match co x y with
                | 0 -> compare xs ys
                | i -> i
            end
        | [], _ -> -1
        | _, [] -> 1
    in
    compare xs ys

(** Check for membership, given an equality function for the elements. *)
let mem eq x lst =
    let rec mem x = function
        | x' :: lst -> x' == x || eq x' x || mem x lst
        | [] -> false
    in
    mem x lst

(** Compare two lists for equality, given an equality function for the elements. *)
let equal eq xs ys =
    let rec equal xs ys = xs == ys || match xs, ys with
        | x::xs, y::ys -> eq x y && equal xs ys
        | _, _ -> false
    in
    equal xs ys

(** Hash a list, given an hash function for the elements. Only the first 10 elements will be considered. *)
let hash ha xs =
    let rec hash h n xs = if n <= 0 then h else match xs with
        | x::xs -> hash (Hashtbl.hash (ha x, h)) (n - 1) xs
        | [] -> h
    in
    hash 0 10 xs

(** Functor for ordered list of elements. *)
module MakeOrderedList (E : sig type t val compare : t -> t -> int end) = struct
    type t = E.t list
    let compare = compare E.compare
end

(** Functor for hashable list of elements. *)
module MakeHashedList (E : sig type t val equal : t -> t -> bool val hash : t -> int end) = struct
    type t = E.t list
    let equal = equal E.equal
    let hash = hash E.hash 
end

(** Functor for ordered and hashable list of elements. *)
module MakeOrderedHashedList (E : sig type t val compare : t -> t -> int val equal : t -> t -> bool val hash : t -> int end) = struct
    include MakeOrderedList (E)
    include (MakeHashedList (E) : module type of MakeHashedList (E) with type t := t)
end

(** Remove the first matching item from a list. *)
let remove_first f list =
    let rec remove list = function
        | item::rest when f item -> Some (item, List.rev_append list rest)
        | item::rest -> remove (item::list) rest
        | [] -> None
    in
    remove [] list

(** Fold over a list using a binary-subdivision scheme. *)
let foldm f xs =
    let rec foldm xs = function
        | x::y::ys -> foldm ((f x y)::xs) ys
        | [ x ] -> foldm_next (x::xs)
        | [] -> foldm_next xs
    and foldm_next = function
        | [ x ] -> x
        | [] -> invalid_arg "empty list"
        | xs -> foldm [] xs
    in
    foldm [] xs
