(** Efficient sequence based on Voigtlander (2002), a generalization of Hughes (1986).

    Summary of operations (all tail-recursive):
        - empty/is_empty/singleton/cons/append/from_list: O(1)
        - map/fold/filter/to_list: O(n)

    Hughes, R. J. 1986. A novel representation of lists and its application to the function "reverse". Information
    Processing Letters 22, 3 (Mar. 1986), 141-144.

    Voigtlander, J. 2002. Concatenate, reverse and map vanish for free. In Proceedings of the Seventh ACM SIGPLAN
    International Conference on Functional Programming (Pittsburgh, PA, USA, October 04 - 06, 2002). ICFP '02. ACM,
    New York, NY, 14-25.

    @see <http://dx.doi.org/10.1016/0020-0190(86)90059-1> Hughes, R. J. 1986.
    @see <http://doi.acm.org/10.1145/581478.581481> Voigtlander, J. 2002.
*)

type 'a seq = { length : int; seq : 'b 'c . ('a -> 'b -> 'b) -> 'b -> ('b -> 'c) -> 'c } (* record for rank-2 polymorphism *)

let empty = { length = 0; seq = fun c a k -> k a }

let is_empty xs = xs == empty

let length xs = xs.length

let singleton x = { length = 1; seq = fun c a k -> k (c x a) }

let cons x xs =
    if is_empty xs then singleton x
    else { length = xs.length + 1; seq = fun c a k -> xs.seq c a (fun a -> k (c x a)) }

let append xs ys =
    if is_empty xs then ys
    else if is_empty ys then xs
    else { length = xs.length + ys.length; seq = fun c a k -> ys.seq c a (fun a -> xs.seq c a k) }

let fold f acc xs = xs.seq (fun x acc -> f acc x) acc (fun xs -> xs)

let map f xs = xs.seq (fun x -> cons (f x)) empty (fun xs -> xs)

let map_fold f acc xs = xs.seq (fun x (acc, xs) -> let acc, x = f acc x in (acc, cons x xs)) (acc, empty) (fun xs -> xs)

let concat xss = xss.seq append empty (fun xs -> xs)

let concat_map f xs = xs.seq (fun x -> append (f x)) empty (fun xs -> xs)

let concat_map_fold f acc xs = xs.seq (fun x (acc, xs) -> let acc, x = f acc x in (acc, append x xs)) (acc, empty) (fun xs -> xs)

let filter f xs = xs.seq (fun x -> if f x then cons x else fun xs -> xs) empty (fun xs -> xs)

let map_to_list f xs = xs.seq (fun x a -> (f x)::a) [] (fun xs -> xs)

let to_list xs = xs.seq (fun x a -> x::a) [] (fun xs -> xs)

let rec from_list = function
    | [] ->
        empty
    | xs ->
        let length, xs = List.fold_left (fun (length, xs) x -> (length + 1, x::xs)) (0, []) xs in
        { length = length; seq = fun c a k -> k (List.fold_left (fun xs x -> c x xs) a xs) }

