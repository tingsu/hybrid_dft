(** Collection for random selection. *)

open RandomAccessList

type 'a t = 'a RandomAccessList.t

let empty = empty
let is_empty = is_empty
let length = length
let put = cons

let get bag =
    match length bag with
        | 0 -> None
        | 1 -> Some (empty, head bag)
        | n ->
            let n = Random.int n in
            let x = lookup n bag in
            let bag = tail (update n (head bag) bag) in
            Some (bag, x)

let fold = fold_left

