(** Stack-ordered set. *)

module type OrderedType = PrioritySearchQueue.OrderedType

module Make (Element : OrderedType) = struct
    module S = PrioritySearchQueue.Make (Element) (struct type t = int let compare = Pervasives.compare end)

    type t = unit S.t

    let empty : t = S.empty

    let is_empty (stack : t) = S.is_empty stack

    let singleton x : t = S.singleton x max_int ()

    let push x (stack : t) =
        let priority = try let _, priority, () = S.find_min stack in priority with S.Empty -> max_int in
        S.insert ~combine:(fun (x, ()) (x', ()) -> (max x x', ())) x (priority - 1) () stack

    let pop (stack : t) =
        let x, _, () = S.find_min stack in
        (x, S.delete_min stack)
end

