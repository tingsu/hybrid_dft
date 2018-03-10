(** Purely functional priority search queues based on Hinze (2001). Mostly direct translation of the PSQueue package
    Haskell implementation, and extends bindings from keys to priorities and values.

    Summary of operations:
        - insert/delete/adjust/lookup: O(log n)
        - find_min: O(1)
        - delete_min: O(log n)
        - at_most/at_most_range: O(r log(n/r)), where r is the length of the output

    Hinze, R. 2001. A simple implementation technique for priority search queues. In Proceedings of the Sixth ACM
    SIGPLAN international Conference on Functional Programming (Florence, Italy, September 03 - 05, 2001). ICFP '01.
    ACM, New York, NY, 110-121.

    @see <http://doi.acm.org/10.1145/507635.507650> http://doi.acm.org/10.1145/507635.507650
    @see <http://hackage.haskell.org/package/PSQueue> The PSQueue package Haskell implementation.
*)

module type OrderedType = sig
    type t
    val compare: t -> t -> int
end

module Make (Key : OrderedType) (Priority : OrderedType) = struct
    type key = Key.t
    type priority = Priority.t

    exception Empty
    exception Key

    module LoserTree = struct
        type +'value t =
            | Start
            | LLoser of int * key * priority * 'value * 'value t * key * 'value t
            | RLoser of int * key * priority * 'value *  'value t * key * 'value t

        let size = function
            | LLoser (s, _, _, _, _, _, _) | RLoser (s, _, _, _, _, _, _) -> s
            | Start -> 0
        let left = function
            | LLoser (_, _, _, _, t, _, _) | RLoser (_, _, _, _, t, _, _) -> t
            | Start -> raise Empty
        let right = function
            | LLoser (_, _, _, _, _, _, t) | RLoser (_, _, _, _, _, _, t) -> t
            | Start -> raise Empty
        let lloser k p v t m t' =  LLoser (1 + size t + size t', k, p, v, t, m, t')
        let rloser k p v t m t' =  RLoser (1 + size t + size t', k, p, v, t, m, t')
        let omega = 4

        let lsingle_left k p v t m r = match r with
            | LLoser (_, k', p', v', t', m', t'') when Priority.compare p p' <= 0 -> lloser k p v (rloser k' p' v' t m t') m' t''
            | LLoser (_, k', p', v', t', m', t'') -> lloser k' p' v' (lloser k p v t m t') m' t''
            | RLoser (_, k', p', v', t', m', t'') -> rloser k' p' v' (lloser k p v t m t') m' t''
            | Start -> raise Empty

        let rsingle_left k p v t m r = match r with
            | LLoser (_, k', p', v', t', m', t'') -> rloser k p v (rloser k' p' v' t m t') m' t''
            | RLoser (_, k', p', v', t', m', t'') -> rloser k' p' v' (rloser k p v t m t') m' t''
            | Start -> raise Empty

        let lsingle_right k p v l m' t'' = match l with
            | LLoser (_, k', p', v', t, m, t') -> lloser k' p' v' t m (lloser k p v t' m' t'')
            | RLoser (_, k', p', v', t, m, t') -> lloser k p v t m (lloser k' p' v' t' m' t'')
            | Start -> raise Empty

        let rsingle_right k p v l m' t'' = match l with
            | LLoser (_, k', p', v', t, m, t') -> lloser k' p' v' t m (rloser k p v t' m' t'')
            | RLoser (_, k', p', v', t, m, t') when Priority.compare p p' <= 0 -> rloser k p v t m (lloser k' p' v' t' m' t'')
            | RLoser (_, k', p', v', t, m, t') -> rloser k' p' v' t m (rloser k p v t' m' t'')
            | Start -> raise Empty

        let ldouble_left k p v t m r = match r with
            | LLoser (_, k', p', v', t', m', t'') -> lsingle_left k p v t m (lsingle_right k' p' v' t' m' t'')
            | RLoser (_, k', p', v', t', m', t'') -> lsingle_left k p v t m (rsingle_right k' p' v' t' m' t'')
            | Start -> raise Empty

        let ldouble_right k p v l m' t'' = match l with
            | LLoser (_, k', p', v', t, m, t') -> lsingle_right k p v (lsingle_left k' p' v' t m t') m' t''
            | RLoser (_, k', p', v', t, m, t') -> lsingle_right k p v (rsingle_left k' p' v' t m t') m' t''
            | Start -> raise Empty

        let rdouble_left k p v t m r = match r with
            | LLoser (_, k', p', v', t', m', t'') -> rsingle_left k p v t m (lsingle_right k' p' v' t' m' t'')
            | RLoser (_, k', p', v', t', m', t'') -> rsingle_left k p v t m (rsingle_right k' p' v' t' m' t'')
            | Start -> raise Empty

        let rdouble_right k p v l m' t'' = match l with
            | LLoser (_, k', p', v', t, m, t') -> rsingle_right k p v (lsingle_left k' p' v' t m t') m' t''
            | RLoser (_, k', p', v', t, m, t') -> rsingle_right k p v (rsingle_left k' p' v' t m t') m' t''
            | Start -> raise Empty

        let lbalance_left k p v l m r =
            if size (left r) < size (right r) then
                lsingle_left k p v l m r
            else
                ldouble_left k p v l m r

        let lbalance_right k p v l m r =
            if size (left l) > size (right l) then
                lsingle_right k p v l m r
            else
                ldouble_right k p v l m r

        let rbalance_left k p v l m r =
            if size (left r) < size (right r) then
                rsingle_left k p v l m r
            else
                rdouble_left k p v l m r

        let rbalance_right k p v l m r =
            if size (left l) > size (right l) then
                rsingle_right k p v l m r
            else
                rdouble_right k p v l m r

        let lbalance k p v l m r =
            if size l + size r < 2 then
                lloser k p v l m r
            else if size r > omega * size l then
                lbalance_left k p v l m r
            else if size l > omega * size r then
                lbalance_right k p v l m r
            else
                lloser k p v l m r

        let rbalance k p v l m r =
            if size l + size r < 2 then
                rloser k p v l m r
            else if size r > omega * size l then
                rbalance_left k p v l m r
            else if size l > omega * size r then
                rbalance_right k p v l m r
            else
                rloser k p v l m r
    end

    type +'value t = Void | Winner of key * priority * 'value * 'value LoserTree.t * key

    (* helper functions *)
    let play q q' = match q, q' with
        | Void, q' -> q'
        | q, Void -> q
        | Winner (k, p, v, t, m), Winner (k', p', v', t', m') ->
            let i = Priority.compare p p' in
            if i < 0 then
                Winner (k, p, v, LoserTree.rbalance k' p' v' t m t', m')
            else if i = 0 then
                let j = Key.compare k k' in
                if j <= 0 then
                    Winner (k, p, v, LoserTree.rbalance k' p' v' t m t', m')
                else
                    Winner (k', p', v', LoserTree.lbalance k p v t m t', m')
            else
                Winner (k', p', v', LoserTree.lbalance k p v t m t', m')

    let rec second_best t m' = match t with
        | LoserTree.Start -> Void
        | LoserTree.LLoser (_, k, p, v, l, m, r) -> play (Winner (k, p, v, l, m)) (second_best r m')
        | LoserTree.RLoser (_, k, p, v, l, m, r) -> play (second_best l m) (Winner (k, p, v, r, m'))

    let tour_view = function
        | Void -> `Null
        | Winner (k, p, v, LoserTree.Start, _) -> `Single (k, p, v)
        | Winner (k, p, v, LoserTree.RLoser (_, k', p', v', l, m, r), m') -> `Play (Winner (k, p, v, l, m), Winner (k', p', v', r, m'))
        | Winner (k, p, v, LoserTree.LLoser (_, k', p', v', l, m, r), m') -> `Play (Winner (k', p', v', l, m), Winner (k, p, v, r, m'))

    let max_key = function
        | Void -> raise Empty
        | Winner (_, _, _, _, m) -> m

    (* constructors *)
    let empty = Void

    let singleton k p v = Winner (k, p, v, LoserTree.Start, k)

    (* size queries *)
    let is_empty q = q = Void

    let size = function
        | Void -> 0
        | Winner (_, _, _, l, _) -> 1 + LoserTree.size l

    (* insertion/deletion/modification *)
    let insert ?(combine=(fun x _ -> x)) k p v q =
        let rec insert q = match tour_view q with
            | `Null -> singleton k p v
            | `Single (k', p', v') ->
                let i = Key.compare k k' in
                if i < 0 then
                    play (singleton k p v) (singleton k' p' v')
                else if i = 0 then
                    let p, v = combine (p, v) (p', v') in
                    singleton k p v
                else
                    play (singleton k' p' v') (singleton k p v)
            | `Play (l, r) when Key.compare k (max_key l) <= 0 -> play (insert l) r
            | `Play (l, r) -> play l (insert r)
        in
        insert q

    let delete k q =
        let rec delete q = match tour_view q with
            | `Null -> empty
            | `Single (k', p, _) when Key.compare k k' = 0 -> empty
            | `Single (k', p, v) -> singleton k' p v
            | `Play (l, r) when Key.compare k (max_key l) <= 0 -> play (delete l) r
            | `Play (l, r) -> play l (delete r)
        in
        delete q

    let adjust f k q =
        let rec adjust q = match tour_view q with
            | `Null -> empty
            | `Single (k', p, v) when Key.compare k k' = 0 -> singleton k' (f p) v
            | `Single (k', p, v) -> singleton k' p v
            | `Play (l, r) when Key.compare k (max_key l) <= 0 -> play (adjust l) r
            | `Play (l, r) -> play l (adjust r)
        in
        adjust q

    (* dictionary operations *)
    let lookup k q =
        let rec lookup q = match tour_view q with
            | `Null -> raise Key
            | `Single (k', p, v) when Key.compare k k' = 0 -> (p, v)
            | `Single _ -> raise Key
            | `Play (l, _) when Key.compare k (max_key l) <= 0 -> lookup l
            | `Play (_, r) -> lookup r
        in
        lookup q

    (* priority queue operations *)
    let find_min = function
        | Void -> raise Empty
        | Winner (k, p, v, _, _) -> (k, p, v)

    let delete_min = function
        | Void -> empty
        | Winner (_, _, _, t, m) -> second_best t m

    let at_most pt = function
        | Void -> []
        | Winner (k, p, v, t, _) ->
            let rec prune k p v t =
                if Priority.compare p pt <= 0 then traverse k p v t else EfficientSequence.empty
            and traverse k p v = function
                | LoserTree.Start -> EfficientSequence.singleton (k, p, v)
                | LoserTree.LLoser (_, k', p', v', tl, _, tr) -> EfficientSequence.append (prune k' p' v' tl) (traverse k p v tr)
                | LoserTree.RLoser (_, k', p', v', tl, _, tr) -> EfficientSequence.append (traverse k p v tl) (prune k' p' v' tr)
            in
            EfficientSequence.to_list (prune k p v t)

    let at_most_range pt (kl, kr) = function
        | Void -> []
        | Winner (k, p, v, t, _) ->
            let rec prune k p v t =
                if Priority.compare p pt <= 0 then traverse k p v t else EfficientSequence.empty
            and traverse k p v = function
                | LoserTree.Start ->
                    if Key.compare kl k <= 0 && Key.compare k kr <= 0 then EfficientSequence.singleton (k, p, v) else EfficientSequence.empty
                | LoserTree.LLoser (_, k', p', v', tl, m, tr) ->
                    let left = if Key.compare kl m <= 0 then prune k' p' v' tl else EfficientSequence.empty in
                    let right = if Key.compare m kr <= 0 then traverse k p v tr else EfficientSequence.empty in
                    EfficientSequence.append left right
                | LoserTree.RLoser (_, k', p', v', tl, m, tr) ->
                    let left = if Key.compare kl m <= 0 then traverse k p v tl else EfficientSequence.empty in
                    let right = if Key.compare m kr <= 0 then prune k' p' v' tr else EfficientSequence.empty in
                    EfficientSequence.append left right
            in
            EfficientSequence.to_list (prune k p v t)
end
