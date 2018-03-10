(** Deferred computation of a value in a  state. *)

type ('state, 'value) t =
    | Immediate of 'value
        (** The result of a deferred computation. *)
    | Deferred of ('state -> ('state * 'value))
        (** The deferred computation as a function that computes a value in a given state, updating the state
            if necessary. *)

(** Return the value of a deferred computation, computing the value in the given state if necessary. Note that the
    deferred computation is responsible for updating the deferred computation to an {!Immediate} value in the given
    state if necessary.
        @param state is the state in which to force the deferred computation
        @param deferred is the deferred computation to force
        @return the updated state and computed value
*)
let force state deferred = match deferred with
    | Immediate x -> (state, x)
    | Deferred f -> f state


(** Test if a deferred computation has been forced. *)
let is_forced deferred = match deferred with
    | Immediate _ -> true
    | Deferred _ -> false
