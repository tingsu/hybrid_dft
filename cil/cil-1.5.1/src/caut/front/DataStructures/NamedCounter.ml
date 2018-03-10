 (** Module for a simple counter. *)

(* Maps a string to an integer *)
module StringMap = Map.Make(String)

(* Global map *)
let counters : int StringMap.t ref = ref StringMap.empty
let default_value = 0

let get name = if StringMap.mem name (!counters) then StringMap.find name (!counters) else default_value
let set name value = counters := StringMap.add name value (!counters)

let incr ?(step=1) name = let next = (get name) + step in set name next

(* TODO: use StringMap.bindings for Ocaml 3.12 *)
let report () = StringMap.fold (fun k v l -> (k,v)::l) (!counters) []

let reset () = counters := StringMap.empty
