
type t = True | False | Unknown

let not = function
	| True -> False
	| False -> True
	| Unknown -> Unknown

let of_bool b = if b then True else False
