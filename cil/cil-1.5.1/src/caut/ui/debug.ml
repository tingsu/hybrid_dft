open Cil
open Pretty
module E = Errormsg

let print_expr (string_t : string) (exp_t : exp ) =
	ignore(E.log string_t (sprint 20 (d_exp () exp_t)))

;;