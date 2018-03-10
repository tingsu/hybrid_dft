(* Utilities for tuple manipulations
 *)
let combine_hashes t1_hash t2_hash (t1, t2) = Hashtbl.hash (t1_hash t1, t2_hash t2)
let combine_hashes3 t1_hash t2_hash t3_hash (t1, t2, t3) = Hashtbl.hash (t1_hash t1, t2_hash t2, t3_hash t3)

let combine_equals t1_equal t2_equal (t1, t2) (t1', t2') = t1_equal t1 t1' && t2_equal t2 t2'
let combine_equals3 t1_equal t2_equal t3_equal (t1, t2, t3) (t1', t2', t3') = t1_equal t1 t1' && t2_equal t2 t2' && t3_equal t3 t3'

let combine_compares t1_compare t2_compare (t1, t2) (t1', t2') = match t1_compare t1 t1' with 0 -> t2_compare t2 t2' | i -> i
let combine_compares3 t1_compare t2_compare t3_compare (t1, t2, t3) (t1', t2', t3') = match t1_compare t1 t1' with 0 -> (match t2_compare t2 t2' with 0 -> t3_compare t3 t3' | i -> i) | i -> i
