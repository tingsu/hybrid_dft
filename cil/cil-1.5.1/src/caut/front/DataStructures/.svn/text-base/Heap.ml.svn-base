(*
   Leftist Heap

   Original source code in SML from:

     Purely Functional Data Structures
     Chris Okasaki
     Cambridge University Press, 1998
     Copyright (c) 1998 Cambridge University Press

   Translation from SML to OCAML (this file):

     Copyright (C) 1999, 2000, 2001  Markus Mottl
     email:  markus.mottl@gmail.com
     www:    http://www.ocaml.info

   Unless this violates copyrights of the original sources, the following
   licence applies to this file:

   This source code is free software; you can redistribute it and/or
   modify it without any restrictions. It is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY.
*)

exception Empty

type 'a heap = E | T of int * ('a * float) * ('a heap) * ('a heap)

let rank = function E -> 0 | T (r,_,_,_) -> r

let makeT x a b =
  if rank a >= rank b then T (rank b + 1, x, a, b)
  else T (rank a + 1, x, b, a)

let empty = E
let is_empty h = h = E

let rec merge h1 h2 = match h1, h2 with
  | _, E -> h1
  | E, _ -> h2
  | T (_, (x, rx), a1, b1), T (_, (y, ry), a2, b2) ->
      if rx <= ry then makeT (x, rx) a1 (merge b1 h2)
      else makeT (y, ry) a2 (merge h1 b2)

let insert x h = merge (T (1, x, E, E)) h
let find_min = function E -> raise Empty | T (_, x, _, _) -> x
let delete_min = function E -> raise Empty | T (_, x, a, b) -> merge a b

