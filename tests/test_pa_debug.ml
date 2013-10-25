(* #require "deriving-ocsigen";; *)

(* #load_rec "pa_deriving.cma";; *)
(* #load "deriving.cma";; *)
(* #require "compiler-libs.common";; *)
(* #load_rec "pa_debug.cmo";; *)

(* #load "str.cma";; *)
(* #require "debuglib";; *)

(* debugging simple functions *)
let debug<int, int> add1 x = x + 1;;


add1 5;;

(* debugging functions with two or more parameters *)

let debug<float, float, float> dist x y = sqrt( x ** 2.0 +. y ** 2.0 );;

dist 3.4 6.7;;

let debug<int, int> rec fib n =
    if n < 1 then
      1
    else (fib (n- 1) + (fib (n - 2)))
;;

fib 10;;

(* string list and int output *)
let debug<string list, int> rec length lst =
  match lst with
  | [] -> 0
  | (_::t) -> 1 + length t
;;

(* length [1;2;3;4;5];; *)

length ["a"; "b"; "c"];;

(* User defined types *)
type 'a my_list = Cons of ('a * 'a my_list) | Nil
  deriving (Show);;

(* I can't do something like debug<'a my_list, int> tho :( *)
let debug<int my_list, int> rec length2 lst =
  match lst with
  | Nil -> 0
  | Cons (_, rest) -> 1 + length2 rest
;;

let l = Cons (1, Cons (2, Nil));;

length2 l;;

(* let debug fib n = *)
(*   if n < 1 then *)
(*     n *)
(*   else fib (n - 1) + fib (n - 2) *)
(* ;; *)
