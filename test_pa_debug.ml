open Pa_debug
open Camlp4.PreCast

let _ =
  print_endline "START!"
  (* Printers.OCaml.print_implem (test_gen_debug ());; *)

let test_gen_debug () =
  let _loc = Loc.ghost and
      <:expr< let $bi$ in $e$ >> = <:expr< let fact n = n * 2 in fact 2>> and
      ty = Some (Ast.TyId (_loc, Ast.IdLid (_loc, "int"))) and
      r = Ast.ReRecursive
  in generate_debug_function ~bi:bi ~ty:ty ~r:r _loc
;;

#use "topfind";;
#camlp4o;;
#load "utils.cmo";;
#load "type.cmo";;
#load "clusters.cmo";;
#load "base.cmo";;

#load "pa_debug.cmo";;
#load "str.cma"
#load "globals.cmo"
#load "error.cmo"
#load "gen.cmo"
#load "debug.cmo";;

let debug rec fact n =
    if n < 1 then
      1
    else n * (fact (n - 1))
;;
let debug rec fib n =
    if n < 1 then
      1
    else (fib (n- 1) + (fib (n - 2)))
;;

let debug<int> rec length lst =
  match lst with
  | [] -> 0
  | (_::t) -> 1 + length t
;;

length [1;2;3;4;5];;
