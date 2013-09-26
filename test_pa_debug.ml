#use "topfind";;
#camlp4o;;
#require "deriving-ocsigen";;


#load "pa_deriving.cma";;
#load "deriving.cma";;
#load "pp.cmo";;
#load "pa_debug.cmo";;

#load "str.cma";;
#require "debuglib";;

let debug rec fact n =
    if n < 1 then
      1
    else n * (fact (n - 1))
;;
let debug<int, int> rec fib n =
    if n < 1 then
      1
    else (fib (n- 1) + (fib (n - 2)))
;;

fib 10;;

let debug<int * int, int> mult (x,y) =
  x * y
;;

mult (4,3)

let debug<string list, int> rec length lst =
  match lst with
  | [] -> 0
  | (_::t) -> 1 + length t
;;

length [1;2;3;4;5];;

length ["a"; "b"; "c"];;
(* let pr x =
  (let module Show =
     struct
       type inline = int;;
       module Show_inline : Deriving_Show.Show with type a = inline =
         Deriving_Show.Show_int;;
       include Show_inline;;
     end
    in Show.show) x;;*)
