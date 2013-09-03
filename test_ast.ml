(*
play with ocaml and see their ast here!
Dump the ast with camlp4orf test_ast.ml -printer o

If you are playing with another syntax extension, pass it to camlp4orf like this:

camlp4orf -I ~/.opam/system/lib/deriving-ocsigen pa_deriving.cma test_ast.ml

*)
let bi = <:expr< let int a = 1 in a>>

<:ctyp< type a = int list >>

let m = <:expr< x + b >> in
<:str_item<
  let x = 1 in
  let b = 2 in
  let hee = $m$ in
  b
>>

type 'a my_list = Cons of 'a * 'a my_list | Nil
     deriving (Show)
;;

let a = Cons (1, Cons (2, Nil));;

let apply_pr p lst =
  print_string (p lst)
;;

let string_of_my_list lst = (Show.show<int my_list> lst);;

let _ =
  let pr = (fun lst -> Show.show<int my_list> lst) in
    apply_pr pr a
;;

let a = <:str_item<
value rec fact (n:int) =
  if n < 1 then 1
  else n * (fact (n - 1))
>>
