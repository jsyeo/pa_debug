let debug add1 x = x +. 5.1;;

let debug addf x y = x +. y;;

let debug length lst =
  match lst with
  | [] -> 0
  | (_::t) -> 1 + length t
;;

let _ =
  add1 3.2 |> string_of_float |> print_endline;
  addf 3.4 4.2 |> string_of_float |> print_endline;;
