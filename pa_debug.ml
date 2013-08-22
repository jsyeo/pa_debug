(*
  # Compiling

  ocamlc -I +camlp4 camlp4lib.cma -pp camlp4orf -c pa_debug.ml

  # Executing

  Start ocaml interpreter and execute these:
*)
(* #use "topfind";; *)
(* #camlp4o;; *)
(* #load "pa_debug.cmo";; *)
(* debug.(let fact n = n*2);; *)

open Camlp4.PreCast
open Syntax
open Debug

let _loc = Loc.ghost;;

(*
  turns a list of patterns into variables bounded to functions
  [a;b;c] ==> (\a -> \b -> \c -> expression)
*)
let rec mk_fun _loc patts e =
  match patts with
  | p :: patts ->
    <:expr< fun $p$ -> $mk_fun _loc patts e$ >>
  | [] -> e

(* let mk_sequence _loc e = *)
(*   match e with *)
(*   | <:expr< $_$ ; $_$ >> as e -> <:expr< do { $e$ } >> *)
(*   | e -> e *)
(* ;; *)

let print_patt patt =
  match patt with
  | <:patt< $lid:lid$ >> -> print_string lid
  | _ -> print_string ""
;;

let get_patts bi =
  let rec get_args match_arr =
    match match_arr with
    | (Ast.McArr (_, patt, _, expr)) ->
      patt::(begin
        match expr with
        | Ast.ExFun (_, match_arr) -> get_args match_arr
        | _ -> []
      end)
    | _ -> [] in
  match bi with
  | <:binding@loc< $patt$ = fun [ $a$ ] >> ->
    patt :: get_args a
  | _ -> []
;;

let gen_fresh_name =
  let count = ref (-1) in
  (fun () ->
    incr count;
    "var" ^ string_of_int !count)
;;

let rename_fun bi new_name =
  match bi with
  | <:binding< $lid:fun_id$ = $e$ >> ->
    <:binding< $lid:new_name$ = $e$ >>
  | _ -> bi
;;

(* applies e to the args in patts *)
let mk_appln _loc e patts =
  let fun_id_patt = List.hd patts in
  match fun_id_patt with
  | <:patt< $lid:fun_id$ >> ->
    List.fold_left
      (fun e patt ->
        match patt with
        | <:patt< $lid:lid$ >> ->
          <:expr< $e$ $lid:lid$ >>
        | _ -> <:expr<>>
      ) <:expr< $e$ $lid:fun_id$>> (List.tl patts)
  | _ -> <:expr< >>
;;

let get_fun_id bi =
  match bi with
  | <:binding< $lid:fun_id$ = $e$ >> ->
    fun_id
  | _ -> ""

let count_args bi =
  let rec helper match_arr =
    match match_arr with
    | (Ast.McArr (_, patt, _, expr)) ->
      begin
        match expr with
        | Ast.ExFun (_, match_arr) -> 1 + (helper match_arr)
        | _ -> 0
      end
    | _ -> 0 in
  match bi with
  | <:binding@loc< $patt$ = fun [ $a$ ] >> ->
    1 + helper a
  | _ -> 0
;;

(*
  Generate the head part of the function body.
  This includes the Debug function call and the printers.
*)
let mk_head _loc fun_name num_args =
  let pr_int = "string_of_int" in
  let debug_funs = [| <:expr< Debug.ho_1 >>;<:expr< Debug.ho_2 >>; <:expr< Debug.ho_3 >> |] in
  let debug_fun = debug_funs.(num_args - 1) in
  let rec mk_printers num =
    if num > 0 then
      <:expr< $mk_printers (num-1)$ ($lid:pr_int$) >>
    else
      <:expr< $debug_fun$ $str:fun_name$ >> in
  <:expr< $mk_printers num_args$ $lid:pr_int$ >>

EXTEND Gram
str_item:
      [ "top"
          [ "let"; "debug" ; r = opt_rec; bi = binding ->
          let patts = get_patts bi in
          let new_fun_name = gen_fresh_name () in
          let renamed_fun = rename_fun bi new_fun_name in
          let new_patts = get_patts renamed_fun in
          let fun_id = get_fun_id bi in
          let num_args = count_args bi in
          let _ = print_string (string_of_int 10000) in
          let types = mk_head _loc fun_id num_args in
          let debug_fun_body = mk_appln _loc types new_patts in
          let debug_fun = mk_fun _loc (List.tl patts) debug_fun_body in
          let res = <:str_item< value $rec:r$ $renamed_fun$ and $lid:fun_id$ = $debug_fun$ >> in
          let _ = print_string "====PRINTING GENERATED AST===\n" in
          let _ = Printers.OCaml.print_implem res in
          let _ = print_string "====FINISH===\n" in
          res
          ] ]
    ;
    END

(* #use "topfind";; *)
(* #camlp4o;; *)
(* #load "pa_debug.cmo";; *)
(* #load "str.cma" *)
(* #load "globals.cmo" *)
(* #load "error.cmo" *)
(* #load "gen.cmo" *)
(* #load "debug.cmo";; *)
(* let debug rec fact n = *)
(*     if n < 1 then *)
(*       1 *)
(*     else n * (fact (n - 1)) *)
(* ;; *)
(* let debug rec fib n = *)
(*     if n < 1 then *)
(*       1 *)
(*     else (fib (n- 1) + (fib (n - 2))) *)
(* ;; *)
