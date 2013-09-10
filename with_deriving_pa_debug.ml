open Camlp4.PreCast
open Syntax
open Debug

let _loc = Loc.ghost

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

(*
Returns the patterns in the binding.
See http://brion.inria.fr/gallium/index.php/Abstract_Syntax_Tree#Bindings.

<: binding< $pat:f$ $pat:x$ = $exp:ex$ >>

This is how ocaml makes its AST. For function bindings like this:

let q = <:str_item< let f x y z = x + y in 1>>

it will produce this chunk of AST:

let q =
  Ast.StExp (_loc,
    (Ast.ExLet (_loc, Ast.ReNil,
       (Ast.BiEq (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "f")))),
          (Ast.ExFun (_loc,
             (Ast.McArr (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "x")))),
                (Ast.ExNil _loc),
                (Ast.ExFun (_loc,
                   (Ast.McArr (_loc,
                      (Ast.PaId (_loc, (Ast.IdLid (_loc, "y")))),
                      (Ast.ExNil _loc),
                      (Ast.ExFun (_loc,
                         (Ast.McArr (_loc,
                            (Ast.PaId (_loc, (Ast.IdLid (_loc, "z")))),
                            (Ast.ExNil _loc),
                            (Ast.ExApp (_loc,
                               (Ast.ExApp (_loc,
                                  (Ast.ExId (_loc, (Ast.IdLid (_loc, "+")))),
                                  (Ast.ExId (_loc, (Ast.IdLid (_loc, "x")))))),
                               (Ast.ExId (_loc, (Ast.IdLid (_loc, "y")))))))))))))))))))),
       (Ast.ExInt (_loc, "1")))))

Therefore, to get all the arguments in the function, we have to iterative through the tree looking
for Ast.McArr (_, patt, _, )
*)
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
  | <:binding@loc< $patt$ = $exfun$ >> ->
    (begin
      match exfun with
      | Ast.ExFun (_, match_arr) -> patt::get_args match_arr
      | _ -> []
    end)
  | _ -> []
;;

(* generates a fresh name *)
let gen_fresh_name =
  let count = ref (-1) in
  (fun () ->
    incr count;
    "pa_debug" ^ string_of_int !count)
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

(* Returns the name of the function *)
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
  | <:binding@loc< $patt$ = $exfun$ >> ->
    begin
      match exfun with
      | Ast.ExFun (_, match_arr) -> 1 + helper match_arr
      | _ -> 0
    end
  | _ -> 0
;;

(*
Taken from ocsigen/deriving/syntax.ml
*)
let instantiate_show_printer _loc t =
  try
    let classname = "Show" in
    let class_ = Base.find classname in
    let module U = Type.Untranslate(struct let _loc = _loc end) in
    let binding = Ast.TyDcl (_loc, "inline", [], t, []) in
    let decls = Base.display_errors _loc Type.Translate.decls binding in
    if List.exists Type.contains_tvars_decl decls then
      Base.fatal_error _loc ("deriving: type variables cannot be used in `method' instantiations");
    let tdecls = List.map U.decl decls in
    let m = Base.derive_str _loc decls class_ in
    <:module_expr< struct
      type $list:tdecls$
      $m$
      include $uid:classname ^ "_inline"$
    end >>
  with Base.NoSuchClass classname ->
    Base.fatal_error _loc ("deriving: " ^ classname ^ " is not a known `class'")
;;

(*
  Generate the head part of the function body.
  This includes the Debug function call and the printers.
*)
let mk_head _loc fun_name num_args ty =
  let pr_int = "string_of_int" in
  let debug_funs = [| <:expr< Debug.ho_1 >>;<:expr< Debug.ho_2 >>; <:expr< Debug.ho_3 >> |] in
  let debug_fun = debug_funs.(num_args - 1) in
  let rec mk_printers num =
    if num > 0 then
      <:expr< $mk_printers (num-1)$ $lid:pr_int$ >>
    else
      <:expr< $debug_fun$ $str:fun_name$ >> in
  <:expr< $mk_printers num_args$ $lid:pr_int$ >>
;;


let generate_debug_function ~r ~bi ~ty _loc =
  match ty with
  | Some ty ->
    begin
      let show_module_expr = instantiate_show_printer _loc ty in
      let _ = Printers.OCaml.print_implem <:str_item< value module Show = $show_module_expr$ >> in
      let show_module_name = "Show" in
      let patts = get_patts bi in
      let new_fun_name = gen_fresh_name () in
      let renamed_fun = rename_fun bi new_fun_name in
      let new_patts = get_patts renamed_fun in
      let fun_id = get_fun_id bi in     (* Original function's name *)
      let num_args = count_args bi in
      let types = mk_head _loc fun_id num_args ty in
      let debug_fun_body = mk_appln _loc types new_patts in
      let debug_fun = mk_fun _loc (List.tl patts) debug_fun_body in
      let res = <:str_item<
        (* let pr lst = *)
        (*   (let module $uid:show_module_name$ = $show_module_expr$ *)
        (*    in $lid:show_module_name$.$lid:"show"$) lst in let*)
        let $rec:r$ $renamed_fun$ and $lid:fun_id$ = $debug_fun$>> in
      let _ = print_string ("====PRINTING GENERATED AST for " ^ fun_id ^ "===\n") in
      let _ = Printers.OCaml.print_implem res in
      let _ = print_string "====FINISH===\n" in
      res
    end
  | None -> failwith "OOPS. TODO: No type. Please handle it"
;;

(* The Grammar to add the debug annotation *)
EXTEND Gram
str_item:
      [ "top"
          [ "let"; "debug" ; r = opt_rec; bi = binding ->
          let _ = Printers.OCaml.print_implem <:str_item< let $bi$ in 2 >> in
          generate_debug_function ~r:r ~bi:bi ~ty:None _loc
          | "let"; "debug"; "<"; t = ctyp; ">"; r = opt_rec; bi = binding -> generate_debug_function ~r:r ~bi:bi ~ty:(Some t) _loc]
      ]
;
END
