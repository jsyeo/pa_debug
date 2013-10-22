open Camlp4.PreCast
open Syntax

let _loc = Loc.ghost

let debug = false

let debugpr =
  if debug then
    print_endline
  else
    (fun s -> ())

let syntax_printer =
  let module PP = Camlp4.Printers.OCaml.Make (Syntax) in
  new PP.printer ~comments:false ()

let string_of_expr expr =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#expr expr;
  Buffer.contents buffer

let string_of_str_item str =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#str_item str;
  Buffer.contents buffer

let string_of_ctyp ctyp =
  let buffer = Buffer.create 16 in
  Format.bprintf buffer "%a%!" syntax_printer#simple_ctyp ctyp;
  Buffer.contents buffer

let string_of_list printer lst =
  String.concat ";" @@ List.map printer lst

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

let type_to_fun_name ty =
  let rec helper = function
    | <:ctyp< $lid:i$ >> | <:ctyp< $uid:i$ >> -> i
    | <:ctyp< $t1$ * $t2$ >> ->
      Format.sprintf "%s_x_%s" (helper t1) (helper t2)
    | <:ctyp< $t1$ $t2$ >> ->
      Format.sprintf "%s_app_%s" (helper t1) (helper t2)
    | _ -> failwith "type not handled"
  in "pr_" ^ helper ty
(*
Taken from ocsigen/deriving/syntax/extend.ml
*)
let instantiate_show_printer _loc t =
  let open Pa_deriving_common in
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
let mk_head _loc fun_name num_args printers_assoc =
  let debug_funs = [| <:expr< Debug.ho_1 >>;<:expr< Debug.go_2 false false >>; <:expr< Debug.no_3 >> |] in
  let debug_fun = debug_funs.(num_args - 1) in
  List.fold_left (fun accum (pr_name, _) -> <:expr< $accum$ $lid:pr_name$ >>) <:expr< $debug_fun$ $str:fun_name$ >> printers_assoc

let mk_printers _loc types =
  let mk_printer ty pr_fun_name =
    let show_module_expr = instantiate_show_printer _loc ty in
    let show_module_name = "Show" in
    <:str_item<
      let $lid:pr_fun_name$ x =
        (let module $uid:show_module_name$ = $show_module_expr$
         in $uid:show_module_name$.$lid:"show"$) x>> in
  let printers_assoc = [] in
  let rec helper = function
  | [] -> printers_assoc
  | (ty::tys) ->
    let pr_fun_name = type_to_fun_name ty in
    if List.mem_assoc pr_fun_name printers_assoc then
      helper tys
    else
      (pr_fun_name, mk_printer ty pr_fun_name) :: (helper tys)
  in helper types

let generate_debug_function ~r ~bi ~ty _loc =
  match ty with
  | Some ty ->
    begin
      let num_args = count_args bi in
      let num_types = List.length ty in
      if num_args + 1 <> num_types then
        failwith "Pa_debug: Number of types not equal number of args"
      else
        let printers_assoc = mk_printers _loc ty in
        let patts = get_patts bi in
        let new_fun_name = gen_fresh_name () in
        let renamed_fun = rename_fun bi new_fun_name in
        let new_patts = get_patts renamed_fun in
        let fun_id = get_fun_id bi in     (* Original function's name *)
        let types = mk_head _loc fun_id num_args printers_assoc in
        let debug_fun_body = mk_appln _loc types new_patts in
        let debug_fun = mk_fun _loc (List.tl patts) debug_fun_body in
        let res = <:str_item<
          $list:List.map snd printers_assoc$;;
          let rec $renamed_fun$ and $lid:fun_id$ = $debug_fun$>> in
        let _ = debugpr ("====PRINTING GENERATED AST for " ^ fun_id ^ "===\n") in
        let _ = debugpr @@ string_of_str_item res in
        let _ = debugpr "====FINISH===\n" in
        res
    end
  | None ->
     (* No types supplied, figure out types from bin_annot. *)
     
     failwith "OOPS. TODO: No type. Please handle it"
;;

(* The Grammar to add the debug annotation *)
EXTEND Gram
str_item:
      [ "top"
          [ "let"; "debug" ; r = opt_rec; bi = binding ->
          generate_debug_function ~r:r ~bi:bi ~ty:None _loc
          | "let"; "debug"; "<"; types = LIST1 ctyp SEP "," ; ">"; r = opt_rec; bi = binding ->
          let _ = debugpr @@ string_of_list string_of_ctyp types in
          generate_debug_function ~r:r ~bi:bi ~ty:(Some types) _loc
          ]
      ]
;
END

module M = Camlp4.Register.OCamlPrinter(Camlp4.Printers.OCaml.Id)(Camlp4.Printers.OCaml.Make)
