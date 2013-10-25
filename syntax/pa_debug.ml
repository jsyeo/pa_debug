open Camlp4.PreCast
open Syntax

let _loc = Loc.ghost

let debug = true

let debugpr =
  if debug then
    print_endline
  else
    (fun s -> ())

let debuglog msg =
  if debug then
    let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 "log.txt" in
    Printf.fprintf oc "%s\n" msg;
    close_out oc
  else
    ()

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

module Retype =
  struct
    open Types
    open Typedtree
    open Location
    open Cmt_format

    let todo msg = failwith ("TODO: " ^ msg)

    let match_loc loc1 loc2 = (* TODO : do more tests *)
      let open Lexing in
      let _, st_line1, st_char1 = get_pos_info loc1.loc_start in
      let _, st_line2, st_char2 = get_pos_info loc2.loc_start in
      let _, end_line1, end_char1 = get_pos_info loc1.loc_end in
      let _, end_line2, end_char2 = get_pos_info loc2.loc_end in
      st_line1 = st_line2
      && end_line1 = end_line2
      && st_char1 <= st_char2
      && end_char1 >= end_char2

    let rec from_binary_annots loc = function
      | Packed _ -> todo "Packed... "
      | Interface _ -> todo "Interface..."
      | Partial_implementation _ -> todo "Partial implementation..."
      | Partial_interface _ -> todo "Partial interface..."
      | Implementation structure ->
         from_structures loc structure.str_items

    and from_structures loc = function
      | [] -> None     (* No type founded *)
      | strct :: tl ->
         match from_structure loc strct.str_desc with
         | None -> from_structures loc tl
         | Some _ as ty -> ty      (* Stop looping, type founded *)

    and from_structure loc = function
      | Tstr_eval e ->
         from_loc_expr loc e
      | Tstr_value (_, pat_expr_list) ->
         from_params loc pat_expr_list
      | Tstr_modtype (_, sloc, mtype) ->
         if match_loc loc sloc.loc then todo "Tstr_modtype..."
         else from_mod_type loc mtype.mty_desc
      | Tstr_type tcl ->
         from_type_decl loc tcl
      | Tstr_primitive _ -> todo "Primitive..."
      | Tstr_exception _ -> todo "Exception..."
      | Tstr_exn_rebind _ -> todo "Exn_rebind..."
      | Tstr_module _ -> todo "Module..."
      | Tstr_recmodule _ -> todo "Recmodule..."
      | Tstr_open _ -> todo "Open..."
      | Tstr_class _ -> todo "Class..."
      | Tstr_class_type _ -> todo "Class type..."
      | Tstr_include _ -> todo "Include..."

    and from_type_decl loc = function
      | [] -> None
      | (id, sloc, tdecl) :: tl ->
         if match_loc loc sloc.loc then
           todo "Type constructore name ?"
         else
           from_type_decl loc tl

    and from_mod_type loc = function
      | Tmty_ident (path, lloc) -> assert false
      | Tmty_signature sign ->
         let rec loop loc = function
           | [] -> None
           | sign :: tl ->
              match from_signature loc sign.sig_desc with
              | None -> loop loc tl
              | Some _ as ty -> ty     (* Stop looping *)
         in
         loop loc sign.sig_items
      | Tmty_functor (id, sloc, mtyp1, mtyp2) ->
         begin match from_mod_type loc mtyp1.mty_desc with
               | None -> from_mod_type loc mtyp2.mty_desc
               | Some _ as ty -> ty
         end
      | Tmty_with (mtyp1, _) -> assert false
      | Tmty_typeof mexp -> assert false

    and from_signature loc = function
      | Tsig_value _ -> assert false
      | Tsig_type _ -> assert false
      | Tsig_exception _ -> assert false
      | Tsig_module _ -> assert false
      | Tsig_recmodule _ -> assert false
      | Tsig_modtype _ -> assert false
      | Tsig_open _ -> assert false
      | Tsig_include _ -> assert false
      | Tsig_class _ -> assert false
      | Tsig_class_type _ -> assert false

    and from_loc_expr loc tt =
      if match_loc loc tt.exp_loc then Some tt.exp_type
      else from_loc_edesc loc tt.exp_desc

    and from_loc_edesc loc t =
      match t with
      | Texp_ident (path, lloc, vdesc) ->
         if match_loc loc lloc.loc then Some vdesc.val_type
         else None
      | Texp_constant _ ->
         None
      | Texp_let (_, binds, body) ->
         begin match from_params loc binds with
               | None -> from_loc_expr loc body
               | Some _ as ty -> ty
         end
      | Texp_function (_, params, _) ->
         from_params loc params
      | Texp_apply (e, args) ->
         begin match from_loc_expr loc e with
               | None ->
                  let rec loop loc args =
                    match args with
                    | [] -> None
                    | (_, None, _) :: tl -> loop loc tl
                    | (_, Some e, _) :: tl ->
                       begin match from_loc_expr loc e with
                             | None -> loop loc tl
                             | Some _ as ty -> ty
                       end
                  in
                  loop loc args
               | Some _ as ty -> ty (* type of e *)
         end
      | Texp_match (e, cases, _) ->
         begin match from_loc_expr loc e with
               | None -> from_params loc cases
               | Some _ as ty -> ty
         end
      | Texp_try (e, cases) ->
         begin match from_loc_expr loc e with
               | None -> from_params loc cases
               | Some _ as ty -> ty
         end
      | Texp_tuple tuples ->
         from_loc_exprs loc tuples
      | Texp_construct (lloc, _, constrs, _) ->
         if match_loc loc lloc.loc then todo "Texp_construct cases..."
         else from_loc_exprs loc constrs

      | Texp_variant (_, None) -> None
      | Texp_variant (_, Some e) ->
         from_loc_expr loc e

      | Texp_record (lbl_exp_list, e_opt) ->
         let rec loop loc = function
           | [] ->
              begin match e_opt with
                    | None -> None
                    | Some e -> from_loc_expr loc e
              end
           | (lloc, _, e) :: tl ->
              if match_loc loc lloc.loc then todo "Texp_record cases..."
              else
                begin match from_loc_expr loc e with
                      | None -> loop loc tl
                      | Some _ as ty -> ty
                end in
         loop loc lbl_exp_list
      | Texp_field (e, lloc, _) ->
         if match_loc loc lloc.loc then todo "Texp_field cases..."
         else from_loc_expr loc e
      | Texp_setfield (record, lloc, _, new_val) ->
         if match_loc loc lloc.loc then todo "Texp_setfield cases..."
         else
           begin match from_loc_expr loc record with
                 | None -> from_loc_expr loc new_val
                 | Some _ as ty -> ty
           end
      | Texp_array vals ->
         from_loc_exprs loc vals

      | Texp_ifthenelse (cond, thenb, None) ->
         from_loc_exprs loc [cond; thenb]
      | Texp_ifthenelse (cond, thenb, Some elseb) ->
         from_loc_exprs loc [cond; thenb; elseb]

      | Texp_sequence (e1, e2) ->
         from_loc_exprs loc [e1; e2]
      | Texp_while (cond, body) ->
         from_loc_exprs loc [cond; body]
      | Texp_for _ -> todo "Texp_for cases..."
      | Texp_when (e1, e2) ->
         from_loc_exprs loc [e1; e2]

      | Texp_send (e, _, None) ->
         from_loc_expr loc e
      | Texp_send (e1, _, Some e2) ->
         from_loc_exprs loc [e1; e2]
      | Texp_assert e -> from_loc_expr loc e

      | Texp_new (_, _, _)     -> todo "Texp_new cases..."
      | Texp_instvar (_, _, _) -> todo "Texp_instvar cases..."
      | Texp_setinstvar _      -> todo "Texp_setinstvar cases..."
      | Texp_override _        -> todo "Texp_override cases..."
      | Texp_letmodule _       -> todo "Texp_letmodule cases..."
      | Texp_assertfalse       -> todo "Texp_assert false cases..."
      | Texp_lazy _            -> todo "Texp_Lazy cases..."
      | Texp_object _          -> todo "Texp_object cases..."
      | Texp_pack _            -> todo "Texp_pack cases..."

    and from_params loc params =
      match params with
      | [] -> None
      | (pat, exp) :: tl ->
         if match_loc loc pat.pat_loc then Some pat.pat_type
         else
           match from_loc_expr loc exp with
           | None -> from_params loc tl
           | Some _ as ty -> ty

    and from_loc_exprs loc = function
      | [] -> None
      | e :: tl ->
         match from_loc_expr loc e with
         | None -> from_loc_exprs loc tl
         | Some _ as ty -> ty (* Stop the loop, type founded *)

    let from_loc loc bin_annot =
      from_binary_annots loc bin_annot

    let types_from_cmt loc cmtfilename =
      let cmt = Cmt_format.read_cmt cmtfilename in
      let bin_annot = cmt.cmt_annots in
      match from_loc loc bin_annot with
      | None -> failwith "No type found...\n!"
      | Some ty -> ty

    let types_from_arrow ty =
      let rec helper ty =
        match ty.desc with
        | Tlink ty -> helper ty
        | Tarrow (_, t1, t2, _) -> helper t1 @ helper t2
        | Tconstr (Path.Pident id, _, _) -> [Ident.name id]
        | _ -> failwith "oops" in
      helper ty

    let print_ty_from_cmt loc cmtfilename =
      let cmt = Cmt_format.read_cmt cmtfilename in
      let bin_annot = cmt.cmt_annots in
      match from_loc loc bin_annot with
      | None -> failwith "No type found...\n%!"
      | Some ty ->
         debuglog "Type found...";
         Printtyp.raw_type_expr Format.str_formatter ty;
         Format.flush_str_formatter ()
  end

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

let ml_to_cmt filename =
  let len = String.length filename in
  String.sub filename 0 (len - 3) ^ ".cmt"

let read_types_at_loc loc =
  (* No types supplied, figure out types from bin_annot. *)
  let filename = Loc.file_name loc in
  let _ = debuglog @@ filename in
  let cmtfilename = ml_to_cmt filename in
  let cmtfilename = "whee.cmt" in
  let _ = debuglog @@ Loc.to_string loc in
  let start, stop = Loc.start_pos loc, Loc.stop_pos loc in
  let exprloc =
    {Location.loc_start = start;
     Location.loc_end = stop;
     Location.loc_ghost = false} in
  let _ = debuglog @@ Retype.print_ty_from_cmt exprloc cmtfilename in
  let ty = Retype.types_from_cmt exprloc cmtfilename in
  let typnames = Retype.types_from_arrow ty in
  let ctyps = List.map (fun name -> <:ctyp< $lid:name$ >>) typnames in
  ctyps

let infer_types str_item_string =
  let open Typedtree in
  let open Types in
  let types_from_arrow ty =
    let rec helper ty =
      match ty.desc with
      | Tlink ty -> helper ty
      | Tarrow (_, t1, t2, _) -> helper t1 @ helper t2
      | Tconstr (Path.Pident id, _, _) -> [Ident.name id]
      | _ -> failwith "oops" in
    helper ty in
  let e = let _ = Compmisc.init_path false in
          Compmisc.initial_env () in
  let buf = Lexing.from_string str_item_string in
  let p = Parse.implementation buf in
  let tstr, _tsig, _newe = Typemod.type_structure e p Location.none in
  let {str_items} = tstr in
  let {str_desc} :: _ = str_items in
  let pat_lst =
    match str_desc with
    | Tstr_value (_, l) -> l
    | _ -> failwith "Not handled" in
  let arrow_type =
    match pat_lst with
    | [] -> failwith "Got empty pat list"
    | ({ pat_type }, _) :: _ ->
       pat_type in
  let typnames = types_from_arrow arrow_type in
  let ctyps = List.map (fun name -> <:ctyp< $lid:name$ >>) typnames in
  ctyps


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
        let _ = debuglog ("====PRINTING GENERATED AST for " ^ fun_id ^ "===\n") in
        let _ = debuglog @@ string_of_str_item res in
        let _ = debuglog "====FINISH===\n" in
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
          (* let types = read_types_at_loc _loc in*)                 
          let s = <:str_item< let $rec:r$ $binding:bi$ >> in
          let str_string = string_of_str_item s in
          let types = infer_types str_string in
          let _ = debuglog @@ string_of_list string_of_ctyp types in
          generate_debug_function ~r:r ~bi:bi ~ty:(Some types) _loc
          | "let"; "debug"; "<"; types = LIST1 ctyp SEP "," ; ">"; r = opt_rec; bi = binding ->
          let _ = debuglog @@ string_of_list string_of_ctyp types in
          generate_debug_function ~r:r ~bi:bi ~ty:(Some types) _loc
          ]
      ]
;
END

module M = Camlp4.Register.OCamlPrinter(Camlp4.Printers.OCaml.Id)(Camlp4.Printers.OCaml.Make)
