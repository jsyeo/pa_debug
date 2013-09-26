open Camlp4.PreCast
open Format

module OCamlPrinters = Camlp4.Printers.OCaml.Make(Syntax)

let printer = new OCamlPrinters.printer

let pobj = printer ~comments:true ~curry_constr:true ()

let print_ctyp ty = pobj#simple_ctyp std_formatter ty

let print_ctyp_list types =
  let rec helper = function
    | [] -> ()
    | [x] ->
      pobj#simple_ctyp str_formatter x
    | (x::xs) ->
      pobj#simple_ctyp str_formatter x;
      fprintf str_formatter "; ";
      helper xs in
  helper types;
  print_endline ("[" ^ flush_str_formatter () ^ "]")
;;

let print_str_item str_item = pobj#str_item std_formatter str_item
