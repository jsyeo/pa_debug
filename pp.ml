open Camlp4.PreCast
open Format

module OCamlPrinters = Camlp4.Printers.OCaml.Make(Syntax)

let printer = new OCamlPrinters.printer

let pobj = printer ~comments:true ~curry_constr:true ()

let print_ctyp ty = pobj#simple_ctyp std_formatter ty
