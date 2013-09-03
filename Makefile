pa_debug:
	ocamlc -I +camlp4 camlp4lib.cma debug.cmo -pp camlp4orf -c pa_debug.ml

LIBS = dynlink,camlp4lib,deriving

INCLUDES = -I,+camlp4

FLAGS = $(INCLUDES),-dtypes

pa_debug_cmo: debug
	ocamlc -I +camlp4 camlp4lib.cma -pp camlp4orf -c pa_debug.ml

debug:
	ocamlc -c globals.cmo
	ocamlc -c error.cmo
	ocamlc -c gen.cmo
	ocamlc -c debug.cmo

pa_debug_ob:
	ocamlbuild -no-links -libs $(LIBS) -I debug_lib -I deriving_syntax_lib -use-ocamlfind -tag annot pa_debug.cmo

test_pa_debug.native:
	ocamlbuild -no-links -libs $(LIBS) -I debug_lib -I deriving_syntax_lib -use-ocamlfind -tag annot test_pa_debug.cmo

test_pa_debug.cmo:
	ocamlbuild -no-links -libs $(LIBS) -I debug_lib -I deriving_syntax_lib -use-ocamlfind -tag annot test_pa_debug.cmo

test_ast.native:
	ocamlbuild -no-links -libs $(LIBS) -use-ocamlfind -tag annot test_ast.native

test_ast_pp:
	camlp4orf -I ~/.opam/system/lib/deriving-ocsigen pa_deriving.cma test_ast.ml

clean:
	rm -f *.cm[aoi]
	rm -rf _build
