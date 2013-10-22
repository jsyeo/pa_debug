pa_debug:
	ocamlc -I +camlp4 camlp4lib.cma debug.cmo -pp camlp4orf -c pa_debug.ml

LIBS = pa_debug_lib,dynlink,deriving,pa_deriving

INCLUDES = -I,+camlp4

FLAGS = $(INCLUDES),-dtypes

pa_debug_ob:
	ocamlbuild -no-links -libs $(LIBS) -use-ocamlfind -tag annot syntax/pa_debug.cmo

test_ast.native:
	ocamlbuild -no-links -libs $(LIBS) -use-ocamlfind -tag annot test_ast.native

test_ast_pp:
	camlp4orf -I ~/.opam/system/lib/deriving-ocsigen pa_deriving.cma test_ast.ml

oasis_install:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build
	ocaml setup.ml -uninstall
	ocaml setup.ml -install

clean:
	rm -f *.cm[aoi]
	rm -rf _build
