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
	ocamlbuild -no-links -libs $(LIBS) -use-ocamlfind pa_debug.cmo
	cp _build/pa_debug.cmo .

clean:
	rm -f *.cm[aoi]
	rm -f vec_deriving
	rm -f variant
	rm -f *.out
