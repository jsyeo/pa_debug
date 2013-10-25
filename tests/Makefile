no_types.native:
	ocamlbuild -classic-display -use-ocamlfind -tag bin_annot no_types.native

test_pa_debug.native:
	ocamlbuild -classic-display -use-ocamlfind test_pa_debug.native

test_pa_debug.byte:
	ocamlbuild -classic-display -use-ocamlfind test_pa_debug.byte

clean:
	rm -rf _build *.native *.byte

.PHONY: all no_types.native test_pa_debug.native clean
