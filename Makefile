# OASIS_START
# DO NOT EDIT (digest: bc1e05bfc8b39b664f29dae8dbd3ebbb)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: 
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: 
	$(SETUP) -clean $(CLEANFLAGS)

distclean: 
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP
pa_debug:
	ocamlc -I +camlp4 camlp4lib.cma debug.cmo -pp camlp4orf -c pa_debug.ml

LIBS = dynlink,deriving,pa_deriving,pa_debug_lib

INCLUDES = -I,+camlp4

FLAGS = $(INCLUDES),-dtypes

pa_debug_cmo: debug
	ocamlc -I +camlp4 camlp4lib.cma -pp camlp4orf -c pa_debug.ml

pa_debug_ob:
	ocamlbuild -no-links -libs $(LIBS) -use-ocamlfind -tag annot syntax/pa_debug.cmo

pa_debug.cma:
	ocamlbuild -no-links -libs $(LIBS) -use-ocamlfind -tag annot pa_debug.cma

test_pa_debug.native:
	ocamlbuild -no-links -libs $(LIBS) -I debug_lib -I deriving_syntax_lib -use-ocamlfind -tag annot test_pa_debug.cmo

test_pa_debug.byte:
	ocamlbuild -libs $(LIBS) -use-ocamlfind -tag annot test_pa_debug.byte

test_ast.native:
	ocamlbuild -no-links -libs $(LIBS) -use-ocamlfind -tag annot test_ast.native

test_ast_pp:
	camlp4orf -I ~/.opam/system/lib/deriving-ocsigen pa_deriving.cma test_ast.ml

oasis_install:
	ocaml setup.ml -configure
	ocaml setup.ml -build
	ocaml setup.ml -uninstall
	ocaml setup.ml -install

clean:
	rm -f *.cm[aoi]
	rm -rf _build
