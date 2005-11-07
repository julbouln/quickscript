OCAMLMAKEFILE = OCamlMakefile

LIBS=str dl
PACKS=dl

SOURCES = qs_types.ml qs_parser.mly qs_lexer.mll qs_lib.ml qs_func.ml qs_stdlib.ml qs.ml
RESULT  = quickscript

test_lib:
	ocamlc -custom -output-obj -o test_lib.o test_lib.ml
	ocamlc -c qs_lib_wrap.c 
	gcc -shared -o test_lib.so test_lib.o qs_lib_wrap.o -L/usr/lib/ocaml/3.08.3 -lcamlrun -ltermcap
#	ocamlfind ocamlopt -o qs_lib -package dl -linkpkg qs_lib.ml
#	LD_LIBRARY_PATH=.:$$LD_LIBRARY_PATH ./qs_lib

all: ncl

include $(OCAMLMAKEFILE)