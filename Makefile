OCAMLMAKEFILE = OCamlMakefile

LIBS=str

SOURCES = qs_types.ml qs_func.ml qs_parser.mly qs_lexer.mll qs.ml
RESULT  = quickscript

all: ncl

include $(OCAMLMAKEFILE)