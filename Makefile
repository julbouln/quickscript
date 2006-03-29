OCAMLMAKEFILE = OCamlMakefile

LIBS=str 
#dl
#PACKS=dl

SOURCES = qs_types.ml qs_parser.mly qs_lexer.mll qs_func.ml qs_stdlib.ml qs.ml
RESULT  = quickscript

all: ncl

include $(OCAMLMAKEFILE)