#!/bin/sh

MYLIB=$1

echo "Build $MYLIB ..."

ocamlc -custom -output-obj -o $MYLIB.o qs_lib_helper.ml $MYLIB.ml
ocamlc -c qs_lib_wrap.c 
gcc -shared -o $MYLIB.so $MYLIB.o qs_lib_wrap.o -lcamlrun -ltermcap
#-L/usr/lib/ocaml/3.08.3 
strip $MYLIB.so