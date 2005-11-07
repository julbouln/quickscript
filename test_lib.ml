open Qs_types;;
open Qs_lib_helper;;

let print_string2 s=
  (match s with
  | QsString ss->print_string ss;print_newline()
  | _ ->());
  QsNil

let test s=print_string "blabla";print_newline();QsNil;;

qs_lib_register [
("test",test);
("print_string2",print_string2)
]

