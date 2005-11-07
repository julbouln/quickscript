open Qs_types;;

let print_string2 s=
  (match s with
  | QsString ss->print_string ss;print_newline()
  | _ ->());
  QsNil

let test()=print_string "blabla";print_newline();;

let test2 s=print_string s;print_newline();;

let _ = Callback.register "test" test;;
let _ = Callback.register "test2" test2;;
let _ = Callback.register "print_string2" print_string2;;
