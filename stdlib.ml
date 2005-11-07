open Qs_types;;
open Qs_lib_helper;;

let print args=( 
  let parse_args a=
    (match a with
    | QsInt x -> print_int x;print_newline();
    | QsString x -> print_string x;print_newline();
(*    | QsEnum x->
	List.iter 
          ( fun ca -> parse_args ca)
          x;*)
    | _ -> ()) in
  parse_args args;QsNil ) ;;

(*
let random args=( 
  let parse_args a=
    (match a with
    | QsInt x -> QsInt (Random.int x)
    | _ ->QsNil(* raise (Qs_func_invalid_argument "random")*)) in
  parse_args args; ) ;;
*)
      
let concat args=(
  let res=ref "" in
  let parse_args a=
    (match a with
    | QsEnum x->
	List.iter 
	  ( fun ca ->
	    match ca with
	    | QsString v -> res:= !res^v
	    | _->()(* raise (Qs_func_invalid_argument "concat")*)
	   )
	  x;
    | _-> ()(*raise (Qs_func_invalid_argument "concat")*)) in
  parse_args args;QsString (!res));;

qs_lib_register [
("print",print);
(*("random",random);*)
("concat",concat)

]
