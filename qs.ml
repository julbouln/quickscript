(** QS: the Poc Engine simple Language *)

open Qs_types;;
open Qs_parser;;
open Qs_func;;

(** parse s string with convertion function (for recup obj variables) (object:string -> variable:string -> result:int) and return the result of operation*)

  
let qs_parse s=
         let lexbuf=Lexing.from_string (s) in
           Qs_parser.block (Qs_lexer.token ) lexbuf;;
 

let filename=(Sys.argv).(1) in
let file=open_in filename in
let buf=Buffer.create 1024 in
  while (
    try
      Buffer.add_string buf (input_line file);
      Buffer.add_string buf "\n";
      true;
    with End_of_file -> false
  ) do () done;
  (*print_string (Buffer.contents buf);*)
  qs_parse (Buffer.contents buf);
  close_in file


(*
(* TEST *)

(* add val to QS *)
kernel#add_val "c" (QsInt 2);;

(* add function to QS *) 
kernel#add_func "test" ( 
  fun a->
    (match a with
     | QsInt x->print_string "blabla";print_int x;print_newline();
     | _-> raise (Qs_func_invalid_argument "test"));QsInt 4
);;
*)
(* parse some QS code *)

(*
let test=qs_parse "
debut
a=1,
b=\"bla\",
(g=concat: (b b)),
fonction bla: ( afficher: 10,afficher: 20),
(si a<20 alors afficher: g sinon afficher: 0),
bla: nil,
afficher: a,
(r=aleatoire: 100),
afficher: r
fin";; 

*)
