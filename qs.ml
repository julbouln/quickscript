(** QS: the Poc Engine simple Language *)

open Qs_types;;
open Qs_parser;;
open Qs_func;;
open Qs_stdlib;;

(** parse s string with convertion function (for recup obj variables) (object:string -> variable:string -> result:int) and return the result of operation*)

  

let filename=(Sys.argv).(1) in

  let kernel=new qs_kernel in
(*    load_stdlib kernel; *)
    kernel#file_exec filename




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
