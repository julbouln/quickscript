open Qs_types;;
open Qs_func;;

let load_stdlib kernel=
  
  let print=( 
    let rec parse_args a=
      (match a with
	 | QsInt x -> print_int x;print_newline();
	 | QsString x -> print_string x;print_newline();
	 | QsEnum x->
	     List.iter 
               ( fun ca -> parse_args ca)
               x;
	 | _ -> ()) in
      fun args-> parse_args args;QsNil ) in
    
    kernel#add_func "print" ( print);
    
    let random=( 
      let rec parse_args a=
	(match a with
	   | QsInt x -> QsInt (Random.int x)
	   | _ -> raise (Qs_func_invalid_argument "random")) in
	fun args-> parse_args args; ) in   
      
      kernel#add_func "random" ( random);
      
      kernel#add_func "concat" (( 
				  let res=ref "" in
				  let parse_args a=
				    (match a with
				       | QsEnum x->
					   List.iter 
					     ( fun ca ->
						 match ca with
						   | QsString v -> res:= !res^v
						   | _-> raise (Qs_func_invalid_argument "concat")
					     )
					     x;
				       | _-> raise (Qs_func_invalid_argument "concat")) in
				    fun args-> parse_args args;QsString (!res)))
;;
