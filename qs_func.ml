open Qs_types;;

let debug=false;;

exception Qs_val_not_declared of string
exception Qs_func_not_declared of string
exception Qs_func_invalid_argument of string


let int_of_qs p=match p with
  | QsInt x-> x
  | _ -> raise Bad_qs_type
;;

let bool_of_qs p=match p with
  | QsBool x-> x
  | _ -> raise Bad_qs_type
;;

let string_of_qs p=match p with
  | QsString x-> x
  | _ -> raise Bad_qs_type
;;

let struct_of_qs p=match p with
  | QsStruct x-> x
  | _ -> raise Bad_qs_type
;;


let qs_exp_list_concat l1 l2=
  match l1 with
    | QsEList v-> 
        (match l2 with
	   | QsEList w-> QsEList (List.append v w)
           | _ -> QsEList (List.append v [l2]))
    | _ -> 
        (match l2 with
	   | QsEList w-> QsEList (List.append [l1] w)
           | _ -> QsEList (List.append [l1] [l2]));;

let qs_val_list_concat l1 l2=
  match l1 with
    | QsValList v-> 
        (match l2 with
	   | QsValList w-> QsValList (List.append v w)
           | _ -> QsValList (List.append v [l2]))
    | _ -> 
        (match l2 with
	   | QsValList w-> QsValList (List.append [l1] w)
           | _ -> QsValList (List.append [l1] [l2]));;


let qs_inst_block_concat l1 l2=
  match l1 with
    | QsInstBlock v-> 
        (match l2 with
	   | QsInstBlock w-> QsInstBlock (List.append v w)
           | _ -> QsInstBlock(List.append v [l2]))
    | _ -> 
        (match l2 with
	   | QsInstBlock w-> QsInstBlock (List.append [l1] w)
           | _ -> QsInstBlock (List.append [l1] [l2]));;

let add_val_struct s v c=
  let h=struct_of_qs s in
    (*    print_string ("add val "^v);print_newline(); *)
    Hashtbl.add h v c
      
let set_val_struct s v c=
  let h=struct_of_qs s in
    (*    print_string ("add val "^v);print_newline(); *)
    Hashtbl.replace h v c
      
let get_val_struct s v=
  let h=struct_of_qs s in
    if Hashtbl.mem h v then
      Hashtbl.find h v
    else
      raise (Qs_val_not_declared v)
	

	
class qs_mem=
object(self)
  val mutable val_hash=Hashtbl.create 2
			 
  method add_val (id:string) (v:qs_val)=
    Hashtbl.add val_hash id v
  method set_val (id:string) (v:qs_val)=
    if debug then
      (print_string ("QS:set val "^id);print_newline());
      Hashtbl.replace val_hash id v
  method del_val (id:string)=Hashtbl.remove val_hash id
  method get_val (id:string)=
    if Hashtbl.mem val_hash id then
      Hashtbl.find val_hash id
    else 
      raise (Qs_val_not_declared id) 	
end;;  

class qs_funcs=
object(self)
  val mutable func_hash=Hashtbl.create 2
			 
  method add_func (id:string) (v:(qs_val->qs_val))=
    Hashtbl.add func_hash id v
  method set_func (id:string) (v:(qs_val->qs_val))=
      Hashtbl.replace func_hash id v
  method del_func (id:string)=Hashtbl.remove func_hash id
  method get_func (id:string)=
    if Hashtbl.mem func_hash id then
      Hashtbl.find func_hash id
    else 
      raise (Qs_func_not_declared id)
end;;  

  
class qs_kernel=
object(self)
  inherit qs_mem
  inherit qs_funcs
  val mutable inst_stack=Stack.create()
			   
  method push_inst (i:qs_inst)=Stack.push i inst_stack
  method pop_inst()=Stack.pop inst_stack
		      
  method func_decl (id:string) (aref:qs_exp) (inst)=
    self#add_func id (
		      fun args->
			let lmem=new qs_mem in
			  (match aref with
			     | QsEVal v->
				 (match v with
				    | QsVar vid->
					lmem#set_val vid (args)
				    | QsNil -> ()
				    | _ -> raise (Qs_func_invalid_argument id)
				 )
			     | QsEList vl->
				 let i=ref 0 in
				   List.iter (fun v->
						(match v with
						   | QsEVal vv->
						       (match vv with
							  | QsVar vid ->
							      let argl=
								(match args with
								   | QsValList al ->
								       (*List.map (
									fun arg->
									 match arg with 
									 | QsVar argv->argv
									 | _ -> raise (Qs_func_invalid_argument id)
									 ) *)al
								  | _ -> raise (Qs_func_invalid_argument id)
								) in
							       lmem#set_val vid  (List.nth argl !i)
								 
							 | _ -> raise (Qs_func_invalid_argument id)
						      );
						  | _ -> raise (Qs_func_invalid_argument id)
						);
						      i:= !i+1;
					     ) vl;

(*				   raise (Qs_func_invalid_argument "list not supported") *)
			     | _ -> raise (Qs_func_invalid_argument id)
			  );
			  
			  self#inst_exec (Some lmem) (inst) 
		     );
    QsUnit   

  method func_exec (n:string) (lmem:qs_mem option) (args:qs_exp)=
    if debug then
      (print_string ("QS:call func "^n);print_newline());
    let (f)=self#get_func n in	 
      f (self#exp_exec lmem args); 
      
  method exp_exec lmem (v:qs_exp)=
    let exp_finalize (v:qs_val)=
      match v with
	| QsVar id-> 
	    (match lmem with
	       | Some lm ->
		   (try
		      lm#get_val id 
		    with
			Qs_val_not_declared did->	   
			  self#get_val id)
	       | None -> 
		   self#get_val id)
	| x -> x in
      
    let rec qs_exec_exp (v:qs_exp)=
      match v with
	| QsEVal v1->exp_finalize v1
	| QsEEgal(v1,v2)->
	    QsBool(bool_of_qs(qs_exec_exp v1)=bool_of_qs(qs_exec_exp v2))
	| QsESup(v1,v2)->
	    QsBool(int_of_qs(qs_exec_exp v1)>int_of_qs(qs_exec_exp v2))
	| QsEInf(v1,v2)->
	    QsBool(int_of_qs(qs_exec_exp v1)<int_of_qs(qs_exec_exp v2))
	| QsESupEgal(v1,v2)->
	    QsBool(int_of_qs(qs_exec_exp v1)>=int_of_qs(qs_exec_exp v2))
	| QsEInfEgal(v1,v2)->
	    QsBool(int_of_qs(qs_exec_exp v1)<=int_of_qs(qs_exec_exp v2))
	| QsEAnd(v1,v2)->
	    QsBool(bool_of_qs(qs_exec_exp v1) && bool_of_qs(qs_exec_exp v2))
	| QsEOr(v1,v2)->
	    QsBool(bool_of_qs(qs_exec_exp v1) || bool_of_qs(qs_exec_exp v2))
	| QsEPlus(v1,v2)->
	    QsInt(int_of_qs(qs_exec_exp v1) + int_of_qs(qs_exec_exp v2))
	| QsEMinus(v1,v2)->
	    QsInt(int_of_qs(qs_exec_exp v1) - int_of_qs(qs_exec_exp v2))
	| QsETimes(v1,v2)->
	    QsInt(int_of_qs(qs_exec_exp v1) * int_of_qs(qs_exec_exp v2))
	| QsEDiv(v1,v2)->
	    QsInt(int_of_qs(qs_exec_exp v1) / int_of_qs(qs_exec_exp v2))
	| QsEConcat(v1,v2)->
	    QsString(string_of_qs(qs_exec_exp v1) ^ string_of_qs(qs_exec_exp v2))
	| QsEList(vl)->
	    QsValList(List.map (fun ve->
			   qs_exec_exp ve
			) vl);

	| _ -> QsNil 
    in
      qs_exec_exp v
	    
  method inst_exec (lmem:qs_mem option) v=
    let rec qs_exec_inst (v:qs_inst)=
      match v with
	| QsGetVal (id)->
	    (match lmem with
	       | Some lm ->
		   (try lm#get_val id 
		   with
		       Qs_val_not_declared did->	   
			 self#get_val id)
	       | None -> self#get_val id)

	| QsSetVal (id,v)->
	    if debug then
              (print_string ("QS:set_val " ^id);print_newline());
	    self#set_val id (self#exp_exec lmem v);QsNil

	| QsSetValInst (id,v)->
	    if debug then
              (print_string ("QS:set_val with func" ^id);print_newline());
	    self#set_val id (self#inst_exec lmem v);QsNil


	| QsInstBlock (vl)->
	    let r=ref QsNil in
              List.iter (
		fun vc->
		  r:=self#inst_exec lmem vc
	      ) vl;
	      !r

	| QsIf (r,ifi,elsei)->if (bool_of_qs (self#exp_exec lmem r)) then self#inst_exec lmem ifi else self#inst_exec lmem elsei
	| QsWhile (r,d)->while (bool_of_qs (self#exp_exec lmem r)) do self#inst_exec lmem d done;QsNil
	| QsFuncRet v->
	    self#exp_exec lmem v
	| QsFuncDecl (n,a,i)->
            if debug then
	      (print_string ("QS:declare func "^n);print_newline());
	    self#func_decl n a i;
	    QsNil
	| QsFunc (n,args)->
	    self#func_exec n lmem args
	| QsUnit -> QsNil
	| QsVal v-> v
	| _ -> QsNil in
      
      qs_exec_inst v




        
  method exec ()=
    let r=ref QsNil in
      while (Stack.is_empty inst_stack==false) do
	let inst=self#pop_inst() in
	  r:=self#inst_exec (None) inst 
      done;!r
		
end;;  

let kernel=new qs_kernel ;;

(* DEFAULT FUNC *)
(* PRINT func *)
let print=( 
  let rec parse_args a=
    (match a with
       | QsInt x -> print_int x;print_newline();
       | QsString x -> print_string x;print_newline();
       | QsValList x->
	   List.iter 
             ( fun ca -> parse_args ca)
             x;
       | _ -> ()) in
    fun args-> parse_args args;QsNil );;

kernel#add_func "print" ( print);; 

let random=( 
  let rec parse_args a=
    (match a with
       | QsInt x -> QsInt (Random.int x)
       | _ -> raise (Qs_func_invalid_argument "random")) in
    fun args-> parse_args args; );;   

kernel#add_func "random" ( random);; 

kernel#add_func "concat" (( 
  let res=ref "" in
  let parse_args a=
    (match a with
       | QsValList x->
	   List.iter 
	     ( fun ca ->
		 match ca with
		   | QsString v -> res:= !res^v
	           | _-> raise (Qs_func_invalid_argument "concat")
	     )
             x;
       | _-> raise (Qs_func_invalid_argument "concat")) in
    fun args-> parse_args args;QsString (!res)));; 
