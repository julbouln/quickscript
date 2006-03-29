open Qs_types;;
open Qs_parser;;

(*open Qs_lib;;*)

let debug=false;;

exception Qs_val_not_declared of string
exception Qs_val_not_an_enum of string
exception Qs_class_not_declared of string
exception Qs_func_not_declared of string
exception Qs_func_invalid_argument of string
exception Qs_syntax_error of string

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

  method iter_val (f:string->qs_val->unit)=
    Hashtbl.iter f val_hash
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

  method iter_func (f:string->(qs_val->qs_val)->unit)=
    Hashtbl.iter f func_hash

end;;  

class qs_class=
object(self)
  val mutable mem= new qs_mem
  val mutable funcs=new qs_funcs
  method get_mem=mem
  method get_funcs=funcs

  method inherit_mem (cmem:qs_mem)=
    mem#iter_val (fun id v->
		       cmem#set_val id v
		    );
  method inherit_funcs (cfuncs:qs_funcs)=
    funcs#iter_func (fun id v->
		       cfuncs#set_func id v
		    );

end;;


class qs_objects=
object(self)
  val mutable obj_hash=Hashtbl.create 2
			 
  method add_obj (id:string) (v:qs_class)=
    Hashtbl.add obj_hash id v
  method set_obj (id:string) (v:qs_class)=
      Hashtbl.replace obj_hash id v
  method del_obj (id:string)=Hashtbl.remove obj_hash id
  method get_obj (id:string)=
    if Hashtbl.mem obj_hash id then
      Hashtbl.find obj_hash id
    else 
      raise (Qs_class_not_declared id)
end;;

class qs_classes=
object(self)

  val mutable class_hash=Hashtbl.create 2
			 
  method add_class (id:string) (v:unit->qs_class)=
    Hashtbl.add class_hash id v
  method set_class (id:string) (v:unit->qs_class)=
      Hashtbl.replace class_hash id v
  method del_class (id:string)=Hashtbl.remove class_hash id
  method get_class (id:string)=
    if Hashtbl.mem class_hash id then
      Hashtbl.find class_hash id
    else 
      raise (Qs_class_not_declared id)
end;;


  
class qs_kernel=
object(self)
  inherit qs_mem
  inherit qs_funcs
  inherit qs_classes
  inherit qs_objects

(*  val mutable libs=new qs_libs *)

  val mutable inst_stack=Stack.create()
			   
  method push_inst (i:qs_inst)=Stack.push i inst_stack
  method pop_inst()=Stack.pop inst_stack


  method get_local_mem (lmem:qs_mem option)=
    (match lmem with
       | Some lm ->
	   lm
       | None -> 
	   (self:>qs_mem)
    )

  method get_local_funcs (lfuncs:qs_funcs option)=
    (match lfuncs with
       | Some lf ->
	   lf
       | None -> 
	   (self:>qs_funcs)
    )


  method get_local_val id (lmem:qs_mem option)=
    (match lmem with
       | Some lm ->
	   (try
	      lm#get_val id 
	    with
		Qs_val_not_declared did->	   
		  self#get_val id)
       | None -> 
	   self#get_val id)

  method get_local_func id (lfunc:qs_funcs option)=
    (match lfunc with
       | Some lf ->
	   (try
	      lf#get_func id 
	    with
		Qs_val_not_declared did->	   
		  self#get_func id)
       | None -> 
	   self#get_func id)



  method file_load filename=
    let qs_parse s=
      let lexbuf=Lexing.from_string (s) in
        Qs_parser.block (Qs_lexer.token ) lexbuf in

    let file=open_in filename in
    let buf=Buffer.create 1024 in
      while (
	try
	  Buffer.add_string buf (input_line file);
	  Buffer.add_string buf "\n";
	  true;
	with End_of_file -> false
      ) do () done;

      let inst=qs_parse (Buffer.contents buf) in
	close_in file;
	inst

  method file_exec filename=
    let inst=self#file_load filename in
      self#push_inst(inst);
      self#exec();
      

      
  method class_decl (id:string) (inst:qs_inst)=
    let c()=
      let nc=new qs_class in      
	self#inst_exec (Some nc#get_mem) (Some nc#get_funcs) inst;
	nc
    in
  
      self#add_class id c

  method class_new (id:string) (cl:string)=
    let c=self#get_class cl in
      self#add_obj id (c())
		      
  method func_decl (id:string) (aref:qs_exp) (lfunc:qs_funcs option) (inst)=
    let cf=(match lfunc with
	      | Some lf -> lf
	      | None -> (self:>qs_funcs)) in
    cf#add_func id (
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
			     | QsEEnum vl->
				 let i=ref 0 in
				   List.iter (fun v->
						(match v with
						   | QsEVal vv->
						       (match vv with
							  | QsVar vid ->
							      let argl=
								(match args with
								   | QsEnum al ->
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
			  
			  self#inst_exec (Some lmem) None (inst) 
		     );
    QsUnit   

      

  method func_exec (n:string) (lmem:qs_mem option) (lfunc:qs_funcs option) (args:qs_exp)=
    if debug then
      (print_string ("QS:call func "^n);print_newline());
    let (f)=self#get_local_func n lfunc in	 
      f (self#exp_exec lmem args); 
      
  method exp_exec lmem (v:qs_exp)=
    let rec exp_finalize (v:qs_val)=
      match v with
	| QsVar id-> 
	    self#get_local_val id lmem
	| QsObjectMember(o,m)->
	    let obj=self#get_obj o in
	      exp_finalize m
(*	| QsEnumMember(id,i)->
	    print_string id;print_newline();
	    let e=self#get_local_val id lmem in
	      (match e with
		| QsEnum l->
		    List.nth l (int_of_qs i)
		| _ -> raise (Qs_val_not_an_enum id)
	      )
*)
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
	| QsEEnum(vl)->
	    QsEnum(List.map (fun ve->
			   qs_exec_exp ve
			) vl);
	| QsEEnumEntry(id,i)->
	    let iv=qs_exec_exp i in
	    let e=self#get_local_val id lmem in
	      (match e with
		| QsEnum l->
		    List.nth l (int_of_qs iv)
		| _ -> raise (Qs_val_not_an_enum id)
	      )

	| _ -> QsNil 
    in
      qs_exec_exp v
	    
  method inst_exec (lmem:qs_mem option) (lfunc:qs_funcs option) v=
    let rec qs_exec_inst (v:qs_inst)=
      match v with
	| QsError p->
	    raise (Qs_syntax_error ("Offset "^string_of_int p.Lexing.pos_cnum));
	| QsInclude file->
	    self#inst_exec lmem lfunc (self#file_load file);QsNil

	| QsLoad file->
(*	    libs#load file; *)
	    QsNil

	| QsGetVal (id)->
	    self#get_local_val id lmem
	| QsSetVal (id,v)->
	    if debug then
              (print_string ("QS:set_val " ^id);print_newline());
	    self#set_val id (self#exp_exec lmem v);QsNil

	| QsSetValObject (id,v)->
	    if debug then
              (print_string ("QS:set_val with object " ^id);print_newline());
	    (match (self#exp_exec lmem v) with
	      | QsObject oid->
		  self#set_obj id (self#get_obj oid);
(*		  self#set_val id (self#exp_exec lmem v);*)
		  QsNil
	      | _ -> QsNil
	    )
	| QsSetValInst (id,v)->
	    if debug then
              (print_string ("QS:set_val with func " ^id);print_newline());
	    self#set_val id (self#inst_exec lmem lfunc v);QsNil


	| QsInstBlock (vl)->
	    let r=ref QsNil in
              List.iter (
		fun vc->
		  r:=self#inst_exec lmem lfunc vc
	      ) vl;
	      !r

	| QsIf (r,ifi,elsei)->if (bool_of_qs (self#exp_exec lmem r)) then self#inst_exec lmem lfunc ifi else self#inst_exec lmem lfunc elsei
	| QsWhile (r,d)->while (bool_of_qs (self#exp_exec lmem r)) do self#inst_exec lmem lfunc d done;QsNil
	| QsFor (c1,c2,c3,i)->
	    self#inst_exec lmem lfunc c1;
	    while (bool_of_qs (self#exp_exec lmem c2)) do
	      self#inst_exec lmem lfunc i;
	      self#inst_exec lmem lfunc c3;
	    done;QsNil
(*	    for (self#inst_exec lmem lfunc c1) to (bool_of_qs (self#exp_exec lmem c2)) do self#inst_exec lmem lfunc i done;QsNil *)
	| QsFuncRet v->
	    self#exp_exec lmem v
	| QsFuncDecl (n,a,i)->
            if debug then
	      (print_string ("QS:declare func "^n);print_newline());
	    self#func_decl n a lfunc i;
	    QsNil
	| QsFunc (n,args)->
(*	    (try *)
	      self#func_exec n lmem lfunc args
(*	    with
	      Qs_func_not_declared f-> 	      libs#call n (self#exp_exec lmem args))
*)
	| QsClassDecl(n,i)->
	    self#class_decl n i;
	    QsNil

	| QsIncludeAs (file,cl) ->
	    let inst=self#file_load file in
	      self#class_decl cl inst;
(*	    self#inst_exec lmem lfunc (self#file_load file);*)
	      QsNil

	| QsClassNew(n,c)->
	    self#class_new n c;
	    self#set_val n (QsObject c);
	    QsNil
	| QsClassMethod(o,m,args)->
	    let obj=self#get_obj o in
	      self#func_exec m (Some obj#get_mem) (Some obj#get_funcs) args

	| QsClassInherit(c)->
	    let cl=self#get_class c in
	    let icl=cl() in
	      icl#inherit_mem (self#get_local_mem lmem);
	      icl#inherit_funcs (self#get_local_funcs lfunc);
	      QsNil
(*
	| QsObjectMember(o,m)->
	    let obj=self#get_obj o ni
	      print_string "get member ";print_string m;print_newline();
	      self#get_local_val m (Some obj#get_mem)
*)
	| QsUnit -> QsNil
	| QsVal v-> v
	| _ -> QsNil in
      
      qs_exec_inst v




        
  method exec ()=
    let r=ref QsNil in
      while (Stack.is_empty inst_stack==false) do
	let inst=self#pop_inst() in
	  r:=self#inst_exec (None) None inst 
      done;
(*    libs#unload_all(); *)
    !r
		
end;;  

