
open Dl;;
open Qs_types;;

let qs_lib_open libname=
  let lib = dl_open libname in
  let init_symbol = dl_sym lib "qs_lib_init" in
  let qs_lib_init = (call1 init_symbol : unit -> unit) in
  qs_lib_init();
  lib

let qs_lib_func_exists lib name=
  let qs_lib_is_func_symbol = dl_sym lib "qs_lib_is_func" in
  let qs_lib_is_func = (call1 qs_lib_is_func_symbol : string -> bool) in
  qs_lib_is_func name

let qs_lib_func lib name args=
  let qs_lib_call_func_symbol = dl_sym lib "qs_lib_call_func" in
  let qs_lib_call_func = (call2 qs_lib_call_func_symbol : string -> qs_val -> qs_val) in
  qs_lib_call_func name args

let qs_lib_close lib=
  dl_close lib

exception Qs_lib_func_not_found;;

class qs_libs=
  object
    val mutable libs=Hashtbl.create 2
	
    method load l=Hashtbl.add libs l (qs_lib_open l)

    method call fn (args:qs_val)= 
      let found=ref false in
      let r=ref QsNil in
      Hashtbl.iter (fun ln l->
	if (qs_lib_func_exists l fn) then
	  (
	   found:=true;
	   r:=qs_lib_func l fn args 
	       
	  )
			) libs;
      if !found!=true then
	raise Qs_lib_func_not_found;
      !r

    method unload_all()=
      Hashtbl.iter (fun ln l->
	qs_lib_close l
			) libs
	
  end
