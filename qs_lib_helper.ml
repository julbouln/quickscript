let qs_lib_register l=
  List.iter (fun (n,f)->
    Callback.register n f
	    ) l

