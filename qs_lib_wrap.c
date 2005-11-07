#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

value qs_lib_init(value v)
{
  caml_startup(NULL);
  return Val_unit;
}

value qs_lib_is_func(value fn)
{
  static value * closure = NULL;
  value ret;

  if (closure == NULL) closure = caml_named_value(String_val(fn));
  if (closure != NULL) {
    ret=Val_true;
  }
  else
    {
      ret=Val_false;
    }
  return ret;
}


value qs_lib_call_func(value fn, value v)
{
  static value * closure = NULL;
  value ret;

  if (closure == NULL) closure = caml_named_value(String_val(fn));
  if (closure != NULL) {
    ret=caml_callback(*closure, v);
  }
  else
    {
      printf("Not initialized: call init() first.\n");
    }
  return ret;
}
