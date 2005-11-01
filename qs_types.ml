type qs_val=
    QsInt of int
  | QsString of string 
  | QsBool of bool
  | QsStruct of (string, qs_val) Hashtbl.t 
  | QsValList of (qs_val list)
  | QsVar of string
  | QsNil
;;


type qs_exp=
    QsEVal of qs_val
  | QsEEgal of qs_exp * qs_exp
  | QsESup of qs_exp * qs_exp
  | QsEInf of qs_exp * qs_exp
  | QsESupEgal of qs_exp * qs_exp
  | QsEInfEgal of qs_exp * qs_exp
  | QsEAnd of qs_exp * qs_exp
  | QsEOr of qs_exp * qs_exp
  | QsEPlus of qs_exp * qs_exp
  | QsEMinus of qs_exp * qs_exp
  | QsETimes of qs_exp * qs_exp
  | QsEDiv of qs_exp * qs_exp
  | QsEConcat of qs_exp * qs_exp
  | QsEList of qs_exp list
;;

type qs_inst=
  | QsGetVal of (string)
  | QsSetVal of (string * qs_exp)
  | QsSetValInst of (string * qs_inst)
  | QsIf of (qs_exp * qs_inst * qs_inst)
  | QsFunc of (string * qs_exp)
  | QsFuncDecl of (string * qs_exp * qs_inst)
  | QsFuncRet of qs_exp
  | QsWhile of (qs_exp * qs_inst)
  | QsVal of qs_val
  | QsInstBlock of (qs_inst list)
  | QsComment of string
  | QsUnit
;;

exception Bad_qs_type
