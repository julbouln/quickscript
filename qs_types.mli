type qs_val=
    QsInt of int
  | QsString of string 
  | QsBool of bool
  | QsStruct of (string, qs_val) Hashtbl.t 
  | QsEnum of (qs_val list)

  | QsVar of string
  | QsObject of string
  | QsObjectMember of (string * qs_val)

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
  | QsEEnum of qs_exp list
  | QsEEnumEntry of (string * qs_exp)
;;

type qs_inst=
  | QsError of Lexing.position
  | QsInclude of string

  | QsGetVal of (string)

  | QsSetVal of (string * qs_exp)
  | QsSetValInst of (string * qs_inst)
  | QsSetValObject of (string * qs_exp)

  | QsIf of (qs_exp * qs_inst * qs_inst)
  | QsWhile of (qs_exp * qs_inst)
  | QsFor of (qs_inst * qs_exp * qs_inst * qs_inst)

  | QsFunc of (string * qs_exp)
  | QsFuncDecl of (string * qs_exp * qs_inst)
  | QsFuncRet of qs_exp

  | QsVal of qs_val
  | QsInstBlock of (qs_inst list)
  | QsComment of string

  | QsClassMethod of (string * string * qs_exp)
  | QsClassDecl of (string * qs_inst)
  | QsClassNew of (string * string)
  | QsClassInherit of (string)

  | QsUnit
;;

exception Bad_qs_type
