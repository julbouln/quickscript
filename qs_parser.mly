%{
  open Qs_types;;


let qs_exp_list_concat l1 l2=
  match l1 with
    | QsEEnum v-> 
        (match l2 with
	   | QsEEnum w-> QsEEnum (List.append v w)
           | _ -> QsEEnum (List.append v [l2]))
    | _ -> 
        (match l2 with
	   | QsEEnum w-> QsEEnum (List.append [l1] w)
           | _ -> QsEEnum (List.append [l1] [l2]));;

let qs_val_list_concat l1 l2=
  match l1 with
    | QsEnum v-> 
        (match l2 with
	   | QsEnum w-> QsEnum (List.append v w)
           | _ -> QsEnum (List.append v [l2]))
    | _ -> 
        (match l2 with
	   | QsEnum w-> QsEnum (List.append [l1] w)
           | _ -> QsEnum (List.append [l1] [l2]));;


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

%}

%token <int> INT
%token <string> STRING

%token <string> VAL
%token <string> OVAL
%token <string> EVAL

%token <string> FUNC
%token <string> REF

%token NIL

%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token LAC RAC
%token LBRA RBRA
%token EOL

%token BEGIN END

%token <string> COMMENT

%token UNIT

%token EGAL

%token <bool> BOOL

%token SUP
%token INF
%token SUPEGAL
%token INFEGAL
%token IF
%token THEN
%token ELSE
%token WHILE
%token FOR

%token AND
%token OR

%token SEP
%token CSEP
%token LSEP

%token FUNCDEC
%token FUNCRET

%token INCLUDE
%token AS
%token PACKAGE

%token CLASS
%token INHERIT
%token CLASSMEMBER
%token NEW

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */


%start block             /* the entry point */
%type <Qs_types.qs_inst> block


%%

block:
| BEGIN inst END  { $2 }
| PACKAGE LAC inst RAC  { $3 }
;

inst:
| error                   { QsError(Parsing.symbol_start_pos()) }
| COMMENT                 { QsComment($1) }
| LPAREN inst RPAREN      { $2 }

| inst SEP {$1}
| inst SEP inst { qs_inst_block_concat $1 $3 }
| LAC inst RAC { $2 }

| LAC inst RAC inst { qs_inst_block_concat $2 $4 }

| COMMENT inst { QsUnit }

| INCLUDE LPAREN STRING RPAREN {QsInclude($3)}
| INCLUDE LPAREN STRING RPAREN AS REF {QsIncludeAs($3,$6)}

| VAL EGAL func exp RPAREN {QsSetValInst($1, (QsFunc($3,$4)))}
| VAL EGAL func RPAREN {QsSetValInst($1, (QsFunc($3,QsEVal(QsNil))))}

| VAL EGAL inst {QsSetValInst($1, $3)}
| VAL EGAL exp { QsSetVal($1, $3)}

| EVAL EGAL exp { QsSetVal($1, $3)}
| OVAL EGAL exp { QsSetValObject($1, $3)}

| VAL PLUS PLUS {QsSetVal($1,QsEPlus(QsEVal(QsVar($1)),QsEVal(QsInt(1))))}

| FOR LPAREN inst SEP exp SEP inst RPAREN LAC inst RAC {QsFor($3,$5,$7,$10)}


| WHILE LPAREN exp RPAREN LAC inst RAC { QsWhile($3,$6) }

| IF LPAREN exp RPAREN LAC inst  RAC ELSE LAC inst  RAC { (QsIf($3,$6,$10))}
| IF LPAREN exp RPAREN LAC inst RAC { (QsIf($3,$6,QsUnit))}

| FUNCRET exp { QsFuncRet($2)  }
| FUNCDEC func RPAREN LAC inst RAC { QsFuncDecl($2,QsEVal(QsNil),$5) }
| FUNCDEC func exp RPAREN LAC inst RAC { QsFuncDecl($2,$3,$6) }
| func exp RPAREN { QsFunc($1,$2)}
| func RPAREN { QsFunc($1,QsEVal(QsNil))}

| CLASS REF LAC inst RAC {QsClassDecl($2,$4)}
| OVAL EGAL NEW REF {QsClassNew($1,$4)}
| OVAL CLASSMEMBER func exp RPAREN { QsClassMethod($1,$3,$4) }
| OVAL CLASSMEMBER func RPAREN { QsClassMethod($1,$3,QsEVal(QsNil)) }
| INHERIT REF {QsClassInherit($2)}

| UNIT { QsUnit }



;

func:
| REF LPAREN { $1 }

exp:
| exp_enum      { $1 }
| exp_bool      { $1 }
| exp_int       { $1 }
| exp_string    { $1 }
| exp_val       { $1 }
| exp CSEP exp { (qs_exp_list_concat $1 $3)}
| NIL           { QsEVal(QsNil) }
| LPAREN exp RPAREN      { $2 }


exp_enum:
| EVAL LSEP LPAREN exp_int RPAREN {QsEEnumEntry($1,$4)}
/*| EVAL LBRA exp_int RBRA {QsEEnumEntry($1,$3)}*/

exp_bool:
BOOL			 { QsEVal(QsBool($1)) }
| LPAREN exp_bool RPAREN      { $2 }
| exp_bool AND exp_bool    { QsEAnd($1,$3) }
| exp_bool OR exp_bool     { QsEOr($1, $3) }
| exp_int EGAL exp_int { QsEEgal($1,$3) }
| exp_int SUP exp_int { QsESup($1,$3) }
| exp_int INF exp_int { QsEInf($1,$3) }
| exp_int SUPEGAL exp_int { QsESupEgal($1,$3) }
| exp_int INFEGAL exp_int { QsEInfEgal($1,$3) }
| exp_string EGAL exp_string { QsEEgal($1,$3) }

| exp_val     {$1}
;


exp_int:  
  INT                        {  QsEVal(QsInt($1)) }
| LPAREN exp_int RPAREN      { $2 }
| exp_int PLUS exp_int          { QsEPlus($1 ,$3) }
| exp_int MINUS exp_int         { QsEMinus($1, $3) }
| exp_int TIMES exp_int         { QsETimes($1 ,$3) }
| exp_int DIV exp_int           { QsEDiv($1, $3) }
/*| MINUS exp_int %prec UMINUS { QsEMinus(QsEVal(QsInt 0)), - $2) }*/

|  exp_val     {$1}
;

exp_string:
  STRING                   { QsEVal(QsString($1)) }  
| exp_string LSEP exp_string      { QsEConcat($1,$3)}
|  exp_val     {$1}
;

exp_val:
/*| VAL { QsEVal(QsVar(Str.string_after $1 1)) } */
| vval    { QsEVal($1) }


vval:
| VAL { QsVar($1) } 
| OVAL { QsObject($1) } 
| OVAL CLASSMEMBER vval {(QsObjectMember($1,$3))}
| OVAL LSEP vval {(QsObjectMember($1,$3))}
