%{
  open Qs_types;;
  open Qs_func;;

%}

%token <int> INT
%token <string> STRING
%token <string> VAL
%token <string> FUNC

%token NIL

%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token LAC RAC
%token EOL

%token BEGIN END

%token <string> COMMENT

%token UNIT

%token STRUCTSUB
%token STRUCTEGAL

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

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */


%start block             /* the entry point */
%type <Qs_types.qs_inst> block


%%

block:
| BEGIN inst END  { kernel#push_inst($2);kernel#exec();QsUnit }
;

inst:
| COMMENT                 { QsComment($1) }
| LPAREN inst RPAREN      { $2 }
| inst SEP {$1}
| inst SEP inst { qs_inst_block_concat $1 $3 }
| LAC inst RAC { $2 }

| COMMENT inst { QsUnit }

/*| VAL EGAL STRUCTSUB  { (QsAddVal($1, QsEVal(Qs_types.QsStruct (Hashtbl.create 2)))); }*/
/*| VAL STRUCTSUB VAL EGAL exp { let r=kernel#exec() in add_val_struct (kernel#get_val $1) $3 $5;QsUnit}*/

| VAL EGAL func exp RPAREN {QsSetValInst($1, (QsFunc($3,$4)))}
| VAL EGAL func RPAREN {QsSetValInst($1, (QsFunc($3,QsEVal(QsNil))))}

| VAL EGAL inst {QsSetValInst($1, $3)}
| VAL EGAL exp { QsSetVal($1, $3)}

/*| FOR LPAREN inst SEP exp SEP exp RPAREN LAC inst RAC {
    $10
  }
*/

| WHILE LPAREN exp RPAREN LAC inst RAC { QsWhile($3,$6) }

| IF LPAREN exp RPAREN LAC inst RAC ELSE LAC inst RAC { (QsIf($3,$6,$10))}
| IF LPAREN exp RPAREN LAC inst RAC { (QsIf($3,$6,QsUnit))}

| FUNCRET exp { QsFuncRet($2)  }
| FUNCDEC func RPAREN LAC inst RAC { QsFuncDecl($2,QsEVal(QsNil),$5) }
| FUNCDEC func exp RPAREN LAC inst RAC { QsFuncDecl($2,$3,$6) }
| func exp RPAREN { QsFunc($1,$2)}
| func RPAREN { QsFunc($1,QsEVal(QsNil))}


| UNIT { QsUnit }

;

func:
| FUNC { Str.string_before $1 (String.length $1 - 1) } 

exp:
| exp_bool      { $1 }
| exp_int       { $1 }
| exp_string    { $1 }
| exp_val       { $1 }
| exp CSEP exp { (qs_exp_list_concat $1 $3)}
| NIL           { QsEVal(QsNil) }
| LPAREN exp RPAREN      { $2 }

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
| VAL    { QsEVal(QsVar($1)) }
