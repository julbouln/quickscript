{
  open Qs_parser        (* The type token is defined in parser.mli *)

  exception Eof
}
rule token = parse 
    [' ' '\t' '\n']      { token lexbuf }     (* skip blanks *) 
  | '\n'            { EOL } 
   
  | "nil"           { NIL }
 
  | "begin"         { BEGIN }
  | "end"           { END }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "while"         { WHILE }
  | "for"           {FOR}
  | "function"      {FUNCDEC}
  | "return"      {FUNCRET}

  | "true"          { BOOL(true) }
  | "false"         { BOOL(false) }
  
  
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) } 
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }

  | '{'            { LAC }
  | '}'            { RAC }
  | '='            { EGAL }
  | '>'            { SUP }
  | '<'            { INF }

  | ">="            { SUPEGAL }
  | "<="            { INFEGAL }

  | '&'            { AND }
  | '|'            { OR }

  | "//" [^ ';']*  { COMMENT(Lexing.lexeme lexbuf) }

  | "unit"         { UNIT }
  | ',' 	   {  CSEP }
  | ';' 	   {  SEP }


  | '.' 	   {  LSEP }
 
  | ":="            { STRUCTEGAL }
  | '#'            {STRUCTSUB }

 
  | ['A'-'z' '_' '0'-'9']+ as lxm  { VAL(lxm) }
  | ['A'-'z' '_' '0'-'9']+ '(' as lxm  { FUNC(lxm) }
  | '"' (['A'-'z' '_' ' ' '@' '0'-'9']+ as lxm) '"' { STRING(lxm) }

(*  | (['A'-'z']+ as id) '#' (['A' - 'z']+ as lxm) { INT(f id lxm) } *)
  | eof            { raise Eof } 
