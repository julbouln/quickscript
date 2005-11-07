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

  | "include"      {INCLUDE}
  | "load"      {LOAD}
  | "as"      {AS}

  | "package"      {PACKAGE}

  | "class"      {CLASS}
  | "inherit"        {INHERIT}
  | "->"      {CLASSMEMBER}
  | "new"        {NEW}

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

  | '['            { LBRA }
  | ']'            { RBRA }

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
 
 
  | '$' ['A'-'z' '_' '0'-'9']+ as lxm  { VAL(Str.string_after lxm 1) }
  | '#' ['A'-'z' '_' '0'-'9']+ as lxm  { OVAL(Str.string_after lxm 1) }
  | '@' ['A'-'z' '_' '0'-'9']+ as lxm  { EVAL(Str.string_after lxm 1) }
  | '"' (['A'-'z' '_' ' ' '@' '0'-'9' '/' '\\' '*' '#' '.']+ as lxm) '"' { STRING(lxm) }
  | ['A'-'z' '_' '0'-'9']+ as lxm  { REF(lxm) }

  | eof            { raise Eof } 
