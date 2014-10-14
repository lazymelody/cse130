{
  (*Daniel Poplawski
    A09968967 *)
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
    eof         { EOF }
  | [' ' '\t' '\n' '\r']    { token lexbuf }
  | "true" 	{ TRUE } 
  | "false"	{ FALSE } 
  | "let" 	{ LET }
  | "rec" 	{ REC }
  | "=" 	{ EQ } 
  | "in" 	{ IN } 
  | "fun" 	{ FUN } 
  | "->"	{ ARROW } 
  | "if" 	{ IF }
  | "then"	{ THEN } 
  | "+" 	{ PLUS }
  | "-" 	{ MINUS }
  | "*" 	{ MUL }
  | "/"		{ DIV }
  | "<" 	{ LT }
  | "<=" 	{ LE }
  | "!="	{ NE }	
  | "&&" 	{ AND }
  | "||"	{ OR }
  | "else" 	{ ELSE } 
  | "("		{ LPAREN }
  | ")"		{ RPAREN }
  | ";"		{ SEMI }
  | "::"	{ COLONCOLON } 
  | "[" 	{ LBRAC }
  | "]" 	{ RBRAC } 
  | ['0' - '9']+ as n { Num (int_of_string n ) } 
  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str
                       { Id(str) }

  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }





