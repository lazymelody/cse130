type token =
  | Id of (string)
  | TRUE
  | FALSE
  | Num of (int)
  | LET
  | REC
  | EQ
  | IN
  | FUN
  | ARROW
  | IF
  | THEN
  | ELSE
  | EOF
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LT
  | LE
  | NE
  | AND
  | OR
  | LPAREN
  | RPAREN
  | LBRAC
  | RBRAC
  | SEMI
  | COLONCOLON

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
