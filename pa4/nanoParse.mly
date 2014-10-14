%{
(* Daniel Poplawski 
 * A09968967 *)
open Nano 
%}

%token <string> Id
%token TRUE
%token FALSE
%token <int> Num
%token LET
%token REC
%token EQ 
%token IN
%token FUN
%token ARROW
%token IF
%token THEN
%token ELSE
%token EOF
%token PLUS
%token MINUS
%token MUL
%token DIV
%token LT
%token LE
%token NE
%token AND
%token OR
%token LPAREN
%token RPAREN
%token LBRAC
%token RBRAC
%token SEMI
%token COLONCOLON

%start exp 
%type <Nano.expr> exp

%%

exp:
    | LET Id EQ exp IN exp      { Let($2,$4,$6) }
    | LET REC Id EQ exp IN exp  { Letrec($3,$5,$7) }
    | FUN Id ARROW exp          { Fun($2,$4) }
    | IF exp THEN exp ELSE exp  { If($2,$4,$6) }
    | exp1                      { $1 }

exp1:  
   | exp1 OR exp2 		{ Bin( $1, Or, $3 ) }
   | exp2			{ $1 }

exp2: 
   | exp2 AND exp3		{ Bin( $1, And, $3 ) }
   | exp3			{ $1 }

exp3:
   | exp3 EQ exp4		{ Bin( $1, Eq, $3) }
   | exp3 LE exp4		{ Bin( $1, Le, $3 ) }
   | exp3 LT exp4		{ Bin( $1, Lt, $3 ) }
   | exp3 NE exp4		{ Bin( $1, Ne, $3 ) } 
   | exp4			{ $1 }


 exp4:
      exp7 COLONCOLON exp4      { Bin($1, Cons, $3) }
    | exp7 SEMI exp4            { Bin($1, Cons, $3) }
    | exp4 RBRAC                { Bin($1, Cons, NilExpr) }
    | LBRAC exp4                { $2 }
    | exp7			{ $1 } 

exp7:
   | exp7 MINUS exp5		{ Bin( $1, Minus, $3 ) }
   | exp7 PLUS exp5		{ Bin( $1, Plus, $3 ) }
   | exp5			{ $1 }

exp5: 
   | exp5 DIV exp6		{ Bin( $1, Div, $3 ) }
   | exp5 MUL exp6		{ Bin( $1, Mul, $3 ) }
   | exp6			 {$1 } 

exp6:
   | exp6 expEnd                { App( $1, $2 ) }
   | expEnd			{ $1 } 

expEnd: 
    | Num                       { Const( $1) }
    | Id		  	{ Var( $1) }
    | TRUE			{ True } 
    | FALSE			{ False }
    | LPAREN exp RPAREN		{ $2 } 
    | LBRAC RBRAC		{ NilExpr }

