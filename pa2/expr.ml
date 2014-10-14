(* Daniel Poplawski
 * A09968967
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* REMEMBER TO DOCUMENT ALL FUNCTIOONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	
  | MyExp1   of expr * expr
  | MyExp2   of expr * expr * expr

(* exprToString : expr -> string
 * this takes in a expr and matches it while recurring on the parts
 * till it is var x or y and the converts to a string
 *
 *)
let rec exprToString e =
  match e with   
  | VarX  -> "x"
  | VarY -> "y"
  | Sine (e)->  "sin(pi*"^(exprToString e)^")"
  | Cosine (e)  ->  "cos(pi*"^(exprToString e)^")"
  | Average (e1, e2) ->  "(("^(exprToString e1)^"+"^(exprToString e2)^")/2)" 
  | Times (e1, e2) -> (exprToString e1)^"*"^(exprToString e2)
  | Thresh (a,b, a_less, b_less) -> "("^(exprToString a)^"<"^(exprToString b)
    ^"?"^(exprToString a_less)^":"^(exprToString b_less)^")" 
  | MyExp1 (e1, e2) -> "(floor( "^(exprToString e1)^"*"^(exprToString e2)^")"
  | MyExp2 (e1, e2, e3) -> "(ceiling( "^(exprToString e1)^"*"^(exprToString e2)
    ^"*"^(exprToString e3)^")"
(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildMyExp1(e1, e2)            = MyExp1(e1, e2)
let buildMyExp2(e1, e2, e3)        = MyExp2(e1, e2, e3) 

let pi = 4.0 *. atan 1.0

(* eval : expr * float * float -> float
 * Takes in a triple (e,x, y) and evaluates e at the point x, y
 * Matches e with the correct type while recursing till the base
 * case of x or y
 *)
let rec eval (e,x,y) =  
   match e with
   | VarX -> x
   | VarY -> y
   | Sine(e1)  -> sin(pi *. eval(e1,x,y))
   | Cosine(e1) -> cos(pi *. eval(e1,x ,y ))
   | Average(e1,e2) -> ( ( (eval(e1, x, y)) +. (eval(e2, x, y)) ) /. 2.0 )
   | Times(e1, e2) -> ((eval(e1, x, y)) *. (eval(e2, x, y)))
   | Thresh(a,b,a_less, b_less) -> if ((eval (a, x,  y)) < ((eval (b, x, y))))
     then eval(a_less, x, y) else eval(b_less, x, y)
   | MyExp1(e1,e2) -> (floor ((eval(e1, x, y)) *. (eval(e2, x, y))))
   | MyExp2(e1, e2, e3) ->  (ceil ((eval (e1, x, y)) *. (eval (e2, x, y)) 
     *. (eval (e3, x, y))))
(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
