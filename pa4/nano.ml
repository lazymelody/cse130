(* Daniel Poplawski A09968967 *)
exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* lookup: string * env -> value
 * finds the most recent binding of for a variable and
 * in the list representing the enviroment
 *)
let lookup (x, evn) = 
  let y = listAssoc (x,evn) in
  match y with 
  | Some x -> x
  | None -> raise (MLFailure("Variable not bound: " ^ x))

(* eval : env * expr -> value
 * evaluates e in the enviroment evn 
 * raises expection for an unbound type
 *)
let rec eval (evn,e) = 
  match e with
  | Bin ( e1, oper, e2) ->
     begin match ( (eval( evn, e1)), oper, (eval(evn, e2))) with
     | Int e1, Minus, Int e2 -> Int (e1 - e2) 
     | Int e1, Plus, Int e2 -> Int (e1 + e2) 
     | Int e1, Div, Int e2 -> Int (e1 / e2)
     | Int e1, Mul, Int e2 -> Int(e1 * e2)
     | Int e1, Ne, Int e2 -> Bool (e1 != e2)
     | Int e1, Eq, Int e2 -> Bool (e1 = e2)
     | Int e1, Lt, Int e2 -> Bool (e1 < e2)
     | Int e1, Le, Int e2 -> Bool (e1 <= e2)
     | Bool e1, And, Bool e2 -> Bool (e1 && e2)
     | Bool e1, Or, Bool e2 -> Bool (e1 || e2)
     | Bool e1, Ne, Bool e2 -> Bool (e1 != e2)
     | Bool e1, Eq, Bool e2 -> Bool (e1 = e2)
     | _-> raise (MLFailure ("not valid"))
     end
  | Const i -> Int i
  | Var x -> lookup (x, evn) 
  | True -> Bool true
  | False -> Bool false
  | If (e1, e2, e3) ->
      let Bool condition = eval (evn, e1) in
      if condition then eval (evn, e2) else eval (evn, e3) 
  | Let (x, e1, e2) -> eval ((x, eval(evn, e1))::evn, e2)
  | Letrec(x, e1, e2) -> eval ((x, eval(evn, e1))::evn, e2) 
  | App(e1, e2) ->
            let Closure (evn2, n, x, e) = eval (evn, e1) in
            eval ((x, eval (evn, e2))::evn2, e)
  | Fun(x,e) -> Closure(evn, None, x, e)
  | _ -> raise (MLFailure ("not valid" ))

(*********  test  Code  ******************************)
