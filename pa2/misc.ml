(* Daniel Poplawski
 * A09968967
 * CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)
(* assoc : int * string * (string * int) list -> int  
 * Assc takes in three values d, k, l. d is a default value 
 * if k is not found. L is a list of pairs and the func looks for a match 
 * for k in the list of l and returns the other value in the pair. Looks for
 * a string and returns the int paired with it
 *)
let rec assoc (d,k,l) = 
    match l with
    | [] -> d
    | ((head_key, head_value)::t) ->
      if head_key = k then head_value 
      else assoc (d, k, t);;

(* removeDuplicates : int list -> int list
 * removeDuplicates take in a list and goes through the list
 * looking for any duplicates and removing them.
 * It uses List.mem to check if the head of the list has been seen and if 
 * if it has not it addes to the list seen, or it it has been seen it skip
 * it. After recursing only unique elements are left and the new list is reversed 
 * and returned.
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' =  if List.mem h seen then seen else h::seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))

(* wwhile : (int -> int * bool) * int -> int
 * wwhile takes in a f and b, and calls f on b to get a pair b and c
 * and continus to call f on b while c is true. I created a new func that
 * had total and bol and continued adding on total till bol is false then
 * I returned total
 *)
let rec wwhile (f,b) = let (total, bol ) = f b in
       if bol then wwhile(f, total) else total;;
(* fixpoint : (int -> int) * int -> int
 * fixpoint takes in two inputs f and b, and keeps updating b on f(b)
 * till f(b) = b . It uses wwhile and passes in a fun x and updates b
 * till b != x is returned false
 *)
let fixpoint (f,b) = wwhile (( fun helper -> let b = (f helper) in 
   (b, b != helper)), b);;


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
