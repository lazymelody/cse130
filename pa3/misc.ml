(* 
 * Daniel Poplawski
 * A09968967
 * CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)
(*  sqsum : int list -> int
 *  Takes in a list of integers and returns
 *  the sum of the square of each element
 *  uses fold left with fun f that squares the element
 *  and adds to acc till 0
 *)
let sqsum xs = 
  let f a x = a + x * x in
  let base = 0 in 
    List.fold_left f base xs

(* pipe : ('a -> 'a) list -> ('a -> 'a)
 * takes in a list of functions and a int x
 * and applies those functions to the value
 * by folding to the left till it reaches the
 * last function
 *)
let pipe fs = 
  let f a x = fun y -> x (a y) in
  let base = fun y -> y in
    List.fold_left f base fs

(* sepConcat : string -> string list -> -> string
 * a curried function that take in a string and
 * a list of strings and inserts the first string in
 * between each of the strings in the list and returns
 * them as a list
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l

(* stringOfList : ('a -> string) -> 'a list -> string
 * takes in a function and list, uses the function
 * sepConcat and List.map to convert the list to strings
 *)
let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]";;

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(*  clone : 'a -> int -> 'a list
 *  takes in a value x and an int n and
 *  returns a list of n x's
 *)
let rec clone x n = 
   if n <= 0 then []
   else x::(clone x (n-1))

(*  padZero : int list -> int list -> int list * int list
 *  takes in two list and adds zeros to the shorter
 *  one till they are the same length
 *)
let rec padZero l1 l2 = 
   let dif = List.length l1 - List.length l2 in
   if List.length l1 < List.length l2 then
   clone 0 (abs (dif))@ l1, l2
   else 
   if List.length l1 > List.length l2 then 
   l1, clone 0 (abs (dif))@l2
   else
   (l1, l2);;

(* removeZero : int list -> int list 
 * takes in a list and removes any leading
 * zeros in the list
 *)
let rec removeZero l = 
   match l with 
   | [] -> []
   | h::t -> if h = 0 then removeZero t 
             else l

(* bigAdd : int list -> int list -> int list
 * takes in two list with int in the range of 0-9
 * and returns a list of int that represent the 
 * addition of the two list as though the were one 
 * big single int
 *)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x =
      let (carry, sum) = a in
      let (x1, x2) = x in
      let t = x1 + x2 + carry in
      (t / 10, (t mod 10)::sum) in
    let base = (0,[]) in 
    let args = List.combine (List.rev ( 0::l1)) (List.rev (0::l2)) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* mulByDigit : int -> int list -> int list
 * takes in an integer and a big interger
 * and returns the big int result of multipling the two
 *)
let rec mulByDigit i l =
  if i <= 0 then [] 
  else if i = 1 then l
  else bigAdd( bigAdd l l )(mulByDigit (i-2) l);;

(*  bigMul : int list -> int list -> int list 
 *  takes in two list that are big ints and returns
 *  the list, big int that is the result of multi the two
 *)
let bigMul l1 l2 = 
  let f a x =   
    let (shift, sum) = a in
    let (fact, dig) = x in
    let product = mulByDigit dig (fact @ (clone 0 shift)) in
    (shift + 1, bigAdd sum product) in
  let base = (0, [])  in
  let args = List.combine (clone l1 (List.length l2)) (List.rev l2) in
  let (_, res) = List.fold_left f base args in
    res;;      

