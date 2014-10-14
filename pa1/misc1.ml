(* CSE 130: Programming Assignment 1
 * misc.ml
 * Daniel Poplawski
 * A09968967
 *)

(* sumList : int list -> int 
 * takes in a list of ints and sums the values of the 
 * elements.
 * if it is an empty list 1 is returned
 * else the head of the list is added to the recursion of 
 * the rest, or the tail
 *) 

let rec sumList l = 
   match l with
     | [] -> 0
     | (h::t) -> h + (sumList t);;


(* digitsOfInt : int -> int list 
 * takes in an int and returns the digits of the int in a 
 * list of ints.
 * if the int is zero returns the an empty list
 * otherwise recursively calls on n/10 and appends to 
 * a list of n mod 10 ( which gets the lsd )  
 *)

let rec digitsOfInt n = 
   match n with
     | 0 -> []
     | n -> digitsOfInt(n/10)@[n mod 10];;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)

let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)

(* additivePersistence : int -> int
 * takes in an int and returns its additive presistence described above
 * if n is less than 10 then it returns zero, else
 * it sets t to the value of the sum of the digits of the int and 
 * rec calls the function and adds one, returning when done the number
 * of rec calls 
*)

let rec additivePersistence n =  
   if n < 10 then 0 
   else  
      let t = sumList(digits n) in 
         additivePersistence(t) + 1;; 

(* digitalRoot : int -> int
 * takes in an int and gets its digital root which is explained above
 * if n is less than 10 ( base case)  is returns n, else it sets t to the sum of the digits
 *  and rec calls itself on t
 *)    

let rec digitalRoot n = 
   if n < 10 then n
   else
     let t = sumList(digits n) in 
        digitalRoot(t);;

(* listReverse : 'a list -> 'a list
 * takes in a list and returns the reverse of that list
 * if it is an empty list and empty list is returned else,
 * the tali is rec on and appened to the head
 *)

let rec listReverse l = 
   match l with
       [] -> []
     | (h::t) -> (listReverse t)@[h];;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)

let explode s = 
  let rec _exp i = 
     if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool
 * takes in a string and gets checked if it is a palindrome
 * a value x is set to the explode of w ( a list of the chars of the string)
 * and y is set to the revers of x and then x is compared to y
 *)
let palindrome w =
   let x =  explode w in 
      let y = listReverse x in 
         x = y;;




(************** Add Testing Code Here ***************)
