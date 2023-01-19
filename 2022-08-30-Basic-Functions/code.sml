(* Q1 *)

(* curry: ('a * 'b '*c -> 'd) -> 'a -> 'b -> 'c -> 'd *)
fun curry f a b c = f (a,b,c)
		      
(* uncurry: ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd *)
fun uncurry f (a,b,c) = f a b c

(* Q2 *)
			    
(* fst: 'a * 'b -> 'a *)
fun fst (x,y) = x
		    
(* snd: 'a * 'b -> 'b *)
fun snd (x,y) = y
   
(* Q3 *)
		    
(* length: 'a list -> int *)
fun length [] = 0
  | length (x::xs) = 1 + length(xs)

(* Q4 *)
			       
(* reverseHelper: 'a list -> 'a list -> 'a list *)
fun reverseHelper [] xs = xs
  | reverseHelper (x::xs) ys = reverseHelper xs (x::ys)

(* reverse: 'a list -> 'b list *)
 fun reverse [] = []
  | reverse (x::xs) = reverseHelper (x::xs) []
			     		
(* Q5 *)

(* fiboHelper: int -> int -> int -> int -> int *)
fun fiboHelper from to prev prevPrev = if from = to then prev+prevPrev
				else (fiboHelper (from+1) to (prev+prevPrev) prev)

(* fibo: int -> int *)												   
fun fibo 0 = 0
  | fibo 1 = 1
  | fibo n = fiboHelper 2 n 1 0
