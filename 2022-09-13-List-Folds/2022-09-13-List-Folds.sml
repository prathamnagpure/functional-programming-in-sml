(* Q1 *)

(* foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
fun foldr sfun s [] = s
  | foldr sfun s (x::xs) = sfun(x,foldr sfun s xs)

(* foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
fun foldl sfun s [] = s
  | foldl sfun s (x::xs) = foldl sfun (sfun(x,s)) xs

(* Q2 *)

(* sum : int list -> int *)
fun sum list0 = foldl op + 0 list0
						       

(* Q3 *)

(* partition : ('a -> bool) -> 'a list -> 'a list * 'a list *)
fun partition f list0 = 
			let
				(* sfun : ('a -> bool) -> 'a * ('a list * 'a list) -> 'a list * 'a list *)
				fun sfun f (ele,(list1,list2)) = if (f ele = true) then ((ele::list1),list2) else (list1,(ele::list2))
				in  				
					foldr (sfun f) ([],[]) list0
				end


(* map : (a' -> 'b) -> 'a list -> 'b list *)
fun map f list0 =
		let
			(* sfun : a' -> b' list -> b' list *)
			fun sfun f (ele,list0) = (f ele)::list0
		in 
			foldr (sfun f) [] list0
		end
  
(* reverse : a' list -> 'a list *)
fun reverse list0 =
		let
			(* sfun : a' * a' list -> a' list *)
			fun sfun(a,list0) = a::list0
		in 
			foldl sfun [] list0
		end

datatype 'a option = None | Some of 'a

(* nth : 'a list * int -> 'a option *)
fun nth (lst,n) = 
		let
			datatype 'a Find = LookingFor of int | Found of 'a
			(* nthAux : 'a list * int -> 'a Find *)
			fun nthAux (lst,n) = 
						let
							(*  sfun : 'a * 'a Find -> 'a Find *)
							fun sfun (ele,LookingFor(0)) = Found(ele)
						  	  | sfun (ele,LookingFor(n)) = LookingFor(n-1)
						  	  | sfun (ele,x) = x
						in
							foldl sfun (LookingFor(n)) lst
					   	end
		in
			case (nthAux (lst,n)) of
	                        Found(ele) => Some(ele)
	                        | x => None
		end


			      
