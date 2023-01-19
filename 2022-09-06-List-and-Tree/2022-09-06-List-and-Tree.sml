(* Q1 *)

(* map : ('a -> 'b) -> 'a list -> 'b list *)
fun map f [] = []
  | map f (x::xs) = f x :: map f xs 

(* Q2 *)
			       
datatype 'a tree = nulltree
		 | node of 'a tree * 'a * 'a tree

(* Q3 *)
					     
(* treemap: ('a -> 'b) -> 'a tree -> 'b tree *)

fun treemap  f nulltree = nulltree
  | treemap  f (node(lefttree,root,righttree)) = node(treemap f lefttree,f root,treemap f righttree)

(* Q4 *)
						     
(* inorder 'a tree -> 'a list *)
fun inorder nulltree = []
  | inorder (node(lefttree,root,righttree)) = inorder lefttree @ root::inorder righttree

(* preorder 'a tree -> 'a list *)
fun preorder nulltree = []
  | preorder (node(lefttree,root,righttree)) = root::preorder lefttree @ preorder righttree

(* postorder 'a tree -> 'a list *)
fun postorder nulltree = []
  | postorder (node(lefttree,root,righttree))= postorder lefttree @ postorder righttree @ root::nil

(* Q5 *)

(* 'a tree -> 'a tree*)
fun rotate_cw (node(node(t1,b,t2),a,t3)) = node(t1,b,node(t2,a,t3))
  | rotate_cw x = x												    

												    
												    
