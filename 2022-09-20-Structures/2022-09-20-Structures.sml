 signature SORT = sig
 	type t
 	val sort : t list -> t list
 end

 signature ORD_KEY =
 sig
 	type ord_key
     (* abstract type of keys that have a total order *)

 	val compare : ord_key * ord_key -> order
     (* comparison function for the ordering on keys *)

 end (* ORD_KEY *)

(* Q1 *)
 functor QSort ( O : ORD_KEY ) : SORT = struct
 	type t = O.ord_key
    fun sort [] = []
      | sort (x::xs) =
            let
                fun f x y = O.compare(x,y) = GREATER
                val (left, right) = List.partition (f x) xs
            in
                sort left @ [x] @ sort right
            end

 end

(* Q2 *)
 structure IntOrd : ORD_KEY = struct
    type ord_key = int
    val compare = Int.compare
 end

 structure sortInt = QSort (IntOrd)
 val lst = [4,3,2,5,6,~1,3,4]
 val sortedlst = sortInt.sort(lst)
