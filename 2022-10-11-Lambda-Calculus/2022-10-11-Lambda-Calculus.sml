(* Q1 *)

datatype expr = Var    of Atom.atom
              | Apply  of expr * expr
              | Lambda of Atom.atom * expr

(* Q2 *)

(* free : expr -> AtomRedBlackSet.set *)
fun free (Var var)             = AtomSet.singleton var
  | free (Apply (expr1,expr2)) = AtomSet.union(free expr1,free expr2)
  | free (Lambda (var,expr))   = AtomSet.subtract(free expr,var)

(* bound : expr -> AtomRedBlackSet.set *)

fun bound (Var var)             = AtomSet.empty
  | bound (Apply (expr1,expr2)) = AtomSet.union(bound expr1,bound expr2)
  | bound (Lambda (var,expr))   = AtomSet.add(bound expr,var)

(* Q3 *)

(* subst : expr -> Atom.atom -> expr -> expr *)

fun subst (Var var2) var1 expr0             = if Atom.same(var1,var2) then expr0 else (Var var2)
  | subst (Apply (expr1, expr2)) var1 expr0 = Apply (subst expr1 var1 expr0, subst expr2 var1 expr0)
  | subst (Lambda (var2, expr1)) var1 expr0 = if Atom.same(var1,var2) then Lambda (var2, expr1) 
                                              else Lambda (var2, subst expr1 var1 expr0)


(* Q4 *)

(* diag : string -> string -> string *)

fun diag x y = if String.isPrefix x y then y^"a" else x^"a"

(* diagA : string -> Atom.atom -> string *)
fun diagA x yA = let
                    val y = Atom.toString(yA)
                 in
                    if String.isPrefix x y then y^"a" else x^"a"
                 end

(* fresh : AtomRedBlackSet.set -> Atom.atom *)

fun fresh variables = let
                         fun g f (x,y) = f y x
                      in
                         Atom.atom( (AtomSet.foldl) (g diagA) "" variables)
                      end

                                                        
