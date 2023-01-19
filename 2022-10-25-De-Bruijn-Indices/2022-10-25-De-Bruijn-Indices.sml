(* Q1 *)

datatype DeBruijnExpr = DBIND of int
                      | DBAPP of DeBruijnExpr * DeBruijnExpr
                      | DBABS of DeBruijnExpr

(* Q2 *)

datatype lambdaExpr = VAR of Atom.atom
                    | APP of lambdaExpr * lambdaExpr
                    | ABS of Atom.atom  * lambdaExpr

(*diagA : string -> Atom.atom -> string *)
fun diagA x yA = let
                    val y = Atom.toString(yA)
                 in
                    if String.isPrefix x y then y^"a" else x^"a"
                 end

(* fresh : Atom.atom list -> Atom.atom *)
fun fresh variables = let
                         fun g f (x,y) = f y x
                      in
                         Atom.atom(List.foldl (g diagA) "a" variables)
                      end

(* DeBruijnToLambda : DeBruijnExpr -> lambdaExpr *)
(* Code explaination *)
(*
Jist of code is that at every lambda create a new fresh variable and add it to the list of variables and assign proper variables
to the indices
Cases:
index i : it is the (i-1)th element in the variables list as list is 0 indexed

apply : convert the two expressions to lambda expressions and do lambda application

abstraction : make a fresh variable and add it to the variables list and convert the
expression to lambda expresstion and then do lambda abstraction on the variable and expression
*)
fun DeBruijnToLambda expr = let
                                fun helper (DBIND i) variables             = VAR (List.nth(variables,i-1))
                                  | helper (DBAPP (expr1,expr2)) variables = APP(helper expr1 variables,helper expr2 variables)
                                  | helper (DBABS expr1) variables         = let
                                                                                val freshVariable = fresh variables
                                                                                val newVariablesList = freshVariable::variables
                                                                             in
                                                                                ABS (freshVariable, helper expr1 newVariablesList)
                                                                             end
                             in
                                helper expr []
                             end

fun printer (VAR(x)) = print(Atom.toString(x))
  | printer (APP(x,y)) = let
                           val _ = print( "(" )
                           val _ = printer(x)
                           val _ = print(" ")
                           val _ = printer(y)
                           val _ = print( ")" )
                        in
                           ()
                        end
  | printer (ABS(x,y)) = let
                           val _ = print("(")
                           val _ = print("λ")
                           val _ = print(Atom.toString(x))
                           val _ = print(".")
                           val _ = printer(y)
                           val _ = print(")")
                         in
                           ()
                         end

fun printerdb (DBIND(x)) = print(Int.toString(x))
  | printerdb (DBAPP(x,y)) = let
                           val _ = print( "(" )
                           val _ = printerdb(x)
                           val _ = print(" ")
                           val _ = printerdb(y)
                           val _ = print( ")" )
                        in
                           ()
                        end
  | printerdb (DBABS(x)) = let
                           val _ = print("(")
                           val _ = print("λ")
                           val _ = printerdb(x)
                           val _ = print(")")
                         in
                           ()
                         end

(* Examples *)
(* λ 1 is λa.a *)
val dbe1 = DBABS (DBIND 1)
val le1 = DeBruijnToLambda dbe1
val _ = printerdb(dbe1)
val _ = print(" is ")
val _ = printer le1
val _ = print("\n")

(* λ λ 2 is λa.λaa.(a) *)
val dbe2 = DBABS (DBABS (DBIND 2))
val le2 = DeBruijnToLambda dbe2
val _ = printerdb(dbe2)
val _ = print(" is ")
val _ = printer le2
val _ = print("\n")

(* λ 1 (λ 2) is λa.(a λaa.(a)) *)
val dbe3 = DBABS (DBAPP(DBIND 1,DBABS(DBIND 2)))
val le3 = DeBruijnToLambda dbe3
val _ = printerdb(dbe3)
val _ = print(" is ")
val _ = printer le3
val _ = print("\n")

(* λ 1 1 is λa.(a a) *)
val dbe4 = DBABS (DBAPP(DBIND 1,DBIND 1))
val le4 = DeBruijnToLambda dbe4
val _ = printerdb(dbe4)
val _ = print(" is ")
val _ = printer le4
val _ = print("\n")

(* from wikipedia:
λ (λ 1 (λ 1)) (λ 2 1) is λa.(λaa.(aa λaaaa.aaaa) λaa.(a aa))
*)
val dbe5 = DBABS (DBAPP(DBABS(DBAPP(DBIND 1,DBABS(DBIND 1))),DBABS(DBAPP(DBIND 2,DBIND 1))))
val le5 = DeBruijnToLambda dbe5
val _ = printerdb(dbe5)
val _ = print(" is ")
val _ = printer le5
val _ = print("\n")
