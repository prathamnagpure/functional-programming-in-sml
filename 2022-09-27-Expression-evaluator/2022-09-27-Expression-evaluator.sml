(* Q1 *)

(* datatype Expr *)
datatype Expr = Const of real
              | Var   of string
              | Plus  of Expr * Expr
              | Mul   of Expr * Expr

(* datatype Stmt *)
datatype Stmt = Assign of string * Expr
              | Print  of Expr

(* Program *)
type Program = Stmt list


(* Q2 *)

(* Env *)
type Env = real AtomMap.map

(* eval : Env -> Expr -> real option *)
fun eval _   (Const x)              = SOME x
  | eval env (Var x)                = AtomMap.find (env, Atom.atom x)
  | eval env (Plus (expr1, expr2))  = (case (eval env expr1) of
                                        NONE          => NONE
                                      | (SOME value1) =>  case (eval env expr2) of
                                                            NONE        => NONE
                                                          | (SOME value2) => SOME (value1 + value2))
  | eval env (Mul (expr1, expr2))   = (case (eval env expr1) of
                                        NONE          => NONE
                                      | (SOME value1) =>  case (eval env expr2) of
                                                            NONE        => NONE
                                                          | (SOME value2) => SOME (value1 * value2))          

(* execute : Env -> Stmt -> Env *)
fun execute env (Assign(varName,expr)) = (case (eval env expr) of
                                          NONE       => env
                                        | SOME value => AtomMap.insert(env,Atom.atom varName,value))
  | execute env (Print(expr))          = (case (eval env expr) of
                                          NONE       => (print "NONE";env)
                                        | SOME value => (print(Real.toString value);print("\n");env))

(* interpret : Program -> unit *)
fun interpret [] = ()
  | interpret program =((let
                         (* Stmt * Env -> Env *)
                          fun f (stm,env) = execute env stm
                        in
                          List.foldl f AtomMap.empty program
                        end); ())

(* Example *)
val main = interpret 
    [
        Assign("a",Const(4.0)),
        Assign("a",Plus(Var "a",Var "a")),
        Print(Var "a"),
        Assign("b",Plus(Const 1.5, Const 2.0)),
        Print(Mul(Var "a",Var "b"))
    ]