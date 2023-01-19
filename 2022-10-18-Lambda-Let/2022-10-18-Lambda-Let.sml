(* Q1 *)

datatype lambda_let_exp = VAR_LET   of Atom.atom
                        | APPLY_LET of lambda_let_exp * lambda_let_exp
                        | ABS_LET   of Atom.atom      * lambda_let_exp
                        | LET       of Atom.atom      * lambda_let_exp * lambda_let_exp


datatype lambda_letrec_exp = VAR_LETREC   of Atom.atom
                           | APPLY_LETREC of lambda_letrec_exp * lambda_letrec_exp
                           | ABS_LETREC   of Atom.atom         * lambda_letrec_exp
                           | LETREC       of Atom.atom         * lambda_letrec_exp * lambda_letrec_exp

(* Q2 *)

datatype lambda_exp = VAR   of Atom.atom
                    | APPLY of lambda_exp * lambda_exp  
                    | ABS   of Atom.atom  * lambda_exp


(* remove_let : lambda_let_exp -> lambda_exp *)
fun remove_let (VAR_LET x)              = VAR x
  | remove_let (APPLY_LET (exp1, exp2)) = APPLY (remove_let exp1, remove_let exp2)
  | remove_let (ABS_LET (x, exp1))      = ABS (x, remove_let exp1)
  | remove_let (LET (x, exp1, exp2))    = APPLY (ABS (x, remove_let exp2), remove_let exp1)
