signature SIGNATURE = sig
  type symbol
  val compare : symbol * symbol -> order
  val arity : symbol -> int
end

structure TypeSig : SIGNATURE = struct
    
  datatype symbol = INT
                  | BOOL
                  | Arrow

    fun arity INT = 0
      | arity BOOL = 0
      | arity ARROW = 2

    fun toInt INT = 0
      | toInt BOOL = 1
      | toInt ARROW = 2

    fun compare (s1, s2) = Int.compare(toInt s1, toInt s2)

    fun comp (INT, INT) = EQUAL
      | comp (INT, _  ) = LESS
   
end

   
signature VAR = sig
    type var
    val fresh : unit -> var
    val user : string -> var
    val toString : var -> string
    val compare : var * var -> order
end
   
   

functor Unify (S : SIGNATURE) = struct
  datatype term = Var of Atom.atom
                | Apply of S.symbol * term list

  type telescope = term AtomMap.map
  type equation = term * term


  fun checkRecursion (tel : telescope) (x : Atom.atom) (Var y) = 
                                                                ( 
                                                                  case (AtomMap.find(tel,y)) of
                                                                    SOME a => checkRecursion tel x a
                                                                  | NONE   => if Atom.same(x,y) then true 
                                                                              else false
                                                                )
    | checkRecursion (tel : telescope) (x : Atom.atom) (Apply (s,lst)) = case lst of 
                                                                        [] => false
                                                                      |  y::ys => if checkRecursion tel x y then true 
                                                                         else checkRecursion tel x (Apply(s,ys)) 



    
  fun unify (tel : telescope) (sEQt : equation) : telescope =
      case sEQt of
          (Var x           , t               )     => unifyVar tel x t
        | (s               , Var y           )     => unifyVar tel y s
        | (Apply (f, fargs), Apply (g, gargs))     => 
                                                      let 
                                                        fun g ([]) ([]) = []
                                                          | g (x::xs) (y::ys) = (x,y)::(g xs ys)
                                                      in
                                                          unifyList tel (g fargs gargs)
                                                      end

  and unifyList (tel : telescope ) (eqns : equation list) = 
      case eqns of
      [] => tel
      |  (x::xs) => 
              let 
                val tel1 = unify tel x
              in 
                  unifyList tel1 xs
              end
  and unifyVar (tel : telescope) (x : Atom.atom) (t : term) : telescope = 
      (
        case t of 
        Var a => if(Atom.same(x,a)) then tel else 
        (
          case (AtomMap.find(tel,x)) of
            SOME a => unify tel (a,t)
          | NONE   => if checkRecursion tel x t then AtomMap.empty else AtomMap.insert(tel,x,t)

        )
        | _ => ( 
                  case (AtomMap.find(tel,x)) of
                  SOME a => unify tel (a,t)
                | NONE   => if checkRecursion tel x t then AtomMap.empty else AtomMap.insert(tel,x,t)
                )
      )
    


end  

structure unifier = Unify (TypeSig)
datatype symbol = INT
                  | BOOL
                  | Arrow
datatype expr = V of Atom.atom | APP of expr * expr |  ABS of string * expr
datatype t = VART of Atom.atom | APPT of symbol * t list
datatype answer = none | pair of t * t AtomMap.map

structure Fresh = struct
  val a = ref 1
  fun fresh () = let 
               val x = !a
               val _ = a := x + 1
               in 
               Int.toString(x)
               end    
end

structure TypeInfer = struct 
  fun typeInfer (V x) = pair ( VART (Atom.atom (Fresh.fresh())) , AtomMap.empty)
    | typeInfer (APP (e1,e2)) = pair ( VART (Atom.atom (Fresh.fresh())) , AtomMap.empty)
    | typeInfer (ABS (s,e)) = pair ( VART (Atom.atom (Fresh.fresh())) , AtomMap.empty)
end






