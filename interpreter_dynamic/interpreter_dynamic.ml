(* open Base ;; *)

exception Eval_error;;
exception Substitution_error;;

type typ = TBool | TInt | TArrow of typ * typ;;

type exp = 
 |True
 |False
 |If of exp * exp * exp
 |Num of int
 |IsZero of exp
 |Plus of exp * exp
 |Mult of exp * exp
 |Var of string
 |Lambda of string * exp
 |Apply of exp * exp
 |Let of string * exp * exp
 |TypeError ;;

 type enviroment = (string * exp) list ;;

 let rec string_of_typ typ = match typ with 
  |TBool -> "TBool"
  |TInt -> "TInt"
  |TArrow (t1, t2) -> "(TArrow (" ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ "))";;

let rec string_of_exp exp = match exp with 
  | True -> "True"
  | False -> "False"
  | If (val1, val2, val3) -> "If (" ^ string_of_exp val1 ^ ", " ^  string_of_exp val2 ^ ", " ^ string_of_exp val3 ^ ")"
  | Num val1 -> "Num " ^ string_of_int(val1)
  | IsZero val1 -> "IsZero (" ^ string_of_exp val1 ^ ")" 
  | Plus (left, right) -> "Plus(" ^ string_of_exp left ^ ", " ^ string_of_exp right ^ ")"
  | Mult (left, right) -> "Mult(" ^ string_of_exp left ^ ", " ^ string_of_exp right ^ ")" 
  | Var var -> "Var " ^ "\"" ^ var ^ "\"" 
  | Lambda (s, e ) -> "Lambda (\"" ^ s ^ "\", " ^ ", " ^ string_of_exp e ^ ")"
  | Apply (e1, e2) -> "Apply (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | TypeError -> "TypeError"
  | Let(s, e1, e2) -> "Let " ^ s ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ;;

let rec step enviroment exp = begin match (enviroment, exp) with 
 |(enviroment, If (True, e1, e2)) -> (enviroment, e1)
 |(enviroment, If (False, e1, e2)) -> (enviroment, e2)
 |(enviroment, If(TypeError, e1, e2)) -> (enviroment, TypeError)
 |(enviroment, If (e1, e2, e3)) -> begin match step enviroment e1 with
   |(enviroment, e) -> (enviroment, If (e, e2, e3)) end
 |(enviroment, IsZero (Num n))-> if n = 0 then (enviroment, True) else (enviroment, False)
 |(enviroment, IsZero TypeError) -> (enviroment, TypeError)
 |(enviroment, IsZero True) -> (enviroment, TypeError)
 |(enviroment, IsZero False) -> (enviroment, TypeError)
 |(enviroment, IsZero e) -> begin match step enviroment e with
   |(enviroment, e') -> (enviroment, IsZero e') end
 |(enviroment, Plus(Num n1, Num n2)) -> (enviroment, Num (n1 + n2))
 |(enviroment, Plus(Num n, e)) -> begin match e with 
   |True -> (enviroment, TypeError)
   |False -> (enviroment, TypeError)
   |_-> begin match step enviroment e with
     |(enviroment, e') -> (enviroment, Plus (Num n, e')) end end 
 |(enviroment, Plus(e, Num n)) -> begin match e with
   |True -> (enviroment, TypeError)
   |False -> (enviroment, TypeError)
   |_-> begin match step enviroment e with
     |(enviroment, e') -> (enviroment, Plus (e', Num n)) end end
 |(enviroment, Plus(exp1, exp2)) -> begin match exp1 with
   |True -> (enviroment, TypeError)
   |False -> (enviroment, TypeError)
   |_-> begin match step enviroment exp1 with
     |(enviroment, e') -> (enviroment, Plus (e', exp2)) end end 
 |(enviroment, Mult(Num n1, Num n2)) -> (enviroment, Num (n1 * n2))
 |(enviroment, Mult(Num n, e)) -> begin match e with 
   |True -> (enviroment, TypeError)
   |False -> (enviroment, TypeError)
   |_-> begin match step enviroment e with
     |(enviroment, e') -> (enviroment, Mult (Num n, e')) end end 
 |(enviroment, Mult(e, Num n)) -> begin match e with
   |True -> (enviroment, TypeError)
   |False -> (enviroment, TypeError)
   |_-> begin match step enviroment e with
     |(enviroment, e') -> (enviroment, Mult (e', Num n)) end end
 |(enviroment, Mult(exp1, exp2)) -> begin match exp1 with
   |True -> (enviroment, TypeError)
   |False -> (enviroment, TypeError)
   |_-> begin match step enviroment exp1 with
     |(enviroment, e') -> (enviroment, Mult (e', exp2)) end end
 |(enviroment, True) -> (enviroment, TypeError)
 |(enviroment, False) -> (enviroment, TypeError)
 |(enviroment, Num n) -> (enviroment, TypeError)
 |(enviroment, Var var) -> if (List.mem_assoc var enviroment) then (enviroment, List.assoc var enviroment) else (enviroment, TypeError)
 |(enviroment, TypeError) -> (enviroment, TypeError)
 |(enviroment, Lambda (s, e)) -> (enviroment, Lambda(s, e))
 |(enviroment, Apply (e1, e2)) -> begin match step enviroment e1 with 
   |(enviroment, Lambda (s, e)) -> begin match step enviroment e2 with 
     |(enviroment, e2') -> step ((s, e2')::enviroment) e end
   |_-> (enviroment, TypeError) end 
 |(enviroment, Let (s, e1, e2)) -> step ((s, e1)::enviroment) e2
 end ;;

let rec multi_step enviroment exp = begin match (enviroment, exp) with
  |(enviroment, True) -> (enviroment, True )
  |(enviroment, False) -> (enviroment, False)
  |(enviroment, Num n) -> (enviroment, Num n)
  |(enviroment, Var var) -> if (List.mem_assoc var enviroment) then (enviroment, (List.assoc var enviroment))  else (enviroment, TypeError)
  |(enviroment, TypeError) -> (enviroment, TypeError)
  |(enviroment, If (True, e2, e3)) -> multi_step enviroment e2
  |(enviroment, If (False, e2, e3)) -> multi_step enviroment e3
  |(enviroment, If(TypeError, e2, e3)) -> (enviroment, TypeError)
  |(enviroment, If (e1, e2, e3)) -> begin match multi_step enviroment e1 with
    |(enviroment, e1') -> multi_step enviroment (If (e1', e2, e3)) end 
  |(enviroment, IsZero e) -> begin match multi_step enviroment e with 
    |(enviroment, Num n) -> if n = 0 then (enviroment, True) else (enviroment, False)
    |_-> (enviroment, TypeError) end
  |(enviroment, Plus (Num n1, Num n2)) -> (enviroment, Num (n1 + n2))
  |(enviroment, Plus (Num n, e)) -> begin match multi_step enviroment e with
    |(enviroment, e') -> ( match e' with | Num n' -> multi_step enviroment (Plus (Num n, e')) |_-> (enviroment, TypeError) ) end
  |(enviroment, Plus (e, Num n)) -> begin match multi_step enviroment e with
    |(enviroment, e') -> ( match e' with | Num n' -> multi_step enviroment (Plus (e', Num n)) |_-> (enviroment, TypeError) ) end
  |(enviroment, Plus (TypeError, e)) -> (enviroment, TypeError)
  |(enviroment, Plus (e, TypeError)) -> (enviroment, TypeError)
  |(enviroment, Plus (e1, e2)) -> begin match multi_step enviroment e1 with
    |(enviroment, e1') -> (match e1' with | Num n1' -> multi_step enviroment (Plus (e1', e2)) |_-> (enviroment, TypeError) ) end
  |(enviroment, Mult (Num n1, Num n2)) -> (enviroment, Num (n1 * n2))
  |(enviroment, Mult (Num n, e)) -> begin match multi_step enviroment e with
    |(enviroment, e') -> (match e' with | Num n' -> multi_step enviroment (Mult (Num n, e')) |_-> (enviroment, TypeError) ) end
  |(enviroment, Mult (e, Num n)) -> begin match multi_step enviroment e with
    |(enviroment, e') -> (match e' with | Num n' -> multi_step enviroment (Mult (e', Num n)) |_-> (enviroment, TypeError) ) end 
  |(enviroment, Mult (TypeError, e)) -> (enviroment, TypeError)
  |(enviroment, Mult (e, TypeError)) -> (enviroment, TypeError )
  |(enviroment, Mult (e1, e2)) -> begin match multi_step enviroment e1 with
    |(enviroment, e1') -> (match e1' with Num n1' -> multi_step enviroment (Mult (e1', e2)) |_-> (enviroment, TypeError) ) end 
  |(enviroment, Lambda (s, e)) -> begin match e with
    |TypeError -> (enviroment, TypeError)
    |_-> (enviroment, Lambda (s, e)) end 
  |(enviroment, Apply (e1, e2)) -> begin match multi_step enviroment e1 with
    |(enviroment, Lambda (s, e)) -> begin match multi_step enviroment e2 with 
      |(enviroment, e2') -> multi_step ((s, e2')::enviroment) e end 
    |_-> (enviroment, TypeError) end  
  |(enviroment, Let (s, e1, e2)) -> multi_step ((s, e1)::enviroment) e2 
  end ;;

print_endline (string_of_exp
(multi_step [] (If (IsZero (Plus (True, Num 4)), Num 3, Num 4)) |> snd)
);
