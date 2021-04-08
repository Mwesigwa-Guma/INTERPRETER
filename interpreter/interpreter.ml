

exception Eval_error;;
exception Type_error;;
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
 |Lambda of string * typ * exp
 |Apply of exp * exp;;

 let rec string_of_typ typ = match typ with 
  |TBool -> "TBool"
  |TInt -> "TInt"
  |TArrow (t1, t2) -> "(TArrow (" ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ "))";;

let rec string_of_exp exp = match exp with 
  | True -> "True"
  | False -> "False"
  | If (val1, val2, val3) -> "If (" ^ string_of_exp(val1) ^ ", " ^ string_of_exp (val2) ^ ", " ^ string_of_exp(val3) ^ ")"
  | Num val1 -> "Num " ^ string_of_int(val1)
  | IsZero val1 -> "IsZero (" ^ string_of_exp (val1) ^ ")" 
  | Plus (left, right) -> "Plus(" ^ string_of_exp (left) ^ ", " ^ string_of_exp (right) ^ ")"
  | Mult (left, right) -> "Mult(" ^ string_of_exp (left) ^ ", " ^ string_of_exp (right) ^ ")" 
  | Var var -> "Var " ^ "\"" ^ var ^ "\"" 
  | Lambda (s, t, e ) -> "Lambda (\"" ^ s ^ "\", " ^ string_of_typ (t) ^ ", " ^ string_of_exp (e) ^ ")"
  | Apply (e1, e2) -> "Apply (" ^ string_of_exp (e1) ^ ", " ^ string_of_exp(e2) ^ ")";;



let rec step exp = begin match exp with 
 |If (True, e1, e2) -> e1
 |If (False, e1, e2) -> e2
 |If (e1, e2, e3) -> begin match e1 with 
   |Num e -> raise Eval_error
   |Var var -> raise Eval_error
   |_-> let e1' = step e1 in If (e1', e2, e3) end
 |IsZero e -> begin match e with
   |Num n -> if n = 0 then True else False
   |True -> raise Eval_error
   |False -> raise Eval_error 
   |Var var -> raise Eval_error
   |_-> IsZero (step e) end
 |Plus(Num n1, Num n2) -> Num (n1 + n2)
 |Plus(Num n, e) -> begin match e with 
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |_-> Plus (Num n, (step e)) end 
 |Plus(e, Num n) -> begin match e with
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |_-> Plus ((step e), Num n) end 
 |Plus(exp1, exp2) -> begin match exp1 with
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |_-> Plus ((step exp1), exp2) end 
 |Mult(Num n1, Num n2) -> Num (n1 * n2)
 |Mult(Num n, e) -> begin match e with 
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |_-> Mult (Num n, (step e)) end 
 |Mult(e, Num n) -> begin match e with
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |_-> Mult ((step e), Num n) end 
 |Mult(exp1, exp2) -> begin match exp1 with
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |_-> Mult((step exp1), exp2) end 
 |True -> raise Eval_error
 |False -> raise Eval_error
 |Num n -> raise Eval_error 
 |Var var -> raise Eval_error
 |Lambda (s, t, e) -> begin match e with 
   |Num n -> raise Eval_error
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |_-> Lambda (s, t, step e) end
 |Apply (Num n, e2) -> Apply (Num n, step e2)
 |Apply (True, e2) -> Apply (True, step e2)
 |Apply (False, e2) -> Apply (False, step e2)
 |Apply (Var var, e2) -> Apply (Var var, step e2)
 |Apply (e1, Num n) -> Apply (step e1, Num n)
 |Apply (e1, True) -> Apply (step e1, True)
 |Apply (e1, False) -> Apply (step e1, False)
 |Apply (e1, Var var) -> Apply (step e1, Var var)
 |Apply (e1, e2) -> Apply (step e1, e2)
 end ;;

 let rec substitution (e1 : exp) (x : string) (e2 : exp) = begin match e1 with 
  |Var(var) -> if var = x then e2 else Var(var)
  |Lambda(s, t, e) -> if s = x then Lambda (s, t, e) else Lambda (s, t, substitution e x e2)
  |Apply(left, right) -> Apply ((substitution left x e2), (substitution right x e2))
  |True -> True
  |False -> False
  |Num n -> Num n
  |If (i1, i2, i3) -> If ((substitution i1 x e2), (substitution i2 x e2), (substitution i3 x e2))
  |IsZero e -> IsZero (substitution e x e2)
  |Plus (left, right) -> Plus ((substitution left x e2), (substitution right x e2))
  |Mult (left, right) -> Mult ((substitution left x e2), (substitution right x e2)) end ;;

let rec multi_step exp = begin match exp with
  |True -> True
  |False -> False
  |Num n -> Num n
  |Var var -> Var var
  |If (True, e2, e3) -> multi_step e2
  |If (False, e2, e3) -> multi_step e3
  |If (e1, e2, e3) -> multi_step (If ((step e1), e2, e3))
  |IsZero e -> begin match e with 
    |Num n -> if n = 0 then True else False
    |_-> multi_step (IsZero (step e)) end
  |Plus (Num n1, Num n2) -> Num (n1 + n2)
  |Plus (Num n, e) -> multi_step (Plus (Num n, step e))
  |Plus (e, Num n) -> multi_step (Plus (step e, Num n))
  |Plus (e1, e2) -> multi_step (Plus (step e1, e2))
  |Mult (Num n1, Num n2) -> Num (n1 * n2)
  |Mult (Num n, e) -> multi_step (Mult (Num n, step e))
  |Mult (e, Num n) -> multi_step (Mult (step e, Num n))
  |Mult (e1, e2) -> multi_step (Mult (step e1, e2)) 
  |Lambda (s, t, e) -> Lambda (s, t, e)
  |Apply (e1, e2) -> begin match (multi_step e1) with 
    |Lambda (s, t, e) -> multi_step (substitution e s (multi_step e2))
    |_-> raise Eval_error
    end  
  end ;;

let rec type_check te exp = begin match exp with 
  |True -> TBool
  |False -> TBool
  |Num n -> TInt
  |Var var -> if (List.mem_assoc var te ) then (List.assoc var te)  else raise Type_error
  |Plus (e1, e2) -> begin match (type_check te e1, type_check te e2) with 
    |(TInt, TInt) -> TInt
    |_-> raise Type_error end
  |Mult (e1, e2) -> begin match (type_check te e1, type_check te e2 ) with 
    |(TInt, TInt) -> TInt
    |_-> raise Type_error end
  |IsZero e -> begin match (type_check te e) with 
    |TInt -> TBool
    |_-> raise Type_error end
  |If (e1, e2, e3) -> begin match (type_check te e1) with 
    |TBool -> begin match (type_check te e2, type_check te e3) with 
      |(TInt, TInt) -> TInt
      |(TBool, TBool) -> TBool
      |_-> raise Type_error end 
    |_-> raise Type_error  end 
  |Lambda (s, t, e) -> TArrow (t, type_check ((s, t)::te) e) 
  |Apply (e1, e2) -> let t1 = type_check te e1 in
                     let t2 = type_check te e2 in 
                     begin match t1 with
                     |TArrow (t1f, t1s) -> if t1f <> t2 then raise Type_error else t1s
                     |_-> raise Type_error end
  end;;


(* print_endline (string_of_typ
(type_check []
       (Apply
          ( Lambda
              ("x", TInt, Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x")))
          , Num 2 )))
); *)