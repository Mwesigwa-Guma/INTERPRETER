

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
 |Apply of exp * exp
 |LambdaRec of string * typ * typ * string * exp
 |Div of exp * exp
 |Try of exp * exp 
 |RaiseDivByZero of typ * exp
 ;;
 

 let rec string_of_typ typ = match typ with 
  |TBool -> "TBool"
  |TInt -> "TInt"
  |TArrow (t1, t2) -> "TArrow (" ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ ")";;

let rec string_of_exp exp = match exp with 
  |True -> "True"
  |False -> "False"
  |If (val1, val2, val3) -> "If (" ^ string_of_exp(val1) ^ ", " ^ string_of_exp (val2) ^ ", " ^ string_of_exp(val3) ^ ")"
  |Num val1 -> "Num " ^ string_of_int(val1)
  |IsZero val1 -> "IsZero (" ^ string_of_exp (val1) ^ ")" 
  |Plus (left, right) -> "Plus(" ^ string_of_exp (left) ^ ", " ^ string_of_exp (right) ^ ")"
  |Mult (left, right) -> "Mult(" ^ string_of_exp (left) ^ ", " ^ string_of_exp (right) ^ ")" 
  |Var var -> "Var " ^ "\"" ^ var ^ "\"" 
  |Lambda (s, t, e ) -> "Lambda (\"" ^ s ^ "\", " ^ string_of_typ (t) ^ ", " ^ string_of_exp (e) ^ ")"
  |Apply (e1, e2) -> "Apply (" ^ string_of_exp (e1) ^ ", " ^ string_of_exp(e2) ^ ")"
  |LambdaRec (f, t1, t2, x, e) -> "LambdaRec (\"" ^ f ^ "\", " ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ "\"" ^ x ^ "\"" ^ string_of_exp e ^ ")"
  |Div (e1, e2) -> "Div (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  |Try (e1, e2) -> "Try (" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  |RaiseDivByZero (t, e) -> "RaiseDivByZero (" ^ string_of_typ t ^ ", " ^ string_of_exp e ^ ")"
  ;;

  (* implement free variables function *)

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
  |Mult (left, right) -> Mult ((substitution left x e2), (substitution right x e2)) 
  |LambdaRec (f, t1, t2, x2, e) -> substitution e x e2 
  |Div (de1, de2) -> Div ((substitution de1 x e2), (substitution de2 x e2))
  |Try (te1, te2) -> Try ((substitution te1 x e2, substitution te2 x e2))
  |RaiseDivByZero (t, e) -> RaiseDivByZero (t, substitution e x e2) 
  end ;;


let rec step exp = begin match exp with 
 |If (True, e1, e2) -> e1
 |If (False, e1, e2) -> e2
 |If (e1, e2, e3) -> begin match e1 with 
   |Num e -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e) -> RaiseDivByZero (t, step e)
   |_-> let e1' = step e1 in If (e1', e2, e3) end
 |IsZero e -> begin match e with
   |Num n -> if n = 0 then True else False
   |RaiseDivByZero(t, e') -> RaiseDivByZero (t, e')
   |True -> raise Eval_error
   |False -> raise Eval_error 
   |Var var -> raise Eval_error
   |_-> IsZero (step e) end
 |Plus(Num n1, Num n2) -> Num (n1 + n2)
 |Plus(Num n, e) -> begin match e with 
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e') -> RaiseDivByZero (t, step e')
   |_-> Plus (Num n, (step e)) end 
 |Plus(e, Num n) -> begin match e with
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e') -> RaiseDivByZero (t, step e')
   |_-> Plus ((step e), Num n) end 
 |Plus(exp1, exp2) -> Plus ((step exp1), exp2)
 |Mult(Num n1, Num n2) -> Num (n1 * n2)
 |Mult(Num n, e) -> begin match e with 
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e') -> RaiseDivByZero (t, step e')
   |_-> Mult (Num n, (step e)) end 
 |Mult(e, Num n) -> begin match e with
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e') -> RaiseDivByZero (t, step e')
   |_-> Mult ((step e), Num n) end 
 |Mult(exp1, exp2) -> Mult((step exp1), exp2)
 |True -> raise Eval_error
 |False -> raise Eval_error
 |Num n -> raise Eval_error
 |Var var -> raise Eval_error
 |Lambda (s, t, e) -> Lambda (s, t, e)
 |Apply (Lambda (s, t, e1), e2) -> (substitution e1 s e2)
 |Apply (LambdaRec(f, t1, t2, x, e1), e2) -> 
   let sub1 = (substitution e1 x e2) in (step (substitution sub1 f (LambdaRec (f, t1, t2, x, e1))))
 |Apply (Num n, e2) -> Apply (Num n, step e2)
 |Apply (True, e2) -> Apply (True, step e2)
 |Apply (False, e2) -> Apply (False, step e2)
 |Apply (Var var, e2) -> Apply (Var var, step e2)
 |Apply (e1, Num n) -> Apply (step e1, Num n)
 |Apply (e1, True) -> Apply (step e1, True)
 |Apply (e1, False) -> Apply (step e1, False)
 |Apply (e1, Var var) -> Apply (step e1, Var var) 
 |Apply (e1, e2) -> Apply (step e1, e2)
 |LambdaRec (f, t1, t2, x, e) -> LambdaRec (f, t1, t2, x, e)
 |Div (Num n1, Num n2) -> if n2 = 0 then RaiseDivByZero (TInt, Num n1) else Num (n1/n2)
 |Div (e, Num n) -> Div (step e, Num n)
 |Div (Num n, e) -> Div (Num n, step e)
 |Div (e1, e2) -> Div (step e1, e2)
 |Try (e1, e2) -> Apply (e2, e1)
 |RaiseDivByZero (t, e) -> RaiseDivByZero (t, e)
 end ;;

let rec multi_step exp = begin match exp with
  |True -> True
  |False -> False
  |Num n -> Num n
  |Var var -> Var var
  |If (True, e2, e3) -> multi_step e2
  |If (False, e2, e3) -> multi_step e3
  |If (RaiseDivByZero(t, e ), e2, e3) -> RaiseDivByZero (t, e)
  |If (e1, e2, e3) -> multi_step (If ((step e1), e2, e3))
  |IsZero e -> begin match multi_step e with 
    |Num n -> if n = 0 then True else False
    |RaiseDivByZero (t, e') -> RaiseDivByZero (t, e')
    |_ -> raise Eval_error end 
  |Plus (RaiseDivByZero(t, e1), e2) -> RaiseDivByZero(t, e1)
  |Plus (e1, RaiseDivByZero(t, e2)) -> RaiseDivByZero(t, e2)
  |Plus (Num n1, Num n2) -> Num (n1 + n2)
  |Plus (Num n, e) -> multi_step (Plus (Num n, step e))
  |Plus (e, Num n) -> multi_step (Plus (step e, Num n))
  |Plus (e1, e2) -> multi_step (Plus (step e1, e2))
  |Mult (Num n1, Num n2) -> Num (n1 * n2)
  |Mult (RaiseDivByZero(t, e1), e2) -> RaiseDivByZero(t, e1)
  |Mult (e1, RaiseDivByZero(t, e2)) -> RaiseDivByZero(t, e2)
  |Mult (Num n, e) -> multi_step (Mult (Num n, step e))
  |Mult (e, Num n) -> multi_step (Mult (step e, Num n))
  |Mult (e1, e2) -> multi_step (Mult (step e1, e2)) 
  |Lambda (s, t, e) -> Lambda (s, t, e)
  |Apply (e1, e2) -> begin match (multi_step e1) with 
    |Lambda (s, t, e) -> multi_step (substitution e s e2)
    |LambdaRec (f, t1, t2, x, e) -> 
      let sub1 = (substitution e x e2) in (multi_step (substitution sub1 f (LambdaRec (f, t1, t2, x, e))))
    |_-> raise Eval_error end
  |LambdaRec (f, t1, t2, x, e) -> LambdaRec (f, t1, t2, x, e)
  |Div (Num n1, Num n2) -> if n2 = 0 then RaiseDivByZero (TInt, Num n1) else Num (n1/n2)
  |Div (e, Num n) -> multi_step (Div (step e, Num n))
  |Div (Num n, e) -> multi_step (Div (Num n, step e)) 
  |Div (e1, e2 ) ->  multi_step (Div (step e1, e2))
  |Try (e1, e2) -> begin match multi_step e1 with 
    |RaiseDivByZero(t, e) -> multi_step (Apply (e2, e))
    |e1' -> multi_step (Apply(e2, e1')) end 
  |RaiseDivByZero (t, e) -> RaiseDivByZero (t, multi_step e)
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
  |Div (e1, e2) -> begin match (type_check te e1, type_check te e2) with 
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
  |LambdaRec (f, t1, t2, x, e) ->  TArrow (t1, type_check ((f, TArrow(t1, t2))::((x, t1)::te)) e)
  |Apply (e1, e2) -> let t1 = type_check te e1 in
                     let t2 = type_check te e2 in 
                     begin match t1 with
                     |TArrow (t1f, t1s) -> if t1f <> t2 then raise Type_error else t1s
                     |_-> raise Type_error end
  |Try (e1, e2) -> begin match type_check te e2 with 
    |TArrow(t1, t2) -> begin match type_check te e1 with 
      |TArrow (t3, t4) -> if t2 = t4 then t2 else raise Type_error
      |t' -> if t' = t2 then t2 else raise Type_error end 
    |_-> raise Eval_error end 
  |RaiseDivByZero (t, e) -> t
  end;;

(* print_endline (string_of_typ
(type_check []
   (Try (Div (Num 4, Num 0), Lambda ("x", TInt, False))))
); *)