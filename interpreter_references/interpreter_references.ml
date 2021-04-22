exception Eval_error;;
exception Type_error;;
exception Substitution_error;;

type typ = TBool | TInt | TArrow of typ * typ | TRef of typ | TUnit;;

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
 |Label of int
 |Malloc of exp
 |Mread of exp
 |Assign of exp * exp
 |Sequence of exp * exp
 |Unit
 ;;
 
 type memory = (int * exp) list ;;

 let rec string_of_typ typ = match typ with 
  |TBool -> "TBool"
  |TInt -> "TInt"
  |TArrow (t1, t2) -> "TArrow (" ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ ")"
  |TRef t -> "TRef " ^ string_of_typ t
  |TUnit -> "TUnit";;

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
  |Label n -> "Lable " ^ string_of_int n 
  |Malloc e -> "Malloc (" ^ string_of_exp e ^ ")"
  |Mread e -> "Mread (" ^ string_of_exp e ^ ")"
  |Assign (a1, a2) -> "Assign (" ^ string_of_exp a1 ^ ", " ^ string_of_exp a2 ^ ")"
  |Sequence (s1, s2) -> "Sequence (" ^ string_of_exp s1 ^ string_of_exp s2 ^ ")"
  |Unit -> "Unit"
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
  |Label n -> Label n 
  |Malloc e -> Malloc (substitution e x e2)
  |Mread e -> Mread (substitution e x e2)
  |Assign (a1, a2) -> Assign ((substitution a1 x e2), (substitution a2 x e2))
  |Sequence (s1, s2) -> Sequence ((substitution s1 x e2), (substitution s2 x e2))
  |Unit -> Unit
  end ;;

let rec nextLable memory = match memory with 
  |[] -> 0
  |l::[] -> begin match l with |(key, value) -> key+1 end 
  |lh::lt -> nextLable lt 

(* let rec nLV memory = match memory with 
  |[] -> 0
  |l::[] -> 1 
  |lh::lt -> (nLV lt) + 1  *)

let rec step exp memory = begin match (exp, memory) with 
 |(If (True, e1, e2), mem) -> (e1, mem)
 |(If (False, e1, e2), mem) -> (e2, mem)
 |(If (e1, e2, e3), mem) -> begin match e1 with 
   |Num e -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e) -> begin match step e mem with 
     |(e', mem') -> (RaiseDivByZero (t, e'), mem') end
   |_-> begin match step e1 mem with 
     |(e1', mem') -> (If (e1', e2, e3), mem') end end
 |(IsZero e, mem) -> begin match e with
   |Num n -> if n = 0 then (True, mem) else (False, mem)
   |RaiseDivByZero(t, e') -> (RaiseDivByZero (t, e'), mem)
   |True -> raise Eval_error
   |False -> raise Eval_error 
   |Var var -> raise Eval_error
   |_-> begin match step e mem with 
     |(e', mem') -> (IsZero e', mem') end end
 |(Plus(Num n1, Num n2), mem) -> (Num (n1 + n2), mem)
 |(Plus(Num n, e), mem) -> begin match e with 
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e') -> begin match step e' mem with 
     |(e'', mem') -> (RaiseDivByZero (t, e''), mem') end
   |_-> begin match step e mem with 
     |(e', mem') -> (Plus (Num n, e'), mem') end end
 |(Plus(e, Num n), mem) -> begin match e with
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e') -> begin match step e' mem with 
     |(e'', mem') -> (RaiseDivByZero (t, e''), mem') end
   |_-> begin match step e mem with 
     |(e', mem') -> (Plus ( e', Num n), mem') end end
 |(Plus(exp1, exp2), mem) -> begin match step exp1 mem with 
   |(exp1', mem') -> (Plus (exp1', exp2), mem') end
 |(Mult(Num n1, Num n2), mem) -> (Num (n1 * n2), mem)
 |(Mult(Num n, e), mem) -> begin match e with 
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e') -> begin match step e' mem with 
     |(e'', mem') -> (RaiseDivByZero (t, e''), mem') end
   |_-> begin match step e mem with 
     |(e', mem') -> (Mult ( Num n, e'), mem') end end 
 |(Mult(e, Num n), mem) -> begin match e with
   |True -> raise Eval_error
   |False -> raise Eval_error
   |Var var -> raise Eval_error
   |RaiseDivByZero (t, e') -> begin match step e' mem with 
     |(e'', mem') -> (RaiseDivByZero (t, e''), mem') end
   |_-> begin match step e mem with 
     |(e', mem') -> (Mult ( e', Num n), mem') end end 
 |(Mult(exp1, exp2), mem) -> begin match step exp1 mem with 
   |(exp1', mem') -> (Mult (exp1', exp2), mem') end
 |(True, mem) -> raise Eval_error
 |(False, mem) -> raise Eval_error
 |(Num n, mem) -> raise Eval_error
 |(Var var, mem) -> raise Eval_error
 |(Lambda (s, t, e), mem) -> (Lambda (s, t, e), mem)
 |(Apply (Lambda (s, t, e1), e2), mem) -> ( (substitution e1 s e2), mem)
 |(Apply (LambdaRec(f, t1, t2, x, e1), e2), mem) -> 
   let sub1 = (substitution e1 x e2) in (step (substitution sub1 f (LambdaRec (f, t1, t2, x, e1))) mem)
 |(Apply (Num n, e2), mem) -> begin match step e2 mem with 
   |(e2', mem') -> (Apply (Num n, e2'), mem') end
 |(Apply (True, e2), mem) -> begin match step e2 mem with 
   |(e2', mem') -> (Apply (True, e2'), mem') end
 |(Apply (False, e2), mem) -> begin match step e2 mem with
   |(e2', mem') -> (Apply (False, e2'), mem') end
 |(Apply (Var var, e2), mem) -> begin match step e2 mem with
   |(e2', mem') -> (Apply (Var var, e2'), mem') end
 |(Apply (e1, Num n), mem) -> begin match step e1 mem with
   |(e1', mem') -> (Apply (e1', Num n), mem') end
 |(Apply (e1, True), mem) -> begin match step e1 mem with
   |(e1', mem') -> (Apply (e1', True), mem') end
 |(Apply (e1, False), mem) -> begin match step e1 mem with
   |(e1', mem') -> (Apply (e1', False), mem') end
 |(Apply (e1, Var var), mem) -> begin match step e1 mem with
   |(e1', mem') -> (Apply (e1', Var var), mem') end
 |(Apply (e1, e2), mem) -> begin match step e1 mem with
   |(e1', mem') -> (Apply (e1', e2), mem') end
 |(LambdaRec (f, t1, t2, x, e), mem) -> (LambdaRec (f, t1, t2, x, e), mem)
 |(Div (Num n1, Num n2), mem) -> if n2 = 0 then (RaiseDivByZero (TInt, Num n1), mem) else (Num (n1/n2), mem)
 |(Div (e, Num n), mem) -> begin match step e mem with 
   |(e', mem') -> (Div (e', Num n), mem') end
 |(Div (Num n, e), mem) -> begin match step e mem with 
   |(e', mem') -> (Div (Num n, e'), mem') end 
 |(Div (e1, e2), mem) -> begin match step e1 mem with
   |(e1', mem') -> (Div (e1', e2), mem') end
 |(Try (e1, e2), mem) -> (Apply (e2, e1), mem)
 |(RaiseDivByZero (t, e), mem) -> (RaiseDivByZero (t, e), mem)
 |(Label n, mem) -> raise Eval_error
 |(Malloc e, mem) -> begin match e with 
   |True -> begin match (nextLable mem) with
      |nextL -> begin match (nextL, e)::mem with 
        |mem' -> (Label nextL, mem') end end 
   |False -> begin match (nextLable mem) with
      |nextL -> begin match (nextL, e)::mem with 
        |mem' -> (Label nextL, mem') end end 
   |Var var -> begin match (nextLable mem) with
      |nextL -> begin match (nextL, e)::mem with
        |mem' -> (Label nextL, mem') end end
   |Num n -> begin match (nextLable mem) with
      |nextL -> begin match (nextL, e)::mem with
        |mem' -> (Label nextL, mem') end end
   |LambdaRec (f, t1, t2, x, e1) -> begin match (nextLable mem) with
      |nextL -> begin match (nextL, e)::mem with
        |mem' -> (Label nextL, mem') end end
   |Lambda (s, t, e1) -> begin match (nextLable mem) with
      |nextL -> begin match (nextL, e)::mem with 
        |mem' -> (Label nextL, mem') end end 
   |Unit -> begin match (nextLable mem) with
      |nextL -> begin match (nextL, e)::mem with 
        |mem' -> (Label nextL, mem') end end 
   |Label n -> begin match (nextLable mem) with
      |nextL -> begin match (nextL, e)::mem with 
        |mem' -> (Label nextL, mem') end end 
   |_-> begin match step e mem with
     |(e', mem') -> (Malloc e', mem') end
   end
 |((Mread exp), mem) -> begin match exp with 
   |Label n -> if (List.mem_assoc n mem) then ((List.assoc n mem), mem) else raise Eval_error
   |_ -> begin match step exp mem with 
     |(exp', mem') -> (Mread exp', mem') end end
 |(Assign (e1, e2), mem) -> begin match e1 with 
   |Label n -> begin match e2 with 
     |True -> begin match (n, True)::(List.remove_assoc n mem) with 
       |(mem') -> (Unit, mem') end
     |False -> begin match (n, False)::(List.remove_assoc n mem) with 
       |(mem') -> (Unit, mem') end
     |Num n -> begin match (n, Num n)::(List.remove_assoc n mem) with 
       |(mem') -> (Unit, mem') end
     |Var var -> begin match (n, Var var)::(List.remove_assoc n mem) with 
       |(mem') -> (Unit, mem') end
     |Label n -> begin match (n, Label n)::(List.remove_assoc n mem) with 
       |(mem') -> (Unit, mem') end
     |Lambda (s, t, e) -> begin match (n, Lambda (s, t, e))::(List.remove_assoc n mem) with 
       |(mem') -> (Unit, mem') end
     |Unit -> begin match (n, Unit)::List.remove_assoc n mem with 
       |(mem') -> (Unit, mem') end 
     |RaiseDivByZero (t, e) -> begin match (n, RaiseDivByZero(t, e))::(List.remove_assoc n mem) with 
       |(mem') -> (Unit, mem') end 
     |_-> begin match step e2 mem with 
       |(e2', mem') -> (Assign (e1, e2'), mem') end end  
   |_-> begin match step e1 mem with 
     |(e1', mem') -> (Assign (e1', e2), mem') end end
 |(Sequence(e1, e2), mem) -> begin match e1 with 
   |True -> step e2 mem 
   |False -> step e2 mem
   |Var var -> step e2 mem
   |Label n -> step e2 mem
   |Num n -> step e2 mem 
   |Lambda (s, t, e) -> step e2 mem
   |Unit -> step e2 mem
   |RaiseDivByZero (t, e) -> step e2 mem
   |_-> begin match step e1 mem with 
     |(e1', mem') -> (Sequence (e1', e2), mem') end end
 |(Unit, mem) -> raise Eval_error
 end ;;

let rec multi_step exp memory = begin match (exp, memory) with
  |(True, mem) -> (True, mem)
  |(False, mem) -> (False, mem)
  |(Num n, mem) -> (Num n, mem)
  |(Var var, mem) -> (Var var, mem)
  |(If (True, e2, e3), mem) -> multi_step e2 mem
  |(If (False, e2, e3), mem) -> multi_step e3 mem
  |(If (RaiseDivByZero(t, e ), e2, e3), mem) -> (RaiseDivByZero (t, e), mem)
  |(If (e1, e2, e3), mem) -> begin match step e1 mem with 
    |(e1', mem') -> multi_step (If (e1', e2, e3)) mem' end
  |(IsZero e, mem) -> begin match multi_step e mem with 
    |(Num n, mem') -> if n = 0 then (True, mem') else (False, mem')
    |(RaiseDivByZero (t, e'), mem) -> (RaiseDivByZero (t, e'), mem)
    |_ -> raise Eval_error end
  |(Plus (RaiseDivByZero(t, e1), e2), mem) -> (RaiseDivByZero(t, e1), mem)
  |(Plus (e1, RaiseDivByZero(t, e2)), mem) -> (RaiseDivByZero(t, e2), mem)
  |(Plus (Num n1, Num n2), mem) -> (Num (n1 + n2), mem)
  |(Plus (Num n, e), mem) -> begin match step e mem with
    |(e', mem') -> multi_step (Plus (Num n, e'))  mem' end
  |(Plus (e, Num n), mem) -> begin match step e mem with
    |(e', mem') -> multi_step (Plus ( e', Num n))  mem' end
  |(Plus (e1, e2), mem) -> begin match step e1 mem with
    |(e1', mem') -> multi_step (Plus (e1', e2))  mem' end
  |(Mult (Num n1, Num n2), mem) -> (Num (n1 * n2), mem)
  |(Mult (RaiseDivByZero(t, e1), e2), mem) -> (RaiseDivByZero(t, e1), mem)
  |(Mult (e1, RaiseDivByZero(t, e2)), mem) -> (RaiseDivByZero(t, e2), mem)
  |(Mult (Num n, e), mem) -> begin match step e mem with 
    |(e', mem') -> multi_step (Mult (Num n, e'))  mem' end
  |(Mult (e, Num n), mem) -> begin match step e mem with 
    |(e', mem') -> multi_step (Mult (e', Num n))  mem' end
  |(Mult (e1, e2), mem) -> begin match step e1 mem with 
    |(e1', mem') -> multi_step (Mult (e1', e2))  mem' end
  |(Lambda (s, t, e), mem) -> (Lambda (s, t, e), mem)
  |(Apply (e1, e2), mem) -> begin match (multi_step e1 mem) with 
    |(Lambda (s, t, e), mem') -> begin match multi_step e2 mem' with 
      |(e2', mem'') -> multi_step (substitution e s e2') mem'' end 
    |(LambdaRec (f, t1, t2, x, e), mem) -> 
      let sub1 = (substitution e x e2) in (multi_step (substitution sub1 f (LambdaRec (f, t1, t2, x, e))) mem)
    |_-> raise Eval_error end
  |(LambdaRec (f, t1, t2, x, e), mem) -> (LambdaRec (f, t1, t2, x, e), mem)
  |(Div (Num n1, Num n2), mem) -> if n2 = 0 then (RaiseDivByZero (TInt, Num n1), mem) else (Num (n1/n2), mem)
  |(Div (e, Num n), mem) -> begin match step e mem with
    |(e', mem') -> multi_step (Div (e', Num n))  mem' end
  |(Div (Num n, e), mem) -> begin match step e mem with 
    |(e', mem') -> multi_step (Div (Num n, e'))  mem' end
  |(Div (e1, e2 ), mem) ->  begin match step e1 mem with
    |(e1', mem') -> multi_step (Div (e1', e2))  mem' end
  |(Try (e1, e2), mem) -> begin match multi_step e1 mem with
    |(RaiseDivByZero(t, e), mem') -> multi_step (Apply (e2, e)) mem'
    |(e1', mem') -> multi_step (Apply(e2, e1')) mem' end
  |(RaiseDivByZero (t, e), mem) -> begin match multi_step e mem with
    |(e', mem') -> (RaiseDivByZero (t, e'), mem') end
  |(Label n, mem) -> (Label n, mem)
  |(Malloc e, mem) -> begin match e with
    |True -> step (Malloc e) mem
    |False -> step (Malloc e) mem
    |Var va -> step (Malloc e) mem
    |Num n -> step (Malloc e) mem
    |Label n -> step (Malloc e) mem
    |Lambda (s, t, e') -> step (Malloc e) mem
    |LambdaRec(f, t1, t2, x, e) -> step (Malloc e) mem
    |_-> begin match (step e mem) with 
      |(e', mem') -> multi_step (Malloc e') mem' end end
  |((Mread exp), mem) -> begin match exp with 
    |Label n -> if (List.mem_assoc n mem) then ((List.assoc n mem), mem) else raise Eval_error
    |_ -> begin match step exp mem with 
      |(exp', mem') -> multi_step (Mread exp') mem' end end
  |(Assign (e1, e2), mem) -> begin match e1 with 
    |Label n -> begin match multi_step e2 mem with 
      |(e2', mem') -> begin match ((n, e2')::(List.remove_assoc n mem')) with 
        |mem'' -> (Unit, mem'') end end
    |_-> begin match step e1 mem with
      |(e1', mem') -> multi_step (Assign (e1', e2)) mem' end end
  |(Sequence(e1, e2), mem) -> begin match e1 with 
   |True -> multi_step e2 mem
   |False -> multi_step e2 mem
   |Var var -> multi_step e2 mem
   |Label n -> multi_step e2 mem
   |Num n -> multi_step e2 mem
   |Lambda (s, t, e) -> multi_step e2 mem
   |Unit -> multi_step e2 mem
   |_ -> begin match (step e1 mem) with
    |(e1', mem') -> multi_step (Sequence (e1', e2)) mem' end end
  |(Unit, mem) -> (Unit, mem)
  end ;;

(* print_endline (string_of_int (nLV [(0, (Num 64)); (0, (Num 32)); (0, (Num 16)); (0, (Num 8))])); *)

print_endline (string_of_exp
( multi_step
    (Apply
       ( Lambda
           ( "n"
           , TRef TInt
           , Apply
               ( Lambda
                   ( "f"
                   , TArrow (TUnit, TUnit)
                   , Sequence
                       ( Apply (Var "f", Unit)
                       , Sequence (Apply (Var "f", Unit), Mread (Var "n"))
                       ) )
               , Lambda
                   ( "_"
                   , TUnit
                   , Assign (Var "n", Div (Mread (Var "n"), Num 2)) ) )
           )
       , Malloc (Num 32) ))
    []
|> fst )
);


(* let rec type_check te exp = begin match exp with 
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
  end;; *)

(* print_endline (string_of_typ
(type_check []
   (Try (Div (Num 4, Num 0), Lambda ("x", TInt, False))))
); *)