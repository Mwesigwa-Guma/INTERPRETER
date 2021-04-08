
exception Eval_error

type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp;;

let rec string_of_exp exp = match exp with 
  | True -> "True"
  | False -> "False"
  | If (val1, val2, val3) -> "If " ^ string_of_exp(val1) ^ " then " ^ string_of_exp (val2) ^ " else " ^ string_of_exp(val3)
  | Num value -> "Num " ^ string_of_int(value)
  | IsZero value -> "IsZero (" ^ string_of_exp (value) ^ ")"  
  | Plus (left, right) -> "Plus(" ^ string_of_exp (left) ^ " + " ^ string_of_exp (right) ^ ")"
  | Mult (left, right) -> "Mult(" ^ string_of_exp (left) ^ " * " ^ string_of_exp (right) ^ ")" ;;

let to_int exp = match exp with 
  | Num value -> value
  | _ -> raise Eval_error;;

let rec eval exp = match exp with 
  |True -> True
  |False -> False
  |If (True, e2, e3) -> eval e2
  |If (False, e2, e3) -> eval e3
  |If (e1, e2, e3) -> begin match (eval e1) with 
    |True -> eval e2
    |False -> eval e3
    |_-> raise Eval_error end
  |Num value -> Num value
  |IsZero value -> begin match (eval value) with
    |Num val1 -> if val1 = 0 then True else False
    |_ -> raise Eval_error end 
  |Plus (Num n1, Num n2) ->  Num (n1 + n2)
  |Plus (Num n, e) -> begin match (eval e) with 
    |Num n1 -> Num (n + n1)
    |_-> raise Eval_error end
  |Plus (e, Num n) -> begin match (eval e) with 
    |Num n1 -> Num (n1 + n)
    |_-> raise Eval_error end
  |Plus (e1, e2) -> eval (Plus (eval e1, eval e2))
  |Mult (Num n1, Num n2) ->  Num (n1 * n2)
  |Mult (Num n, e) -> begin match (eval e) with 
    |Num n1 -> Num (n * n1)
    |_-> raise Eval_error end
  |Mult (e, Num n) -> begin match (eval e) with 
    |Num n1 -> Num (n1 * n)
    |_-> raise Eval_error end
  |Mult (e1, e2) -> eval (Mult (eval e1, eval e2)) ;;

  (* print_endline(string_of_exp(eval True));
  print_endline(string_of_exp(eval False));
  print_endline(string_of_exp(eval (Num 0)));
  print_endline(string_of_exp(IsZero (Num 0)));
  print_endline(string_of_exp(eval (IsZero (Plus (Num 1, Num 1)))));
  print_endline(string_of_exp(eval (IsZero (Plus ((Plus (Num 2, Num (-1))), Num 1)))));
  print_endline(string_of_exp(eval (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1)))));
  print_endline(string_of_exp (eval (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1)))));
  print_endline(string_of_exp (eval (Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1)))));
  print_endline(string_of_exp(eval (Plus (IsZero (Plus (Num (-1), Num 1)), Num 1))));
  print_endline(string_of_exp(eval (IsZero (If (IsZero (Num 0), True, Num 0)))));
  print_endline (string_of_exp(
   eval 
    (IsZero
      (If
         ( IsZero (Mult (Num 5, Num 0))
         , If (False, Num 0, IsZero (Plus (Num (-1), Num 0)))
         , Num 0 )))
  ));
  print_endline(string_of_exp(
    eval (If (IsZero (Plus (Num (-1), Num 1)), Num 2, True))
  ));
  print_endline(string_of_exp(
    eval
   (If
      ( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True)
      , Mult (Num 1, Num 2)
      , True ))
  ));
  print_endline(string_of_exp(
    eval
   (If
      ( If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0)
      , Mult (Num 2, Mult (Num 1, Num 1))
      , Plus
          ( Plus
              ( Plus
                  ( Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1))
                  , Num 1 )
              , Num (-1) )
          , Num 1 ) ))
  ));

  print_endline(string_of_exp(
    eval
   (If
      ( True
      , If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5)
      , Plus (Mult (Num 4, Num 1), Num 1) ))
  ));
  print_endline(string_of_exp(
    eval
   (If
      ( IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1))
      , If
          ( True
          , If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1))
          , Num 5 )
      , Num 5 ))
  ));
  (* print_endline(string_of_exp(
    eval
   (If
      ( IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1))))
      , IsZero True
      , Num 1 ))
  )); *)
  print_endline(string_of_exp(
    eval
   (Plus
      ( Num 1
      , Plus
          ( Num (-1)
          , If
              ( IsZero (Plus (Num 1, If (True, Num 1, Num 2)))
              , Plus (Num 1, Num 2)
              , Mult (Num 2, Num 2) ) ) ))
  ));
  print_endline(string_of_exp(
    eval
   (Plus
      ( Num (-1)
      , If
          ( IsZero (Plus (Num 5, Num (-4)))
          , Mult (Num 123, Plus (Num 5, Num (-4)))
          , IsZero (Num 0) ) ))
  )); *)
