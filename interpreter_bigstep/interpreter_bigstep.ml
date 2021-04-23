
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
  | True -> "true"
  | False -> "false"
  | If (val1, val2, val3) -> "if " ^ string_of_exp val1 ^ " then " ^ string_of_exp val2 ^ " else " ^ string_of_exp val3
  | Num value -> string_of_int value 
  | IsZero value -> "isZero (" ^ string_of_exp value ^ ")"  
  | Plus (left, right) -> "(" ^ string_of_exp left ^ " + " ^ string_of_exp right ^ ")"
  | Mult (left, right) -> "(" ^ string_of_exp left ^ " * " ^ string_of_exp right ^ ")" ;;

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

  print_endline (string_of_exp (Num 3));
  print_endline (string_of_exp (True));
  print_endline (string_of_exp (False));
  print_endline (string_of_exp (Plus (Num 3, Num 2)));
  print_endline (string_of_exp (Mult (Num 3, Num 2)));