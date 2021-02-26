
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
  | True -> string_of_bool (true)
  | False -> string_of_bool (false)
  | If (val1, val2, val3) -> "if " ^ string_of_exp(val1) ^ " then " ^ string_of_exp (val2) ^ " else " ^ string_of_exp(val3)
  | Num value -> string_of_int(value)
  | IsZero value -> "isZero " ^ string_of_exp (value)  
  | Plus (left, right) -> "(" ^ string_of_exp (left) ^ " + " ^ string_of_exp (right) ^ ")"
  | Mult (left, right) -> "(" ^ string_of_exp (left) ^ " * " ^ string_of_exp (right) ^ ")" ;;

let to_int exp = match exp with 
  | Num value -> value
  | _ -> raise Eval_error;;

let rec eval exp = match exp with 
  | True -> True
  | False -> False
  | If (val1, val2, val3) -> if (eval val1 = True ) then eval val2 else eval val3
  | Num value -> Num value
  | IsZero value -> match value with  
      | Num val1 -> if val1 = 0 then True else False
      | _ -> raise Eval_error
  | Plus (val1, val2) ->  Num (to_int (eval val1) + to_int (eval val2))
  | Mult (val1, val2) ->  Num (to_int (eval val1) * to_int (eval val2));;

  print_endline(string_of_exp(eval True));
  print_endline(string_of_exp(eval False));
  print_endline(string_of_exp(eval (Num 0)));
  print_endline(string_of_exp(eval (IsZero (Plus (Num 1, Num 1)))));
  print_endline(string_of_exp(eval (IsZero (Plus ((Plus (Num 2, Num (-1))), Num 1)))));
  print_endline(string_of_exp(eval (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1)))));
  print_endline (string_of_exp (eval (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1)))));


  