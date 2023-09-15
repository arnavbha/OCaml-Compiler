open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        

(******************************************************)
(******************************************************)
(******************************************************)


(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)

let get_int num = match num with
  | Int(val1) -> val1
  | _ -> (raise (TypeError "not an int"))

let get_bool bol = match bol with
  | Bool(val1) -> val1
  | _ -> (raise (TypeError "not a boolean"))

let get_string str = match str with
  | String(val1) -> val1
  | _ -> (raise (TypeError "not a string"))

let typeint num = match num with
  | Int(val1) -> true
  | _ -> false

let typebool bol = match bol with
  | Bool(val1) -> true
  | _ -> false 

let typestring str = match str with
  | String(val1) -> true
  | _ -> false

let sametype x1 x2 = match x1 with
  | Int(val1) -> typeint x2 
  | Bool(val1) -> typebool x2
  | String(val1) -> typestring x2
  | _ -> (raise (TypeError "not int, boolean, or string"))

let rec contains env val1 = 
  match env with
  | (x, y)::t -> if x = val1 then true else (contains t val1)
  | [] -> false
 
(* end of helpers *)


let rec eval_expr env e = match e with

  | Value(val1) -> val1                                                                         (* VALUE *)
  | ID(val1) -> lookup env val1                                                                 (* ID *)
  | Not(val1) -> (let x = (eval_expr env val1) in                                               (* NOT *)
    match x with
    | Bool(bol) -> (Bool(not bol))
    | _ -> (raise (TypeError "BOOLEAN REQUIRED")))

  | Binop(oper, val1, val2) -> (let x1 = (eval_expr env val1) in let x2 = (eval_expr env val2) in (* BINOP *)
    match oper with
    | Add -> if ((typeint x1) && (typeint x2)) then Int((get_int x1) + (get_int x2)) else (raise (TypeError "EITHER X1 OR X2 IS NOT AN INT"))
    | Sub -> if ((typeint x1) && (typeint x2)) then Int((get_int x1) - (get_int x2)) else (raise (TypeError "EITHER X1 OR X2 IS NOT AN INT"))
    | Mult -> if ((typeint x1) && (typeint x2)) then Int((get_int x1)*(get_int x2)) else (raise (TypeError "EITHER X1 OR X2 IS NOT AN INT"))

    | Div -> if ((typeint x1) && (typeint x2)) then 
              ( if (get_int x2 = 0) then raise DivByZeroError else Int((get_int x1)/(get_int x2)) )
            else (raise (TypeError "EITHER X1 OR X2 IS NOT AN INT"))

    | Greater -> if ((typeint x1) && (typeint x2)) then Bool((get_int x1) > (get_int x2)) else (raise (TypeError "EITHER X1 OR X2 IS NOT AN INT"))
    | Less ->    if ((typeint x1) && (typeint x2)) then Bool((get_int x1) < (get_int x2)) else (raise (TypeError "EITHER X1 OR X2 IS NOT AN INT"))
    | GreaterEqual -> if ((typeint x1) && (typeint x2)) then Bool((get_int x1) >= (get_int x2)) else (raise (TypeError "EITHER X1 OR X2 IS NOT AN INT"))
    | LessEqual ->    if ((typeint x1) && (typeint x2)) then Bool((get_int x1) <= (get_int x2)) else (raise (TypeError "EITHER X1 OR X2 IS NOT AN INT"))
    | Concat -> if ((typestring x1) && (typestring x2)) then (String((get_string x1)^(get_string x2))) else (raise (TypeError "EITHER X1 OR X2 IS NOT A STRING"))
    | Equal ->  if (sametype x1 x2) then Bool(x1 = x2)
                else (raise (TypeError "NOT THE SAME TYPE"))
    | NotEqual ->   if (sametype x1 x2) then Bool(x1 != x2)
                    else (raise (TypeError "NOT THE SAME TYPE"))
    | Or -> if ((typebool x1) && (typebool x2)) then Bool((get_bool x1) || (get_bool x2)) else (raise (TypeError "EITHER X1 OR X2 IS NOT A BOOLEAN"))
    | And -> if ((typebool x1) && (typebool x2)) then Bool((get_bool x1) && (get_bool x2)) else (raise (TypeError "EITHER X1 OR X2 IS NOT A BOOLEAN")))
   
  | If(y1, y2, y3) -> let g = (eval_expr env y1) in 
    if (typebool g) then let x = (get_bool g) in                           (* IF *)
      (if x then (eval_expr env y2) else eval_expr env y3) 
    else (raise (TypeError "Y1 NOT A BOOLEAN"))

  | Let(id, recur, val1, val2) ->                                                                 (* LET *)
      (if recur then
        (let env2 = (extend_tmp env id) in let expr = (eval_expr env2 val1) in (update env2 id expr);
        (eval_expr env2 val2))
      else
        (let expr = (eval_expr env val1) in let env2 = (extend env id expr) in eval_expr env2 val2))

  | Fun(arg, val1) -> Closure(env, arg, val1)                                                     (* FUN *)

  | FunctionCall(val1, val2) -> let y = (eval_expr env val1) in (match y with                     (* FUNCTION CALL *)
    | Closure(env1, arg, body) -> let v = (eval_expr env val2) in let env2 = (extend env1 arg v) in (eval_expr env2 body)
    | _ -> (raise (TypeError "FUNCTION SYNTAX INCORRECT")))


  
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)

let eval_mutop env m = match m with 

  | Def(place, expr) -> (let env1 = (extend_tmp env place) in let var1 = (eval_expr env1 expr) in (update env1 place var1) ;  (* DEF *)
    match var1 with 
    | res -> (env1, (Some res)))
  | Expr(expr1) -> env, Some(eval_expr env expr1)                                                                             (* EXPR *)
  | NoOp -> (env, None)                                                                                                       (* NO_OP*)

