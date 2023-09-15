open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = match toks with

  | Tok_Let::t -> parse_let t
  | Tok_Fun::t -> parse_fun t
  | Tok_If::t -> parse_if t
  | _ -> parse_or toks 

and parse_let toks = 

  let (rest1, recursion) = (parse_recursion toks) in
  let (rest2, parsed) = (parse_prime rest1) in
  let (rest3, expr1) = (parse_expr (match_token rest2 Tok_Equal)) in
  let (rest4, expr2) = (parse_expr (match_token rest3 Tok_In)) in
  let identifier = (match parsed with
    | ID(exprid) -> exprid
    | _ -> raise (InvalidInputException("ID MISSING"))) in
    (rest4, Let(identifier, recursion, expr1, expr2))

and parse_fun toks = 

  let (rest1, parsed) = (match toks with
    | Tok_ID(ident)::t -> (t, ident)
    | _ -> raise (InvalidInputException("ID MISSING IN PARSE_FUN"))) in
  let (rest2, expr) = (parse_expr (match_token rest1 Tok_Arrow)) in
  (rest2, Fun(parsed, expr))

and parse_if toks =

  let (rest1, ifexpr) = (parse_expr toks) in
  let (rest2, thenexpr) = (parse_expr (match_token rest1 Tok_Then)) in
  let (rest3, elseexpr) = (parse_expr (match_token rest2 Tok_Else)) in
  (rest3, If(ifexpr, thenexpr, elseexpr))

and parse_or toks = 
  let (rest1, andexpr) = (parse_and toks) in
  match rest1 with
  | Tok_Or::t -> let (rest2, orexpr) = (parse_or t) in (rest2, Binop(Or, andexpr, orexpr))
  | _ -> (rest1, andexpr)

and parse_equal toks = 
  let (rest1, relexpr) = (parse_relat toks) in
    match rest1 with
    | Tok_Equal::t -> let (rest2, eqexpr) = (parse_equal t) in (rest2, Binop(Equal, relexpr, eqexpr))
    | Tok_NotEqual::t -> let (rest2, eqexpr) = (parse_equal t) in (rest2, Binop(NotEqual, relexpr, eqexpr))
    | _ -> (rest1, relexpr)

and parse_and toks = 
  let (rest1, eqexpr)= parse_equal toks in
  match lookahead rest1 with 
  | Some Tok_And -> let (rest2, andexpr) = parse_and (match_token rest1 Tok_And) in (rest2, Binop(And, eqexpr, andexpr))
  | _ -> (rest1, eqexpr)

and parse_relat toks = 
  let (rest1, addexpr)= parse_addition toks in
    match lookahead rest1 with 
    | Some Tok_Less -> let (rest2, relexpr)= parse_relat (match_token rest1 Tok_Less) in (rest2, Binop(Less, addexpr, relexpr))
    | Some Tok_LessEqual-> let (rest2, relexpr)= parse_relat (match_token rest1 Tok_LessEqual) in (rest2, Binop(LessEqual, addexpr, relexpr))
    | Some Tok_Greater-> let (rest2, relexpr)= parse_relat (match_token rest1 Tok_Greater) in (rest2, Binop(Greater, addexpr, relexpr))
    | Some Tok_GreaterEqual-> let (rest2, relexpr)= parse_relat (match_token rest1 Tok_Greater) in (rest2, Binop(GreaterEqual, addexpr, relexpr))
    | _ -> (rest1, addexpr)

and parse_addition toks = 
  let (rest1, multexpr) = (parse_multiply toks) in
    match rest1 with
    | Tok_Add::t -> let (rest2, addexpr) = (parse_addition t) in (rest2, Binop(Add, multexpr, addexpr))
    | Tok_Sub::t -> let (rest2, addexpr) = (parse_addition t) in (rest2, Binop(Sub, multexpr, addexpr))
    | _ -> (rest1, multexpr)

and parse_multiply toks = 
  let (rest1, conexpr) = (parse_concat toks) in
    match rest1 with
    | Tok_Mult::t -> let (rest2, multexpr) = (parse_multiply t) in (rest2, Binop(Mult, conexpr, multexpr))
    | Tok_Div::t -> let (rest2, multexpr) = (parse_multiply t) in (rest2, Binop(Div, conexpr, multexpr))
    | _ -> (rest1, conexpr)

and parse_concat toks = 
  let (rest1, unaryexpr) = (parse_unary toks) in
    match rest1 with
    | Tok_Concat::t -> let (rest2, conexpr) = (parse_concat t) in (rest2, Binop(Concat, unaryexpr, conexpr))
    | _ -> (rest1, unaryexpr)

and parse_unary toks = match lookahead toks with 
  | Some Tok_Not -> let (funexpr, unaryexpr)= parse_unary (match_token toks Tok_Not) in (funexpr, Not unaryexpr)
  | _ -> (parse_funcCall toks)

and parse_recursion toks = match toks with
    | Tok_Rec::t -> (t, true)
    | _ -> (toks, false)

and parse_funcCall toks = 
  let (rest1, primexpr1) = (parse_prime toks) in
  let funcalled = 
    match (lookahead rest1) with
    | (Some Tok_Int(ranval)) -> true
    | (Some (Tok_Bool(ranval))) -> true
    | (Some (Tok_String(ranval))) -> true
    | (Some Tok_ID(ranval)) -> true
    | (Some Tok_LParen) -> true
    | _ -> false
  in
  if (funcalled) then 
    (let (rest2, primexpr2) = (parse_prime rest1) in
    (rest2, FunctionCall(primexpr1, primexpr2)))
  else
    (rest1, primexpr1) 

and parse_prime toks = match lookahead (toks) with 
  | Some Tok_Int ranval -> (match_token toks (Tok_Int ranval), Value (Int ranval))
  | Some Tok_Bool ranval -> ((match_token toks (Tok_Bool ranval)), Value (Bool ranval))
  | Some Tok_String ranval -> (match_token toks (Tok_String ranval), Value (String ranval))
  | Some Tok_ID ranval -> (match_token toks (Tok_ID ranval), ID (ranval))
  | Some Tok_LParen -> let (val1, val2) = parse_expr (match_token toks (Tok_LParen)) in ((match_token val1 (Tok_RParen)), val2)
  | _ -> raise (InvalidInputException "PARSE_PRIMARY ERROR")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = match toks with
  | Tok_Def::t -> (parse_def t)
  | Tok_DoubleSemi::t -> (t, NoOp)
  | _ -> let (rest1, expr) = (parse_expr toks) in ((match_token rest1 Tok_DoubleSemi), Expr(expr))

and parse_def toks = 
  let (rest1, val1) = (match toks with
    | Tok_ID(val2)::t -> (t, ID(val2))
    | _ -> raise (InvalidInputException("INVALID ID IN PARSE_DEF"))) in

  let (rest2, expr) = (parse_expr ((match_token rest1 Tok_Equal))) in
  let val2 = match val1 with
    | ID(ranval) -> ranval
    | _ -> raise (InvalidInputException("INVALID TOKEN IN PARSE_DEF")) in
  (((match_token rest2 Tok_DoubleSemi)), Def(val2, expr))
