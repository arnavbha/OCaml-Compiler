open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let lparen = Str.regexp "(" in
	let rparen = Str.regexp ")" in
	let eq = Str.regexp "=" in 
	let noteq = Str.regexp "<>" in
  let greater = Str.regexp ">" in
	let less = Str.regexp "<" in
	let greatereq = Str.regexp ">=" in
	let lesseq = Str.regexp "<=" in
  let l_or = Str.regexp "||" in
	let l_and = Str.regexp "&&" in
	let l_not = Str.regexp "not" in
	let l_if = Str.regexp "if" in
  let l_then = Str.regexp "then" in
	let l_else = Str.regexp "else" in
  let l_plus = Str.regexp "+" in
	let l_sub = Str.regexp "-" in
	let l_mult = Str.regexp "*" in
	let l_div = Str.regexp "/" in
  let l_concat = Str.regexp "\\^" in
  let l_let = Str.regexp "let" in 
  let l_def = Str.regexp "def" in 
  let l_in = Str.regexp "in" in
  let l_rec = Str.regexp "rec" in 
  let l_fun = Str.regexp "fun" in 
  let l_arrow = Str.regexp "->" in 
  let l_doublesemi = Str.regexp ";;" in 
  let l_posint = Str.regexp "[0-9]+" in
  let l_negint = Str.regexp "(-[0-9]+)" in
  let l_bool = Str.regexp "true\\|false" in 
  let l_string = Str.regexp "\"\\([^\"]*\\)\"" in
  let l_emptystr = Str.regexp "[ \t\n]+" in 
  let l_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in 
  let l_id2 = Str.regexp "[a-zA-Z0-9]+" in

let rec tok_helper index str = 
  if String.length str <= index then []

  else if(Str.string_match l_posint str index) then  (*positive int*)
    let same = (Str.matched_string str) in 
    Tok_Int (int_of_string same)::(tok_helper (Str.match_end()) str)

  else if (Str.string_match l_negint str index) then  (*negative int*)
    let same = (Str.matched_string str) in
    let x = (-2) + (String.length same) in 
    let new_str = String.sub same 1 x in
    Tok_Int(int_of_string new_str)::(tok_helper (Str.match_end()) str)

  else if (Str.string_match lparen str index) then (Tok_LParen)::(tok_helper (Str.match_end()) str) (* ( *)
  else if (Str.string_match rparen str index) then (Tok_RParen)::(tok_helper (Str.match_end()) str) (* ) *)
  else if (Str.string_match l_arrow str index) then (Tok_Arrow)::(tok_helper (Str.match_end()) str) (* -> *)
  else if (Str.string_match noteq str index) then (Tok_NotEqual)::(tok_helper (Str.match_end()) str) (* <> *)
  else if (Str.string_match greatereq str index) then (Tok_GreaterEqual)::(tok_helper (Str.match_end()) str) (* >= *)
  else if (Str.string_match lesseq str index) then (Tok_LessEqual)::(tok_helper (Str.match_end()) str) (* <= *)
  else if (Str.string_match greater str index) then (Tok_Greater)::(tok_helper (Str.match_end()) str) (* > *)
  else if (Str.string_match eq str index) then (Tok_Equal)::(tok_helper (Str.match_end()) str)      (*equals*)
  else if (Str.string_match less str index) then (Tok_Less)::(tok_helper (Str.match_end()) str)     (* < *)
  else if (Str.string_match l_or str index) then (Tok_Or)::(tok_helper (Str.match_end()) str)       (* || *)
  else if (Str.string_match l_and str index) then (Tok_And)::(tok_helper (Str.match_end()) str) (* && *)
  else if (Str.string_match l_plus str index) then (Tok_Add)::(tok_helper (Str.match_end()) str) (* + *)
  else if (Str.string_match l_sub str index) then (Tok_Sub)::(tok_helper (Str.match_end()) str) (* - *)
  else if (Str.string_match l_mult str index) then (Tok_Mult)::(tok_helper (Str.match_end()) str) (* * *)
  else if (Str.string_match l_div str index) then (Tok_Div)::(tok_helper (Str.match_end()) str) (* / *)
  else if (Str.string_match l_concat str index) then (Tok_Concat)::(tok_helper (Str.match_end()) str) (* ^ *)
  else if (Str.string_match l_doublesemi str index) then (Tok_DoubleSemi)::(tok_helper (Str.match_end()) str) (* ;; *)

  (* keyword so that id does not catch it *)

(* not *)

else if (Str.string_match l_not str index) then
  let tok = Str.matched_string str in
  let last = (Str.match_end()) in
  if (Str.string_match l_id2 str last) then
    let id = Str.matched_string str in 
    (Tok_ID(tok^id))::(tok_helper (Str.match_end()) str)
  else
    Tok_Not::(tok_helper last str)

(* if *)

else if (Str.string_match l_if str index) then
  let tok = Str.matched_string str in
  let last = (Str.match_end()) in
  if (Str.string_match l_id2 str last) then
    let id = Str.matched_string str in 
    (Tok_ID(tok^id))::(tok_helper (Str.match_end()) str)
  else
    Tok_If::(tok_helper last str)

(* then *)

else if (Str.string_match l_then str index) then
  let tok = Str.matched_string str in
  let last = (Str.match_end()) in
  if (Str.string_match l_id2 str last) then
    let id = Str.matched_string str in 
    (Tok_ID(tok^id))::(tok_helper (Str.match_end()) str)
  else
    Tok_Then::(tok_helper last str)

(* else *)

else if (Str.string_match l_else str index) then
  let tok = Str.matched_string str in
  let last = (Str.match_end()) in
  if (Str.string_match l_id2 str last) then
    let id = Str.matched_string str in 
    (Tok_ID(tok^id))::(tok_helper (Str.match_end()) str)
  else
    Tok_Else::(tok_helper last str)

(* let *)

else if (Str.string_match l_let str index) then
  let tok = Str.matched_string str in
  let last = (Str.match_end()) in
  if (Str.string_match l_id2 str last) then
    let id = Str.matched_string str in 
    (Tok_ID(tok^id))::(tok_helper (Str.match_end()) str)
  else
    Tok_Let::(tok_helper last str)

(* def *)

else if (Str.string_match l_def str index) then
  let tok = Str.matched_string str in
  let last = (Str.match_end()) in
  if (Str.string_match l_id2 str last) then
    let id = Str.matched_string str in 
    (Tok_ID(tok^id))::(tok_helper (Str.match_end()) str)
  else
    Tok_Def::(tok_helper last str)

(* in *)

else if (Str.string_match l_in str index) then
  let tok = Str.matched_string str in
  let last = (Str.match_end()) in
  if (Str.string_match l_id2 str last) then
    let id = Str.matched_string str in 
    (Tok_ID(tok^id))::(tok_helper (Str.match_end()) str)
  else
    Tok_In::(tok_helper last str)

(* rec *)

else if (Str.string_match l_rec str index) then
  let tok = Str.matched_string str in
  let last = (Str.match_end()) in
  if (Str.string_match l_id2 str last) then
    let id = Str.matched_string str in 
    (Tok_ID(tok^id))::(tok_helper (Str.match_end()) str)
  else
    Tok_Rec::(tok_helper last str)

(* fun *)

else if (Str.string_match l_fun str index) then
  let tok = Str.matched_string str in
  let last = (Str.match_end()) in
  if (Str.string_match l_id2 str last) then
    let id = Str.matched_string str in 
    (Tok_ID(tok^id))::(tok_helper (Str.match_end()) str)
  else
    Tok_Fun::(tok_helper last str)
  
  (* complex rules *)

else if (Str.string_match l_string str index) then   
  let same = (Str.matched_string str) in
  let groupmatch = (Str.matched_group 1 str) in
  let x = index + (String.length same) in 
  Tok_String(groupmatch)::(tok_helper x str)


else if (Str.string_match l_bool str index) then
  let same = (Str.matched_string str) in
  Tok_Bool(bool_of_string same)::(tok_helper (Str.match_end()) str)


else if (Str.string_match l_id str index) then
  let same = (Str.matched_string str) in
  let x = index + (String.length same) in
  Tok_ID(same)::(tok_helper x str)


else if (Str.string_match l_emptystr str index) then
  tok_helper (Str.match_end()) str

else
  raise (InvalidInputException "ERROR IN LEXING")

  in tok_helper 0 input

  


	