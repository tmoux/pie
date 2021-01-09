module C = Concrete
module S = Syntax
open Printf

exception PPError of string

(* 
let rec string_of_prog p = match p with
  | [] -> ""
  | [d] -> string_of_decl d
  | d::ds ->
      string_of_decl d ^ "\n" ^ string_of_prog ds
and string_of_decl d = match d with
  | C.Claim (x,ty) -> "(claim " ^ x ^ " " ^ string_of_term ty ^ ")"
  | C.Define (x,e) -> "(define " ^ x ^ " " ^ string_of_term e ^ ")"
  | C.Normalize t -> sprintf "(normalize %s)" (string_of_term t)
and string_of_term t = match t with
  | C.Univ -> "U"
  | C.Pi (x,t1,t2) -> "(Pi (" ^ x ^ " " ^ string_of_term t1 ^ ") " ^ string_of_term t2 ^ ")"
  | C.Arr (t1,t2) -> "(-> " ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | C.Trivial -> "Trivial"
  | C.Annot (e,ty) -> "(the " ^ string_of_term ty ^ " " ^ string_of_term e ^ ")"
  | C.Var x -> x
  | C.Lambda (x,e) -> "(lambda " ^ x ^ " " ^ string_of_term e ^ ")"
  | C.App (e1,e2) -> "(" ^ string_of_term e1 ^ " " ^ string_of_term e2 ^ ")"
  | C.Sole -> "sole"
  | C.Nat -> "Nat"
  | C.Zero -> "zero"
  | C.Add1 x -> sprintf "(add1 %s)" (string_of_term x)
  | C.IndNat (n, mot, base, step) -> sprintf "(ind-Nat %s %s %s %s)" (string_of_term n) (string_of_term mot) (string_of_term base) (string_of_term step)
*)

let rec string_of_cterm : S.c_term -> string = function
  | S.Synth e -> string_of_sterm e
  | S.Lambda e -> "(lambda # " ^ string_of_cterm e ^ ")" 
  | S.Sole -> "Sole"
  | S.Zero -> "zero"
  | S.Add1 x -> if (is_number (S.Add1 x))
                then (string_of_int (term_to_decimal (S.Add1 x)))
                else sprintf "(add1 %s)" (string_of_cterm x)
  | S.Same x -> sprintf "(same %s)" (string_of_cterm x)
and string_of_sterm : S.s_term -> string = function
  | S.Univ -> "U"
  | S.Pi (t1, t2) -> sprintf "(Pi %s %s)" (string_of_cterm t1) (string_of_cterm t2)
  | S.Trivial -> "Trivial"
  | S.Annot (e, t) -> sprintf "(the %s %s)" (string_of_cterm t) (string_of_cterm e)
  | S.BVar i -> sprintf "#%d" i
  | S.FVar x -> sprintf "free %s" (string_of_name x)
  | S.App (e1, e2) -> sprintf "(%s %s)" (string_of_sterm e1) (string_of_cterm e2)
  | S.Nat -> "Nat"
  | S.IndNat (n, mot, base, step) -> sprintf "(ind-Nat %s %s %s %s)" (string_of_cterm n) (string_of_cterm mot) (string_of_cterm base) (string_of_cterm step)
  | S.Equal (ty, e1, e2) -> sprintf "(= %s %s %s)" (string_of_cterm ty) (string_of_cterm e1) (string_of_cterm e2)
  | S.Symm e -> sprintf "(same %s)" (string_of_sterm e)
and string_of_name n = match n with
  | Global s -> s
  | Local x -> sprintf "local %d" x
  | Quote x -> sprintf "quote %d" x
and is_number e = match e with
  | S.Zero -> true
  | S.Add1 x -> is_number x
  | _ -> false
and term_to_decimal n = match n with
  | S.Zero -> 0
  | S.Add1 x -> 1 + term_to_decimal x
  | _ -> raise (PPError "printing error: not a nat")
