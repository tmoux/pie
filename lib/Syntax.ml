type name =
  | Global of string
  | Local of int
  | Quote of int

type ty =
  | TTrivial
  | TFree of name
  | TArr of ty * ty

let string_of_name n = match n with
  | Global s -> s
  | Local x | Quote x -> string_of_int x

type s_term =
  | Univ
  | Pi of c_term * c_term
  | Trivial
  | Annot of c_term * c_term
  | BVar of int
  | FVar of name
  | App of s_term * c_term
and c_term =
  | Synth of s_term
  | Lambda of c_term
  | Sole

(* Values *)
type value =
  | VLambda of (value -> value)
  | VUniv 
  | VPi of value * (value -> value)
  | VNeutral of neutral
  | VTrivial
  | VSole
and neutral =
  | NFree of name
  | NApp of neutral * value

