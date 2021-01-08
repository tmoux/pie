type program = decl list
and decl =
  | Claim of string * term
  | Define of string * term
and term =
  | Univ
  | Pi of string * term * term
  | Arr of term * term
  | Trivial
  | Annot of term * term
  | Var of string
  | Lambda of string * term
  | App of term * term
  | Sole
