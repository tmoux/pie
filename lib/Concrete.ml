type program = decl list
and decl =
  | Claim of string * term
  | Define of string * term
  | Normalize of term
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
  | Nat
  | Zero
  | Add1 of term
  | IndNat of term * term * term * term
  | Equal of term * term * term
  | Same of term
  | Symm of term
  | Sigma of string * term * term
  | Pair of term * term
  | Cons of term * term
  | Car of term
  | Cdr of term

let rec convert_num n =
  if n = 0 then Zero
           else Add1 (convert_num (n-1))
