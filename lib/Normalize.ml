open Syntax

exception NormError of string

let vfree n = VNeutral (NFree n)

let rec s_norm : value list -> s_term -> value =
  fun env -> function
  | Univ -> VUniv 
  | Pi (t1, t2) ->
      let t1' = c_norm env t1 in
      let t2' = fun x -> c_norm (x::env) t2 in
      VPi (t1', t2')
  | Trivial -> VTrivial
  | Annot (e, _) -> c_norm env e
  | BVar i -> List.nth env i
  | FVar x -> vfree x
  | App (e1, e2) ->
      let e1' = s_norm env e1 in
      let e2' = c_norm env e2 in
      vapp e1' e2'
and c_norm : value list -> c_term -> value =
  fun env -> function
  | Synth e -> s_norm env e
  | Lambda e -> VLambda (fun x -> c_norm (x::env) e)
  | Sole -> VSole
and vapp : value -> value -> value =
  fun v1 v2 -> match v1 with
  | VLambda f -> f v2
  | VNeutral n -> VNeutral (NApp (n, v2))
  | _ -> raise (NormError "invalid value application")
and normalize t = c_norm [] t

let rec quote : int -> value -> c_term =
  fun i -> function
  | VLambda f -> Lambda (quote (i+1) (f (vfree (Quote i))))
  | VUniv -> Synth Univ
  | VPi (t1, t2) -> Synth (Pi (quote i t1, quote (i+1) (t2 (vfree (Quote i)))))
  | VNeutral n -> Synth (quoteNeutral i n)
  | VTrivial -> Synth Trivial
  | VSole -> Sole
and quoteNeutral : int -> neutral -> s_term =
  fun i -> function
  | NFree x -> boundfree i x
  | NApp (n, v) -> App (quoteNeutral i n, quote i v)
and boundfree : int -> name -> s_term =
  fun i -> function
  | Quote k -> BVar (k-i-1)
  | x -> FVar x
and quote0 : value -> c_term =
  fun v -> quote 0 v
