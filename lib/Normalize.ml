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
  | Nat -> VNat
  | IndNat (n, mot, base, step) ->
     let motVal = c_norm env mot in
     let baseVal = c_norm env base in
     let stepVal = c_norm env step in
     let rec recurse k = (match k with
       | VZero -> baseVal
       | VAdd1 x -> vapp (vapp stepVal x) (recurse x)
       | VNeutral n -> VNeutral (NNatElim (n, motVal, baseVal, stepVal))
       | _ -> raise (NormError "error ind-Nat")
       )
     in recurse (c_norm env n)
  | Equal (ty, e1, e2) -> VEqual (c_norm env ty, c_norm env e1, c_norm env e2)
  | Symm e -> s_norm env e
  | Sigma (t1, t2) ->
      let t1' = c_norm env t1 in
      let t2' = fun x -> c_norm (x::env) t2 in
      VSigma (t1', t2')
  (* Probably need to add Neutral case for these eliminators *)
  | Car e ->
      (match s_norm env e with
      | VCons (e1, _) -> e1
      | _ -> raise (NormError "car: not a pair"))
  | Cdr e ->
      (match s_norm env e with
      | VCons (_, e2) -> e2
      | _ -> raise (NormError "car: not a pair"))

and c_norm : value list -> c_term -> value =
  fun env -> function
  | Synth e -> s_norm env e
  | Lambda e -> VLambda (fun x -> c_norm (x::env) e)
  | Sole -> VSole
  | Zero -> VZero
  | Add1 x -> VAdd1 (c_norm env x)
  | Same x -> VSame (c_norm env x)
  | Cons (e1, e2) -> VCons (c_norm env e1, c_norm env e2)
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
  | VNat -> Synth Nat
  | VZero -> Zero
  | VAdd1 x -> Add1 (quote i x)
  | VEqual (ty, v1, v2) -> Synth (Equal (quote i ty, quote i v1, quote i v2))
  | VSame v -> Same (quote i v)
  | VSigma (v1, v2) -> Synth (Sigma (quote i v1, quote (i+1) (v2 (vfree (Quote i)))))
  | VCons (v1, v2) -> Cons (quote i v1, quote i v2)
and quoteNeutral : int -> neutral -> s_term =
  fun i -> function
  | NFree x -> boundfree i x
  | NApp (n, v) -> App (quoteNeutral i n, quote i v)
  | NNatElim (n, mot, base, step) ->
      IndNat (Synth (quoteNeutral i n), quote i mot, quote i base, quote i step)
and boundfree : int -> name -> s_term =
  fun i -> function
  | Quote k -> BVar (i-k-1)
  | x -> FVar x
and quote0 : value -> c_term =
  fun v -> quote 0 v
