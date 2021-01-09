open Syntax
open Normalize
open PrettyPrint

(* Typechecking is split up between inferring a type of a term (synth) and checking that an expression has a particular type (check) *) 

exception TypeError of string

(* Typechecking *)

type context = (name * value) list

let rec lookup x ls =
  match ls with
  | [] -> None
  | ((k,v)::ls') ->
    if x = k then Some v
                      else lookup x ls'


let rec s_subst : int -> s_term -> s_term -> s_term =
  fun i r -> function
  | Univ -> Univ
  | Pi (t1, t2) -> Pi (c_subst i r t1, c_subst (i+1) r t2)
  | Trivial -> Trivial
  | Annot (e',t) -> Annot (c_subst i r e', c_subst i r t)
  | BVar j -> if i = j then r else BVar j
  | FVar x -> FVar x
  | App (e1, e2) -> App (s_subst i r e1, c_subst i r e2)
  | Nat -> Nat
  | IndNat (n, mot, base, step) -> IndNat (c_subst i r n, c_subst i r mot, c_subst i r base, c_subst i r step)
  | Equal (ty, e1, e2) -> Equal (c_subst i r ty, c_subst i r e1, c_subst i r e2)
  | Symm e -> Symm (s_subst i r e)
  | Sigma (t1, t2) -> Sigma (c_subst i r t1, c_subst (i+1) r t2)
  | Car e -> Car (s_subst i r e)
  | Cdr e -> Cdr (s_subst i r e)
and c_subst : int -> s_term -> c_term -> c_term =
  fun i r -> function
  | Synth e' -> Synth (s_subst i r e')
  | Lambda e' -> Lambda (c_subst (i+1) r e')
  | Sole -> Sole
  | Zero -> Zero
  | Add1 x -> Add1 (c_subst i r x)
  | Same e -> Same (c_subst i r e)
  | Cons (e1, e2) -> Cons (c_subst i r e1, c_subst i r e2)

let rec synth : context -> int -> s_term -> value
  = fun ctx i -> function
    | Univ -> VUniv (* inconsistent! *)
    | Pi (t1, t2) ->
        check ctx i t1 VUniv;
        let t1' = normalize t1 in
        check ((Local i, t1')::ctx) (i+1) (c_subst 0 (FVar (Local i)) t2) VUniv;
        VUniv
    | Trivial -> VUniv
    | Annot (e', ty) ->
        check ctx i ty VUniv;
        let ty' = normalize ty in
        check ctx i e' ty';
        ty'
    | FVar x -> (match lookup x ctx with
      | Some ty' -> ty'
      | _ -> raise (TypeError "unbound variable"))
    | App (e1, e2) ->
      let ty' = synth ctx i e1 in
      (match ty' with
      | VPi (ty1,ty2) ->
          check ctx i e2 ty1;
          ty2 (normalize e2)
      | _ -> raise (TypeError "expected Pi type"))
    | Nat -> VUniv
    | IndNat (n, mot, base, step) ->
        check ctx i mot (VPi (VNat, fun _ -> VUniv));
        let motV = normalize mot in
        check ctx i base (vapp motV VZero);
        check ctx i step (VPi (VNat, (fun l -> VPi (vapp motV l, fun _ -> vapp motV (VAdd1 l)))));
        check ctx i n VNat;
        (vapp motV (normalize n))
    | Equal (ty, e1, e2) ->
        check ctx i ty VUniv;
        let ty' = normalize ty in
        check ctx i e1 ty';
        check ctx i e2 ty';
        VUniv
    | Symm e ->
        (match synth ctx i e with
        | VEqual (ty, e1, e2) -> VEqual (ty, e2, e1)
        | _ -> raise (TypeError "not an = type"))
    | Sigma (t1, t2) ->
        check ctx i t1 VUniv;
        let t1' = normalize t1 in
        check ((Local i, t1')::ctx) (i+1) (c_subst 0 (FVar (Local i)) t2) VUniv;
        VUniv
    | Car e ->
        (match synth ctx i e with
        | VSigma (t1, _) -> t1
        | _ -> raise (TypeError "car: expression is not a Sigma type"))
    | Cdr e ->
        (match synth ctx i e with
        | VSigma (_, t2) ->
            (match normalize (Synth e) with
             | VCons (v1, _) -> t2 v1
             | _ -> raise (TypeError "evaluating pair"))
        | _ -> raise (TypeError "car: expression is not a Sigma type"))
    | _ -> raise (TypeError "unreachable")
and check : context -> int -> c_term -> value -> unit
  = fun ctx i e ty -> match (e, ty) with
    | (Synth e', ty) ->
        let ty' = synth ctx i e' in
        if quote0 ty = quote0 ty'
           then ()
           else raise (TypeError (Printf.sprintf "type mismatch: %s %s: %s" (string_of_cterm (quote0 ty)) (string_of_cterm (quote0 ty')) (string_of_sterm e')))
    | (Lambda e', VPi (ty1, ty2)) ->
        check ((Local i, ty1)::ctx) (i+1) (c_subst 0 (FVar (Local i)) e') (ty2 (vfree (Local i)))
    | (Sole,VTrivial) -> ()
    | (Zero, VNat) -> ()
    | (Add1 x, VNat) -> check ctx i x VNat
    | (Same x, VEqual (ty, e1, e2)) ->
        check ctx i x ty;
        let x' = normalize x in
        if quote0 x' = quote0 e1 && quote0 x' = quote0 e2
          then ()
          else raise (TypeError (Printf.sprintf "type mismatch: same\n%s\n%s" (string_of_cterm (quote0 e1)) (string_of_cterm (quote0 e2))))
    | (Cons (e1, e2), VSigma (v1, v2)) ->
        check ctx i e1 v1;
        let e1' = normalize e1 in
        (* print_endline (string_of_cterm (quote0 (v2 e1'))); *)
        check ((Local i, e1')::ctx) (i+1) (c_subst 0 (FVar (Local i)) e2) (v2 e1')
    | _ -> raise (TypeError "type mismatch")
and check0 e ty = check [] 0 e ty
