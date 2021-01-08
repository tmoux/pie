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
and c_subst : int -> s_term -> c_term -> c_term =
  fun i r e -> match e with
  | Synth e' -> Synth (s_subst i r e')
  | Lambda e' -> Lambda (c_subst (i+1) r e')
  | Sole -> Sole

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
    | _ -> raise (TypeError "type mismatch")
and check0 e ty = check [] 0 e ty
