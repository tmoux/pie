module C = Concrete
module S = Syntax
module PP = PrettyPrint
module N = Normalize
open Printf

(* Creates an AST from the concrete syntax.
 * Replaces variables with DeBrujin indices,
 * desugars non-dependent functions/pairs,
 * and expands user-defined definitions *)

type top_level =
  { defined : (string, S.c_term * S.c_term) Hashtbl.t;
    claimed : (string, S.c_term) Hashtbl.t;
  }

exception ConversionError of string

let binding_index : string list -> string -> int option =
  fun bindings s ->
  let rec go i bindings s = match bindings with
    | [] -> None
    | x::xs -> if x = s then Some i else go (i+1) xs s
  in go 0 bindings s

let rec run_program : C.program -> unit =
  let tl = {defined = Hashtbl.create 1; claimed = Hashtbl.create 1} in
  let rec go = function
  | [] -> ()
  | d::ds -> (* print_endline (PP.string_of_decl d);*) run_decl tl d; go ds
  in go
and run_decl : top_level -> C.decl -> unit =
  fun tl -> function
  | Claim (s,ty) ->
      if Hashtbl.mem tl.claimed s || Hashtbl.mem tl.defined s
        then raise (ConversionError "already claimed")
        else let ty' = desugar_cterm tl [] ty in
             (try
                Typecheck.check0 ty' VUniv;
                Hashtbl.add tl.claimed s ty'
              with 
                Typecheck.TypeError _ -> raise (Typecheck.TypeError "not a type")
             ) 
            (* print_endline (PP.string_of_cterm ty') *)
  | Define (s,t) ->
      if Hashtbl.mem tl.defined s
        then raise (ConversionError "already defined")
        else (match Hashtbl.find_opt tl.claimed s with
          | Some ty' ->
              let t' = desugar_cterm tl [] t in
              (try (* print_endline (PP.string_of_cterm t'); *)
                   let ty'' = N.normalize ty' in
                   Typecheck.check0 t' ty'';
                   Hashtbl.add tl.defined s (t',ty')
               with
              Typecheck.TypeError err -> raise (Typecheck.TypeError err));
          | None -> raise (ConversionError "not claimed yet"))
  | Normalize t ->
      let t' = desugar_cterm tl [] t in
      (match t' with
      | Synth t' -> 
        (try let ty = Typecheck.synth [] 0 t' in
             Typecheck.check0 (Synth t') ty;
             print_endline (sprintf "normalize %s: %s"
              (PP.string_of_cterm (Synth t'))
              (PP.string_of_cterm (N.quote0 (N.normalize (Synth t')))))
            with
            | Typecheck.TypeError err -> raise (Typecheck.TypeError err)
            | Normalize.NormError err -> raise (Normalize.NormError err))
      | _ -> raise (ConversionError "can't determine a type, try adding an annotation"))
and desugar_sterm : top_level -> string list -> C.term -> S.s_term =
  fun tl bindings t -> match t with
  | Univ -> Univ
  | Pi (x, t1, t2) ->
      let t1' = desugar_cterm tl bindings t1 in
      let t2' = desugar_cterm tl (x::bindings) t2 in
      Pi(t1', t2')
  | Arr (t1, t2) ->
      let t1' = desugar_cterm tl bindings t1 in
      let t2' = desugar_cterm tl ("_"::bindings) t2 in
      Pi(t1', t2')
  | Trivial -> Trivial
  | Annot (t',ty) -> Annot (desugar_cterm tl bindings t', desugar_cterm tl bindings ty)
  | Var s -> (match binding_index bindings s with
    | Some i -> BVar i
    | None -> (match Hashtbl.find_opt tl.defined s with
      | Some (t',ty) -> Annot (t',ty)
      | None -> raise (ConversionError "unbound variable")))
  | App (t1, t2) -> App (desugar_sterm tl bindings t1, desugar_cterm tl bindings t2)
  | Nat -> Nat
  | IndNat (n, mot, base, step) -> IndNat (desugar_cterm tl bindings n, desugar_cterm tl bindings mot, desugar_cterm tl bindings base, desugar_cterm tl bindings step)
  | _ -> raise (ConversionError "couldn't convert concrete term")
and desugar_cterm : top_level -> string list -> C.term -> S.c_term =
  fun tl bindings t -> match t with
  | Lambda (x,t') -> Lambda (desugar_cterm tl (x::bindings) t')
  | Sole -> Sole
  | Zero -> Zero
  | Add1 x -> Add1 (desugar_cterm tl bindings x)
  | _ -> Synth (desugar_sterm tl bindings t)
