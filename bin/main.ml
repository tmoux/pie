open Lib

(*
let tfree x = TFree (Global x)
let id = Annot (Lambda (Synth (BVar 0)), TArr (tfree "a", tfree "a"))
let const = Annot (Lambda (Lambda (Synth (BVar 1))), TArr (tfree "a", TArr (tfree "b", tfree "a")))
let env1 = [ (Global "x", HasType (tfree "a"))
           ; (Global "b", HasKind Star)
           ; (Global "a", HasKind Star)];;

print_endline (string_of_type (synth env1 0 id));;
print_endline (string_of_type (synth env1 0 const));;
print_endline (string_of_type (synth env1 0 (App (const,Synth (FVar (Global "x"))))));;
*)
open Core
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Grammar.prog Lexer.token lexbuf with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Grammar.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let run_file filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let program = parse_with_error lexbuf in
  Desugar.run_program program;
  In_channel.close inx

let main = 
  let filename = ((Sys.get_argv) ()).(1) in
  run_file filename

