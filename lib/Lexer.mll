{
open Lexing
open Grammar

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let make_table num elems =
  let table = Hashtbl.create num in
  List.iter (fun (k, v) -> Hashtbl.add table k v) elems;
  table

let keywords =
  make_table 0 [
    ("claim", CLAIM);
    ("define", DEFINE);
    ("normalize", NORMALIZE);
    ("the", THE);
    ("U", U);
    ("Pi", PI);
    ("lambda", LAMBDA);
    ("Trivial", TRIVIAL);
    ("sole", SOLE);
    ("Nat", NAT);
    ("zero", ZERO);
    ("add1", ADD1);
    ("ind-Nat", INDNAT);
  ]
}

let number = ['0'-'9']['0'-'9']*
let whitespace = [' ' '\t']+
let line_ending = '\r' | '\n' | "\r\n"
let atom_first = ['a'-'z' 'A'-'Z' '_']
let atom_next = ['a'-'z' 'A'-'Z' '_' '-' '*' '/' '0'-'9']
let atom = atom_first atom_next*

rule token = parse
  | ";"
    {comment lexbuf}
  | '('
    { L_PARENS }
  | ')'
    { R_PARENS }
  | "->"
    { ARROW }
  | line_ending
    { new_line lexbuf; token lexbuf }
  | whitespace
    { token lexbuf }
  | eof
    { EOF }
  | number
    { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | atom
    {
      let input = lexeme lexbuf in
      begin try
        let kwd = Hashtbl.find keywords input in
        kwd
      with Not_found ->
        (Grammar.STRING input)
      end
    }
  | _
    { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }
and comment = parse
  | line_ending
    { new_line lexbuf; token lexbuf }
  | _
    { comment lexbuf }
