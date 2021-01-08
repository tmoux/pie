%{ open Concrete %}
%token EOF
%token L_PARENS
%token R_PARENS
%token <string> STRING

%token CLAIM
%token DEFINE

%token THE

%token U

%token PI

%token ARROW
%token LAMBDA

%token TRIVIAL
%token SOLE

%start <Concrete.program> prog
%%

prog:
  | EOF
    { [] }
  | d = decl; p = prog
    { d :: p }

decl:
  | L_PARENS; CLAIM; s = STRING; t = term; R_PARENS
    { Claim (s, t) }
  | L_PARENS; DEFINE; s = STRING; t = term; R_PARENS
    { Define (s, t) }

term:
  | U
    { Univ }
  | TRIVIAL
    { Trivial }
  | L_PARENS; PI; L_PARENS; x = STRING; t1 = term; R_PARENS; t2 = term; R_PARENS
    { Pi (x, t1, t2) }
  | L_PARENS; ARROW; t1 = term; t2 = term; R_PARENS
    { Arr (t1, t2) }
  | L_PARENS; THE; ty = term; t = term; R_PARENS
    { Annot (t, ty) }
  | s = STRING
    { Var s }
  | L_PARENS; LAMBDA; x = STRING; t = term; R_PARENS
    { Lambda (x, t) }
  | L_PARENS; t = appseq; R_PARENS
    { t }
  | SOLE
    { Sole }

appseq:
  | t1 = term; t2 = term;
    { App(t1, t2) }
  | t1 = appseq; t2 = term;
    { App(t1, t2) }
