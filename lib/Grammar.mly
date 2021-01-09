%{ open Concrete %}
%token EOF
%token L_PARENS
%token R_PARENS
%token <string> STRING
%token <int> NUMBER

%token CLAIM
%token DEFINE
%token NORMALIZE

%token THE

%token U

%token PI

%token ARROW
%token LAMBDA

%token TRIVIAL
%token SOLE

%token NAT
%token ZERO
%token ADD1
%token INDNAT

%token EQUALS
%token SAME
%token SYMM

%token SIGMA
%token PAIR
%token CONS
%token CAR
%token CDR

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
  | L_PARENS; NORMALIZE; t = term; R_PARENS
    { Normalize t } 

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
  | NAT
    { Nat }
  | ZERO
    { Zero }
  | L_PARENS; ADD1; t = term; R_PARENS
    { Add1 t }
  | L_PARENS; INDNAT; n = term; mot = term; base = term; step = term; R_PARENS
    { IndNat (n, mot, base, step) }
  | x = NUMBER
    { convert_num x }
  | L_PARENS; EQUALS; ty = term; e1 = term; e2 = term; R_PARENS
    { Equal (ty, e1, e2) }
  | L_PARENS; SAME; x = term; R_PARENS
    { Same x }
  | L_PARENS; SYMM; x = term; R_PARENS
    { Symm x }
  | L_PARENS; SIGMA; L_PARENS; x = STRING; t1 = term; R_PARENS; t2 = term; R_PARENS;
    { Sigma (x, t1, t2) }
  | L_PARENS; PAIR; t1 = term; t2 = term; R_PARENS;
    { Pair (t1, t2) }
  | L_PARENS; CONS; t1 = term; t2 = term; R_PARENS;
    { Cons (t1, t2) }
  | L_PARENS; CAR; t = term; R_PARENS;
    { Car t }
  | L_PARENS; CDR; t = term; R_PARENS;
    { Cdr t }

appseq:
  | t1 = term; t2 = term;
    { App(t1, t2) }
  | t1 = appseq; t2 = term;
    { App(t1, t2) }
