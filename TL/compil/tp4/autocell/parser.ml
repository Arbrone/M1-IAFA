type token =
  | EOF
  | DIMENSIONS
  | END
  | OF
  | ASSIGN
  | COMMA
  | LBRACKET
  | RBRACKET
  | DOT_DOT
  | DOT
  | LPARENT
  | RPARENT
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | IF
  | THEN
  | ELSE
  | ELSIF
  | EQ
  | NE
  | GT
  | GE
  | LT
  | LE
  | ID of (string)
  | INT of (int)

open Parsing;;
let _ = parse_error;;
# 17 "parser.mly"

open Common
open Ast
open Printf
open Symbols

(** Raise a syntax error with the given message.
	@param msg	Message of the error. *)
let error msg =
	raise (SyntaxError msg)


(** Restructure the when assignment into selections.
	@param f	Function to build the assignment.
	@param v	Initial values.
	@param ws	Sequence of (condition, expression).
	@return		Built statement. *)
let rec make_when f v ws =
	match ws with
	| [] ->	f v
	| (c, nv)::t ->
		IF_THEN(c, f v, make_when f nv t)

# 59 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* DIMENSIONS *);
  258 (* END *);
  259 (* OF *);
  260 (* ASSIGN *);
  261 (* COMMA *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* DOT_DOT *);
  265 (* DOT *);
  266 (* LPARENT *);
  267 (* RPARENT *);
  268 (* ADD *);
  269 (* SUB *);
  270 (* MUL *);
  271 (* DIV *);
  272 (* MOD *);
  273 (* IF *);
  274 (* THEN *);
  275 (* ELSE *);
  276 (* ELSIF *);
  277 (* EQ *);
  278 (* NE *);
  279 (* GT *);
  280 (* GE *);
  281 (* LT *);
  282 (* LE *);
    0|]

let yytransl_block = [|
  283 (* ID *);
  284 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\006\000\006\000\006\000\006\000\006\000\007\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\012\000\012\000\012\000\
\012\000\008\000\008\000\008\000\009\000\009\000\009\000\009\000\
\009\000\009\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\003\000\003\000\005\000\007\000\009\000\005\000\001\000\001\000\
\001\000\001\000\003\000\002\000\002\000\001\000\003\000\003\000\
\003\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\018\000\017\000\016\000\
\000\000\000\000\000\000\015\000\026\000\000\000\001\000\008\000\
\000\000\006\000\000\000\000\000\020\000\021\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\027\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\024\000\025\000\014\000\011\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\013\000"

let yydgoto = "\002\000\
\004\000\009\000\021\000\010\000\011\000\022\000\032\000\033\000\
\034\000\035\000\036\000\037\000"

let yysindex = "\005\000\
\243\254\000\000\022\255\000\000\030\255\031\255\033\255\046\255\
\028\255\062\255\000\000\010\255\043\255\255\254\045\255\065\255\
\000\000\047\255\253\254\070\255\076\000\255\254\073\255\000\000\
\050\255\074\255\253\254\253\254\253\254\000\000\000\000\000\000\
\027\255\063\255\254\254\000\000\000\000\253\254\000\000\000\000\
\253\254\000\000\052\255\006\255\000\000\000\000\253\254\253\254\
\253\254\253\254\253\254\253\254\253\254\253\254\255\254\253\254\
\253\254\253\254\048\255\048\255\075\255\000\000\000\000\000\000\
\048\255\048\255\048\255\048\255\048\255\048\255\002\255\000\000\
\000\000\000\000\000\000\000\000\255\254\253\254\081\255\066\255\
\000\000\255\254\083\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\084\255\000\000\000\000\000\000\087\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\255\000\000\
\000\000\000\000\027\000\032\000\000\000\000\000\000\000\000\000\
\071\255\072\255\076\255\077\255\078\255\079\255\000\000\000\000\
\000\000\000\000\000\000\000\000\086\255\000\000\000\000\000\000\
\000\000\086\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\236\255\000\000\077\000\000\000\242\255\249\255\
\013\000\041\000\000\000\008\000"

let yytablesize = 315
let yytable = "\023\000\
\022\000\040\000\018\000\076\000\018\000\001\000\027\000\023\000\
\028\000\029\000\007\000\056\000\057\000\058\000\003\000\019\000\
\062\000\047\000\048\000\044\000\077\000\078\000\005\000\030\000\
\031\000\020\000\010\000\007\000\007\000\014\000\059\000\009\000\
\006\000\060\000\071\000\012\000\007\000\016\000\047\000\048\000\
\023\000\065\000\066\000\067\000\068\000\069\000\070\000\049\000\
\050\000\051\000\052\000\053\000\054\000\013\000\063\000\064\000\
\079\000\007\000\008\000\047\000\048\000\083\000\023\000\072\000\
\073\000\074\000\015\000\023\000\045\000\046\000\017\000\007\000\
\025\000\038\000\026\000\039\000\041\000\042\000\043\000\061\000\
\055\000\075\000\081\000\082\000\084\000\003\000\007\000\007\000\
\029\000\030\000\080\000\024\000\000\000\031\000\032\000\033\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000\000\000\000\000\022\000\000\000\
\000\000\000\000\000\000\022\000\022\000\022\000\000\000\000\000\
\000\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\010\000\000\000\000\000\000\000\
\010\000\009\000\000\000\000\000\000\000\009\000\007\000\000\000\
\000\000\000\000\000\000\010\000\000\000\010\000\010\000\000\000\
\009\000\000\000\009\000\009\000\000\000\010\000\000\000\007\000\
\007\000\000\000\009\000"

let yycheck = "\014\000\
\000\000\022\000\006\001\002\001\006\001\001\000\010\001\022\000\
\012\001\013\001\002\001\014\001\015\001\016\001\028\001\017\001\
\011\001\012\001\013\001\027\000\019\001\020\001\001\001\027\001\
\028\001\027\001\000\000\019\001\020\001\002\001\038\000\000\000\
\003\001\041\000\055\000\003\001\000\000\028\001\012\001\013\001\
\055\000\049\000\050\000\051\000\052\000\053\000\054\000\021\001\
\022\001\023\001\024\001\025\001\026\001\008\001\047\000\048\000\
\077\000\027\001\028\001\012\001\013\001\082\000\077\000\056\000\
\057\000\058\000\005\001\082\000\028\000\029\000\028\001\027\001\
\008\001\004\001\028\001\000\000\004\001\028\001\005\001\028\001\
\018\001\007\001\002\001\018\001\002\001\002\001\000\000\002\001\
\018\001\018\001\078\000\015\000\255\255\018\001\018\001\018\001\
\018\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\013\001\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\002\001\255\255\255\255\255\255\
\006\001\002\001\255\255\255\255\255\255\006\001\002\001\255\255\
\255\255\255\255\255\255\017\001\255\255\019\001\020\001\255\255\
\017\001\255\255\019\001\020\001\255\255\027\001\255\255\019\001\
\020\001\255\255\027\001"

let yynames_const = "\
  EOF\000\
  DIMENSIONS\000\
  END\000\
  OF\000\
  ASSIGN\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  DOT_DOT\000\
  DOT\000\
  LPARENT\000\
  RPARENT\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  MOD\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ELSIF\000\
  EQ\000\
  NE\000\
  GT\000\
  GE\000\
  LT\000\
  LE\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'config) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 88 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 288 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 96 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 299 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 101 "parser.mly"
  ( set_fields _1 )
# 306 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 106 "parser.mly"
  ( [_1] )
# 313 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 108 "parser.mly"
  (_3 :: _1 )
# 321 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 113 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 333 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
  ( NOP )
# 339 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 123 "parser.mly"
  ( SEQ (_1, _2) )
# 347 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 130 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 359 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 136 "parser.mly"
  (
			SET_VAR (declare_var _1, _3)
		)
# 369 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 141 "parser.mly"
        (
			IF_THEN(_2,_4,NOP)
        )
# 379 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 146 "parser.mly"
        (
			IF_THEN(_2,_4,_6)
        )
# 390 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 151 "parser.mly"
        (
                IF_THEN(_2,_4,NOP);
                IF_THEN(_6,_8,NOP);
        )
# 403 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 160 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 415 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 169 "parser.mly"
                ( _1 )
# 422 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 172 "parser.mly"
                 ( CELL (0, fst _1, snd _1) )
# 429 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 174 "parser.mly"
                 ( CST _1 )
# 436 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 177 "parser.mly"
                 ( VAR (get_var _1) )
# 443 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 180 "parser.mly"
                ( _2 )
# 450 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic) in
    Obj.repr(
# 186 "parser.mly"
                ( _2 )
# 457 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic) in
    Obj.repr(
# 188 "parser.mly"
                ( NEG (_2))
# 464 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic) in
    Obj.repr(
# 193 "parser.mly"
               ( _1 )
# 471 "parser.ml"
               : 'lower))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 196 "parser.mly"
                ( BINOP (OP_MUL, _1, _3) )
# 479 "parser.ml"
               : 'lower))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 199 "parser.mly"
                ( BINOP (OP_DIV, _1, _3) )
# 487 "parser.ml"
               : 'lower))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 202 "parser.mly"
                ( BINOP (OP_MOD, _1, _3) )
# 495 "parser.ml"
               : 'lower))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 207 "parser.mly"
  ( _1 )
# 502 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 210 "parser.mly"
                ( BINOP (OP_ADD, _1, _3) )
# 510 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 213 "parser.mly"
                ( BINOP (OP_SUB, _1, _3) )
# 518 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 219 "parser.mly"
                ( COMP (COMP_EQ, _1, _3) )
# 526 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 222 "parser.mly"
                ( COMP (COMP_NE, _1, _3) )
# 534 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 225 "parser.mly"
                ( COMP (COMP_GT, _1, _3) )
# 542 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 228 "parser.mly"
                ( COMP (COMP_GE, _1, _3) )
# 550 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 231 "parser.mly"
                ( COMP (COMP_LT, _1, _3) )
# 558 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 234 "parser.mly"
                ( COMP (COMP_LE, _1, _3) )
# 566 "parser.ml"
               : 'condition))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
