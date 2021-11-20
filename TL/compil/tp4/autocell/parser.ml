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

# 58 "parser.ml"
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
  276 (* EQ *);
  277 (* NE *);
  278 (* GT *);
  279 (* GE *);
  280 (* LT *);
  281 (* LE *);
    0|]

let yytransl_block = [|
  282 (* ID *);
  283 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\003\000\003\000\006\000\006\000\008\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\012\000\012\000\012\000\012\000\
\009\000\009\000\009\000\007\000\007\000\007\000\007\000\007\000\
\007\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\006\000\002\000\003\000\003\000\005\000\001\000\001\000\001\000\
\001\000\003\000\002\000\002\000\001\000\003\000\003\000\003\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\017\000\016\000\
\000\000\015\000\000\000\000\000\014\000\025\000\010\000\000\000\
\001\000\008\000\000\000\006\000\000\000\000\000\019\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\026\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\023\000\024\000\013\000\000\000\009\000"

let yydgoto = "\002\000\
\004\000\009\000\022\000\010\000\011\000\023\000\033\000\034\000\
\035\000\036\000\037\000\038\000"

let yysindex = "\004\000\
\232\254\000\000\007\255\000\000\012\255\024\255\015\255\014\255\
\022\255\027\255\000\000\002\255\019\255\017\255\035\255\055\255\
\000\000\042\255\004\255\017\255\066\255\071\000\017\255\068\255\
\000\000\046\255\069\255\004\255\004\255\004\255\000\000\000\000\
\057\255\000\000\032\255\033\255\000\000\000\000\000\000\004\255\
\000\000\000\000\004\255\000\000\049\255\047\255\000\000\000\000\
\017\255\004\255\004\255\004\255\004\255\004\255\004\255\004\255\
\004\255\004\255\004\255\004\255\053\255\053\255\070\255\000\000\
\076\255\000\000\000\000\053\255\053\255\053\255\053\255\053\255\
\053\255\000\000\000\000\000\000\000\000\017\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\077\255\000\000\000\000\000\000\080\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\079\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\027\000\000\000\000\000\
\000\000\000\000\000\000\064\255\065\255\067\255\071\255\072\255\
\073\255\000\000\000\000\000\000\000\000\002\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\240\255\000\000\069\000\000\000\000\000\242\255\
\241\255\038\000\000\000\217\255"

let yytablesize = 309
let yytable = "\024\000\
\021\000\007\000\003\000\039\000\001\000\024\000\042\000\005\000\
\024\000\018\000\066\000\067\000\046\000\028\000\006\000\029\000\
\030\000\012\000\074\000\075\000\076\000\013\000\018\000\014\000\
\061\000\012\000\011\000\062\000\016\000\031\000\032\000\015\000\
\065\000\019\000\024\000\020\000\068\000\069\000\070\000\071\000\
\072\000\073\000\021\000\050\000\051\000\017\000\058\000\059\000\
\060\000\007\000\008\000\052\000\053\000\054\000\055\000\056\000\
\057\000\064\000\050\000\051\000\007\000\079\000\026\000\024\000\
\050\000\051\000\047\000\048\000\027\000\040\000\041\000\043\000\
\044\000\045\000\049\000\063\000\077\000\078\000\003\000\007\000\
\007\000\028\000\029\000\025\000\030\000\000\000\000\000\000\000\
\031\000\032\000\033\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\007\000\000\000\000\000\021\000\000\000\
\000\000\000\000\000\000\021\000\021\000\021\000\000\000\000\000\
\000\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\012\000\011\000\000\000\000\000\012\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\000\011\000\012\000\011\000\000\000\000\000\
\000\000\000\000\000\000\012\000\011\000"

let yycheck = "\014\000\
\000\000\000\000\027\001\020\000\001\000\020\000\023\000\001\001\
\023\000\006\001\050\000\051\000\028\000\010\001\003\001\012\001\
\013\001\003\001\058\000\059\000\060\000\008\001\006\001\002\001\
\040\000\000\000\000\000\043\000\027\001\026\001\027\001\005\001\
\049\000\017\001\049\000\019\001\052\000\053\000\054\000\055\000\
\056\000\057\000\026\001\012\001\013\001\027\001\014\001\015\001\
\016\001\026\001\027\001\020\001\021\001\022\001\023\001\024\001\
\025\001\011\001\012\001\013\001\026\001\078\000\008\001\078\000\
\012\001\013\001\029\000\030\000\027\001\004\001\000\000\004\001\
\027\001\005\001\018\001\027\001\007\001\002\001\002\001\000\000\
\002\001\018\001\018\001\015\000\018\001\255\255\255\255\255\255\
\018\001\018\001\018\001\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\002\001\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\013\001\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\002\001\002\001\255\255\255\255\006\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\017\001\019\001\019\001\255\255\255\255\
\255\255\255\255\255\255\026\001\026\001"

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
# 87 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 280 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 291 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 100 "parser.mly"
  ( set_fields _1 )
# 298 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 105 "parser.mly"
  ( [_1] )
# 305 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 107 "parser.mly"
  (_3 :: _1 )
# 313 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 325 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
  ( NOP )
# 331 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 122 "parser.mly"
  ( SEQ (_1, _2) )
# 339 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 125 "parser.mly"
        (
			IF_THEN(_2, _4, _6)
        )
# 350 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 129 "parser.mly"
        (
			NOP
        )
# 359 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 138 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 371 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 144 "parser.mly"
  (
			SET_VAR (declare_var _1, _3)
		)
# 381 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 152 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 393 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 161 "parser.mly"
                ( _1 )
# 400 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 164 "parser.mly"
                 ( CELL (0, fst _1, snd _1) )
# 407 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 166 "parser.mly"
                 ( CST _1 )
# 414 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 169 "parser.mly"
                 ( VAR (get_var _1) )
# 421 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 172 "parser.mly"
                ( _2 )
# 428 "parser.ml"
               : 'atomic))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic) in
    Obj.repr(
# 178 "parser.mly"
                ( _2 )
# 435 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic) in
    Obj.repr(
# 180 "parser.mly"
                ( NEG (_2))
# 442 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic) in
    Obj.repr(
# 185 "parser.mly"
               ( _1 )
# 449 "parser.ml"
               : 'lower))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 188 "parser.mly"
                ( BINOP (OP_MUL, _1, _3) )
# 457 "parser.ml"
               : 'lower))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 191 "parser.mly"
                ( BINOP (OP_DIV, _1, _3) )
# 465 "parser.ml"
               : 'lower))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 194 "parser.mly"
                ( BINOP (OP_MOD, _1, _3) )
# 473 "parser.ml"
               : 'lower))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 199 "parser.mly"
  ( _1 )
# 480 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 202 "parser.mly"
                ( BINOP (OP_ADD, _1, _3) )
# 488 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lower) in
    Obj.repr(
# 205 "parser.mly"
                ( BINOP (OP_SUB, _1, _3) )
# 496 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 211 "parser.mly"
                ( COMP (COMP_EQ, _1, _3) )
# 504 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 214 "parser.mly"
                ( COMP (COMP_NE, _1, _3) )
# 512 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 217 "parser.mly"
                ( COMP (COMP_GT, _1, _3) )
# 520 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 220 "parser.mly"
                ( COMP (COMP_GE, _1, _3) )
# 528 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 223 "parser.mly"
                ( COMP (COMP_LT, _1, _3) )
# 536 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 226 "parser.mly"
                ( COMP (COMP_LE, _1, _3) )
# 544 "parser.ml"
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
