type token =
  | T_bool
  | T_break
  | T_byref
  | T_char
  | T_continue
  | T_delete
  | T_double
  | T_else
  | T_for
  | T_false
  | T_if
  | T_int
  | T_new
  | T_NULL
  | T_return
  | T_true
  | T_void
  | T_eof
  | T_intconst of (int)
  | T_doubleconst of (float)
  | T_id of (string)
  | T_charconst of (char)
  | T_stringliteral of (string)
  | T_assign
  | T_eq
  | T_neq
  | T_gt
  | T_lt
  | T_ge
  | T_le
  | T_plus
  | T_minus
  | T_times
  | T_div
  | T_mod
  | T_bitand
  | T_bitnot
  | T_and
  | T_or
  | T_q
  | T_colon
  | T_comma
  | T_plusplus
  | T_minusminus
  | T_pluseq
  | T_minuseq
  | T_timeseq
  | T_diveq
  | T_modeq
  | T_semicol
  | T_lparen
  | T_rparen
  | T_lbracket
  | T_rbracket
  | T_lcurl
  | T_rcurl

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
