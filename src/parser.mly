/* yyparse: called once, includes semantics, basically feeds the whole frontend process */
%{

open Ast
open Types
open List

%}

%token T_char T_int
%token T_fun T_ref T_nothing
%token T_if T_then T_else
%token T_and T_not T_or
%token T_return
%token T_var
%token T_while T_do  
%token T_plus T_minus T_times T_mod T_div
%token T_assign T_eq T_neq
%token T_ge T_le T_gt T_lt
%token T_lparen T_rparen
%token T_lbracket T_rbracket
%token T_lcurl T_rcurl
%token T_colon T_comma T_semicol
%token<char> T_charconst
%token<string> T_id T_stringliteral
%token<int> T_intconst 
%token T_eof


/* ======================================
   | About precedence and associativity |
   ======================================

   Precedence (proteraiothta) = the order in which (different) operations are performed 
   Associativity (prosetairistikothta) = operators of the same precedence in the absence of parentheses 

   %left/right declaration makes all operators left/right-associative 
   %nonassoc declares that it is a syntax error to find the same operator twice “in a row”
   
   The relative precedence of different operators is controlled by the order in which they are declared. The first %left
   or %right declaration in the file declares the operators whose precedence is lowest, the next such declaration
   declares the operators whose precedence is a little higher, and so on.

   low precedence
         ||
         ||
         \/
   high precedence
*/

%left T_or                   /* lowest precedence */
%left T_and
%nonassoc T_not
%left T_plus T_minus              
%left T_times T_div T_mod               
// %nonassoc SIGN               /* highest precedence */
// %nonassoc T_then
// %nonassoc T_else


/* ==============================
   | About conflicts resolution |
   ==============================

   The resolution of conflicts works by comparing the precedence of 
   the rule being considered with that of the lookahead token. 
   
   Bison sets the precedence of a rule based on the 
   precedence of the last token in that rule (unless specified otherwise by %prec -see "Note" below). 
   
   If the token's precedence is higher, the choice is to shift. 
   If the rule's precedence is higher, the choice is to reduce.

   (Or in other words we shift or reduce based on which action has the higher precedence, 
    where the "precedence of shift" is the lookahead's one and the "precedence of reducing" it
    the production's one.)

   Note: %prec refers to the whole rule!
   It's telling yacc how to disambiguate that branch of the parse tree 
   relative to other similar branches. See also https://docs.oracle.com/cd/E19504-01/802-5880/6i9k05dh3/index.html 
*/

   

%start<unit> program

%%

program : func_def T_eof {()}
;

func_def : header list(local_def) block {
    FuncDef({ func_def_header=$1; func_def_local=$2; func_def_block=$3;}) 
};

header : T_fun T_id T_lparen separated_list(T_semicol, fpar_def) T_rparen T_colon ret_type {
     FuncDecl ({ header_id=$2; header_defs=$4; header_ret=$7; })
};

fpar_def : option(T_ref) separated_nonempty_list(T_comma, T_id) T_colon fpar_type {

};

%inline data_type : T_int { TYPE_int }| T_char { TYPE_char } ; 
ttype : data_type list(delimited(T_lbracket, T_intconst, T_rbracket)) {
     let f sz subarr = TYPE_array ({ttype=subarr; size=sz}) in fold_right f $2 $1
     (* A[2][3][4] becomes arr(arr(arr(int,sz=4),sz=3),sz=2) *)
}; 
%inline ret_type : data_type { Some($1) } | T_nothing { None } ; 
fpar_type : data_type option(pair(T_lbracket,T_rbracket)) list(delimited(T_lbracket, T_intconst, T_rbracket)) {} ;

local_def : func_def { $1 } | func_decl { $1 } | var_def { $1 } ;

func_decl : header T_semicol { $1 } ;

var_def : T_var separated_nonempty_list(T_comma, T_id) T_colon ttype T_semicol 
          {VarDef {var_def_id=$2; var_def_ret=$4; }} 
; 

stmt : T_semicol { EmptyStmt } (* todo none *)
     | lvalue T_assign expr T_semicol { Assign({lvalue=$1; rvalue=$3}) }
     | block { Block($1) }
     | func_call T_semicol { StmtFuncCall($1) }
     | T_if cond T_then stmt option(preceded(T_else, stmt)) { If {if_cond = $2; ifstmt = $4; elsestmt = $5} } 
     | T_while cond T_do stmt { While {while_cond = $2 ; whilestmt = $4} } 
     | T_return option(expr) T_semicol { Return $2 } 
;

block : T_lcurl list(stmt) T_rcurl { $2 } ;

func_call : fname=T_id; T_lparen; params=separated_list(T_comma, expr); T_rparen 
          { FuncCall({name=fname; parameters=params}) } ;

lvalue : T_id { LvalueId($1) }
       | T_stringliteral { LvalueString($1) }
       | lvalue delimited(T_lbracket, expr, T_rbracket) { LvalueArr ($1, $2) }
;

expr : T_intconst { Int($1) }
     | T_charconst { Char($1) }
     | lvalue { Lvalue($1) }
     | delimited(T_lparen, expr, T_rparen) { $1 } 
     | func_call { ExprFuncCall($1) } 
     | sign expr { SignedExpr ($1, $2) } 
     | expr; arithmetic_bop; expr { BinExpr ($2, $1, $3) }
;

cond : delimited(T_lparen, cond, T_rparen) { $1 } (* ??? *)
     | T_not cond { NegatedCond($2) } 
     | cond logical_bop cond { CompoundCond ($2, $1, $3) }
     | expr comparison_bop expr { ExprCond ($2, $1, $3) }
;

%inline sign : T_plus { UPlus } | T_minus { UMinus } ;
%inline arithmetic_bop : T_plus { Plus } | T_minus { Minus } | T_times { Times } | T_div { Div } | T_mod { Mod } ;
%inline logical_bop : T_and { And } | T_or { Or } ;
%inline comparison_bop : T_eq { Eq } | T_neq { Neq } | T_lt { Lt } | T_gt { Gt } | T_ge { Ge } | T_le { Le } ;