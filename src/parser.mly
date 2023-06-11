/* yyparse: called once, includes semantics, basically feeds the whole frontend process */
%{


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

func_def : | header; a = list(local_def); block {()}
;

header : T_fun T_id T_lparen separated_list(T_semicol, fpar_def) T_rparen T_colon ret_type {}
;

fpar_def : option(T_ref) separated_nonempty_list(T_comma, T_id) T_colon fpar_type {}
;

%inline data_type : T_int | T_char {} ;
ttype : data_type list(delimited(T_lbracket, T_intconst, T_rbracket)) {} ;
%inline ret_type : data_type | T_nothing {} ;
fpar_type : data_type option(pair(T_lbracket,T_rbracket)) list(delimited(T_lbracket, T_intconst, T_rbracket)) {} ;

local_def : func_def | func_decl | var_def {} ;

func_decl : header T_semicol {} ;

var_def : T_var separated_nonempty_list(T_comma, T_id) T_colon ttype T_semicol {} 
; 

stmt : T_semicol {} 
     | lvalue T_assign expr T_semicol {}
     | block {}
     | func_call T_semicol {}
     | T_if cond T_then stmt option(preceded(T_else, stmt)) {}
     | T_while cond T_do stmt {}
     | T_return option(expr) T_semicol {}
;

block : T_lcurl list(stmt) T_rcurl {} ;

func_call : T_id T_lparen separated_list(T_comma, expr) T_rparen {} ;

lvalue : T_id {}
       | T_stringliteral {}
       | lvalue delimited(T_lbracket, expr, T_rbracket) {}
;

expr : T_intconst {}
     | T_charconst {}
     | lvalue {}
     | delimited(T_lparen, expr, T_rparen) {} 
     | func_call {} 
     | sign expr {} 
     | expr arithmetic_bop expr {}
;

cond : delimited(T_lparen, cond, T_rparen) {}
     | T_not cond {} 
     | cond logical_bop cond {}
     | expr comparison_bop expr {}
;

%inline sign : T_plus {} | T_minus {} ;
%inline arithmetic_bop : T_plus {} | T_minus {} | T_times {} | T_div {} | T_mod {} ;
%inline logical_bop : T_and | T_or {} ;
%inline comparison_bop : T_eq | T_neq | T_lt | T_gt | T_ge | T_le {} ;