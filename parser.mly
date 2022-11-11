/* yyparse: called once, includes semantics, basically feeds the whole frontend process */
%{
    open Printf
    open Ast
%}

%token T_bool 
%token T_break
%token T_byref
%token T_char 
%token T_continue 
%token T_delete 
%token T_double 
%token T_else 
%token T_for 
%token T_false 
%token T_if 
%token T_int 
%token T_new 
%token T_NULL 
%token T_return 
%token T_true 
%token T_void 
%token T_eof 
%token<int> T_intconst 
%token<float> T_doubleconst
%token T_id
%token<char> T_charconst
%token<string> T_stringliteral

//Special chars
%token T_assign     // "="
%token T_eq         // "=="
%token T_neq        // "!="
%token T_gt         // ">"
%token T_lt         // "<" 
%token T_ge         // ">="
%token T_le         // "<="
%token T_plus       // "+"
%token T_minus      // "-"
%token T_times      // "*"
%token T_div        // "/"
%token T_mod        // "%"
%token T_bitand     // "&"
%token T_bitnot     // "!"
%token T_and        // "&&"
%token T_or         // "||"
%token T_q          // "?"
%token T_colon      // ":"
%token T_comma      // ","
%token T_plusplus   // "++"
%token T_minusminus // "--"
%token T_pluseq     // "+="
%token T_minuseq    // "-="
%token T_timeseq    // "*="
%token T_diveq      // "/="
%token T_modeq      // "%="
%token T_semicol    // ";"
%token T_lparen     // "("
%token T_rparen     // ")"
%token T_lbracket   // "["
%token T_rbracket   // "]"
%token T_lcurl      // "{"
%token T_rcurl      // "}"


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

%nonassoc SHIFT_ON_COMMA // comma in expression lists, as opposed to comma as a binary operator 
%left T_comma
%right T_eq, T_pluseq, T_minuseq, T_timeseq, T_modeq, T_diveq
%nonassoc T_q T_colon
%left T_or 
%left T_and
%right T_assign T_neq T_gt T_lt T_le T_ge //????
%left T_plus T_minus
%nonassoc SHIFT_ON_TIMESLIST
%left T_times T_div T_mod
%nonassoc TYPE_CAST
%nonassoc PREFIX // is nonassoc correct??
%nonassoc T_delete // is nonassoc correct??
// T new ??
%nonassoc POINTER_REF_DEREF // UPLUS_UMINUS LOG_NOT
%right T_bitnot // see "State 87" on conflicts.txt and https://en.cppreference.com/w/c/language/operator_precedence
%nonassoc T_plusplus T_minusminus // is nonassoc correct??
%nonassoc SHIFT_ON_NEW
%nonassoc T_lbracket T_rbracket T_lparen T_rparen


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


   

%start program
%type<unit> program
%type<unit> declaration
// %type<expr> binary_assignment

%%

/* Naming convension: E_list = E repeated 1 or more times
 *                    optional_E = E repeated 0 or 1 times
 *                    (thus optional_list_E = E repeated 0 or more times)
 */

program : optional_declaration_list T_eof { () }
;

optional_declaration_list : /* nothing */ { () }
                          | optional_declaration_list declaration { () }
; 

declaration : variable_declaration { () }
            | function_declaration { () } 
            | function_definition { () }           
;

inside_brackets: optional_declaration_list optional_statement_list { () } /* check this ??????? */ 
;

declarator : T_id  { () } 
           | T_id T_lbracket constant_expression T_rbracket { () }
;

declarator_list : declarator { () }
                | declarator T_comma declarator_list { () }
;

variable_declaration : ttype declarator_list T_semicol { () }
;

/* In ttype we want to enforce shifting (even if this is the default action for yacc).
   For this we need the lookahead token's (T_times) precedence to be higher than 
   the reduce production's one (specified by the %prec). That's why we declare SHIFT_ON_TIMESLIST
   right above the T_times. 
*/
ttype : basic_type optional_T_times_list %prec SHIFT_ON_TIMESLIST { () }
;

optional_T_times_list : /*nothing*/ { () }
                      | optional_T_times_list T_times { () }
;

basic_type : T_int  { () }
           | T_char { () }
           | T_bool { () }
           | T_double { () }
;


function_definition : ttype T_id T_lparen optional_parameter_list T_rparen T_lcurl inside_brackets T_rcurl { () }
                    | T_void T_id T_lparen optional_parameter_list T_rparen T_lcurl inside_brackets T_rcurl { () }

function_declaration : ttype T_id T_lparen optional_parameter_list T_rparen T_semicol { () }
                     | T_void T_id T_lparen optional_parameter_list T_rparen T_semicol { () }
;

parameter_list : parameter { () }
               | parameter_list T_comma parameter { () }
;
optional_parameter_list : /*nothing*/ { () }
                        | parameter_list { () }
;

parameter : ttype T_id { () } 
          | T_byref ttype T_id { () }
;


optional_expression : /* nothing */ { () }
                    | expression { () }
;

/* In [ exp ',' exp ',' exp ] ambiguity (reduce exp as an exp_list or shift ',') 
   we want to enforce shifting.
   For this we need the lookahead token's (T_comma) precedence to be higher than 
   the reduce production's one (specified by the %prec). That's why we declare REDUCE_ON_COMMA
   right above the T_comma. 
*/
expression_list : expression %prec SHIFT_ON_COMMA { () }
                | expression_list T_comma expression { () }
;

// optional_expression_list : /* nothing */ { () }
//                          | expression_list T_comma expression { () }
// ;

statement_list : statement { () }
               | statement_list statement { () }
;


optional_statement_list : /* nothing */ { () }
                        | statement_list { () }
;

optional_for_label : /* nothing */ { () }
                   | T_id T_colon { () }
;

optional_label_semi : T_semicol { () }
              | T_id T_semicol { () }
;

/* About dangling-if: To avoid declaring a precedence for T_else (and having to decide 
   where to place it with relation to the other token's precedences),
   I modified the grammar in order to fix the ambiguity.

   For some reason s/r conflicts remained so we ended up giving the reduce rule lower 
   precedence than the T_else.++++++++
*/
statement : matched_if { () } 
          | unmatched_if { () }
;
          
matched_if : T_if T_lparen expression T_rparen matched_if T_else matched_if { () }
           | T_semicol { () }
           | expression T_semicol { () }
           | T_lcurl optional_statement_list T_rcurl { () }
           | optional_for_label T_for T_lparen optional_expression T_semicol optional_expression T_semicol optional_expression T_rparen statement { () } 
           | T_continue optional_label_semi { () }
           | T_break optional_label_semi { () }
           | T_return optional_expression T_semicol { () }        
;

unmatched_if : T_if T_lparen expression T_rparen statement { () }
             | T_if T_lparen expression T_rparen matched_if T_else unmatched_if { () }
;

expression : T_id { () }
           | T_lparen after_lparen { () } 
           | T_true { () }
           | T_false { () }
           | T_NULL { () }
           | T_intconst { () }
           | T_charconst { () }
           | T_doubleconst { () }
           | T_stringliteral { () }
           | T_id T_lparen T_rparen { () }
           | T_id T_lparen expression_list T_rparen { () }
           | expression T_lbracket expression T_rbracket { () } 
           | unary_expression { () }
           | binary_expression { () }
           | unary_assignment { () }
           | binary_assignment { () }
           | T_lparen ttype T_rparen expression %prec TYPE_CAST { () }
           | expression T_q expression T_colon expression{ () }
           | T_new ttype optional_new { () }
           | T_delete expression { () }
;


after_lparen :  expression T_rparen { () }
             | ttype T_rparen { () }
;

unary_expression : T_bitand expression %prec POINTER_REF_DEREF { () }
                 | T_times expression /* %prec POINTER_REF_DEREF */ { () }
                 | T_plus expression /* %prec UPLUS_UMINUS */ { () }
                 | T_minus expression /* %prec UPLUS_UMINUS */ { () }
                 | T_bitnot expression /* %prec LOG_NOT */ { () }
;

binary_expression : expression T_times expression { () }
                  | expression T_div expression { () }
                  | expression T_mod expression { () }
                  | expression T_plus expression { () }
                  | expression T_minus expression { () }
                  | expression T_lt expression { () }
                  | expression T_gt expression { () }
                  | expression T_le expression { () }
                  | expression T_ge expression { () }
                  | expression T_eq expression { () }
                  | expression T_neq expression { () }
                  | expression T_and expression { () }
                  | expression T_or expression { () }
                  | expression T_comma expression { () }
;

unary_assignment : T_plusplus expression %prec PREFIX { () }
                 | T_minusminus expression %prec PREFIX { () }
                 | expression T_plusplus /* %prec POSTFIX */ { () }
                 | expression T_minusminus /* %prec POSTFIX*/  { () }
;

binary_assignment : expression T_assign expression{ (*BinAssign(Assign, $1, $3)*) }
                  | expression T_timeseq expression{ (*BinAssign(TimesEq, $1, $3)*) }
                  | expression T_diveq expression{ (*BinAssign(DivEq, $1, $3)*) }
                  | expression T_modeq expression{ (*BinAssign(ModEq, $1, $3)*) }
                  | expression T_pluseq expression{ (*BinAssign(PlusEq, $1, $3)*) }
                  | expression T_minuseq expression{ (*BinAssign(MinusEq, $1, $3)*) }

/* When an [ T_new ttype . '[' ] is encountered, there is a shift/reduce conflict 
   between reducing epsilon as optional_new or shifting an T_lbrace, 
   in which we want to enforce shifting (even if this is the default action for yacc).
   For this we need the lookahead token's (T_lbrace) precedence to be higher than 
   the reduce production's one (specified by the %prec). That's why we declare SHIFT_ON_NEW
   right above the T_lbrace. 
*/
optional_new : /*nothing*/ %prec SHIFT_ON_NEW { () }
             | T_lbracket expression T_rbracket { () }
;

constant_expression : expression { () } 
;