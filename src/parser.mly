/* yyparse: called once, includes semantics, basically feeds the whole frontend process */
%{
    open Ast
    open Types

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
%token<string> T_id
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
%type<Ast.declaration list> program
%type<declaration list> declaration
%type<expr> expression
%type<expr list> expression_list
%type<statement> statement
%type<label> optional_label_semi
%type<label> optional_for_label
%type<expr> optional_expression
%type<const_expr> constant_expression
%type<Types.typ> ttype
%type<expr> binary_assignment

%%

/* Naming convension: E_list = E repeated 1 or more times
 *                    optional_E = E repeated 0 or 1 times
 *                    (thus optional_list_E = E repeated 0 or more times)
 */

program : optional_declaration_list T_eof { List.rev $1 } //
;

optional_declaration_list : /* nothing */ { [] }
                          | optional_declaration_list declaration { $2 @ $1 }
; 

// decl actually returns a list because of variable declarations but we don't care 
// since it's gonna end up in optional list decl anyway
declaration : variable_declaration { $1 } 
            | function_declaration { [$1] } 
            | function_definition  { [$1] }           
;

inside_brackets: optional_declaration_list optional_statement_list { (List.rev $1, $2) } /* check this ??????? */ 
;

declarator : T_id  { ($1,ConstOne) } 
           | T_id T_lbracket constant_expression T_rbracket { ($1, $3) }
;

declarator_list : declarator { $1::[] }
                | declarator T_comma declarator_list { $1::$3 } 
;

variable_declaration : ttype declarator_list T_semicol 
                        { let f (id,sz) = 
                        match sz with 
                        | ConstOne -> (* this is not an array, pass the declarator's type as it's type *)
                           VarDeclaration ({var_decl_typ=$1; var_decl_name=id})
                        | constant_e -> (* this is an array of size constant_e; set it's type as TYPE_array of it's declarator's type *) 
                           (* Note that the evaluation of the constant expression as int happens during the AST construction *)
                           VarDeclaration ({
                              var_decl_typ=TYPE_array({ttype=$1; size=eval_const constant_e}); 
                              var_decl_name=id})
                        in List.map f (List.rev $2) } 
;

/* In ttype we want to enforce shifting (even if this is the default action for yacc).
   For this we need the lookahead token's (T_times) precedence to be higher than 
   the reduce production's one (specified by the %prec). That's why we declare SHIFT_ON_TIMESLIST
   right above the T_times. 
*/
ttype : basic_type optional_T_times_list %prec SHIFT_ON_TIMESLIST { 
   if $2==0 then $1 else TYPE_ptr ({ttype=$1; level=$2})
};

optional_T_times_list : /* nothing */ { 0 }
                      | optional_T_times_list T_times { $1 + 1 }
;

basic_type : T_int  { TYPE_int }
           | T_char { TYPE_char }
           | T_bool { TYPE_bool }
           | T_double { TYPE_double }
;


function_definition : ttype T_id T_lparen optional_parameter_list T_rparen 
                        T_lcurl inside_brackets T_rcurl 
                        { FuncDef ({ func_def_return_type=$1; func_def_name=$2; func_def_parameters=$4; func_def_body=$7 } ) }
                    | T_void T_id T_lparen optional_parameter_list T_rparen 
                        T_lcurl inside_brackets T_rcurl 
                        { FuncDef ({ func_def_return_type=TYPE_void; func_def_name=$2; func_def_parameters=$4; func_def_body=$7 } ) }

function_declaration : ttype T_id T_lparen optional_parameter_list T_rparen T_semicol 
                     { FuncDecl ({ func_decl_return_type=$1; func_decl_name=$2; func_decl_parameters=$4 }) } 
                     | T_void T_id T_lparen optional_parameter_list T_rparen T_semicol
                     { FuncDecl ({ func_decl_return_type=TYPE_void; func_decl_name=$2; func_decl_parameters=$4 }) } 
;

parameter_list : parameter { $1::[] }
               | parameter_list T_comma parameter { $3::$1 }
;
optional_parameter_list : /*nothing*/ { [] }
                        | parameter_list { List.rev $1 } 
;

parameter : ttype T_id { (PASS_BY_VALUE,$1,$2) } 
          | T_byref ttype T_id { (PASS_BY_REFERENCE,$2,$3) }
;


optional_expression : /* nothing */ { EmptyExpr }
                    | expression { $1 }
;

/* In [ exp ',' exp ',' exp ] ambiguity (reduce exp as an exp_list or shift ',') 
   we want to enforce shifting.
   For this we need the lookahead token's (T_comma) precedence to be higher than 
   the reduce production's one (specified by the %prec). That's why we declare REDUCE_ON_COMMA
   right above the T_comma. 
*/
expression_list : expression %prec SHIFT_ON_COMMA { $1::[] }
                | expression_list T_comma expression { $3::$1 } /*remember to reverse list!!!!!!!!!!!!!!!!*/
;


statement_list : statement { $1::[] }
               | statement_list statement { $2::$1 } /*remember to reverse when used*/
;


optional_statement_list : /* nothing */ { [] }
                        | statement_list { $1 }
;

optional_for_label : /* nothing */ { "" } // TODO handle empty labels not as strings
                   | T_id T_colon { $1 }
;

optional_label_semi : T_semicol { "" }
                    | T_id T_semicol { $1 }
;

/* About dangling-if: To avoid declaring a precedence for T_else (and having to decide 
   where to place it with relation to the other token's precedences),
   I modified the grammar in order to fix the ambiguity.

   For some reason s/r conflicts remained so we ended up giving the reduce rule lower 
   precedence than the T_else.++++++++
*/
statement : matched_if { $1 } 
          | unmatched_if { $1 }
;
          
matched_if : T_if T_lparen expression T_rparen matched_if T_else matched_if 
               { If ({if_cond=$3; ifstmt=$5; elsestmt=$7}) }
           | T_semicol { EmptyStmt }
           | expression T_semicol { Expr $1 }
           | T_lcurl optional_statement_list T_rcurl { StmtList (List.rev $2) }

           | optional_for_label T_for T_lparen optional_expression T_semicol
             optional_expression T_semicol optional_expression 
             T_rparen statement { For ({label=$1; initial=$4; for_cond=$6; update=$8; stmt=$10;}) } 

           | T_continue optional_label_semi { Jump ({name=Continue; label_jump=$2}) }
           | T_break optional_label_semi { Jump ({name=Break; label_jump=$2}) }
           | T_return optional_expression T_semicol { Return $2 }        
;



unmatched_if : T_if T_lparen expression T_rparen statement 
                  { If ({if_cond=$3; ifstmt=$5; elsestmt=EmptyStmt}) }
             | T_if T_lparen expression T_rparen matched_if T_else unmatched_if 
                  { If ({if_cond=$3; ifstmt=$5; elsestmt=$7}) }
;

expression : T_id { Id $1 }
           | T_lparen expression T_rparen { $2 }
         //   | T_lparen ttype T_rparen { () }
           | T_true { Bool true }
           | T_false { Bool false }
           | T_NULL { EmptyExpr }
           | T_intconst { Int $1 }
           | T_charconst { Char $1 }
           | T_doubleconst { Float $1 }
           | T_stringliteral { String $1 }
           | T_id T_lparen T_rparen { FuncCall ({name=$1; parameters=[]}) }
           | T_id T_lparen expression_list T_rparen { FuncCall ({name=$1; parameters=$3}) } //todo: catch foo(100, delete *p, 1)
           | expression T_lbracket expression T_rbracket { Array ({name=$1; size=$3}) } //todo: catch delete *p [new *a]
           | unary_expression { $1 }
           | binary_expression { $1 }
           | unary_assignment { $1 }
           | binary_assignment { $1 }
           | T_lparen ttype T_rparen expression %prec TYPE_CAST { TypeCast ({new_type=$2; casted_expr=$4}) } 
           | expression T_q expression T_colon expression { InlineIf ({cond=$1; true_expr=$3; false_expr=$5}) }
           | T_new ttype optional_new { New ({ttype=$2; size=$3}) } //when not an array, size will be unit
;

unary_expression : T_bitand expression %prec POINTER_REF_DEREF      { UnaryExpr(BitAnd,$2) }
                 | T_times expression /* %prec POINTER_REF_DEREF */ { UnaryExpr(UTimes,$2) }
                 | T_plus expression /* %prec UPLUS_UMINUS */       { UnaryExpr(UPlus,$2)  } 
                 | T_minus expression /* %prec UPLUS_UMINUS */      { UnaryExpr(UMinus,$2) }
                 | T_bitnot expression /* %prec LOG_NOT */          { UnaryExpr(BitNot,$2) }
;

binary_expression : expression T_times expression { BinExpr(Times,$1,$3) }
                  | expression T_div expression   { BinExpr(Div,$1,$3)   }
                  | expression T_mod expression   { BinExpr(Mod,$1,$3)   }
                  | expression T_plus expression  { BinExpr(Plus,$1,$3)  }
                  | expression T_minus expression { BinExpr(Minus,$1,$3) }
                  | expression T_lt expression    { BinExpr(Lt,$1,$3)    }
                  | expression T_gt expression    { BinExpr(Gt,$1,$3)    }
                  | expression T_le expression    { BinExpr(Le,$1,$3)    }
                  | expression T_ge expression    { BinExpr(Ge,$1,$3)    }
                  | expression T_eq expression    { BinExpr(Eq,$1,$3)    }
                  | expression T_neq expression   { BinExpr(Neq,$1,$3)   }
                  | expression T_and expression   { BinExpr(And,$1,$3)   }
                  | expression T_or expression    { BinExpr(Or,$1,$3)    }
                  | expression T_comma expression { BinExpr(Comma,$1,$3) }
;

unary_assignment : T_plusplus expression %prec PREFIX   { UnaryAssign(PrePlusPlus,$2)    }
                 | T_minusminus expression %prec PREFIX { UnaryAssign(PreMinusMinus,$2)  }
                 | expression T_plusplus                { UnaryAssign(PostPlusPlus,$1)   }
                 | expression T_minusminus              { UnaryAssign(PostMinusMinus,$1) }
;

binary_assignment : expression T_assign expression  { BinAssign(Assign, $1, $3)  }
                  | expression T_timeseq expression { BinAssign(TimesEq, $1, $3) }
                  | expression T_diveq expression   { BinAssign(DivEq, $1, $3)   }
                  | expression T_modeq expression   { BinAssign(ModEq, $1, $3)   }
                  | expression T_pluseq expression  { BinAssign(PlusEq, $1, $3)  }
                  | expression T_minuseq expression { BinAssign(MinusEq, $1, $3) }

/* When an [ T_new ttype . '[' ] is encountered, there is a shift/reduce conflict 
   between reducing epsilon as optional_new or shifting an T_lbrace, 
   in which we want to enforce shifting (even if this is the default action for yacc).
   For this we need the lookahead token's (T_lbrace) precedence to be higher than 
   the reduce production's one (specified by the %prec). That's why we declare SHIFT_ON_NEW
   right above the T_lbrace. 
*/
optional_new : /*nothing*/ %prec SHIFT_ON_NEW { EmptyExpr }
             | T_lbracket expression T_rbracket { $2 }
;

constant_expression : expression { ConstExpr $1 } /*symbol table*/
;