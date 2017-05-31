
%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "SymbolTable.h"

    int FunctionNum = 0;
%}

%start Translation_Unit

%left <token> PLUSPLUS_OP MINUSMINUS_OP
%left <token> MUL_OP DIV_OP MOD_OP
%left <token> MINUS_OP PLUS_OP
%left <token> LT_OP LE_OP GT_OP GE_OP EQUAL_OP NEQUAL_OP
%left <token> NOT_OP
%left <token> AND_OP
%left <token> OR_OP

%token <token> VOID INT CHAR BOOL DOUBLE CONST NUL STRUCT TRUE FALSE
%token <token> CASE DEFAULT DO WHILE FOR IF ELSE SWITCH
%token <token> CONTINUE BREAK RETURN
%token INT_CONSTANT
%token DOUBLE_CONSTANT
%token CHAR_CONSTANT
%token STRING_CONSTANT
%token <ident> ID

%token <charVal> ';' ':' '?' '=' '(' ')' '[' ']' '{' '}' '&'
%type <ident> Array

%union {
         int       token ;
         int       intVal;
         double    doubleVal;
         char      charVal ;
         char      *ident;
       }

%%

Translation_Unit:   External_Declaration
                |   Translation_Unit External_Declaration
                ;

External_Declaration:   Function_Definition { FunctionNum++; }
                    |   Declaration
                    ;

Function_Definition:    Function_Declarator  Compound_Statement
                   ;

Function_Declarator:    Non_Void_Type_Specifier ID '(' ')'                   { install_symbol($2); }
                   |    Non_Void_Type_Specifier ID '(' Parameter_List ')'    { install_symbol($2); }
                   |    VOID ID '(' ')'                                      { install_symbol($2); }
                   |    VOID ID '(' Parameter_List ')'                       { install_symbol($2); }
                   ;

Declaration:    Function_Declaration
           |    Const_Declaration
           |    Normal_Declaration
           ;

Function_Declaration:    Function_Declarator ';'
                    ;

Const_Declaration:    CONST Non_Void_Type_Specifier Const_Declarator_List ';'
                 ;

Const_Declarator_List:    Const_Declarator
                     |    Const_Declarator_List ',' Const_Declarator
                     ;

Const_Declarator:    ID '=' INT_CONSTANT        { install_symbol($1); }
                |    ID '=' DOUBLE_CONSTANT     { install_symbol($1); }
                |    ID '=' CHAR_CONSTANT       { install_symbol($1); }
                |    ID '=' STRING_CONSTANT     { install_symbol($1); }
                |    ID '=' TRUE                { install_symbol($1); }
                |    ID '=' FALSE               { install_symbol($1); }
                ;

Normal_Declaration:    Non_Void_Type_Specifier Normal_Declarator_List ';'
                  ;

Normal_Declarator_List:    Normal_Declarator
                      |    Normal_Declarator_List ',' Normal_Declarator
                      ;

Normal_Declarator:    ID                        { install_symbol($1); }
                 |    Array                     { install_symbol($1); }
                 |    ID '=' Init_Expression    { install_symbol($1); }
                 |    Array '=' Array_Content   { install_symbol($1); }
                 ;


Parameter_List:    Parameter
              |    Parameter_List ',' Parameter
              ;

Parameter:    Non_Void_Type_Specifier ID
         |    Non_Void_Type_Specifier Array
         ;

Array:   ID Array_Paranthesis   { $$ = $1; }
      ;

Array_Paranthesis:    '[' INT_CONSTANT ']'
                 |    Array_Paranthesis '[' INT_CONSTANT ']'
                 ;

Array_Content:    '{' '}'
             |    '{' Init_Expression_List '}'
             ;

Array_Expression:    '[' Expression ']'
                |    Array_Expression '[' Expression ']'
                ;

Var:    ID
   |    ID Array_Expression
   ;

Non_Void_Type_Specifier:    INT
                       |    CHAR
                       |    BOOL
                       |    DOUBLE
                       ;






Compound_Statement:   '{' '}'
                  |   '{' Block_Item_List '}'
                  ;

Block_Item_List:    Block_Item
               |    Block_Item_List Block_Item
               ;

Block_Item:    Declaration
          |    Statement
          ;

Statement:    Simple_Statement
         |    Switch_Statement
         |    Selection_Statement
         |    Iteration_Statement
         |    Jump_Statement
         ;

Simple_Statement:    Var '=' Expression ';'
                ;

Switch_Statement:    SWITCH '(' ID ')' '{' Switch_Content '}'
                ;

Switch_Content:    Case_List
              |    Case_List  Default_Content
              ;

Case_List:    Case_Content
         |    Case_List Case_Content
         ;

Case_Content:     CASE INT_CONSTANT ':' Statement_List
            |     CASE CHAR_CONSTANT ':' Statement_List
            |     CASE INT_CONSTANT ':'
            |     CASE CHAR_CONSTANT ':'
            ;

Default_Content:    DEFAULT ':' Statement_List
               |    DEFAULT ':'
               ;

Statement_List:    Statement
              |    Statement_List Statement
              ;

Selection_Statement:    IF '(' Expression ')' Compound_Statement
                   |    IF '(' Expression ')' Compound_Statement ELSE Compound_Statement
                   ;

Iteration_Statement:    WHILE '(' Expression ')' Compound_Statement
                   |    DO Compound_Statement WHILE '(' Expression ')' ';'
                   |    FOR '(' Expression_Statement Expression_Statement ')' Compound_Statement
                   |    FOR '(' Expression_Statement Expression_Statement Expression ')' Compound_Statement
                   ;

Expression_Statement:   ';'
                    |   Expression ';'
                    ;

Jump_Statement:     CONTINUE ';'
              |     BREAK ';'
              |     RETURN Expression ';'
              |     RETURN ';'
              ;






Expression:   Conditional_Expression
          ;

Conditional_Expression:    Logical_Or_Expression
                      |    Logical_Or_Expression '?' Expression ':' Conditional_Expression
                      ;

Logical_Or_Expression:    Logical_And_Expression
                     |    Logical_Or_Expression OR_OP Logical_And_Expression
                     ;

Logical_And_Expression:    Not_Expression
                      |    Logical_And_Expression AND_OP Not_Expression
                      ;

Not_Expression:    Relational_Expression
              |    NOT_OP Relational_Expression
              ;

Relational_Expression:    Additive_Expression
                     |    Relational_Expression LT_OP Additive_Expression
                     |    Relational_Expression LE_OP Additive_Expression
                     |    Relational_Expression GT_OP Additive_Expression
                     |    Relational_Expression GE_OP Additive_Expression
                     |    Relational_Expression EQUAL_OP Additive_Expression
                     |    Relational_Expression NEQUAL_OP Additive_Expression
                     ;

Additive_Expression:    Multiplicative_Expression
                   |    Additive_Expression PLUS_OP Multiplicative_Expression
                   |    Additive_Expression MINUS_OP Multiplicative_Expression
                   ;

Multiplicative_Expression:    Unary_Expression
                         |    Multiplicative_Expression MUL_OP Unary_Expression
                         |    Multiplicative_Expression DIV_OP Unary_Expression
                         |    Multiplicative_Expression MOD_OP Unary_Expression
                         ;

Unary_Expression:    Postfix_Expression
                |    MINUS_OP Postfix_Expression
                ;

Postfix_Expression:    Primary_Expression
                  |    Primary_Expression PLUSPLUS_OP
                  |    Primary_Expression MINUSMINUS_OP
                  ;

Primary_Expression:    Var
                  |    INT_CONSTANT
                  |    DOUBLE_CONSTANT
                  |    CHAR_CONSTANT
                  |    STRING_CONSTANT
                  |    TRUE
                  |    FALSE
                  |    '(' Expression ')'
                  |    ID '(' ')'
                  |    ID '(' Expression_List ')'
                  ;

Expression_List:    Expression
               |    Expression_List ',' Expression
               ;












Init_Expression_List:    Init_Expression
                    |    Init_Expression_List ',' Init_Expression
                    ;
Init_Expression:    Init_Conditional_Expression
               ;
Init_Conditional_Expression:    Init_Logical_Or_Expression
                           |    Init_Logical_Or_Expression '?' Init_Expression ':' Init_Conditional_Expression
                           ;
Init_Logical_Or_Expression:    Init_Logical_And_Expression
                          |    Init_Logical_Or_Expression OR_OP Init_Logical_And_Expression
                          ;
Init_Logical_And_Expression:    Init_Not_Expression
                           |    Init_Logical_And_Expression AND_OP Init_Not_Expression
                           ;
Init_Not_Expression:    Init_Relational_Expression
                   |    NOT_OP Init_Relational_Expression
                   ;
Init_Relational_Expression:    Init_Additive_Expression
                          |    Init_Relational_Expression LT_OP Init_Additive_Expression
                          |    Init_Relational_Expression LE_OP Init_Additive_Expression
                          |    Init_Relational_Expression GT_OP Init_Additive_Expression
                          |    Init_Relational_Expression GE_OP Init_Additive_Expression
                          |    Init_Relational_Expression EQUAL_OP Init_Additive_Expression
                          |    Init_Relational_Expression NEQUAL_OP Init_Additive_Expression
                          ;
Init_Additive_Expression:    Init_Multiplicative_Expression
                        |    Init_Additive_Expression PLUS_OP Init_Multiplicative_Expression
                        |    Init_Additive_Expression MINUS_OP Init_Multiplicative_Expression
                        ;
Init_Multiplicative_Expression:    Init_Unary_Expression
                              |    Init_Multiplicative_Expression MUL_OP Init_Unary_Expression
                              |    Init_Multiplicative_Expression DIV_OP Init_Unary_Expression
                              |    Init_Multiplicative_Expression MOD_OP Init_Unary_Expression
                              ;
Init_Unary_Expression:    Init_Postfix_Expression
                     |    MINUS_OP Init_Postfix_Expression
                     ;
Init_Postfix_Expression:    Init_Primary_Expression
                       |    Init_Primary_Expression PLUSPLUS_OP
                       |    Init_Primary_Expression MINUSMINUS_OP
                       ;
Init_Primary_Expression:    Var
                       |    INT_CONSTANT
                       |    DOUBLE_CONSTANT
                       |    CHAR_CONSTANT
                       |    STRING_CONSTANT
                       |    TRUE
                       |    FALSE
                       |    '(' Init_Expression ')'
                       ;

%%

extern int numL;
extern char buf[1000];
extern char *yytext;

int yyerror( char *msg ) {
	fprintf( stderr, "*** Error at line %d: %s\n", numL, buf );
	fprintf( stderr, "\n" );
	fprintf( stderr, "Unmatched token: %s\n", yytext );
	fprintf( stderr, "*** syntax error\n");
	exit(-1);
}

int main(void)
{
    /* Symbol table init */
    init_symbol_table();

    yyparse();
    if (FunctionNum == 0) yyerror(NULL);
    printf("No syntax error!\n");

    printTable();

    return 0;
}
