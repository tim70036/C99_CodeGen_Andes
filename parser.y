
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
%token <intVal> INT_CONSTANT
%token DOUBLE_CONSTANT
%token CHAR_CONSTANT
%token STRING_CONSTANT
%token <ident> ID

%token <charVal> ';' ':' '?' '=' '(' ')' '[' ']' '{' '}' '&'
%type <ident> Array Function_Declarator Function_Declaration Declaration Var Primary_Expression

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
                    |   Declaration         { /*set_global_vars($1);*/ }
                    ;

Function_Definition:    Function_Declarator '{'
                        {
                            cur_scope++;
                            set_scope_and_offset_of_param($1);
                        }
                        Declaration_List
                        {
                            set_local_vars($1);
                        }
                        Statement_List '}'
                        {
                            //pop_up_symbol(cur_scope);
                            cur_scope--;
                        }
                   ;

Function_Declarator:    Non_Void_Type_Specifier ID       { install_symbol($2); }
                        '(' Parameter_List ')'           { $$ = $2; }
                   |    VOID ID                          { install_symbol($2); }
                        '(' Parameter_List ')'           { $$ = $2; }
                   ;

Declaration:    Function_Declaration    { $$ = $1; }
           |    Const_Declaration       { $$ = NULL; }
           |    Normal_Declaration      { $$ = NULL; }
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

Parameter:    Non_Void_Type_Specifier ID        { install_symbol($2); }
         |    Non_Void_Type_Specifier Array     { install_symbol($2); }
         |    /* empty */
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

Var:    ID                      { $$ = $1; }
   |    ID Array_Expression     { $$ = $1; }
   ;

Non_Void_Type_Specifier:    INT
                       |    CHAR
                       |    BOOL
                       |    DOUBLE
                       ;






Compound_Statement:   '{'
                      {
                          cur_scope++;
                      }
                      Declaration_List
                      {

                      }
                      Statement_List '}'
                      {
                          //pop_up_symbol(cur_scope);
                          cur_scope--;
                      }
                  ;

Declaration_List:    /* empty */
                |    Declaration_List Declaration
                ;

Statement:    Simple_Statement
         |    Switch_Statement
         |    Selection_Statement
         |    Iteration_Statement
         |    Jump_Statement
         ;

Simple_Statement:    Var '=' Expression ';'
                     {
                         int index = look_up_symbol($1);

                         /* Load Expression to $r0 */
                         printf("    pop.s { $r0 }\n");
                         printf("    swi $r0, [$sp+%d]\n",table[index].offset*4);
                     }
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
            ;

Default_Content:    DEFAULT ':' Statement_List
               ;

Statement_List:    /* empty */
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
                        {
                            printf("    pop.s { $r0 }\n");
                            printf("    pop.s { $r1 }\n");
                            printf("    add $r0, $r1, $r0\n");
                            printf("    push.s { $r0 }\n");
                        }
                   |    Additive_Expression MINUS_OP Multiplicative_Expression
                        {
                            printf("    pop.s { $r0 }\n");
                            printf("    pop.s { $r1 }\n");
                            printf("    sub $r0, $r1, $r0\n");
                            printf("    push.s { $r0 }\n");
                        }
                   ;

Multiplicative_Expression:    Unary_Expression
                         |    Multiplicative_Expression MUL_OP Unary_Expression
                              {
                                  printf("    pop.s { $r0 }\n");
                                  printf("    pop.s { $r1 }\n");
                                  printf("    mul $r0, $r1, $r0\n");
                                  printf("    push.s { $r0 }\n");
                              }
                         |    Multiplicative_Expression DIV_OP Unary_Expression
                              {
                                  printf("    pop.s { $r2 }\n");
                                  printf("    pop.s { $r3 }\n");
                                  printf("    divsr $r0, $r1, $r3, $r2\n");
                                  printf("    push.s { $r0 }\n");
                              }
                         |    Multiplicative_Expression MOD_OP Unary_Expression
                              {
                                  printf("    pop.s { $r2 }\n");
                                  printf("    pop.s { $r3 }\n");
                                  printf("    divsr $r0, $r1, $r3, $r2\n");
                                  printf("    push.s { $r1 }\n");
                              }
                         ;

Unary_Expression:    Postfix_Expression
                |    MINUS_OP Postfix_Expression
                ;

Postfix_Expression:    Primary_Expression
                  |    Primary_Expression PLUSPLUS_OP
                  |    Primary_Expression MINUSMINUS_OP
                  ;

Primary_Expression:    Var
                       {
                            int index = look_up_symbol($1);

                            /* Load to r0 */
                            switch(table[index].mode)
                            {
                                case ARGUMENT_MODE:
                                    printf("    lwi $r0, [$sp+%d]\n",table[index].offset*4);
                                    break;
                                case LOCAL_MODE:
                                    printf("    lwi $r0, [$sp+%d]\n",table[index].offset*4);
                                    break;
                                /* global */
                            }

                            /* Push to stack */
                            printf("    push.s { $r0 }\n");
                       }
                  |    INT_CONSTANT
                       {
                            $$ = NULL;

                            /* Move num to $r0 and push to stack */
                            printf("    movi $r0, %d\n",$1);
                            printf("    push.s { $r0 }\n");
                       }
                  |    DOUBLE_CONSTANT                  { $$ = NULL; }
                  |    CHAR_CONSTANT                    { $$ = NULL; }
                  |    STRING_CONSTANT                  { $$ = NULL; }
                  |    TRUE                             { $$ = NULL; }
                  |    FALSE                            { $$ = NULL; }
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
