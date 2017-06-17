%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "SymbolTable.h"

    #define Int 87
    #define Double 8787
    #define Char 9487
    #define Bool 5487
    #define Void 6666
    #define String 7878

    int FunctionNum = 0;
    int LocalOffset = 0;
    int LabelNum = 2;
    FILE *f_asm;
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
%type <ident> Init_Primary_Expression Normal_Declarator_List Normal_Declarator
%type <token> Non_Void_Type_Specifier
%type <token>  Expression Init_Expression Postfix_Expression Init_Postfix_Expression
%type <token> Unary_Expression Init_Unary_Expression Multiplicative_Expression Init_Multiplicative_Expression
%type <token> Additive_Expression Init_Additive_Expression

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
                            LocalOffset = 0;
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
                        '(' Parameter_List ')'
                        {
                            /* Setting type of the func */
                            int index = look_up_symbol($2);

                            if($1 == Int)            table[index].type = Int;
                            else if($1 == Double)    table[index].type = Double;
                            else if($1 == Char)      table[index].type = Char;
                            else if($1 == Bool)      table[index].type = Bool;

                            $$ = $2;
                        }
                   |    VOID ID                          { install_symbol($2); }
                        '(' Parameter_List ')'
                        {
                            /* Setting type of the func */
                            int index = look_up_symbol($2);
                            table[index].type = Void;

                            $$ = $2;
                        }
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

Const_Declarator:    ID '=' INT_CONSTANT
                     {
                         install_symbol($1);
                         int index = look_up_symbol($1);

                         /* Setting, use set_local_vars will be too late */
                         table[index].offset = LocalOffset++;
                         table[index].mode  = LOCAL_MODE;

                         /* Pop Int constant to $r0 */
                         PopReg(0);

                         /* Store it to var */
                         fprintf(f_asm, "    swi $r0, [$sp+%d]\n",table[index].offset*4);
                     }
                |    ID '=' DOUBLE_CONSTANT     { install_symbol($1); }
                |    ID '=' CHAR_CONSTANT       { install_symbol($1); }
                |    ID '=' STRING_CONSTANT     { install_symbol($1); }
                |    ID '=' TRUE                { install_symbol($1); }
                |    ID '=' FALSE               { install_symbol($1); }
                ;

Normal_Declaration:    Non_Void_Type_Specifier Normal_Declarator_List ';'
                       {
                           /* Setting type of the var */
                           int index = look_up_symbol($2);

                           if($1 == Int)            table[index].type = Int;
                           else if($1 == Double)    table[index].type = Double;
                           else if($1 == Char)      table[index].type = Char;
                           else if($1 == Bool)      table[index].type = Bool;

                       }
                  ;

Normal_Declarator_List:    Normal_Declarator                                { $$ = $1; }
                      |    Normal_Declarator_List ',' Normal_Declarator     { $$ = $1; }
                      ;

Normal_Declarator:    ID                        { install_symbol($1);  $$ = $1; }
                 |    Array                     { install_symbol($1); $$ = $1; }
                 |    ID '=' Init_Expression
                      {
                          install_symbol($1);  $$ = $1;
                          int index = look_up_symbol($1);

                          /* Setting, use set_local_vars will be too late */
                          table[index].offset = LocalOffset++;
                          table[index].mode  = LOCAL_MODE;

                          /* Pop Expression to $r0 */
                          PopReg(0);

                          /* Store it to var */
                          fprintf(f_asm, "    swi $r0, [$sp+%d]\n",table[index].offset*4);

                      }
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

Non_Void_Type_Specifier:    INT     { $$ = Int; }
                       |    CHAR    { $$ = Char; }
                       |    BOOL    { $$ = Bool; }
                       |    DOUBLE  { $$ = Double; }
                       ;






Compound_Statement:   '{'
                      {
                          cur_scope++;
                          //LocalOffset = 0;
                      }
                      Declaration_List
                      {
                          //set_local_vars($1);
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

                         /* Pop Expression to $r0 */
                         PopReg(0);

                         /* Store it to var */
                         fprintf(f_asm, "    swi $r0, [$sp+%d]\n",table[index].offset*4);
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

Selection_Statement:    IF '(' Expression ')'
                        {
                            /* Pop expression to r0 */
                            PopReg(0);

                            fprintf(f_asm, "    beqz $r0, .L%d\n", LabelNum);
                            /* If true, continue to compound statment */
                        }
                        Compound_Statement Selection_Statement_Tail

Selection_Statement_Tail:    /* empty, only if */
                             {
                                 /* If false, ignore the compound statement. Branch to End */
                                 fprintf(f_asm, ".L%d:\n", LabelNum);
                                 LabelNum++;
                             }
                        |    ELSE
                             {
                                 /* If true, after compound statement. Must ignore else and branch to End */
                                 fprintf(f_asm, "    j .L%d\n", LabelNum+1);

                                 /* If false, ignore the compound statement. Branch to ELSE */
                                 fprintf(f_asm, ".L%d:\n", LabelNum);
                             }
                             Compound_Statement
                             {
                                 /* End */
                                 fprintf(f_asm, ".L%d:\n", LabelNum+1);
                                 LabelNum += 2;
                             }
                        ;

Iteration_Statement:    WHILE
                        {
                            /* Label for checking expression */
                            fprintf(f_asm, ".L%d:\n", LabelNum);
                        }
                        '(' Expression ')'
                        {
                            /* Pop expression, if false go to end */
                            PopReg(0);
                            fprintf(f_asm, "    beqz $r0, .L%d\n", LabelNum+1);
                        }
                        Compound_Statement
                        {
                            /* Jump back to check expression */
                            fprintf(f_asm, "    j .L%d\n", LabelNum);

                            /* End , if false , branch to here */
                            fprintf(f_asm, ".L%d:\n", LabelNum+1);
                            LabelNum += 2;
                        }
                   |    DO
                        {
                            /* Label for compound statement */
                            fprintf(f_asm, ".L%d:\n", LabelNum);
                        }
                        Compound_Statement WHILE '(' Expression ')' ';'
                        {
                            /* Pop expression, if true go back to compound statement */
                            PopReg(0);
                            fprintf(f_asm, "    bnez $r0, .L%d\n", LabelNum);
                            LabelNum++;
                        }
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
                          {
                              PopReg(0);
                              PopReg(1);
                              fprintf(f_asm, "    or $r0, $r0, $r1\n");
                              PushReg(0);
                          }
                     ;

Logical_And_Expression:    Not_Expression
                      |    Logical_And_Expression AND_OP Not_Expression
                           {
                               PopReg(0);
                               PopReg(1);
                               fprintf(f_asm, "    and $r0, $r0, $r1\n");
                               PushReg(0);
                           }
                      ;

Not_Expression:    Relational_Expression
              |    NOT_OP Relational_Expression
                   {
                       PopReg(0);
                       fprintf(f_asm, "    addi $r0, $r0, 0\n"); /* ??? */
                       fprintf(f_asm, "    slti $r0, $r0, 1\n");
                       fprintf(f_asm, "    zeb $r0, $r0\n");
                       PushReg(0);
                   }
              ;

Relational_Expression:    Additive_Expression
                     |    Relational_Expression LT_OP Additive_Expression
                          {
                              PopReg(0);
                              PopReg(1);
                              fprintf(f_asm, "    slts $r0, $r1, $r0\n");
                              fprintf(f_asm, "    zeb $r0, $r0\n");
                              PushReg(0);
                          }
                     |    Relational_Expression LE_OP Additive_Expression
                          {
                              PopReg(0);
                              PopReg(1);
                              fprintf(f_asm, "    slts $r0, $r0, $r1\n"); /* $r1 > $r0 ? */
                              fprintf(f_asm, "    xori $r0, $r0, 1\n"); /* if $r0 == 1, $r0 = 0 */
                              fprintf(f_asm, "    zeb $r0, $r0\n");
                              PushReg(0);
                          }
                     |    Relational_Expression GT_OP Additive_Expression
                          {
                              PopReg(0);
                              PopReg(1);
                              fprintf(f_asm, "    slts $r0, $r0, $r1\n");
                              fprintf(f_asm, "    zeb $r0, $r0\n");
                              PushReg(0);
                          }
                     |    Relational_Expression GE_OP Additive_Expression
                          {
                              PopReg(0);
                              PopReg(1);
                              fprintf(f_asm, "    slts $r0, $r1, $r0\n"); /* $r1 < $r0 ? */
                              fprintf(f_asm, "    xori $r0, $r0, 1\n"); /* if $r0 == 1, $r0 = 0 */
                              fprintf(f_asm, "    zeb $r0, $r0\n");
                              PushReg(0);
                          }
                     |    Relational_Expression EQUAL_OP Additive_Expression
                          {
                              PopReg(0);
                              PopReg(1);
                              fprintf(f_asm, "    xor $r0, $r0, $r1\n"); /* If $r0 == $r1 , then result of xor will be 0 --> < 1 */
                              fprintf(f_asm, "    slti $r0, $r0, 1\n");
                              fprintf(f_asm, "    zeb $r0, $r0\n");
                              PushReg(0);
                          }
                     |    Relational_Expression NEQUAL_OP Additive_Expression
                          {
                              PopReg(0);
                              PopReg(1);
                              fprintf(f_asm, "    xor $r0, $r0, $r1\n"); /* If $r0 != $r1 , then result of xor will be > 0 */
                              fprintf(f_asm, "    movi $r1, 0\n");
                              fprintf(f_asm, "    slt $r0, $r1, $r0\n");
                              fprintf(f_asm, "    zeb $r0, $r0\n");
                              PushReg(0);
                          }
                     ;

Additive_Expression:    Multiplicative_Expression       { $$ = $1; }
                   |    Additive_Expression PLUS_OP Multiplicative_Expression
                        {
                            /* Check type */
                            if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Additive Expression Type Error");
                            $$ = $1;

                            PopReg(0);
                            PopReg(1);
                            fprintf(f_asm, "    add $r0, $r1, $r0\n");
                            PushReg(0);
                        }
                   |    Additive_Expression MINUS_OP Multiplicative_Expression
                        {
                            /* Check type */
                            if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Additive Expression Type Error");
                            $$ = $1;

                            PopReg(0);
                            PopReg(1);
                            fprintf(f_asm, "    sub $r0, $r1, $r0\n");
                            PushReg(0);
                        }
                   ;

Multiplicative_Expression:    Unary_Expression                  { $$ = $1; }
                         |    Multiplicative_Expression MUL_OP Unary_Expression
                              {
                                  /* Check type */
                                  if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Multiplicative Expression Type Error");
                                  $$ = $1;

                                  PopReg(0);
                                  PopReg(1);
                                  fprintf(f_asm, "    mul $r0, $r1, $r0\n");
                                  PushReg(0);
                              }
                         |    Multiplicative_Expression DIV_OP Unary_Expression
                              {
                                  /* Check type */
                                  if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Multiplicative Expression Type Error");
                                  $$ = $1;

                                  PopReg(2);
                                  PopReg(3);
                                  fprintf(f_asm, "    divsr $r0, $r1, $r3, $r2\n");
                                  PushReg(0);
                              }
                         |    Multiplicative_Expression MOD_OP Unary_Expression
                              {
                                  /* Check type */
                                  if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Multiplicative Expression Type Error");
                                  $$ = $1;

                                  PopReg(2);
                                  PopReg(3);
                                  fprintf(f_asm, "    divsr $r0, $r1, $r3, $r2\n");
                                  PushReg(1);
                              }
                         ;

Unary_Expression:    Postfix_Expression                 { $$ = $1; }
                |    MINUS_OP Postfix_Expression
                     {
                         PopReg(0);
                         fprintf(f_asm, "    subri $r0, $r0, 0\n");
                         PushReg(0);

                         $$ = $2;
                     }
                ;

Postfix_Expression:    Primary_Expression                   { $$ = $1; }
                  |    Primary_Expression PLUSPLUS_OP       { $$ = $1; }
                  |    Primary_Expression MINUSMINUS_OP     { $$ = $1; }
                  ;

Primary_Expression:    Var
                       {
                            int index = look_up_symbol($1);

                            /* Load to r0 */
                            switch(table[index].mode)
                            {
                                case ARGUMENT_MODE:
                                    fprintf(f_asm, "    lwi $r0, [$sp+%d]\n",table[index].offset*4);
                                    break;
                                case LOCAL_MODE:
                                    fprintf(f_asm, "    lwi $r0, [$sp+%d]\n",table[index].offset*4);
                                    break;
                                /* global */
                            }

                            /* Push to stack */
                            PushReg(0);

                            /* Return type */
                            $$ = table[index].type;
                       }
                  |    INT_CONSTANT
                       {
                            $$ = NULL;

                            /* Move num to $r0 and push to stack */
                            fprintf(f_asm, "    movi $r0, %d\n",$1);
                            PushReg(0);

                            /* Return type */
                            $$ = Int;
                       }
                  |    DOUBLE_CONSTANT                  { $$ = Double; }
                  |    CHAR_CONSTANT                    { $$ = Char; }
                  |    STRING_CONSTANT                  { $$ = String; }
                  |    TRUE                             { $$ = Bool; }
                  |    FALSE                            { $$ = Bool; }
                  |    '(' Expression ')'               { $$ = $2; }
                  |    ID '(' ')'
                       {
                           int index = look_up_symbol($1);

                           /* Return type */
                           $$ = table[index].type;
                       }
                  |    ID '(' Expression_List ')'
                       {
                           int index = look_up_symbol($1);

                           /* Return type */
                           $$ = table[index].type;
                       }
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
                               {
                                   PopReg(0);
                                   PopReg(1);
                                   fprintf(f_asm, "    or $r0, $r0, $r1\n");
                                   PushReg(0);
                               }
                          ;
Init_Logical_And_Expression:    Init_Not_Expression
                           |    Init_Logical_And_Expression AND_OP Init_Not_Expression
                                {
                                    PopReg(0);
                                    PopReg(1);
                                    fprintf(f_asm, "    and $r0, $r0, $r1\n");
                                    PushReg(0);
                                }
                           ;
Init_Not_Expression:    Init_Relational_Expression
                   |    NOT_OP Init_Relational_Expression
                        {
                            PopReg(0);
                            fprintf(f_asm, "    addi $r0, $r0, 0\n"); /* ??? */
                            fprintf(f_asm, "    slti $r0, $r0, 1\n");
                            fprintf(f_asm, "    zeb $r0, $r0\n");
                            PushReg(0);
                        }
                   ;
Init_Relational_Expression:    Init_Additive_Expression
                          |    Init_Relational_Expression LT_OP Init_Additive_Expression
                               {
                                   PopReg(0);
                                   PopReg(1);
                                   fprintf(f_asm, "    slts $r0, $r1, $r0\n");
                                   fprintf(f_asm, "    zeb $r0, $r0\n");
                                   PushReg(0);
                               }
                          |    Init_Relational_Expression LE_OP Init_Additive_Expression
                               {
                                   PopReg(0);
                                   PopReg(1);
                                   fprintf(f_asm, "    slts $r0, $r0, $r1\n"); /* $r1 > $r0 ? */
                                   fprintf(f_asm, "    xori $r0, $r0, 1\n"); /* if $r0 == 1, $r0 = 0 */
                                   fprintf(f_asm, "    zeb $r0, $r0\n");
                                   PushReg(0);
                               }
                          |    Init_Relational_Expression GT_OP Init_Additive_Expression
                               {
                                   PopReg(0);
                                   PopReg(1);
                                   fprintf(f_asm, "    slts $r0, $r0, $r1\n");
                                   fprintf(f_asm, "    zeb $r0, $r0\n");
                                   PushReg(0);
                               }
                          |    Init_Relational_Expression GE_OP Init_Additive_Expression
                               {
                                   PopReg(0);
                                   PopReg(1);
                                   fprintf(f_asm, "    slts $r0, $r1, $r0\n"); /* $r1 < $r0 ? */
                                   fprintf(f_asm, "    xori $r0, $r0, 1\n"); /* if $r0 == 1, $r0 = 0 */
                                   fprintf(f_asm, "    zeb $r0, $r0\n");
                                   PushReg(0);
                               }
                          |    Init_Relational_Expression EQUAL_OP Init_Additive_Expression
                               {
                                   PopReg(0);
                                   PopReg(1);
                                   fprintf(f_asm, "    xor $r0, $r0, $r1\n"); /* If $r0 == $r1 , then result of xor will be 0 --> < 1 */
                                   fprintf(f_asm, "    slti $r0, $r0, 1\n");
                                   fprintf(f_asm, "    zeb $r0, $r0\n");
                                   PushReg(0);
                               }
                          |    Init_Relational_Expression NEQUAL_OP Init_Additive_Expression
                               {
                                   PopReg(0);
                                   PopReg(1);
                                   fprintf(f_asm, "    xor $r0, $r0, $r1\n"); /* If $r0 != $r1 , then result of xor will be > 0 */
                                   fprintf(f_asm, "    movi $r1, 0\n");
                                   fprintf(f_asm, "    slt $r0, $r1, $r0\n");
                                   fprintf(f_asm, "    zeb $r0, $r0\n");
                                   PushReg(0);
                               }
                          ;
Init_Additive_Expression:    Init_Multiplicative_Expression     { $$ = $1; }
                        |    Init_Additive_Expression PLUS_OP Init_Multiplicative_Expression
                             {
                                 /* Check type */
                                 if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Additive Expression Type Error");
                                 $$ = $1;

                                 PopReg(0);
                                 PopReg(1);
                                 fprintf(f_asm, "    add $r0, $r1, $r0\n");
                                 PushReg(0);
                             }
                        |    Init_Additive_Expression MINUS_OP Init_Multiplicative_Expression
                             {
                                 /* Check type */
                                 if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Additive Expression Type Error");
                                 $$ = $1;

                                 PopReg(0);
                                 PopReg(1);
                                 fprintf(f_asm, "    sub $r0, $r1, $r0\n");
                                 PushReg(0);
                             }
                        ;
Init_Multiplicative_Expression:    Init_Unary_Expression            { $$ = $1; }
                              |    Init_Multiplicative_Expression MUL_OP Init_Unary_Expression
                                   {
                                       /* Check type */
                                       if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Multiplicative Expression Type Error");
                                       $$ = $1;

                                       PopReg(0);
                                       PopReg(1);
                                       fprintf(f_asm, "    mul $r0, $r1, $r0\n");
                                       PushReg(0);
                                   }
                              |    Init_Multiplicative_Expression DIV_OP Init_Unary_Expression
                                   {
                                       /* Check type */
                                       if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Multiplicative Expression Type Error");
                                       $$ = $1;

                                       PopReg(2);
                                       PopReg(3);
                                       fprintf(f_asm, "    divsr $r0, $r1, $r3, $r2\n");
                                       PushReg(0);
                                   }
                              |    Init_Multiplicative_Expression MOD_OP Init_Unary_Expression
                                   {
                                       /* Check type */
                                       if($1 != $3 || $1 != Int || $1 != Double)  yyerror("Multiplicative Expression Type Error");
                                       $$ = $1;

                                       PopReg(2);
                                       PopReg(3);
                                       fprintf(f_asm, "    divsr $r0, $r1, $r3, $r2\n");
                                       PushReg(1);
                                   }
                              ;
Init_Unary_Expression:    Init_Postfix_Expression               { $$ = $1; }
                     |    MINUS_OP Init_Postfix_Expression
                          {
                              PopReg(0);
                              fprintf(f_asm, "    subri $r0, $r0, 0\n");
                              PushReg(0);

                              $$ = $2;
                          }
                     ;
Init_Postfix_Expression:    Init_Primary_Expression                     { $$ = $1; }
                       |    Init_Primary_Expression PLUSPLUS_OP         { $$ = $1; }
                       |    Init_Primary_Expression MINUSMINUS_OP       { $$ = $1; }
                       ;
Init_Primary_Expression:    Var
                            {
                                 int index = look_up_symbol($1);

                                 /* Load to r0 */
                                 switch(table[index].mode)
                                 {
                                     case ARGUMENT_MODE:
                                         fprintf(f_asm, "    lwi $r0, [$sp+%d]\n",table[index].offset*4);
                                         break;
                                     case LOCAL_MODE:
                                         fprintf(f_asm, "    lwi $r0, [$sp+%d]\n",table[index].offset*4);
                                         break;
                                     /* global */
                                 }

                                 /* Push to stack */
                                 PushReg(0);

                                 /* Return type */
                                 $$ = table[index].type;
                            }
                       |    INT_CONSTANT
                            {
                                $$ = NULL;

                                /* Move num to $r0 and push to stack */
                                fprintf(f_asm, "    movi $r0, %d\n",$1);
                                PushReg(0);

                                /* Return type */
                                $$ = Int;
                            }
                       |    DOUBLE_CONSTANT             { $$ = Double; }
                       |    CHAR_CONSTANT               { $$ = Char; }
                       |    STRING_CONSTANT             { $$ = String; }
                       |    TRUE                        { $$ = Bool; }
                       |    FALSE                       { $$ = Bool; }
                       |    '(' Init_Expression ')'     { $$ = $2; }
                       ;

%%

extern int numL;
extern char buf[1000];
extern char *yytext;

int yyerror( char *msg ) {
	fprintf( stderr, "*** Error at line %d: %s\n", numL, buf );
	fprintf( stderr, "\n" );
    fprintf( stderr, "Error message: %s\n", msg );
	fprintf( stderr, "Unmatched token: %s\n", yytext );
	fprintf( stderr, "*** syntax error\n");
	exit(-1);
}


int main(void)
{
    /* Symbol table init */
    init_symbol_table();

    /* Open assembly file */
    if( (f_asm = fopen("assembly", "w")) == NULL )
        fprintf(stderr, "Can not open the file %s for writing.\n", "assembly");

    yyparse();

    if (FunctionNum == 0) yyerror(NULL);
    printf("No syntax error!\n");

    printTable();
    fclose(f_asm);
    return 0;
}
