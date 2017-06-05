    /* Definition Section */

%{
    #include <stdio.h>
    #include <string.h>
    #include "SymbolTable.h"
    #include "y.tab.h"

    int numL = 1;
    char buf[1000];
    int bufIndex = 0;

    int tokenOn = 1;
    int sourceOn = 1;

    char* copys();
    extern FILE *f_asm;
%}

%x  COMMENT
%x  DIGITALWRITE
%x  DELAY

DIGIT    [0-9]
DIGIT_SEQ  {DIGIT}+
FRAC    ({DIGIT_SEQ}?"."{DIGIT_SEQ})|({DIGIT_SEQ}".")
FS		(f|F|l|L)

DOUBLE  {FRAC}{FS}?

E		[Ee][+-]?{DIGIT_SEQ}
SCI     ({FRAC}|{DIGIT_SEQ}){E}{FS}?

L       (l|L|ll|LL)
U       (u|U)
IS      ({U}|{L}|{L}{U}|{U}{L})

INT     ({DIGIT}|[1-9]{DIGIT}*){IS}?


CHAR    L?'([^'\\\n]|\\(n|t))'
STR     L?\"(\\(n|t)|[^"\\\n])*\"

ID      [a-zA-Z_]+[a-zA-Z_0-9]*

PUNC    ":"|";"|","|"."|"["|"]"|"("|")"|"{"|"}"|"?"
OP      "+"|"-"|"*"|"/"|"%"|"++"|"--"|"<"|"<="|">"|">="|"=="|"!="|"="|"&&"|"||"|"!"|"&"
KEY     "void"|"int"|"double"|"bool"|"char"|"null"|"for"|"while"|"do"|"if"|"else"|"switch"|"return"|"break"|"continue"|"const"|"true"|"false"|"struct"|"case"|"default"
STDIO    "remove"|"rename"|"tmpfile"|"tmpnam"|"fclose"|"fflush"|"fopen"|"freopen"|"setbuf"|"setvbuf"|"fprintf"|"fscanf"|"printf"|"scanf"|"sprintf"|"sscanf"|"vfprintf"|"vprintf"|"vsprintf"|"fgetc"|"fgets"|"fputc"|"fputs"|"getc"|"getchar"|"gets"|"putc"|"putchar"|"puts"|"ungetc"|"fread"|"fwrite"|"fgetpos"|"fseek"|"fsetpos"|"ftell"|"rewind"|"clearerr"|"feof"|"ferror"|"perror"
SPACE   [ \f\t\r\v]+

SOURCE_ON   #{SPACE}?"pragma"{SPACE}"source"{SPACE}"on"{SPACE}?\n
SOURCE_OFF   #{SPACE}?"pragma"{SPACE}"source"{SPACE}"off"{SPACE}?\n
TOKEN_ON    #{SPACE}?"pragma"{SPACE}"token"{SPACE}"on"{SPACE}?\n
TOKEN_OFF   #{SPACE}?"pragma"{SPACE}"token"{SPACE}"off"{SPACE}?\n

%%

{SOURCE_ON}    {
    sourceOn = 1;

    /* Fuck \n */
    yytext[yyleng-1] = 0;   strcpy(&(buf[bufIndex]), yytext);  bufIndex += (yyleng-1);
    unput('\n');
    }

{SOURCE_OFF}   {
    sourceOn = 0;

    /* Fuck \n */
    yytext[yyleng-1] = 0;   strcpy(&(buf[bufIndex]), yytext);  bufIndex += (yyleng-1);
    unput('\n');
    }

{TOKEN_ON}      {
    tokenOn = 1;

    /* Fuck \n */
    yytext[yyleng-1] = 0;   strcpy(&(buf[bufIndex]), yytext);  bufIndex += (yyleng-1);
    unput('\n');
    }

{TOKEN_OFF}    {
    tokenOn = 0;

    /* Fuck \n */
    yytext[yyleng-1] = 0;   strcpy(&(buf[bufIndex]), yytext);  bufIndex += (yyleng-1);
    unput('\n');
    }

"/*"    {
        BEGIN(COMMENT);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;
    }
<COMMENT>"*/" {
        /*  End of Comment  */
        BEGIN(INITIAL);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;
}
<COMMENT>\n {
        /* Comment newline print it out */

        /* Check pragma */
        if(sourceOn == 1)   printf("%d:%s\n",numL,buf);

        numL++; /* Enter newline */

        buf[0] = 0;  /* Clean Buffer */
        bufIndex = 0;
    }
<COMMENT>.  {
        /* If use .*  then it will eat the ending of comment */

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;
    }

"//".*  {
        /* Single line comment , simply copy to buf, and wait for \n to handle*/
        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;
    }



"digitalWrite(" {
        /* Special function from Andes */
        BEGIN(DIGITALWRITE);
    }
<DIGITALWRITE>{ID} {
        int index = look_up_symbol(yytext);

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
    }
<DIGITALWRITE>{INT} {
        int num = atoi(yytext);
        fprintf(f_asm, "    movi $r0, %d\n",num);
    }
<DIGITALWRITE>"LOW" {
        fprintf(f_asm, "    movi $r1, 0\n");
    }
<DIGITALWRITE>"HIGH" {
        fprintf(f_asm, "    movi $r1, 1\n");

    }
<DIGITALWRITE>";" {
        fprintf(f_asm, "    bal digitalWrite\n");
        BEGIN(INITIAL);
    }
"delay(" {
        /* Special function from Andes */
        BEGIN(DELAY);
    }
<DELAY>{INT} {
        int num = atoi(yytext);
        fprintf(f_asm, "    movi $r0, %d\n",num);
    }
<DELAY>{ID} {
        int index = look_up_symbol(yytext);

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
    }
<DELAY>";" {
        fprintf(f_asm, "    bal delay\n");
        BEGIN(INITIAL);
    }

{KEY}   {
        /* Check pragma */
        if(tokenOn == 1)    printf("#key:%s\n",yytext);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Return to parser */
        if(strcmp(yytext, "void") == 0) return VOID;
        if(strcmp(yytext, "int") == 0) return INT;
        if(strcmp(yytext, "double") == 0) return DOUBLE;
        if(strcmp(yytext, "bool") == 0) return BOOL;
        if(strcmp(yytext, "char") == 0) return CHAR;
        if(strcmp(yytext, "null") == 0) return NUL;
        if(strcmp(yytext, "for") == 0) return FOR;
        if(strcmp(yytext, "while") == 0) return WHILE;
        if(strcmp(yytext, "do") == 0) return DO;
        if(strcmp(yytext, "if") == 0) return IF;
        if(strcmp(yytext, "else") == 0) return ELSE;
        if(strcmp(yytext, "switch") == 0) return SWITCH;
        if(strcmp(yytext, "return") == 0) return RETURN;
        if(strcmp(yytext, "break") == 0) return BREAK;
        if(strcmp(yytext, "continue") == 0) return CONTINUE;
        if(strcmp(yytext, "const") == 0) return CONST;
        if(strcmp(yytext, "struct") == 0) return STRUCT;
        if(strcmp(yytext, "true") == 0) return TRUE;
        if(strcmp(yytext, "false") == 0) return FALSE;
        if(strcmp(yytext, "case") == 0) return CASE;
        if(strcmp(yytext, "default") == 0) return DEFAULT;
    }

{STDIO}  {
        /* Check pragma */
        if(tokenOn == 1)    printf("#key:%s\n",yytext);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Return to Parser */
        yylval.ident =  copys(yytext);
        return ID;
    }

0{INT}  {
    /* In order to make TA happy */
    fprintf(stderr, "Error at line %d: %s\n", numL, yytext); exit(1);
    }

{INT}    {
        /* Check pragma */
        if(tokenOn == 1)    printf("#integer:%s\n",yytext);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Return to Parser */
        yylval.intVal = atoi(yytext);
        return INT_CONSTANT;
    }

{DOUBLE}  {
        /* Check pragma */
        if(tokenOn == 1)    printf("#double:%s\n",yytext);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Return to Parser */
        sscanf(yytext, "%lf", &yylval.doubleVal);
        return DOUBLE_CONSTANT;
    }

{SCI}    {
        /* Check pragma */
        if(tokenOn == 1)    printf("#sci:%s\n",yytext);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Return to Parser */
        sscanf(yytext, "%lf", &yylval.doubleVal);
        return DOUBLE_CONSTANT;
    }

{PUNC}   {
        /* Check pragma */
        if(tokenOn == 1)    printf("#punc:%s\n",yytext);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Return to Parser */
        if(strcmp(yytext, ":") == 0) return ':';
        if(strcmp(yytext, ";") == 0) return ';';
        if(strcmp(yytext, ",") == 0) return ',';
        if(strcmp(yytext, ".") == 0) return '.';
        if(strcmp(yytext, "[") == 0) return '[';
        if(strcmp(yytext, "]") == 0) return ']';
        if(strcmp(yytext, "(") == 0) return '(';
        if(strcmp(yytext, ")") == 0) return ')';
        if(strcmp(yytext, "{") == 0) return '{';
        if(strcmp(yytext, "}") == 0) return '}';
        if(strcmp(yytext, "?") == 0) return '?';
    }

{OP}    {
        /* Check pragma */
        if(tokenOn == 1)    printf("#op:%s\n",yytext);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Return to Parser */
        if(strcmp(yytext, "+") == 0) return PLUS_OP;
        if(strcmp(yytext, "-") == 0) return MINUS_OP;
        if(strcmp(yytext, "*") == 0) return MUL_OP;
        if(strcmp(yytext, "/") == 0) return DIV_OP;
        if(strcmp(yytext, "%") == 0) return MOD_OP;
        if(strcmp(yytext, "++") == 0) return PLUSPLUS_OP;
        if(strcmp(yytext, "--") == 0) return MINUSMINUS_OP;
        if(strcmp(yytext, "<") == 0) return LT_OP;
        if(strcmp(yytext, "<=") == 0) return LE_OP;
        if(strcmp(yytext, ">") == 0) return GT_OP;
        if(strcmp(yytext, ">=") == 0) return GE_OP;
        if(strcmp(yytext, "==") == 0) return EQUAL_OP;
        if(strcmp(yytext, "!=") == 0) return NEQUAL_OP;
        if(strcmp(yytext, "=") == 0) return '=';
        if(strcmp(yytext, "&&") == 0) return AND_OP;
        if(strcmp(yytext, "||") == 0) return OR_OP;
        if(strcmp(yytext, "!") == 0) return NOT_OP;
        if(strcmp(yytext, "&") == 0) return '&';
    }

{CHAR}  {

        /* Check pragma */
        if(tokenOn == 1)    printf("#char:%s\n",yytext);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Extract String */
        char str[1000];
        if(yytext[0] == 'L')
        {
            strcpy(str,yytext+2); /* The first ' is gone */
            str[yyleng-3] = 0; /* The last ' is gone */
        }
        else
        {
            strcpy(str,yytext+1); /* The first ' is gone */
            str[yyleng-2] = 0; /* The last " is gone */
        }

        /* Return to Parser */
        yylval.ident = copys(str);
        return CHAR_CONSTANT;
    }


{STR}   {
        /* Extract String */
        char str[1000];
        if(yytext[0] == 'L')
        {
            strcpy(str,yytext+2); /* The first " is gone */
            str[yyleng-3] = 0; /* The last " is gone */
        }
        else
        {
            strcpy(str,yytext+1); /* The first " is gone */
            str[yyleng-2] = 0; /* The last " is gone */
        }

        /* Check pragma */
        if(tokenOn == 1)    printf("#string:%s\n",str);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Return to Parser */
        yylval.ident = copys(str);
        return STRING_CONSTANT;
    }

[0-9]+{ID}  {
    /* In order to make TA happy */
    fprintf(stderr, "Error at line %d: %s\n", numL, yytext); exit(1);
    }

{ID}    {
        /* Check pragma */
        if(tokenOn == 1)    printf("#id:%s\n",yytext);

        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;

        /* Return to Parser */
        yylval.ident = copys(yytext);
        return ID;
    }

{SPACE}   {
        strcpy(&(buf[bufIndex]), yytext);
        bufIndex += yyleng;
    }

\n  {

        /* Check pragma */
        if(sourceOn == 1)   printf("%d:%s\n",numL,buf);

        numL++; /* Enter newline */

        buf[0] = 0;  /* Clean Buffer */
        bufIndex = 0;

    }

.   {
        fprintf(stderr, "Error at line %d: %s\n", numL, yytext); exit(1);
    }


%%

/* C code Section */
/*
int main(int argc, char* argv[])
{
    ++argv; --argc;
    yyin = ( argc > 0 ) ? fopen(argv[0], "r") : stdin;

    yylex();

    return 0;
}
*/
