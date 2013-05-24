 /*
  * this file intends to test the start state in flex
  * how to transfer between diff states and how to handle
  * diff \n in strings
  */
%{
#include<stdio.h>
#define MAX 100
char strtab[MAX];
char *str_ptr;
%}
DIGIT   [0-9]
INTEGER {DIGIT}+
SPACE   [ \n\b\f\v]*
%s STR
%%

<STR>\n(.)*\"    {printf("\nunexpected newline\n");
                  BEGIN(INITIAL);}
<STR><<EOF>>    {printf("\nunexpected eof in str\n");}
<STR>\0         {printf("\nunexpected null\n");
                 BEGIN(INITIAL);}
<STR>\\n       {printf("a \\n character\n");}
<STR>\\0       {printf("zero\n");
                *str_ptr++ = '0';}
<STR>\\\n      {printf("new line\n");
                *str_ptr++ = '\\';
                *str_ptr++ = '\n';}
<STR>[^\\\n\"]+  {
                        char *temp = yytext;
                        while(*temp)
                        {
                            *str_ptr++ = *temp++;
                        }
                        printf("normal str\n");
                 }
<STR>\"         {
                    printf("end of str\n");
                    *str_ptr++ = '\0';
                    printf("the str is:\n%s\n",strtab);
                    BEGIN(INITIAL);
                }

\"              {
                 printf("begin of str\n");
                 str_ptr = strtab;
                 BEGIN(STR);}
{INTEGER}       {printf("integer\n");}
"+"             {printf("plus\n");   }
"-"             {printf("minus\n");  }
"*"             {printf("multi\n");  }
"/"             {printf("div\n");    }
\n              {}
{SPACE}         {}
.               {}
%%

int main(int argc, char * argv[])
{
    if(argc > 1)
    {
        if( !(yyin = fopen(argv[1],"r")) )
        {
            perror(argv[1]);
            return 1;
        }
    }

    yylex();
    return 0;
}

