/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;
int str_count;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;
/*
 *  Nested comments
 */
int nested = 0;

/*
 *  Add Your own definitions here
 */

%}

/*
 * Define names for regular expressions here.
 */

/*
 * Define three start states to handle comment,
 * string and one line comment.
 */
%x COMMENT
%x STR
%x LIE_COM

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
/*
 * key words are case-insensitive, so the letter
 *be either upper or lower case.
 *the following can make words case-insensitive.
 */
a               [aA]
b               [bB]
c               [cC]
d               [dD]
e               [eE]
f               [fF]
g               [gG]
h               [hH]
i               [iI]
j               [jJ]
k               [kK]
l               [lL]
m               [mM]
n               [nN]
o               [oO]
p               [pP]
q               [qQ]
r               [rR]
s               [sS]
t               [tT]
u               [uU]
v               [vV]
w               [wW]
x               [xX]
y               [yY]
z               [zZ]
/*
 *most tokens below are declared in cool-parse.h
 *will be returned into bison in next stage
 */
CLASS           {c}{l}{a}{s}{s}
ELSE            {e}{l}{s}{e}
FALSE           f{a}{l}{s}{e}
FI              {f}{i}
IF              {i}{f}
IN              {i}{n}
INHERITS        {i}{n}{h}{e}{r}{i}{t}{s}
ISVOID          {i}{s}{v}{o}{i}{d}
LET             {l}{e}{t}
LOOP            {l}{o}{o}{p}
POOL            {p}{o}{o}{l}
THEN            {t}{h}{e}{n}
WHILE           {w}{h}{i}{l}{e}
CASE            {c}{a}{s}{e}
ESAC            {e}{s}{a}{c}
NEW             {n}{e}{w}
OF              {o}{f}
NOT             {n}{o}{t}
TRUE            t{r}{u}{e}
TYPEID          [A-Z][A-Za-z0-9_]*
OBJECTID        [a-z][A-Za-z0-9_]*
DARROW          =>
LE              <=
ASSIGN          <-
DIGIT           [0-9]
INTEGER         {DIGIT}+
SPACE           [\v\r\f\t\b ]+
LINE            \n
%%

 /*
  *part two: rules can be matched in text
  */

 /* single line comment, end by \n */
<LIE_COM>\n        {
                       curr_lineno++;
                       BEGIN(INITIAL);
                   }
 /* normal string in comment, do nothing */
<LIE_COM>[^\n]*     {}
 /* new line in multi-line comment */
<COMMENT>\n        {curr_lineno++;}
 /*
  * a comment-start symbol inside a comment,
  * nested_level+=1
  */
<COMMENT>"(*"      {nested++;}
  /* an EOF in comment report an error */
<COMMENT><<EOF>>     {
                        cool_yylval.error_msg = "EOF in comment";
                        BEGIN(INITIAL);
                        return ERROR;
                     }
 /*
  * comment-close symbol, if no other comments
  * nested, return to INITIAL mode, we don't
  * need to return anything in COMMENT mode
  */
<COMMENT>"*)"   {
                    nested--;
                    if(nested == 0)
                        BEGIN(INITIAL);
                }
  /* normal comment string, simply skip */
<COMMENT>.         {}

  /* \n in a string without a close quote */
<STR>\n         {
                    cool_yylval.error_msg = "Unterminated string constant";
                    BEGIN(INITIAL);
                    curr_lineno++;
                    return ERROR;
                }
  /* escaped characters in string */
<STR>\\n        {
                   *string_buf_ptr++ = '\n';
                    str_count++;
                }
<STR>\\f        {
                    *string_buf_ptr++ = '\f';
                    str_count++;
                }
<STR>\\t        {
                    *string_buf_ptr++ = '\t';
                    str_count++;
                }
<STR>\\b        {
                    *string_buf_ptr++ = '\b';
                    str_count++;
                }
  /* multi-line string */

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for
  *  \n \t \b \f, the result is c.
  *
  */
<STR>\\(.|\n)   {
                    if(yytext[1] == '\n')
                    {
                        curr_lineno++;
                    }
                    str_count++;
                    *string_buf_ptr++ = yytext[1];
                }
  /*
   * null character, search until a new line or end of str
   * eat any character, new line or a close-quote after the null
   */
<STR>\0(.|\\n)*\" {
                      cool_yylval.error_msg = "String contains null character";
                      BEGIN(INITIAL);
                      return ERROR;
                  }
<STR>\\\0.*\n*  {
                    cool_yylval.error_msg = "String contains escaped null character";
                    BEGIN(INITIAL);
                    return ERROR;
                }
<STR><<EOF>>    {
                    cool_yylval.error_msg = "EOF in string constant";
                    BEGIN(INITIAL);
                    return ERROR;
                }
  /* normal string, stored in str_buf */
<STR>[^\\\n\"\0]+  {
                       char *temp = yytext;
                       while(*temp)
                       {
                           *string_buf_ptr++ = *temp++;
                           str_count++;
                       }
                   }
 /* end of string */
<STR>\"         {
                    if(str_count < MAX_STR_CONST)
                    {
                        *string_buf_ptr++ = '\0';
                        cool_yylval.symbol = stringtable.add_string(string_buf);
                        BEGIN(INITIAL);
                        return STR_CONST;
                    }
                    else
                    {
                        cool_yylval.error_msg = "String constant too long";
                        BEGIN(INITIAL);
                        return ERROR;
                    }
                }

"*)"            {
                    cool_yylval.error_msg = "Unmatched *)";
                    BEGIN(INITIAL);
                    return ERROR;
                }
 /*
  * following stuff are in INITIAL state,
  * which is defined by flex by default.
  */
 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW);    }
{ASSIGN}        { return (ASSIGN);    }
{CLASS}         { return (CLASS);     }
{ELSE}          { return (ELSE);      }
{FI}            { return (FI);        }
{IF}            { return (IF);        }
{IN}            { return (IN);        }
{INHERITS}      { return (INHERITS);  }
{ISVOID}        { return (ISVOID);    }
{LET}           { return (LET);       }
{LOOP}          { return (LOOP);      }
{POOL}          { return (POOL);      }
{THEN}          { return (THEN);      }
{WHILE}         { return (WHILE);     }
{CASE}          { return (CASE);      }
{ESAC}          { return (ESAC);      }
{NEW}           { return (NEW);       }
{OF}            { return (OF);        }
{NOT}           { return (NOT);       }
"+"             { return '+';         }
"-"             { return '-';         }
"*"             { return '*';         }
"/"             { return '/';         }
"<"             { return '<';         }
{LE}            { return (LE);        }
"="             { return '=';         }
"."             { return '.';         }
"~"             { return '~';         }
"@"             { return '@';         }
"("             { return '(';         }
")"             { return ')';         }
"{"             { return '{';         }
"}"             { return '}';         }
";"             { return ';';         }
":"             { return ':';         }
","             { return ',';         }
self            {
                    cool_yylval.symbol = idtable.add_string(yytext);
                    return OBJECTID;
                }
SELF_TYPE       {
                    cool_yylval.symbol = idtable.add_string(yytext);
                    return TYPEID;
                }
{TRUE}          {
                    cool_yylval.boolean = 1;
                    return BOOL_CONST;
                }
{FALSE}         {
                    cool_yylval.boolean = 0;
                    return BOOL_CONST;
                }
{INTEGER}       {
                    cool_yylval.symbol = inttable.add_string(yytext);
                    return INT_CONST;
                }
{TYPEID}        {
                    cool_yylval.symbol = idtable.add_string(yytext);
                    return TYPEID;
                }
{OBJECTID}      {
                    cool_yylval.symbol = idtable.add_string(yytext);
                    return OBJECTID;
                }
{LINE}          {curr_lineno++;}
 /* skip space */
{SPACE}         {}
"(*"            {
                    nested++;
                    BEGIN(COMMENT);
                }
"--"            {BEGIN(LIE_COM);}

\"              {
                    string_buf_ptr = string_buf;
                    str_count = 0;
                    BEGIN(STR);
                }
 /* invalid characters report an error */
.               {
                    cool_yylval.error_msg = yytext;
                    return ERROR;
                }
%%

