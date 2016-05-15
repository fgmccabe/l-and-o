/*
 * LALR grammar for go's rule language
 */

%{
#include "unicode.h"
#include "io.h"
#include "formio.h"
#include "file.h"
#include "catalogP.h"
%}

%locations
%pure_parser
%defines
%error-verbose

%debug

%parse-param { ioPo yyInfile }
%parse-param { catalogPo *result }
%lex-param { ioPo yyInfile }

%start topLevel

%union{
  string str;
  string id;
  catalogPo a;
 }

// Symbolic tokens
%token PERIOD SEMI
%token IF_ARROW STRONG_ARROW
%token COLON THIN_ARROW EQUAL
%token LBRCE RBRCE

 // Keywords
%token CATALOG IS CONTENT HASH VRSION BASE

// Number and value tokens
%token <str> STRING
%token <id> ID

%right SEMI

%type <a> topLevel catalog catalogStmts

%{
  static void yyerror(YYLTYPE *loc,ioPo yyFile,catalogPo *l, char const *errmsg);
  extern int yylex (YYSTYPE * yylval_param,YYLTYPE * yylloc_param, ioPo yyFile);

  %}
%%

topLevel: rule { *result = $1; }

clause: head IF_ARROW goal
 | head STRONG_ARROW goal
 | head.



%%

static void yyerror(YYLTYPE *loc,ioPo yyFile,catalogPo *a, char const *errmsg)
{
  outMsg(logFile,"syntax error: %s",errmsg);
}





