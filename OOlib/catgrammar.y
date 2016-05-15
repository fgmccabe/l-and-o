/*
 * LALR grammar for catalog structure
 * Based on Cafe grammar, but much simplified
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
%token SEMI
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

topLevel: catalog { *result = $1; }

catalog: CATALOG { $<a>$ = newCatalog(); } LBRCE catalogStmts RBRCE { $<a>$ = $<a>2; }

catalogStmts: CONTENT IS LBRCE catalogContents RBRCE { };
 | BASE IS STRING { setCatalogBase($<a>$,$3); }
 | VRSION IS STRING { setCatalogVersion($<a>$,$3); }
 ;

catalogContents:
| catEntry
| catEntry SEMI catalogContents

catEntry: STRING THIN_ARROW STRING { addCatalogEntry($<a>$,$1,$3); }

%%

static void yyerror(YYLTYPE *loc,ioPo yyFile,catalogPo *a, char const *errmsg)
{
  outMsg(logFile,"syntax error: %s",errmsg);
}


    


