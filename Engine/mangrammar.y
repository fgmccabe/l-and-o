/*
 * LALR grammar for manifest structure
 */

%{
#include "unicode.h"
#include "io.h"
#include "formio.h"
#include "file.h"
#include "manifestP.h"
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
  manifestPo a;
  manifestEntryPo e;
 }

// Symbolic tokens
%token LBRCE RBRCE STAR COLON EQUAL PERIOD

 // Keywords
%token MANIFEST

// Number and value tokens
%token <str> STRING URL
%token <id> ID

%type <a> topLevel manifest
%type <e> entry

%{
  static void yyerror(YYLTYPE *loc,ioPo yyFile,catalogPo *l, char const *errmsg);
  extern int yylex (YYSTYPE * yylval_param,YYLTYPE * yylloc_param, ioPo yyFile);

  %}
%%

topLevel: manifest { *result = $1; }

manifest: MANIFEST { $<a>$ = newManifest(); } LBRCE catalogStmts RBRCE { $<a>$ = $<a>2; }

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


    


