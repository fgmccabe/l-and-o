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
%token LBRCE RBRCE LBRA RBRA STAR COLON EQUAL

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

manifest: MANIFEST { $<a>$ = newManifest(); } LBRCE manifestContents RBRCE { $<a>$ = $<a>2; }

manifestContents:
| packageEntry
| packageEntry SEMI manifestContents
;

packageEntry: package COLON LBRCE { addCatalogEntry($<a>$,$1,$3); } versions RBRCE;

package: PKGNAME

%%

static void yyerror(YYLTYPE *loc,ioPo yyFile,catalogPo *a, char const *errmsg)
{
  outMsg(logFile,"syntax error: %s",errmsg);
}


    


