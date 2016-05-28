/*
 * Grammar for Go! assembler
 */

%{
#include <math.h>
#include "assem.h"
#include "errors.h"
%}

%locations
%pure-parser
%defines
%error-verbose

%debug

%parse-param { ioPo yyInfile }
%parse-param { pkgPo pkg }
%lex-param { ioPo yyInfile } 

%start program

%union{
  string s;
  integer i;
  double f;
  assemInsPo ins;
  lPo lbl;
 }

%{
  static void sserror(YYLTYPE *loc,ioPo asmFile,pkgPo p, char const *errmsg);
  extern int sslex (YYSTYPE * asmlval_param,YYLTYPE * asmlloc_param, ioPo asmFile);

  #define locOf(asmloc)							\
    newLocation(fileName(asmInfile),asmloc.first_line,asmloc.last_line)

  static mtdPo currMtd;
%}

// Assembler mnemonics
%token HALT
%token CALL LCALL DLCALL ESCAPE
%token ALLOC DEALLOC
%token TRYME RETRYME TRUSTME TRY RETRY TRUST
%token SUCC FAIL GOTO CUT
%token INDEXI INDEXS INDEXX
%token TRPBLK TRPEND EXCEPT
%token GCMAP GC
%token SUSP RESUME TRIGGER DIE
%token U M B C CLR VD V NV
%token A Y S
%token FRAME LOCAL
%token END

%token COLON DCOLON LBRA RBRA COMMA
%token NL

// Number and value tokens
%token <i> DECIMAL
%token <s> STRING
%token <s> ID
%token <f> FLOAT

%type <lbl> label;
%type <i> libName;
%type <str> signature;

%%

  module: moduleHeader program ;
  
  moduleHeader : MODULE name version export imports nls ;
   
   program: program function | ;

 function: header instructions trailer ;

 header: ID DCOLON signature COLON signature nls { currMtd = defineMethod(pkg,$1,$3,$5); }

 instructions: instructions instruction nls
     | instructions error nls
     | instruction nls
     ;

trailer: END nls { endFunction(currMtd); }

 nls: nls NL | NL;

 instruction: halt
     | call
     | alloc
     | tryfail
     | index
     | except
     | gc
     | threads
     | unify
     | move
     | bind
     | match
     | clear
     | var
     | directive
     ;   
 halt: HALT { Ahalt(currMtd); };
 
 call: CALL DECIMAL name { Akawl(currMtd,$2,$3); }
   | LCALL DECIMAL name { Alkawl(currMtd,$2,$3); }
   | DLCALL DECIMAL name { Aklkawl(currMtd,$2,$3); }
   | CALL DECIMAL A LBRA DECIMAL RBRA { Akawl0(currMtd,$2,$5); }
   | LCALL DECIMAL A LBRA DECIMAL RBRA { Alkawl0(currMtd,$2,$5); }
   | DCALL DECIMAL A LBRA DECIMAL RBRA { Adlkawl0(currMtd,$2,$5); }
   | ESCAPE DECIMAL name { Aescape(currMtd,$2,$3); }
   | SUCC { Asucc(currMtd); }
   | GOTO label { Agot_to(currMtd($2); }
   ;
   
 alloc: ALLOC DECIMAL DECIMAL { Aalloc(currMtd,$2,$3); }
   | DEALLOC { Adealloc(currMtd); }
   ;
   
 tryfail: TRYME label {Atryme(currMtd,$2); }
   | RETRYME label {Aretryme(currMtd,$2); }
   | TRUSTME {Atrustme(currMtd); }
   | TRY label {Atrycl(currMtd,$2); }
   | RETRY label {Aretry(currMtd,$2); }
   | TRUST label {Atrust(currMtd,$2); }
   | FAIL { Afail(currMtd); }
   | CUT { Acut(currMtd); }
   ;
 
 index: INDEXI DECIMAL label { Aindexi(currMtd,$2,$3); }
  | INDEXS DECIMAL label { Aindexs(currMtd,$2,$3); }
  | INDEXX DECIMAL label {Aindexx(currMtd,$2,$3); }
  ;
 
 except: TRPBLK { Atrpblk(currMtd); }
  | TRPEND { Atrpend(currMtd); }
  | EXCEPT A LBAR DECIMAL RBAR { Aexcept(currMtd,$4); }
  ;
  
 gc: GCMAP DECIMAL DECIMAL { Agcmap(currMtd,$2,$3); }
  | GC DECIMAL DECIMAL { Agc(currMtd,$2,$3); }
  ;
  
 threads: RESUME DECIMAL { Aresume(currMtd,$2); }
  | TRIGGER DECIMAL { Atrgr(currMtd); }
  | DIE { Adie(currMtd); }
  ;
  
 unify: U A LBRA DECIMAL RBRA COMMA A LBRA DECIMAL RBRA { AuAA(currMtd,$4,$9); }
  | U A LBRA DECIMAL RBRA COMMA Y LBRA DECIMAL RBRA { AuAY(currMtd,$4,$9); }
  | U A LBRA DECIMAL RBRA COMMA S { AuAS(currMtd,$4); }
  | UC A LBRA DECIMAL RBRA COMMA S { AucAS(currMtd,$4); }
  | U A LBRA DECIMAL RBRA COMMA literal { AuAlit(currMtd,$4,$7); } 
  | U A LBRA DECIMAL RBRA COMMA LBRA literal RBRA { AuAcns(currMtd,$4,$7); }
  | U Y LBRA DECIMAL RBRA COMMA Y LBRA DECIMAL RBRA { AuYY(currMtd,$4,$9); }
  | U Y LBRA DECIMAL RBRA COMMA S { AuYS(currMtd,$4); }
  | UC Y LBRA DECIMAL RBRA COMMA S { AucYS(currMtd,$4); }
  | U S COMMA literal { AuSlit(currMtd,$4); }
  | U S COMMA LBRA literal RBRA { AuScns(currMtd,$4); }
  ;
  
 move: M A LBRA DECIMAL RBRA COMMA A LBRA DECIMAL RBRA { AmAA(currMtd,$4,$9); }
  | M A LBRA DECIMAL RBRA COMMA Y LBRA DECIMAL RBRA { AmAY(currMtd,$4,$9); }
  | MU A LBRA DECIMAL RBRA COMMA Y LBRA DECIMAL RBRA { AmuAY(currMtd,$4,$9); }
  | M A LBRA DECIMAL RBRA COMMA S { AmAS(currMtd,$4); }
  | M A LBRA DECIMAL RBRA COMMA literal { AmAlit(currMtd,$4,$7); } 
  | M A LBRA DECIMAL RBRA COMMA LBRA literal RBRA { AmAcns(currMtd,$4,$7); }
  | M Y LBRA DECIMAL RBRA COMMA A LBRA DECIMAL RBRA { AmYA(currMtd,$4,$9); }
  | M Y LBRA DECIMAL RBRA COMMA Y LBRA DECIMAL RBRA { AmYY(currMtd,$4,$9); }
  | M Y LBRA DECIMAL RBRA COMMA S { AmYS(currMtd,$4); }
  | M S COMMA A LBRA DECIMAL RBRA { AmSA(currMtd,$6); }
  | M S COMMA Y LBRA DECIMAL RBRA { AmSY(currMtd,$6); }
  | M S COMMA literal { AmSlit(currMtd,$4); }
  | M S COMMA LBRA literal RBRA { AuScns(currMtd,$4); }
  ;

 bind: B A LBRA DECIMAL RBRA  { AoAU(currMtd,$4); }
  | B Y LBRA DECIMAL RBRA  { AoYU(currMtd,$4); }
  | B Y LBRA DECIMAL RBRA COMMA A LBRA DECIMAL RBRA { AoYA(currMtd,$4,$9); }
  | B Y LBRA DECIMAL RBRA COMMA { AoYnil(currMtd,$4); }
  ;
  
 match: C A LBRA DECIMAL RBRA COMMA A LBRA DECIMAL RBRA { AcAA(currMtd,$4,$9); }
  | C A LBRA DECIMAL RBRA COMMA Y LBRA DECIMAL RBRA { AcAY(currMtd,$4,$9); }
  | C A LBRA DECIMAL RBRA COMMA S { AcAS(currMtd,$4); }
  | C A LBRA DECIMAL RBRA COMMA literal { AcAlit(currMtd,$4,$7); } 
  | C A LBRA DECIMAL RBRA COMMA LBRA literal RBRA { AcAcns(currMtd,$4,$7); }
  | C Y LBRA DECIMAL RBRA COMMA A LBRA DECIMAL RBRA { AcYA(currMtd,$4,$9); }
  | C Y LBRA DECIMAL RBRA COMMA S { AcYS(currMtd,$4); }
  | C S COMMA A LBRA DECIMAL RBRA { AcSA(currMtd,$6); }
  | C S COMMA Y LBRA DECIMAL RBRA { AcSY(currMtd,$6); }
  | C S COMMA literal { AcSlit(currMtd,$4); }
  | C S COMMA LBRA literal RBRA { AcScns(currMtd,$4); }
  ;
  
 clear: CL A LBRA DECIMAL RBRA  COMMA A LBRA DECIMAL RBRA { AclAA(currMtd,$4,$9); }
  | CL A LBRA DECIMAL RBRA COMMA Y LBRA DECIMAL RBRA  { AclAY(currMtd,$4,$9); }
  | CL A LBRA DECIMAL RBRA COMMA S  { AclAS(currMtd,$4); }
  | CL S COMMA A LBRA DECIMAL RBRA  { AclSA(currMtd,$6); }
  | CL S COMMA Y LBRA DECIMAL RBRA  { AclSY(currMtd,$6); }
  | CL A LBRA DECIMAL RBRA { AclA(currMtd,$4); }
  | CL Y LBRA DECIMAL RBRA { AclY(currMtd,$4); }
  | CL Y LBRA DECIMAL RBRA COMMA Y LBRA DECIMAL RBRA { AclYY(currMtd,$4,$9); }
  | CL S { AclS(currMtd); }
  | VD A LBRA DECIMAL RBRA { AvdA(currMtd,$4); }
  | VD A LBRA DECIMAL RBRA COMMA A LBRA DECIMAL RBRA { AvdAA(currMtd,$4,$9); }
  | VD Y LBRA DECIMAL RBRA { AvdY(currMtd,$4); }
  | VD Y LBRA DECIMAL RBRA COMMA Y LBRA DECIMAL RBRA { AvdYY(currMtd,$4,$9); }
  ;
  
  var: V A LBRA DECIMAL RBRA { AvrA(currMtd,$4); }
  | V Y LBRA DECIMAL RBRA { AvrY(currMtd,$4); }
  | NV A LBRA DECIMAL RBRA { AnvrA(currMtd,$4); }
  | NV Y LBRA DECIMAL RBRA { AnvrY(currMtd,$4); }
  | B Y LBRA DECIMAL RBRA COMMA A LBRA DECIMAL RBRA { AoYA(currMtd,$4,$9); }
  | B Y LBRA DECIMAL RBRA COMMA { AoYnil(currMtd,$4); }
  ;

 directive: label COLON { defineLbl(currMtd,$1); }
   | FRAME signature { defineFrame(currMtd,$2); }
   | ID LOCAL DECIMAL signature label label { defineLocal(currMtd,$1,$4,$3,$5,$6); }
   ;

 label: ID { $$ = newLbl(currMtd,$1); };
   
 libName: STRING { $$ = newEscapeConstant(currMtd,$1); }

 signature: STRING { $$ = $1; if(!validSignature($1)){
  sserror(&yyloc,yyInfile,pkg,"invalid signature");
  YYERROR;
 }
 }

%%

static void sserror(YYLTYPE *loc,ioPo asmFile,pkgPo p, char const *errmsg)
{
  reportError(loc->first_line,"%s\n",errmsg);
}

