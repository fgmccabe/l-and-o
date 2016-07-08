:- module(grapher,[graph_package/2]).

:- use_module(topsort).
:- use_module(repository).
:- use_module(misc).
:- use_module(abstract).


%% this is in pieces

parseFile(Fl,CWD,Term) :-
  getSrcUri(Fl,CWD,FUrl),
  locateResource(FUrl,Txt),
  allTokens(Txt,Toks),
  parse(Toks,Term,_), !.

scanForImports(Term,Pkg,Imports) :-
    isBraceTerm(Term,P,Els),
    scanPackageName(P,Pkg),
    scanThetaEnv(Els,Imports),!.

scanPackageName(Term,Nm) :- isIden(Term,Nm).
scanPackageName(Term,Nm) :- isString(Term,Nm).
scanPackageName(Term,Pk) :- isBinary(Term,".",L,R), 
  scanPackageName(L,F), 
  scanPackageName(R,B), 
  string_concat(F,".",FF),
  string_concat(FF,B,Pk).
scanPackageName(Name,"") :- 
  locOfAst(Name,Lc),
  reportError("Package name %s not valid",[Name],Lc).

scanThetaEnv([],[]).
scanThetaEnv([St|Stmts],Imports) :-
    scanStmt(St,Imports,MoreImp),
    scanThetaEnv(Stmts,MoreImp).

scanStmt(St,Imp,More) :-
  isPrivate(St,El,_),!,
  scanStmt(El,Imp,More).
scanStmt(St,Imp,More) :-
  isPublic(St,El,_),!,
  scanStmt(El,Imp,More).
scanStmt(St,[Pk|More],More) :- 
  isUnary(St,"import",P),
  scanPackageName(P,Pk).
scanStmt(_,Imp,Imp).




