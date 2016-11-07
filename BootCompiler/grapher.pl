:- module(grapher,[scanPkg/6,scanFile/5,graphPkgs/2,makeGraph/4,pkgOk/3]).

:- use_module(topsort).
:- use_module(uri).
:- use_module(resource).
:- use_module(lexer).
:- use_module(grammar).
:- use_module(repository).
:- use_module(misc).
:- use_module(abstract).
:- use_module(import).

makeGraph(Repo,CWD,Fls,Groups) :-
  scanFiles(Fls,Repo,CWD,[],Pkgs),
  graphPkgs(Pkgs,Groups).

graphPkgs(Pkgs,Groups) :-
  topsort(Pkgs,Groups).

scanPkg(Pkg,Vers,_,_,SoFar,SoFar) :-
  is_member((pk(Pkg,Vers),_,_,_),SoFar),!.
scanPkg(Pkg,Vers,Repo,CWD,SoFar,Pkgs) :-
  packagePresent(Repo,Pkg,Vers,_,SrcFn,SrcWhen,CodeWhen),
  ( CodeWhen>SrcWhen ->
    importPkg(Pkg,Vers,Repo,Spec),
    checkPkg(Spec,Repo,CWD,SrcFn,SoFar,Pkgs) ;
    scanFile(SrcFn,CWD,Repo,SoFar,Pkgs)).

checkPkg(spec(Pkg,Vers,_,_,_,_,_,Imports),Repo,CWD,SrcFn,SoFar,Pkgs) :-
  reformatImports(Imports,Imps),
  scanImports(Imps,Repo,CWD,[(pk(Pkg,Vers),Imps,Imps,SrcFn)|SoFar],Pkgs).

reformatImports([],[]).
reformatImports([import(_,P,V)|L],[pk(P,V)|M]) :- reformatImports(L,M).

scanImports([],_,_,Pkgs,Pkgs).
scanImports([pk(Pkg,Vers)|Imports],Repo,CWD,SoFar,Pkgs) :-
  scanPkg(Pkg,Vers,Repo,CWD,SoFar,Pkg1),
  scanImports(Imports,Repo,CWD,Pkg1,Pkgs).

scanFiles([],_,_,Pkgs,Pkgs).
scanFiles([Fl|L],Repo,CWD,SoFar,Pkgs) :-
  scanFile(Fl,CWD,Repo,SoFar,P1),
  scanFiles(L,Repo,CWD,P1,Pkgs).

scanFile(Fl,CWD,Repo,SoFar,Pkgs) :-
  parseFile(Fl,CWD,Term),
  scanForImports(Term,Pkg,Imps),
  scanImports(Imps,Repo,CWD,[(pk(pkg(Pkg),defltVersion),Imps,Imps,Fl)|SoFar],Pkgs).

parseFile(Fl,CWD,Term) :-
  getSrcUri(Fl,CWD,FUrl),
  locateResource(FUrl,Txt),
  allTokens(Txt,Toks),
  parse(Toks,Term,_), !.

getSrcUri(Fl,WD,FUri) :-
  parseURI(Fl,FU),
  resolveURI(WD,FU,FUri).

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
  isUnary(St,"public",El),!,
  scanStmt(El,Imp,More).
scanStmt(St,Imp,More) :-
  isUnary(St,"private",El),!,
  scanStmt(El,Imp,More).
scanStmt(St,[pk(pkg(Pk),defltVersion)|More],More) :- 
  isUnary(St,"import",P),
  scanPackageName(P,Pk).
scanStmt(_,Imp,Imp).

pkgOk(Pkg,Vers,Repo) :-
  packagePresent(Repo,Pkg,Vers,_,_,SrcWhen,CodeWhen),
  CodeWhen>SrcWhen.
