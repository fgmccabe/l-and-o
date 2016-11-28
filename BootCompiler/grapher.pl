:- module(grapher,[scanPkg/6,scanFile/6,makeGraph/5,pkgOk/2]).

:- use_module(topsort).
:- use_module(uri).
:- use_module(resource).
:- use_module(lexer).
:- use_module(grammar).
:- use_module(repository).
:- use_module(manifest).
:- use_module(misc).
:- use_module(abstract).
:- use_module(import).
:- use_module(errors).
:- use_module(catalog).

makeGraph(Repo,Cat,CWD,Fls,Groups) :-
  scanPkgs(Fls,Repo,Cat,CWD,[],Pkgs),
  graphPkgs(Pkgs,Groups).

graphPkgs(Pkgs,Groups) :-
  topsort(Pkgs,Groups).

scanPkgs([],_,_,_,Pkgs,Pkgs).
scanPkgs([P|L],Repo,Cat,CWD,SoFar,Pkgs) :-
  parsePkgName(P,Pkg),
  scanPkg(Pkg,Repo,Cat,CWD,SoFar,P1),
  scanPkgs(L,Repo,Cat,CWD,P1,Pkgs).

scanPkg(Pkg,_,_,_,SoFar,SoFar) :-
  is_member((Pkg,_,_,_),SoFar),!.
scanPkg(Pkg,Repo,Cat,CWD,SoFar,Pkgs) :-
  packagePresent(Repo,Pkg,_,SrcFn,SrcWhen,CodeWhen),
  ( CodeWhen>SrcWhen ->
    importPkg(Pkg,Repo,Spec),
    checkPkg(Spec,Repo,Cat,CWD,SrcFn,SoFar,Pkgs) ;
    scanFile(SrcFn,Repo,Cat,CWD,SoFar,Pkgs)).
scanPkg(Pkg,Repo,Cat,CWD,Pi,Px) :-
  ( resolveCatalog(Cat,Pkg,Uri) -> scanFile(Uri,Repo,Cat,CWD,Pi,Px) ;
    reportError("cannot locate package %s",[Pkg]),Pi=Px).

parsePkgName(P,pkg(Pkg,Version)) :-
  sub_string(P,Before,_,After,"#"),!,
  sub_string(P,0,Before,_,Pkg),
  sub_string(P,_,After,0,Version).
parsePkgName(P,pkg(P,defltVersion)).

checkPkg(spec(Pkg,_,_,_,_,_,Imports),Repo,Cat,CWD,SrcFn,SoFar,Pkgs) :-
  reformatImports(Imports,Imps),
  scanImports(Imps,Repo,Cat,CWD,[(Pkg,Imps,Imps,SrcFn)|SoFar],Pkgs).

reformatImports([],[]).
reformatImports([import(_,P)|L],[P|M]) :- reformatImports(L,M).

scanImports([],_,_,_,Pkgs,Pkgs).
scanImports([Pkg|Imports],Repo,Cat,CWD,SoFar,Pkgs) :-
  scanPkg(Pkg,Repo,Cat,CWD,SoFar,Pkg1),
  scanImports(Imports,Repo,Cat,CWD,Pkg1,Pkgs).

scanFile(Fl,Repo,Cat,CWD,SoFar,Pkgs) :-
  parseFile(Fl,Term),
  scanForImports(Term,Pkg,Imps),
  scanImports(Imps,Repo,Cat,CWD,[(pkg(Pkg,defltVersion),Imps,Imps,Fl)|SoFar],Pkgs).

parseFile(Fl,Term) :-
  locateResource(Fl,Txt),
  allTokens(Txt,Toks),
  parse(Toks,Term,_), !.

getSrcUri(Fl,WD,FUri) :-
  parseURI(Fl,FU),
  resolveURI(WD,FU,FUri).

scanForImports(Term,Pkg,Imports) :-
    isBraceTerm(Term,_,P,Els),
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
scanStmt(St,[pkg(Pk,defltVersion)|More],More) :- 
  isUnary(St,"import",P),
  scanPackageName(P,Pk).
scanStmt(_,Imp,Imp).

pkgOk(Pkg,Repo) :-
  packagePresent(Repo,Pkg,_,_,SrcWhen,CodeWhen),
  CodeWhen>SrcWhen.
