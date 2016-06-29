% :- module(lo,[main/1]).

:- use_module(ocall).
:- use_module(polyfill).
:- use_module(repository).
:- use_module(import).
:- use_module(uri).
:- use_module(misc).

parseFlags([],_,[],[]).
parseFlags(['-r', R|More],CWD,[repository(Repo)|Opts],Args) :- 
  atom_string(R,RN),
  parseURI(RN,RU),
  resolveURI(CWD,RU,Ruri),
  openRepository(Ruri,Repo),
  parseFlags(More,CWD,Opts,Args).
parseFlags(['--'|More], _, [], Args) :- stringify(More,Args).
parseFlags(More, _, [], Args) :- stringify(More,Args).

openRepo(Opts,Repo) :-
  is_member(repository(Repo),Opts),!.
openRepo(_,Repo) :-
  getCWDUri(CWD),
  openRepository(CWD,Repo).

stringify([],[]).
stringify([Name|More],[Fn|Files]) :- 
  atom_string(Name,Fn),
  stringify(More,Files).

main(Args) :- 
  getCWDUri(CWD),
  parseFlags(Args,CWD,Opts,[Entry|LOArgs]),
  openRepo(Opts,Repo),
  parsePkgName(Entry,Pkg,Vers),
  processPackage(Pkg,Vers,Repo,[],Loaded,[],_).

processPackage(Pkg,'*',Repo,Loaded,Ldx,PrIn,PrOut) :-
  processPackage(Pkg,defltVersion,Repo,Loaded,Ldx,PrIn,PrOut).
processPackage(Pkg,Vers,Repo,Loaded,Ldx,PrIn,PrOut) :-
  loadPkg(Pkg,Vers,Repo,Code,Imports),
  assertAll(Code,PrIn,Pr0),
  processImports(Imports,[(Pkg,Vers)|Loaded],Ldx,Repo,Pr0,PrOut).

assertAll([],Pr0,Pr0).
assertAll([T|M],Pr0,Prx) :-
  checkPred(T,Pr0,Pr1),
  assert(T),
  assertAll(M,Pr1,Prx).

checkPred(T,Pr,Px) :-
  predOf(T,P),!,
  (is_member(P,Pr),!,Px=Pr ; abolish(P),Px=[P|Pr]).

predOf((H :- _),P) :- !,
  predOf(H,P).
predOf(T,T/0) :- atom(T),!.
predOf(T,P/A) :-
  compound_name_arity(T,P,A).


processImports([],Ld,Ld,_,Pr,Pr).
processImports([import(_,Pkg,Vers)|Imports],Loaded,Ldx,Repo,Pr,Prx) :-
  is_member((Pkg,LdVers),Loaded),
  (LdVers \= Vers -> 
      runTimeMsg("not permitted to load multiple versions of same package: %s@%s, %s already loaded",[Pkg,Vers,LdVers]);true),
  processImports(Imports,Loaded,Ldx,Repo,Pr,Prx).
processImports([import(_,Pkg,Vers)|Imports],Loaded,Ldx,Repo,Pr,Prx) :-
  processPackage(Pkg,Vers,Repo,Loaded,LdI,Pr,Pr0),
  processImports(Imports,LdI,Ldx,Repo,Pr0,Prx).

parsePkgName(P,pkg(Pkg),v(Version)) :-
  sub_string(P,Before,_,After,"#"),!,
  sub_string(P,0,Before,_,Pkg),
  sub_string(P,_,After,0,Version).
parsePkgName(P,pkg(P),defltVersion).

getCWDUri(WD) :-
  working_directory(C,C),
  atom_string(C,D),
  string_concat("file:",D,DT),
  parseURI(DT,WD).

