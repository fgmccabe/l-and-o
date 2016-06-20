:- module(lo,[main/1]).

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
  processPackage(Pkg,Vers,Repo,[],Loaded).

processPackage(Pkg,'*',Repo,Loaded,Ldx) :-
  processPackage(Pkg,defltVersion,Repo,Loaded,Ldx).
processPackage(Pkg,Vers,Repo,Loaded,Ldx) :-
  loadPkg(Pkg,Vers,Repo,Code,Imports),
  assertAll(Code),
  processImports(Imports,[(Pkg,Vers)|Loaded],Ldx,Repo).

assertAll([]).
assertAll([T|M]) :-
  assert(T),
  assertAll(M).

processImports([],Ld,Ld,_).
processImports([import(_,Pkg,Vers)|Imports],Loaded,Ldx,Repo) :-
  is_member((Pkg,LdVers),Loaded),
  (LdVers \= Vers -> 
      runTimeMsg("not permitted to load multiple versions of same package: %s@%s, %s already loaded",[Pkg,Vers,LdVers]);true),
  processImports(Imports,Loaded,Ldx,Repo).
processImports([import(_,Pkg,Vers)|Imports],Loaded,Ldx,Repo) :-
  processPackage(Pkg,Vers,Repo,Loaded,LdI),
  processImports(Imports,LdI,Ldx,Repo).

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

