:- module(driver,[main/1,test/1,openR/3]).

/* Logic and Object compiler driver */

:- use_module(resource).
:- use_module(lexer).
:- use_module(grammar).
:- use_module(display).
:- use_module(canon).
:- use_module(checker).
:- use_module(transform).
:- use_module(plog).
:- use_module(errors).
:- use_module(genprolog).
:- use_module(uri).
:- use_module(catalog).
:- use_module(misc).
:- use_module(import).
:- use_module(repository).
:- use_module(grapher).

parseFlags([],_,[],[]).
parseFlags(['-g'|More],CWD,[debugging|Opts],Files) :- 
  parseFlags(More,CWD,Opts,Files).
parseFlags(['-p'|More],CWD,[profiling|Opts],Files) :- 
  parseFlags(More,CWD,Opts,Files).
parseFlags(['-r', R|More],CWD,[repository(Repo)|Opts],Files) :- 
  atom_string(R,RN),
  parseURI(RN,RU),
  resolveURI(CWD,RU,Ruri),
  openRepository(Ruri,Repo),
  parseFlags(More,CWD,Opts,Files).
parseFlags(['-v', V|More],CWD,[version(Vers)|Opts],Files) :-
  atom_string(V,Vers),
  parseFlags(More,CWD,Opts,Files).
parseFlags(['--'|More], _, [], Files) :- stringify(More,Files).
parseFlags(More, _, [], Files) :- stringify(More,Files).

stringify([],[]).
stringify([Name|More],[Fn|Files]) :- 
  atom_string(Name,Fn),
  stringify(More,Files).

main(Args) :- 
  getCWDUri(CWD),
  parseFlags(Args,CWD,Opts,Files),
  openRepo(Opts,Repo),!,
  makeGraph(Repo,CWD,Files,Groups),
  processGroups(Groups,[],Repo,CWD,Opts).

openR(Args,CWD,Repo) :-
  getCWDUri(CWD),
  parseFlags(Args,CWD,Opts,_),
  openRepo(Opts,Repo).

openRepo(Opts,Repo) :-
  is_member(repository(Repo),Opts),!.
openRepo(_,Repo) :-
  getCWDUri(CWD),
  openRepository(CWD,Repo).

processGroups([],_,_,_,_).
processGroups([G|L],CPkgs,Repo,CWD,Opts) :-
  (length(G,1) ; reportError("circular dependency in packages",G)),
  processGroup(G,CPkgs,CP0,Repo,R0,CWD,Opts),
  processGroups(L,CP0,R0,CWD,Opts).

processGroup([],CP,CP,Repo,Repo,_,_).
processGroup([(pk(P,V),Imps,Fl)|L],CP,CPx,Repo,Rx,CWD,Opts) :-
  processPkg(P,V,Imps,Fl,CP,CP0,CWD,Repo,R0,Opts),
  processGroup(L,CP0,CPx,R0,Rx,CWD,Opts).

processPkg(P,V,Imps,_,CP,CP,_,Repo,Repo,_) :-
  importsOk(Imps,CP),
  pkgOk(P,V,Repo),!,
  reportMsg("skipping package %s",[P]).
processPkg(P,V,_,Fl,CP,[pk(P,V)|CP],CWD,Repo,Rx,Opts) :-
  reportMsg("compiling package %s",[P]),
  processFile(Fl,CWD,Repo,Rx,Opts).

importsOk([],_).
importsOk([P|I],CP) :-
  \+ is_member(P,CP),
  importsOk(I,CP).

processFile(Fl,CWD,Repo,Rx,Opts) :-
  startCount,
  getSrcUri(Fl,CWD,SrcUri,FUrl),
  locateResource(FUrl,Src),
  parseFile(Src,Term),!,
  noErrors,
  checkProgram(Term,Repo,Prog),!,
  noErrors,
  displayCanon(Prog),
  transformProg(Prog,Opts,Rules),!,
  noErrors,
  displayPlRules(Rules),
  genRules(Rules,Text),
  packageName(Prog,Pkg),
  packageVersion(Opts,Vers),
  addPackage(Repo,SrcUri,Pkg,Vers,Text,Rx).

packageName(prog(Pkg,_,_,_,_,_,_,_),Pkg).

packageVersion(Opts,v(Vers)) :-
  is_member(version(Vers),Opts),!.
packageVersion(_,defltVersion).

parseFile(Txt,Term) :-
  allTokens(Txt,Toks),
  parse(Toks,Term,_), !.

test(Fl) :- 
  getCWDUri(CWD),
  processFile(Fl,CWD,[/*debugging*/]).

getCWDUri(WD) :-
  working_directory(C,C),
  atom_string(C,D),
  string_concat("file:",D,DT),
  parseURI(DT,WD).

getSrcUri(Fl,WD,FU,FUri) :-
  parseURI(Fl,FU),
  resolveURI(WD,FU,FUri).
