:- module(driver,[main/1,test/1]).

/* Logic and Object compiler driver */

:- use_module(resource).
:- use_module(lexer).
:- use_module(grammar).
:- use_module(display).
:- use_module(canon).
:- use_module(wff).
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
  processFiles(Files,CWD,Repo,Opts).

openRepo(Opts,Repo) :-
  is_member(repository(Repo),Opts),!.
openRepo(_,Repo) :-
  getCWDUri(CWD),
  openRepository(CWD,Repo).

processFiles([],_,_,_).
processFiles([Fn|More],CWD,Repo,Opts) :-
  processFile(Fn,CWD,Repo,Rx,Opts),!,
  processFiles(More,CWD,Rx,Opts).
processFiles([_|More],CWD,Repo,Opts) :-
  processFiles(More,CWD,Repo,Opts).

processFile(Fl,CWD,Repo,Rx,Opts) :-
  startCount,
  getSrcUri(Fl,CWD,FUrl),
  locateResource(FUrl,Src),
  parseFile(Src,Term),!,
  noErrors,
  wffModule(Term),!,
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
  addPackage(Repo,Pkg,Vers,Text,Rx).

packageName(prog(Pkg,_,_,_,_,_),Pkg).

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

getSrcUri(Fl,WD,FUri) :-
  parseURI(Fl,FU),
  resolveURI(WD,FU,FUri).
