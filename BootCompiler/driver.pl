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

parseFlags([],[],[]).
parseFlags(['-g'|More],[debugging|Opts],Files) :- 
  parseFlags(More,Opts,Files).
parseFlags(['-p'|More],[profiling|Opts],Files) :- 
  parseFlags(More,Opts,Files).
parseFlags(['-b', B|More],[build(Build)|Opts],Files) :- 
  atom_string(B,Build), parseFlags(More,Opts,Files).
parseFlags(['--'|More], [], Files) :- stringify(More,Files).
parseFlags(More, [], Files) :- stringify(More,Files).

stringify([],[]).
stringify([Name|More],[Fn|Files]) :- 
  atom_string(Name,Fn),
  stringify(More,Files).

main(Args) :- 
  getCWDUri(CWD),
  parseFlags(Args,CWD,Opts,Files),
  processFiles(Files,Opts).

processFiles([],_,_).
processFiles([Fn|More],CWD,Opts) :-
  processFile(Fn,CWD,Opts),
  processFiles(More,CWD,Opts).

processFile(Fl,CWD,Opts) :-
  startCount,
  getSrcUri(Fl,CWD,FUrl),
  locateCatalog(FUrl,Cat),
  locateResource(FUrl,Src),
  parseFile(Src,Term),!,
  noErrors,
  wffModule(Term),!,
  noErrors,
  checkProgram(Term,Cat,Opts,Prog),!,
  noErrors,
  displayCanon(Prog),
  transformProg(Prog,Opts,Rules),!,
  noErrors,
  genRules(Rules,Text),!,
  makeOutputUri(FUrl,Opts,Fl,OutUri),
  putResource(OutUri,Text).

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



