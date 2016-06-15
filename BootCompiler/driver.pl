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

parseFlags([],[],[]).
parseFlags(['-g'|More],[debugging|Opts],Files) :- parseFlags(More,Opts,Files).
parseFlags(['-p'|More],[profiling|Opts],Files) :- parseFlags(More,Opts,Files).
parseFlags(['--'|More], [], Files) :- stringify(More,Files).
parseFlags(More, [], Files) :- stringify(More,Files).

stringify([],[]).
stringify([Name|More],[Fn|Files]) :- 
  atom_string(Name,Fn),
  stringify(More,Files).

main(Args) :- 
  getBaseUrl(Base),
  locateCatalog(Base,Cat),
  parseFlags(Args,Opts,Files),
  processFiles(Files,Cat,Base,Opts).

processFiles([],_,_,_).
processFiles([Fn|More],Cat,Base,Opts) :-
  processFile(Base,Cat,Fn,Opts),
  processFiles(More,Cat,Base,Opts).

processFile(Base,Cat,Fl,Opts) :-
  startCount,
  parseURI(Fl,FUrl),
  locateResource(Base,FUrl,Text),
  parseFile(Text,Term),!,
  noErrors,
  wffModule(Term),!,
  noErrors,
  checkProgram(Term,Cat,Prog),!,
  noErrors,
  displayCanon(Prog),
  transformProg(Prog,Opts,Rules),!,
  noErrors,
  current_output(Out),
  genRules(Out,Rules),!.

parseFile(Txt,Term) :-
  allTokens(Txt,Toks),
  parse(Toks,Term,_), !.

test(Fl) :- 
  getBaseUrl(Base),
  locateCatalog(Base,Cat),
  processFile(Base,Cat,Fl,[/*debugging*/]).

getBaseUrl(Base) :-
  working_directory(C,C),
  atom_string(C,D),
  string_concat("file:",D,CWD),
  parseURI(CWD,Base).
