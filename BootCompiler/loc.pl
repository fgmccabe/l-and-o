:- use_module(grab).
:- use_module(lexer).
:- use_module(grammar).
:- use_module(display).
:- use_module(canon).
:- use_module(wff).
:- use_module(checker).
:- use_module(transform).
:- use_module(plog).
:- use_module(errors).

/* Logic and Object compiler driver */

parseFile(Fl,Term) :- grab_text(Fl,Txt),
  allTokens(Txt,Toks), parse(Toks,Term,_), !.

wffFile(Fl,Term) :- startCount, parseFile(Fl,Term), wffModule(Term), !, noErrors.

typeFile(Fl,Prog) :-
  wffFile(Fl,Term),
  checkProgram(Term,Prog),
  displayCanon(Prog).

test(Fl) :- 
  typeFile(Fl,Prog),
  transformProg(Prog,[],Rules),
  displayPlRules(Rules).

main :-test("../Tests/a.lo").
