:- use_module(grab).
:- use_module(lexer).
:- use_module(grammar).
:- use_module(display).
:- use_module(canon).
:- use_module(wff).
:- use_module(checker).
:- use_module(errors).

/* Logic and Object compiler driver */

parseFile(Fl,Term) :- grab_text(Fl,Txt),
  allTokens(Txt,Toks), parse(Toks,Term,_), !.

wffFile(Fl,Term) :- startCount, parseFile(Fl,Term), wffModule(Term), !, noErrors.

test(Fl) :- 
  wffFile(Fl,Term),
  checkProgram(Term,Prog),
  displayCanon(Prog).

main :-test("../Tests/tree.lo").
