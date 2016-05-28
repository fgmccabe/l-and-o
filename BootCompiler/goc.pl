:-use_module(grab,[grab_text/2]).
:-use_module(lexer).
:-use_module(grammar).
:-use_module(display).
:-use_module(wff).

parseFile(Fl,Term) :- grab_text(Fl,Txt),
  allTokens(Txt,Toks), parse(Toks,Term,_).

test(Fl) :- 
  parseFile(Fl,Term),
  display(Term,0),
  wffModule(Fl,Term),
  display(Term,2000).

main :-test("../Tests/a.gol").
