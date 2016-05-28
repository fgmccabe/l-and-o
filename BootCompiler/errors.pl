:-module(errors,[reportError/3]).
:- use_module(display).
:- use_module(abstract).

reportError(Msg,A,Lc) :- write("Error: "),writef(Msg,A),nl(),write("At: "),writeln(Lc).

genDisplay([],[]).
genDisplay([A|L],[D|LL]) :- isAbstract(A),!, dispAst(A,0,Chrs,[]), string_chars(D,Chrs), genDisplay(L,LL).
