:-module(dispAst,[dispAst/4,dispAstTerm/3,display/2]).
:- use_module(operators).
:- use_module(misc).

dispAstTerm(Msg,Term,Pr) :- write(Msg), display(Term,Pr), nl().

display(Term,Pr) :- dispAst(Term,Pr,Chrs,[]), string_chars(Res,Chrs), write(Res).

dispAst(name(_,Nm),_,O,E) :- appStr(Nm,O,E).
dispAst(long(_,Nm),_,O,E) :- number_chars(Nm,Chrs), concat(Chrs,E,O).
dispAst(float(_,Nm),_,O,E) :- number_chars(Nm,Chrs), concat(Chrs,E,O).
dispAst(string(_,S),_,['"'|O],E) :- appStr(S,O,O1), appStr(S,O1,O2), appStr(O2,"""",E).
dispAst(display(_,Term),_,['<','<'|O],E) :- dispAst(Term,O,['>','>'|E]).
dispAst(format(_,Term,Fmt),_,['<','<'|O],E) :- dispAst(Term,O,O1), 
    string_chars(Fmt,FChrs),
    concat([':'|FChrs],O2,O1),
    concat(['>','>'],E,O2).
dispAst(interString(_,Segments),_,O,E) :- writeEls(Segments,0,"++",O,E).
dispAst(tuple(_,Nm,A),_,O,E) :- bracket(Nm,Left,Right,Sep,Pr),
    appStr(Left,O,O1),
    writeEls(A,Pr,Sep,O1,O2),
    appStr(Right,O2,E).
dispAst(app(_,name(_,Nm),tuple(_,"()",[A])),Pr,O,E) :- 
  prefixOp(Nm,OpPr, RightPr),!,
  openParen(Pr,OpPr,O,O1),
  appStr(Nm,O1,O2),
  appStr(" ",O2,O3),
  dispAst(A,RightPr,O3,O4),
  closeParen(Pr,OpPr,O4,E).
dispAst(app(_,name(_,Nm),tuple(_,"()",[A])),Pr,O,E) :- 
  postfixOp(Nm,LeftPr,OpPr),!,
  openParen(Pr,OpPr,O,O1),
  dispAst(A,LeftPr,O1,O2),
  appStr(" ",O2,O3),
  appStr(Nm,O3,O4),
  closeParen(Pr,OpPr,O4,E).
dispAst(app(_,name(_,Nm),tuple(_,"()",[A,B])),Pr,O,E) :- 
  infixOp(Nm,LeftPr,OpPr, RightPr),!,
  openParen(Pr,OpPr,O,O1),
  dispAst(A,LeftPr,O1,O2),
  appStr(" ",O2,O3),
  appStr(Nm,O3,O4),
  appStr(" ",O4,O5),
  dispAst(B,RightPr,O5,O6),
  closeParen(Pr,OpPr,O6,E).
dispAst(app(_,Op,A),_,O,E) :- dispAst(Op,0,O,O1), dispAst(A,0,O1,E).

bracket("()","(",")",", ",1000).
bracket("[]","[","]",", ",1000).
bracket("{}","{","}",". ",2000).
bracket("<||>","<|","|>",". ",2000).

appStr(Str,O,E) :- string_chars(Str,Chrs), concat(Chrs,E,O).

writeEls([],_,_,O,O) :- !.
writeEls([H|M],Pr,Sep,O,E) :- dispAst(H,Pr,O,O1), writeMoreEls(M,Pr,Sep,O1,E).

writeMoreEls([],_,_,O,O) :- !.
writeMoreEls([H|M],Pr,Sep,O,E) :- appStr(Sep,O,O1), dispAst(H,Pr,O1,O2), writeMoreEls(M,Pr,Sep,O2,E).

openParen(Pr,OpPr,O,E) :- OpPr > Pr,!, appStr("(",O,E).
openParen(_,_,O,O).

closeParen(Pr,OpPr,O,E) :- OpPr > Pr, appStr(")",O,E).
closeParen(_,_,O,O).

