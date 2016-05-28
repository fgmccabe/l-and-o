:- module(abstract,[locOfAst/2,
      binary/5,unary/4,apply/4,
      isUnary/3,isBinary/4,isBinaryTerm/4,
      roundTerm/4,isRound/3,isRoundTerm/3,isTuple/2,
      braceTerm/4,isBrace/3,isBraceTerm/3,braceTuple/3,
      squareTerm/4,isSquare/3,squareTuple/3,
      isName/2,isAst/1]).
:- use_module(operators).

apply(Lc,Op,Args,app(Lc,Op,Args)).

isTuple(tuple(_,"()",Args),Args).

roundTerm(Lc,Op,Args,app(Lc,name(Lc,Op),tuple(Lc,"()",Args))).

isRound(app(_,name(_,Op),tuple(_,"()",Args)),Op,Args).

isRoundTerm(app(_,Op,tuple(_,"()",Args)),Op,Args).

binary(Lc,Op,L,R,app(Lc,name(Lc,Op),tuple(Lc,"()",[L,R]))).

isBinary(app(_,name(_,Op),tuple(_,"()",[L,R])),Op,L,R).

isBinaryTerm(app(_,Op,tuple(_,"()",[L,R])),Op,L,R).

unary(Lc,Op,L,app(Lc,name(Lc,Op),tuple(Lc,"()",[L]))).

isUnary(app(_,name(_,Op),tuple(_,"()",[L])),Op,L).

nary(Lc,Op,Args,app(Lc,name(Lc,Op),tuple(Lc,"()",Args))).

braceTerm(Lc,Op,Els,app(Lc,name(Lc,Op),tuple(Lc,"{}",Els))).

isBrace(app(_,name(_,Op),tuple(_,"{}",L)),Op,L).

isBraceTerm(app(_,Op,tuple(_,"{}",A)),Op,A).

braceTuple(tuple(Lc,"{}",L),Lc,L).

squareTerm(Lc,Op,Els,app(Lc,name(Lc,Op),tuple(Lc,"[]",Els))).

isSquare(app(_,name(_,Op),tuple(_,"[]",L)),Op,L).

squareTuple(tuple(Lc,"[]",L),Lc,L).

isName(name(_,Nm),Nm).

isAst(A) :- locOfAst(A,_).

locOfAst(name(Lc,_),Lc).
locOfAst(integer(Lc,_),Lc).
locOfAst(long(Lc,_),Lc).
locOfAst(float(Lc,_),Lc).
locOfAst(string(Lc,_),Lc).
locOfAst(interString(Lc,_),Lc).
locOfAst(tuple(Lc,_,_),Lc).
locOfAst(app(Lc,_,_),Lc).

