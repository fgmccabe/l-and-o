:- module(abstract,[locOfAst/2,isAst/1,
      binary/5,unary/4,apply/4,isApply/3,
      isUnary/3,isUnary/4,isBinary/4,isBinary/5,isBinaryTerm/4,
      roundTerm/4,isRound/3,isRoundTerm/3,isRoundTerm/4,isTuple/2,isTuple/3,
      braceTerm/4,isBrace/3,isBraceTerm/3,isBraceTuple/3,
      squareTerm/4,isSquare/3,isSquare/4,isSquareTuple/3,isSquareTuple/2,isSquareTerm/3,
      isName/2,isIden/2,isIden/3,isString/2,
      isQuantified/3]).
:- use_module(operators).

apply(Lc,Op,Args,app(Lc,Op,Args)).

isApply(app(_,Op,Args),Nm,Args) :- isIden(Op,Nm).

isTuple(tuple(_,"()",Args),Args).

isTuple(tuple(Lc,"()",Args),Lc,Args).

roundTerm(Lc,Op,Args,app(Lc,name(Lc,Op),tuple(Lc,"()",Args))).

isRound(app(_,name(_,Op),tuple(_,"()",Args)),Op,Args).

isRoundTerm(app(_,Op,tuple(_,"()",Args)),Op,Args).

isRoundTerm(app(Lc,Op,tuple(_,"()",Args)),Lc,Op,Args).

binary(Lc,Op,L,R,app(Lc,name(Lc,Op),tuple(Lc,"()",[L,R]))).

isBinary(app(_,name(_,Op),tuple(_,"()",[L,R])),Op,L,R).

isBinary(app(Lc,name(_,Op),tuple(_,"()",[L,R])),Lc,Op,L,R).

isBinaryTerm(app(_,Op,tuple(_,"()",[L,R])),Op,L,R).

unary(Lc,Op,L,app(Lc,name(Lc,Op),tuple(Lc,"()",[L]))).

isUnary(app(_,name(_,Op),tuple(_,"()",[L])),Op,L).

isUnary(app(Lc,name(_,Op),tuple(_,"()",[L])),Lc,Op,L).

nary(Lc,Op,Args,app(Lc,name(Lc,Op),tuple(Lc,"()",Args))).

braceTerm(Lc,Op,Els,app(Lc,name(Lc,Op),tuple(Lc,"{}",Els))).

isBrace(app(_,name(_,Op),tuple(_,"{}",L)),Op,L).

isBraceTerm(app(_,Op,tuple(_,"{}",A)),Op,A).

isBraceTuple(tuple(Lc,"{}",L),Lc,L).

squareTerm(Lc,Op,Els,app(Lc,name(Lc,Op),tuple(Lc,"[]",Els))).

isSquare(app(_,name(_,Op),tuple(_,"[]",L)),Op,L).

isSquare(app(Lc,name(_,Op),tuple(_,"[]",L)),Lc,Op,L).

isSquareTerm(app(_,Op,tuple(_,"[]",L)),Op,L).

isSquareTuple(tuple(_,"[]",A),A).

isSquareTuple(tuple(Lc,"[]",L),Lc,L).

isName(name(_,Nm),Nm).

isIden(name(_,Nm),Nm).
isIden(tuple(_,"()",[name(_,Nm)]),Nm).

isIden(name(Lc,Nm),Lc,Nm).
isIden(tuple(Lc,"()",[name(_,Nm)]),Lc,Nm).

isString(string(_,St),St).
isString(interString(_,string(_,St)),St).

isAst(A) :- locOfAst(A,_).

locOfAst(name(Lc,_),Lc).
locOfAst(integer(Lc,_),Lc).
locOfAst(float(Lc,_),Lc).
locOfAst(string(Lc,_),Lc).
locOfAst(interString(Lc,_),Lc).
locOfAst(tuple(Lc,_,_),Lc).
locOfAst(app(Lc,_,_),Lc).


isQuantified(T,V,B) :- isUnary(T,"all",R), isBinary(R,"~~",V,B).
