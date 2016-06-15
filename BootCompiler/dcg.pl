:- module(dcg,[dcgRule/3]).
 
:- use_module(abstract).
:- use_module(misc).

dcgRule(Rule,[Clause|Tail],Tail) :-
 isBinary(Rule,Lc,Hd,B),
 genName("S",Strm),
 dcgBody(B,Strm,Strmx,name(Lc,"true"),Cond),
 dcgHead(Hd,Head,Strm,Strmx,Cond,Body),
 binary(Lc,":-",Head,Body,Clause).

pushString(Str,Lc,V,Strm,Strmx,S0,Sx) :-
  string_codes(Str,Chrs),
  pushCharList(Chrs,Lc,V,Strm,Strmx,S0,Sx).

pushCharList([],_,_,Strm,Strmx,S,Sx) :-
  joinStream(Strm,Strmx,S,Sx).
pushCharList([Ch|Chrs],Lc,Verb,St,Stx,S,Sx) :-
  genName("X",X),
  streamCall(Lc,Verb,[integer(Lc,Ch),X],St,Nx),
  conjunct(Lc,S,Nx,S0),
  pushCharList(Chrs,Lc,Verb,X,Stx,S0,Sx).

dcgList([],_,Strm,Strmx,S,Sx) :-
  joinStream(Strm,Strmx,S,Sx).
dcgList([string(Lc,Str)|More],V,Strm,Strmx,S0,Sx) :-
  pushString(Str,Lc,V,Strm,Strm0,S0,S1),
  dcgList(More,V,Strm0,Strmx,S1,Sx).
dcgList([G|More],V,Strm,Strmx,S,Sx) :-
  isBinary(G,Lc,"::",P,Gu),
  dcgList([P],V,Strm,Strm0,S,S0),
  conjunct(Lc,S0,Gu,S1),
  dcgList(More,V,Strm0,Strmx,S1,Sx).
dcgList([P|More],V,Strm,Strmx,S,Sx) :-
  locOfAst(P,Lc),
  genName("X",X),
  streamCall(Lc,V,[P,X],Strm,Nx),
  conjunct(Lc,S,Nx,S1),
  dcgList(More,V,X,Strmx,S1,Sx).

dcgHead(H,Head,Strm0,Strm,B,Body) :-
  isBinary(H,",",_,Gl,Lk),
  isTuple(Lk,Look),
  isRoundTerm(Gl,Lc,Op,Args),
  dcgList(Look,"cons",Strm,_,B,Body),
  concat(Args,[Strm0,Strm],GArgs),
  roundTerm(Lc,Op,GArgs,Head).
dcgHead(H,Head,Strm0,Strm,B,B) :-
  isRoundTerm(H,Lc,Op,Args),
  concat(Args,[Strm0,Strm],GArgs),
  roundTerm(Lc,Op,GArgs,Head).

dcgBody(Empty,Strm,Strmx,S,Sx) :-
  isNullBody(Empty),
  joinStream(Strm,Strmx,S,Sx).
dcgBody(name(Lc,"eof"),Strm,Strm,S,Sx) :-
  streamCall(Lc,"eof",[],Strm,Nx),
  conjunct(Lc,S,Nx,Sx).
dcgBody(string(Lc,Text),Strm,Strmx,S,Sx) :-
  string_codes(Text,Codes),
  pushCharList(Codes,Lc,"hdtl",Strm,Strmx,S,Sx).
dcgBody(T,Strm,Strmx,S,Sx) :-
  isTuple(T,Els),
  dcgList(Els,"hdtl",Strm,Strmx,S,Sx).
dcgBody(Conj,Strm,Strmx,S,Sx) :-
  isBinary(Conj,",",L,R),
  dcgBody(L,Strm,Strm0,S,S0),
  dcgBody(R,Strm0,Strmx,S0,Sx).
dcgBody(Guarded,Strm,Strmx,S,Sx) :-
  isBinary(Guarded,Lc,"::",L,R),
  dcgBody(L,Strm,Strmx,S,S0),
  conjunct(Lc,S0,R,Sx).
dcgBody(Cond,Strm,Strmx,S0,Sx) :-
  isConditional(Cond,Lc,Test,Then,Else),
  dcgBody(Test,Strm,Strm0,name(Lc,"true"),T),
  dcgBody(Then,Strm0,Strmx,name(Lc,"true"),Th),
  dcgBody(Else,Strm,Strmx,name(Lc,"true"),El),
  conditional(Lc,T,Th,El,Cond),
  conjunct(Lc,S0,Cond,Sx).
dcgBody(Disj,Strm,Strmx,S0,Sx) :-
  isBinary(Disj,Lc,";",L,R),
  dcgBody(L,Strm,Strmx,name(Lc,"true"),Either),
  dcgBody(R,Strm,Strmx,name(Lc,"true"),Or),
  binary(Lc,";",Either,Or,Nx),
  conjunct(Lc,S0,Nx,Sx).
dcgBody(Neg,Strm,Strm,S0,Sx) :-
  isUnary(Neg,Lc,"\\+",Ng),
  dcgBody(Ng,Strm,_,N),
  unary(Lc,"\\+",N,NG),
  conjunct(Lc,S0,NG,Sx).
dcgBody(One,Strm,Strmx,S0,Sx) :-
  isUnary(One,Lc,"!",O),
  dcgBody(O,Strm,Strmx,name(Lc,"true"),OG),
  unary(Lc,"!",OG,OneG),
  conjunct(Lc,S0,OneG,Sx).
%%%% Do iteration as fold when lambdas done
dcgBody(Call,Strm,Strmx,S0,Sx) :-
  isRoundTerm(Call,Lc,Op,Args),
  genName("NX",NX),
  concat(Args,[Strm,NX],GArgs),
  roundTerm(Lc,Op,GArgs,GCall),
  conjunct(Lc,S0,GCall,S1),
  joinStream(Strmx,NX,S1,Sx).

isNullBody(Empty) :-
  isSquareTuple(Empty,[]).

joinStream(Strm,Strm,S,S).
joinStream(Strm,Strmx,S,Sx) :-
  locOfAst(S,Lc),
  binary(Lc,"=",Strm,Strmx,Eq),
  conjunct(Lc,S,Eq,Sx).

isConditional(Cond,Lc,Test,Then,Else) :-
  isBinary(Cond,Lc,";",L,Else),
  isBinary(L,"?",Test,Then).

conditional(Lc,Test,Then,Else,Cond) :-
  binary(Lc,"?",Test,Then,L),
  binary(Lc,";",L,Else,Cond).

streamCall(Lc,Verb,Args,Strm,Goal) :-
  roundTerm(Lc,Verb,Args,Call),
  dotCall(Lc,Strm,Call,Goal).

dotCall(Lc,Rec,Call,Dot) :-
  binary(Lc,".",Rec,Call,Dot).

conjunct(_,name(_,"true"),G,G) :-!.
conjunct(Lc,L,R,C) :-
  binary(Lc,",",L,R,C).
