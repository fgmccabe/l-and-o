:- module(parsetype,[parseType/3,parseType/4,parseType/5,parseTypeRule/4]).

:- use_module(abstract).
:- use_module(dict).
:- use_module(errors).
:- use_module(freshen).
:- use_module(misc).
:- use_module(subsume).

parseType(Tp,Env,T) :-
  parseType(Tp,Env,[],_,T).

parseType(Tp,Env,Bound,T) :-
  parseType(Tp,Env,Bound,_,T).

parseType(Tp,Env,B,Bound,PT) :-
  isQuantified(Tp,V,BT),
  parseBound(V,B,B0,Env,PT,Inner),
  parseType(BT,Env,B0,Bound,Inner).
parseType(Nm,Env,B,B,Tp) :- isName(Nm,Id), parseTypeName(Id,Env,B,Tp).
parseType(Sq,Env,B,Bound,typeExp(Op,ArgTps)) :- 
  isSquare(Sq,N,Args),
  parseTypeName(N,Env,B,typeExp(Op,_ATs)),
  parseTypes(Args,Env,B,Bound,ArgTps). %% should do this too:  sameType(tupleType(ATs),tupleType(ArgTps),Env).
parseType(F,Env,B,Bound,funType(AT,RT)) :-
  isBinary(F,"=>",L,R),
  isTuple(L,LA),
  parseTypes(LA,Env,B,B0,AT),
  parseType(R,Env,B0,Bound,RT).
parseType(F,Env,B,Bound,classType(AT,RT)) :-
  isBinary(F,"<=>",L,R),
  isTuple(L,LA),
  parseTypes(LA,Env,B,B0,AT),
  parseType(R,Env,B0,Bound,RT).
parseType(C,Env,B,Bound,predType(AT)) :-
  isBraceTerm(C,L,[]),
  isTuple(L,A),
  parseTypes(A,Env,B,Bound,AT).
parseType(T,Env,B,Bound,tupleType(AT)) :-
  isTuple(T,A),
  parseTypes(A,Env,B,Bound,AT).
parseType(T,Env,Bound,Bound,faceType(AT)) :-
  isBraceTuple(T,_,L),
  parseTypeFields(L,Env,Bound,AT).
parseType(T,_,_,B,B,anonType) :-
  locOfAst(T,Lc),
  reportError("cannot understand type %s",[T],Lc).

parseTypeName("_",_,_,anonType).
parseTypeName("void",_,_,voidType).
parseTypeName("top",_,_,topType).
parseTypeName("this",_,_,thisType).
parseTypeName(Id,_,B,Tp) :- is_member((Id,Tp),B),!.
parseTypeName(Id,Env,_,FTp) :- typeInDict(Id,Env,Tp),freshen(Tp,thisType,_,FTp).

parseBound(P,BV,Bound,Env,QT,Inner) :-
  isBinary(P,",",L,R),
  parseBound(L,BV,B0,Env,QT,Q0),
  parseBound(R,B0,Bound,Env,Q0,Inner).
parseBound(V,B,[(Nm,kVar(Nm))|B],Env,univType(constrained(Lower,Nm,Upper),Inner),Inner) :-
  isBinary(V,"<~",L,Up),
  isBinary(L,"<~",Lw,Vr),
  isName(Vr,Nm),
  parseType(Up,Env,[(Nm,kVar(Nm))|B],_,Upper),
  parseType(Lw,Env,[(Nm,kVar(Nm))|B],_,Lower).
parseBound(V,B,[(Nm,kVar(Nm))|B],Env,univType(constrained(voidType,Nm,Upper),Inner),Inner) :-
  isBinary(V,"<~",Vr,Up),
  isName(Vr,Nm),
  parseType(Up,Env,[(Nm,kVar(Nm))|B],_,Upper).
parseBound(V,B,[(N,kVar(N))|B],_,univType(N,Inner),Inner) :-
  isName(V,N).

parseTypes([],_,B,B,[]).
parseTypes([A|AT],Env,B,Bound,[Atype|ArgTypes]) :-
  parseArgType(A,Env,B,B0,Atype),
  parseTypes(AT,Env,B0,Bound,ArgTypes).

parseArgType(A,Env,B,Bound,AType) :-
  isUnary(A,"+",AA),
  parseType(AA,Env,B,Bound,AType).
parseArgType(A,Env,B,Bound,AType) :-
  isUnary(A,"-",AA),
  parseType(AA,Env,B,Bound,AType).
parseArgType(A,Env,B,Bound,AType) :-
  isUnary(A,"+-",AA),
  parseType(AA,Env,B,Bound,AType).
parseArgType(A,Env,B,Bound,AType) :-
  isUnary(A,"-+",AA),
  parseType(AA,Env,B,Bound,AType).
parseArgType(A,Env,B,Bound,AType) :-
  parseType(A,Env,B,Bound,AType).

parseTypeFields([],_,_,[]).
parseTypeFields([F|L],Env,Bound,[(Fld,FldTp)|Fields]) :-
  isBinary(F,":",Nm,FT),
  isName(Nm,Fld),
  parseType(FT,Env,Bound,_,FldTp),
  parseTypeFields(L,Env,Bound,Fields).

parseTypeRule(St,Env,Rule,Path) :-
  parseTypeRule(St,[],Env,Rule,Path).

parseTypeRule(St,B,Env,Rule,Path) :-
  isQuantified(St,V,Body),
  noConstraintsBound(V,B,BB),
  parseRule(Body,BB,Env,Ex,Inner,Path),
  parseBound(V,B,_,Ex,Rule,Inner).
parseTypeRule(St,B,Env,Rule,Path) :-
  parseRule(St,B,Env,_,Rule,Path).

noConstraintsBound(P,B,Bound) :-
  isBinary(P,",",L,R),
  noConstraintsBound(L,B,B0),
  noConstraintsBound(R,B0,Bound).
noConstraintsBound(V,B,[(Nm,kVar(Nm))|B]) :-
  isBinary(V,"<~",L,_),
  isBinary(L,"<~",_,Vr),
  isName(Vr,Nm).
noConstraintsBound(V,B,[(Nm,kVar(Nm))|B]) :-
  isBinary(V,"<~",Vr,_),
  isName(Vr,Nm).
noConstraintsBound(V,B,[(N,kVar(N))|B]) :-
  isName(V,N).

parseRule(St,B,Env,Ex,typeRule(Lhs,Rhs),Path) :-
  isBinary(St,"<~",L,R),
  parseTypeHead(L,B,Lhs,Env,Ex,Path),!,
  parseType(R,Ex,B,Rhs).

parseTypeHead(N,B,type(TpNm),Env,Ex,Path) :-
  isIden(N,Lc,Nm),
  (B = [] ; reportError("too many quantifiers in type def for %s",[N],Lc)),
  subPath(Path,Nm,TpNm),
  declareType(Nm,Lc,type(TpNm),Env,Ex). % temporary definition
parseTypeHead(N,B,typeExp(TpNm,Args),Env,Ex,Path) :-
  isSquare(N,Nm,A),
  locOfAst(N,Lc),
  (length(B,Sz),length(A,Sz); reportError("incorrect number of quantifiers for %s",[N],Lc)),
  parseHeadArgs(A,B,Args),
  subPath(Path,Nm,TpNm),
  attachBinding(B,typeExp(TpNm,Args),Tp),
  declareType(Nm,Lc,Tp,Env,Ex). % temporary definition

parseHeadArgs([],_,[]).
parseHeadArgs([H|L],B,[V|Args]) :-
  isIden(H,Lc,Nm),
  (is_member((Nm,V),B) ; reportError("argument %s not quantified ",[H],Lc)),
  parseHeadArgs(L,B,Args).

attachBinding([],Tp,Tp).
attachBinding([(Nm,_)|B],Tp,RT) :- attachBinding(B,univType(Nm,Tp),RT).
