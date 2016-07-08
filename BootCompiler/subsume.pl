:- module(subsume,[subType/3,sameType/3,glb/4,lub/4,getTypeFace/3]).

:- use_module(misc).
:- use_module(freshen).
:- use_module(dict).
:- use_module(types).

subType(T1,T2,Env) :-
  deRef(T1,Tp1),
  deRef(T2,Tp2),
  sb(Tp1,Tp2,Env),!. /* Only one way of doing subtype is permitted */
  
sb(anonType,_,_).
sb(_,anonType,_).
sb(voidType,_,_).
sb(_,topType,_).
sb(thisType,thisType,_).
sb(thisType,T2,Env) :- isVar("this",Env,vr(_,T1)),!,
  subType(T1,T2,Env).
sb(T1,thisType,Env) :- isVar("this",Env,vr(_,T2)),!,
  subType(T1,T2,Env).
sb(kVar(Nm),kVar(Nm),_).
sb(tVar(V1),tVar(V2),Env) :- sm(tVar(V1),tVar(V2),Env).
sb(tVar(V1),T2,Env) :- bounds(tVar(V1),L1,U1), glb(U1,T2,Env,Up), subType(L1,Up,Env), bind(tVar(V1),Up).
sb(T1,tVar(V2),Env) :- bounds(tVar(V2),L2,U2), lub(T1,L2,Env,Lw), subType(Lw,U2,Env), bind(tVar(V2),Lw).
sb(type(Nm),type(Nm),_).
sb(type(Nm),T2,Env) :- getTypeFace(type(Nm),Env,F1), getTypeFace(T2,Env,F2),sbFields(F1,F2,Env).
sb(typeExp(Nm,A1),typeExp(Nm,A2),Env) :- sbList(A1,A2,Env).
sb(typeExp(Nm,A1),T2,Env) :- getTypeFace(typeExp(Nm,A1),Env,F1), getTypeFace(T2,Env,F2), sbFields(F1,F2,Env).
sb(tupleType(A1),tupleType(A2),Env) :- sbList(A1,A2,Env).
sb(funType(A1,R1),funType(A2,R2),Env) :- subType(R1,R2,Env), sbArgs(A2,A1,Env).
sb(grammarType(A1,R1),grammarType(A2,R2),Env) :- subType(R1,R2,Env), sbArgs(A2,A1,Env).
sb(classType(A1,R1),classType(A2,R2),Env) :- subType(R1,R2,Env), sbList(A2,A1,Env).
sb(predType(A1),predType(A2),Env) :- sbArgs(A2,A1,Env).
sb(faceType(E1),faceType(E2),Env) :- sbFields(E1,E2,Env).

sbArgs([],[],_).
sbArgs([A1|L1],[A2|L2],Env) :- sbArg(A1,A2,Env), sbArgs(L1,L2,Env).

sbArg(in(T1),in(T2),Env) :- subType(T1,T2,Env).
sbArg(in(T1),out(T2),Env) :- sameType(T1,T2,Env).
sbArg(in(T1),inout(T2),Env) :- sameType(T1,T2,Env).
sbArg(out(T1),in(T2),Env) :- sameType(T2,T1,Env).
sbArg(out(T1),out(T2),Env) :- subType(T2,T1,Env).
sbArg(out(T1),inout(T2),Env) :- sameType(T2,T1,Env).
sbArg(inout(T1),in(T2),Env) :- sameType(T1,T2,Env).
sbArg(inout(T1),out(T2),Env) :- sameType(T1,T2,Env).
sbArg(inout(T1),inout(T2),Env) :- sameType(T1,T2,Env).

sbList([],[],_).
sbList([E1|L1],[E2|L2],Env) :- subType(E1,E2,Env), sbList(L1,L2,Env).

sbFields(_,[],_).
sbFields(L1,[(F2,E2)|L2],Env) :- is_member((F2,E1),L1), !, subType(E1,E2,Env), sbFields(L1,L2,Env).

glb(voidType,_,_,voidType) :- !.
glb(T,topType,_,T) :- !.
glb(T1,T2,Env,T1) :- subType(T1,T2,Env),!.
glb(T1,T2,Env,T2) :- subType(T2,T1,Env),!.
glb(T1,T2,Env,faceType(Face)) :-  getTypeFace(T1,Env,F1), getTypeFace(T2,Env,F2), unionFaces(F1,F2,Env,Face).

lub(voidType,T,_,T) :- !.
lub(_,topType,_,topType) :- !.
lub(T1,T2,Env,T2) :- subType(T1,T2,Env),!.
lub(T1,T2,Env,T1) :- subType(T2,T1,Env),!.
lub(T1,T2,Env,faceType(Face)) :-  getTypeFace(T1,Env,F1), getTypeFace(T2,Env,F2), intersectFaces(F1,F2,Env,Face).

unionFaces([],F,_,F).
unionFaces([(K,T1)|Rest],F,Env,Face) :-
  is_member((K,T2),F),!,
  sameType(T1,T2,Env),
  unionFaces(Rest,F,Env,Face).
unionFaces([(K,T)|Rest],F,Env,[(K,T)|Face]) :-
  unionFaces(Rest,F,Env,Face).

intersectFaces([],_,_,[]).
intersectFaces([(K,T1)|Rest],F,Env,[(K,T1)|Face]) :-
  is_member((K,T2),F),!,
  sameType(T1,T2,Env),
  intersectFaces(Rest,F,Env,Face).
intersectFaces([_|Rest],F,Env,Face) :-
  intersectFaces(Rest,F,Env,Face).

rewriteType(Nm,T,Tp,Env) :-
  typeRules(Nm,Env,Rules),
  is_member(Rl,Rules),
  freshen(Rl,T,_,typeRule(Arg,Tp)),
  sameType(T,Arg,Env).

sameType(in(T1),T2,Env) :- subType(T1,T2,Env).
sameType(out(T1),T2,Env) :- subType(T2,T1,Env).
sameType(inout(T1),T2,Env) :- sameType(T1,T2,Env).
sameType(T1,T2,Env) :- deRef(T1,Tp1), deRef(T2,Tp2), sm(Tp1,Tp2,Env), !.
% sameType(T1,T2,Env) :- deRef(T1,Tp1), deRef(T2,Tp2), sb(Tp1,Tp2,Env), sb(Tp2,Tp1,Env), !.

sm(_,anonType,_).
sm(anonType,_,_).
sm(voidType,voidType,_).
sm(topType,topType,_).
sm(thisType,T2,Env) :- isVar("this",Env,vr(_,T1)),!,
  sameType(T1,T2,Env).
sm(T1,thisType,Env) :- isVar("this",Env,vr(_,T2)),!,
  sameType(T1,T2,Env).
sm(kVar(Nm),kVar(Nm),_).
sm(tVar(V1),tVar(V2),Env) :- varBinding(tVar(V1),tVar(V2),Env).
sm(tVar(V1),T2,Env) :- checkBinding(V1,T2,Env).
sm(T1,tVar(V2),Env) :- checkBinding(V2,T1,Env).
sm(type(Nm),type(Nm),_).
sm(typeExp(Nm,A1),typeExp(Nm,A2),Env) :- smList(A1,A2,Env).
sm(tupleType(A1),tupleType(A2),Env) :- smList(A1,A2,Env).
sm(funType(A1,R1),funType(A2,R2),Env) :- sameType(R1,R2,Env), smArgs(A2,A1,Env).
sm(grammarType(A1,R1),grammarType(A2,R2),Env) :- sameType(R1,R2,Env), smArgs(A2,A1,Env).
sm(classType(A1,R1),classType(A2,R2),Env) :- sameType(R1,R2,Env), smList(A2,A1,Env).
sm(predType(A1),predType(A2),Env) :- smArgs(A2,A1,Env).
sm(faceType(E1),faceType(E2),Env) :- length(E1,L), length(E2,L), smFields(E1,E2,Env).

varBinding(T1,T2,_) :- isIdentical(T1,T2),!.
varBinding(T1,T2,Env) :- 
  bounds(T1,L1,U1), bounds(T2,L2,U2), glb(U1,U2,Env,Up), 
  lub(L1,L2,Env,Lw),
  subType(Lw,Up,Env),
  markLower(T2,Lw),
  markUpper(T2,Up),
  bind(T1,T2).

checkBinding(V,tVar(TV),Env) :- !, varBinding(tVar(V),tVar(TV),Env).
checkBinding(V,Tp,Env) :- subType(Tp,V.upper,Env), subType(V.lower,Tp,Env),
  bind(tVar(V),Tp).

smArgs([],[],_).
smArgs([E1|L1],[E2|L2],Env) :- sameArgType(E1,E2,Env), smArgs(L1,L2,Env).

sameArgType(in(T1),in(T2),Env) :- sameType(T1,T2,Env).
sameArgType(out(T1),out(T2),Env) :- sameType(T2,T1,Env).
sameArgType(inout(T1),inout(T2),Env) :- sameType(T1,T2,Env).

smList([],[],_).
smList([E1|L1],[E2|L2],Env) :- sameType(E1,E2,Env), smList(L1,L2,Env).

smFields(_,[],_).
smFields(L1,[(F2,E2)|L2],Env) :- is_member((F2,E1),L1), sameType(E1,E2,Env), smFields(L1,L2,Env).

getTypeFace(T,Env,Face) :-
  deRef(T,Tp),
  getFace(Tp,Env,Face).

getFace(type(Nm),Env,Face) :- !,
  typeFaceRule(Nm,Env,FaceRule),
  freshen(FaceRule,voidType,_,typeRule(Lhs,faceType(Face))),
  sameType(type(Nm),Lhs,Env),!.
getFace(typeExp(Nm,Args),Env,Face) :- !,
  typeFaceRule(Nm,Env,FaceRule),
  freshen(FaceRule,voidType,_,typeRule(Lhs,faceType(Face))),
  sameType(Lhs,typeExp(Nm,Args),Env),!.
getFace(T,Env,Face) :- isUnbound(T), !,
  upperBoundOf(T,Up),
  getTypeFace(Up,Env,Face),!.
getFace(faceType(Face),_,Face) :- !.
% getFace(_,_,[]).
