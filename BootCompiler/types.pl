:- module(types,[isType/1,newTypeVar/2,newTypeVar/4,deRef/2, 
      typeArity/2,isFunctionType/2,isPredType/2,isClassType/2,
      showType/3, showTypeRule/3,
      occursIn/2,isUnbound/1,isBound/2, upperBound/2, upperBoundOf/2, lowerBound/2, lowerBoundOf/2, bounds/3, 
      bind/2, isIdentical/2, moveQuants/3,
      markLower/2, markUpper/2]).
:- use_module(misc).

isType(anonType).
isType(voidType).
isType(topType).
isType(kVar(_)).
isType(tVar(_)).
isType(type(_)).
isType(typeExp(_,_)).
isType(tupleType(_)).
isType(funType(_,_)).
isType(classType(_,_)).
isType(predType(_)).
isType(univType(_,_)).
isType(faceType(_)).
isType(constrained(_,_,_)).

% the _ in unb(_) is to work around issues with SWI-Prolog's assignment.
newTypeVar(Nm,tVar(v{lower:voidType,upper:topType,curr:unb(_),name:Nm,id:Id})) :- gensym("_#",Id).
newTypeVar(Nm,tVar(v{lower:Lower,upper:Upper,curr:unb(_),name:Nm,id:Id}),Lower,Upper) :- gensym("_#",Id).

deRef(tVar(V),Tp) :- V.curr \= unb(_),!,deRef(V.curr,Tp),!.
deRef(T,T).

isIdentical(tVar(V1),tVar(V2)) :- V1.id=V2.id.

isUnbound(T) :- deRef(T,tVar(V)), V.curr = unb(_).

isBound(tVar(TV),Tp) :- TV.curr = Tp, Tp \= unb(_).

upperBound(V,Tp) :- V.curr \=unb(_), !, upperBoundOf(V.curr,Tp).
upperBound(V,V.upper).

upperBoundOf(tVar(V),Tp) :- upperBound(V,Tp).
upperBoundOf(T,T).

lowerBound(V,Tp) :- V.curr \=unb(_), !, lowerBoundOf(V.curr,Tp).
lowerBound(V,V.lower).

lowerBoundOf(tVar(V),Tp) :- lowerBound(V,Tp).
lowerBoundOf(T,T).

bounds(tVar(V),Lw,Up) :- !, boundsOf(V,Lw,Up).
bounds(T,T,T).

boundsOf(V,Lw,Up) :- V.curr \= unb(_), bounds(V.curr,Lw,Up).
boundsOf(V,V.lower,V.upper).

markUpper(T,Tp) :- \+occursIn(T,Tp), T=tVar(V), b_set_dict(upper,V,Tp).

markLower(T,Tp) :- \+occursIn(T,Tp), T=tVar(V), b_set_dict(lower,V,Tp).

bind(T,Tp) :- \+occursIn(T,Tp), T=tVar(V),b_set_dict(curr,V,Tp).

occursIn(tVar(TV),Tp) :- deRef(Tp,DTp), \+ sameVar(TV,DTp), occIn(TV.id,DTp),!.

sameVar(V1,tVar(V2)) :- V1.id = V2.id.

occIn(Id,tVar(V)) :- isBound(tVar(V),Tp), !, occIn(Id,Tp).
occIn(Id,tVar(V)) :- V.id=Id,!.
occIn(Id,tVar(V)) :- upperBound(V,Tp), !, occIn(Id,Tp).
occIn(Id,tVar(V)) :- lowerBound(V,Tp), !, occIn(Id,Tp).
occIn(Id,typeExp(_,L)) :- is_member(A,L), occIn(Id,A).
occIn(Id,tupleType(L)) :- is_member(A,L), occIn(Id,A).
occIn(Id,funType(L,_)) :- is_member(A,L), occIn(Id,A).
occIn(Id,funType(_,R)) :- is_member(A,R), occIn(Id,A).
occIn(Id,classType(L,_)) :- is_member(A,L), occIn(Id,A).
occIn(Id,classType(_,R)) :- is_member(A,R), occIn(Id,A).
occIn(Id,predType(L)) :- is_member(A,L), occIn(Id,A).
occIn(Id,funType(_,R)) :- is_member(A,R), occIn(Id,A).
occIn(Id,univType(constrained(Lw,_,Up),Tp)) :- occIn(Id,Lw) ; occIn(Id,Up) ; occIn(Id,Tp).
occIn(Id,univType(_,Tp)) :- occIn(Id,Tp).
occIn(Id,faceType(L)) :- is_member((_,A),L), occIn(Id,A).

moveQuants(univType(B,Tp),[B|Q],Tmpl) :- !,
  moveQuants(Tp,Q,Tmpl).
moveQuants(Tp,[],Tp).

showTypeRule(typeRule(Hd,Bd),O,E) :- showType(Hd,O,O1), appStr("<~",O1,O2),showType(Bd,O2,E).
showTypeRule(univType(V,Tp),O,E) :- appStr("all ",O,O1), showBound(V,O1,O2), showMoreQuantified(Tp,showTypeRule,O2,E).

showType(T,O,E) :- isBound(T,Tp),!,showType(Tp,O,E).
showType(anonType,O,E) :- appStr("_",O,E).
showType(voidType,O,E) :- appStr("void",O,E).
showType(topType,O,E) :- appStr("top",O,E).
showType(thisType,O,E) :- appStr("this",O,E).
showType(kVar(Nm),O,E) :- appStr(Nm,O,E).
showType(tVar(St),O,E) :- showLower(St.lower,O,O1),appStr("%",O1,O2),appStr(St.name,O2,O3),appSym(St.id,O3,O4),showUpper(St.upper,O4,E).
showType(type(Nm),O,E) :- appStr(Nm,O,E).
showType(typeExp(Nm,A),O,E) :- appStr(Nm,O,O1), appStr("[",O1,O2),showTypeEls(A,O2,O3),appStr("]",O3,E).
showType(tupleType(A),O,E) :- appStr("(",O,O1), showTypeEls(A,O1,O2), appStr(")",O2,E).
showType(funType(A,R),O,E) :- appStr("(",O,O1), showTypeEls(A,O1,O2), appStr(")",O2,O3), appStr("=>",O3,O4), showType(R,O4,E).
showType(classType(A,R),O,E) :- appStr("(",O,O1), showTypeEls(A,O1,O2), appStr(")",O2,O3), appStr("<=>",O3,O4), showType(R,O4,E).
showType(predType(A),O,E) :- appStr("(",O,O1), showTypeEls(A,O1,O2), appStr(")",O2,O3), appStr("{}",O3,E).
showType(univType(V,Tp),O,E) :- appStr("all ",O,O1), showBound(V,O1,O2), showMoreQuantified(Tp,showType,O2,E).
showType(faceType(Els),O,E) :- appStr("{ ",O,O1), showTypeFields(Els,O1,O2), appStr("}",O2,E).
showType(typeRule(Hd,Bd),O,E) :- showType(Hd,O,O1), appStr("<~",O1,O2),showType(Bd,O2,E).

showBound(constrained(voidType,Nm,Upper),O,E) :- showType(Nm,O,O1),appStr("<~",O1,O2),showType(Upper,O2,E).
showBound(constrained(Lower,Nm,Upper),O,E) :- showType(Lower,O,O0), showType(Nm,O0,O1),appStr("<~",O1,O2),showType(Upper,O2,E).
showBound(Nm,O,E) :- showType(Nm,O,E).

showLower(voidType,O,O) :-!.
showLower(Tp,O,E) :- isBound(Tp,B), !, showLower(B,O,E).
showLower(Tp,O,E) :- showType(Tp,O,O1), appStr("<~",O1,E).

showUpper(topType,O,O) :-!.
showUpper(Tp,O,E) :- isBound(Tp,B), !, showUpper(B,O,E).
showUpper(Tp,O,E) :- appStr("<~",O,O1), showType(Tp,O1,E).

showTypeEls([],O,O).
showTypeEls([Tp|More],O,E) :- showType(Tp,O,O1), showMoreTypeEls(More,O1,E).

showMoreTypeEls([],O,O).
showMoreTypeEls([Tp|More],O,E) :- appStr(", ",O,O1),showType(Tp,O1,O2), showMoreTypeEls(More,O2,E).

showMoreQuantified(univType(Nm,Tp),P,O,E) :- appStr(", ",O,O1), showType(Nm,O1,O2), showMoreQuantified(Tp,P,O2,E).
showMoreQuantified(Tp,P,O,E) :- appStr(" ~~ ",O,O1), call(P,Tp,O1,E).

showTypeFields([],O,O).
showTypeFields([F|More],O,E) :- showTypeField(F,O,O1), showMoreTypeFields(More,O1,E).

showMoreTypeFields([],O,O).
showMoreTypeFields([Fld|More],O,E) :- appStr(". ",O,O1), showTypeField(Fld,O1,O2), showMoreTypeFields(More,O2,E).

showTypeField((Nm,Tp),O,E) :- appStr(Nm,O,O1), appStr(" : ",O1,O2), showType(Tp,O2,E).

typeArity(univType(_,Tp),Ar) :- typeArity(Tp,Ar).
typeArity(funType(A,_),Ar) :- length(A,Ar).
typeArity(predType(A),Ar) :- length(A,Ar).
typeArity(classType(A,_),Ar) :- length(A,Ar).

isFunctionType(univType(_,Tp),Ar) :- isFunctionType(Tp,Ar).
isFunctionType(funType(A,_),Ar) :- length(A,Ar).

isPredType(univType(_,Tp),Ar) :- isPredType(Tp,Ar).
isPredType(predType(A),Ar) :- length(A,Ar).

isClassType(univType(_,Tp),Ar) :- isClassType(Tp,Ar).
isClassType(classType(A,_),Ar) :- length(A,Ar).

