:- module(freshen,[freshn/3,freshen/4,evidence/2]).

:- use_module(misc).
:- use_module(types).

freshen(Tp,ThisType,B,FTp) :- addThisType(ThisType,[],B0),!,deQuant(Tp,B0,B,T0), !, freshn(T0,B,FTp).

addThisType(voidType,B,B).
addThisType(Tp,B,[(thisType,Tp)|B]).

hasQuants(univType(_,_)).
  
deQuant(univType(constrained(Lower,V,Upper),Tp),B,BV,FTp) :- BB = [(V,TV)|B], freshn(Lower,BB,Lw),freshn(Upper,BB,Up),newTypeVar(V,TV,Lw,Up),deQuant(Tp,BB,BV,FTp).
deQuant(univType(V,Tp),B,BV,FTp) :- newTypeVar(V,TV),deQuant(Tp,[(V,TV)|B],BV,FTp).
deQuant(Tp,B,B,Tp).

evidence(Tp,FTp) :- skolemize(Tp,[],B,T0), freshn(T0,B,FTp).

freshn(Tp,[],Tp) :- !.
freshn(Tp,Binding,FTp) :- frshn(Tp,Binding,FTp),!.

skolemize(univType(constrained(_,V,_),Tp),B,BV,FTp) :- skolemize(Tp,[(V,kVar(V))|B],BV,FTp).
skolemize(univType(V,Tp),B,BV,FTp) :- skolemize(Tp,[(V,kVar(V))|B],BV,FTp).
skolemize(Tp,B,B,Tp).

frshn(voidType,_,voidType) :- !.
frshn(topType,_,topType) :- !.
frshn(thisType,B,Tp) :- (is_member((thisType,Tp),B),! ; Tp=thisType).
frshn(kVar(TV),B,Tp) :- is_member((TV,Tp),B),!.
frshn(tVar(V),_,tVar(V)).
frshn(type(Nm),_,type(Nm)).
frshn(funType(A,R),B,funType(FA,FR)) :-
  frshnTypes(A,B,FA),
  frshn(R,B,FR).
frshn(classType(A,R),B,classType(FA,FR)) :-
  frshnTypes(A,B,FA),
  frshn(R,B,FR).
frshn(predType(A),B,predType(FA)) :- frshnTypes(A,B,FA).
frshn(tupleType(L),B,tupleType(FL)) :- frshnTypes(L,B,FL).
frshn(typeExp(O,A),B,typeExp(O,FA)) :- frshnTypes(A,B,FA).
frshn(univType(V,Tp),B,univType(V,FTp)) :-
  subtract((V,_),B,B0),
  frshn(Tp,B0,FTp).
frshn(faceType(L),B,faceType(FL)) :-
  frshnFields(L,B,FL).
frshn(typeRule(A,R),B,typeRule(FA,FR)) :-
  frshn(A,B,FA),
  frshn(R,B,FR).

frshnFields([],_,[]).
frshnFields([(Mode,A)|L],B,[(Mode,FA)|FL]) :- !, frshn(A,B,FA), frshnFields(L,B,FL).
frshnFields([A|L],B,[FA|FL]) :- frshn(A,B,FA), frshnFields(L,B,FL).

frshnTypes([],_,[]).
frshnTypes([A|L],B,[FA|FL]) :- frshn(A,B,FA), frshnTypes(L,B,FL).
