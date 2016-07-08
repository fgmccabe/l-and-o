:- module(freshen,[freshn/3,freshen/4,evidence/2,freeze/3,freezeType/3,copyFlowMode/3]).

:- use_module(misc).
:- use_module(types).

freshen(Tp,ThisType,B,FTp) :- addThisType(ThisType,[],B0),deQuant(Tp,B0,B,T0),  freshn(T0,B,FTp),!.

addThisType(voidType,B,B).
addThisType(Tp,B,[(thisType,Tp)|B]).

hasQuants(univType(_,_)).
  
deQuant(univType(constrained(Lower,kVar(V),Upper),Tp),B,BV,FTp) :- BB = [(V,TV)|B], freshn(Lower,BB,Lw),freshn(Upper,BB,Up),newTypeVar(V,TV,Lw,Up),deQuant(Tp,BB,BV,FTp).
deQuant(univType(kVar(V),Tp),B,BV,FTp) :- newTypeVar(V,TV),deQuant(Tp,[(V,TV)|B],BV,FTp).
deQuant(Tp,B,B,Tp).

evidence(Tp,FTp) :- skolemize(Tp,[],B,T0), freshn(T0,B,FTp).

freshn(Tp,[],Tp) :- !.
freshn(Tp,Binding,FTp) :- frshn(Tp,Binding,FTp),!.

skolemize(univType(constrained(_,kVar(V),_),Tp),B,BV,FTp) :- skolemize(Tp,[(V,kVar(V))|B],BV,FTp).
skolemize(univType(kVar(V),Tp),B,BV,FTp) :- skolemize(Tp,[(V,kVar(V))|B],BV,FTp).
skolemize(Tp,B,B,Tp).

frshn(anonType,_,anonType).
frshn(voidType,_,voidType) :- !.
frshn(topType,_,topType) :- !.
frshn(thisType,B,Tp) :- (is_member((thisType,Tp),B),! ; Tp=thisType).
frshn(kVar(TV),B,Tp) :- is_member((TV,Tp),B),!.
frshn(tVar(V),_,tVar(V)).
frshn(type(Nm),_,type(Nm)).
frshn(funType(A,R),B,funType(FA,FR)) :-
  frshnArgTypes(A,B,FA),
  frshn(R,B,FR).
frshn(grammarType(A,R),B,grammarType(FA,FR)) :-
  frshnArgTypes(A,B,FA),
  frshn(R,B,FR).
frshn(classType(A,R),B,classType(FA,FR)) :-
  frshnTypes(A,B,FA),
  frshn(R,B,FR).
frshn(predType(A),B,predType(FA)) :- frshnArgTypes(A,B,FA).
frshn(tupleType(L),B,tupleType(FL)) :- frshnTypes(L,B,FL).
frshn(typeExp(O,A),B,typeExp(O,FA)) :- frshnTypes(A,B,FA).
frshn(univType(kVar(V),Tp),B,univType(kVar(V),FTp)) :-
  subtract((V,_),B,B0),
  frshn(Tp,B0,FTp).
frshn(univType(constrained(Lw,kVar(V),Up),Tp),B,univType(constrained(FLw,kVar(V),FUp),FTp)) :-
  subtract((V,_),B,B0),
  frshn(Tp,B0,FTp),
  frshn(Lw,B0,FLw),
  frshn(Up,B0,FUp).
frshn(faceType(L),B,faceType(FL)) :-
  frshnFields(L,B,FL).
frshn(typeRule(A,R),B,typeRule(FA,FR)) :-
  frshn(A,B,FA),
  frshn(R,B,FR).

frshnFields([],_,[]).
frshnFields([(Nm,A)|L],B,[(Nm,FA)|FL]) :- !, frshn(A,B,FA), frshnFields(L,B,FL).
frshnFields([A|L],B,[FA|FL]) :- frshn(A,B,FA), frshnFields(L,B,FL).

frshnTypes([],_,[]).
frshnTypes([A|L],B,[FA|FL]) :- frshn(A,B,FA), frshnTypes(L,B,FL).

frshnArgTypes([],_,[]).
frshnArgTypes([A|L],B,[FA|FL]) :- frshnArg(A,B,FA), frshnArgTypes(L,B,FL).

frshnArg(in(T),B,in(FT)) :- frshn(T,B,FT).
frshnArg(out(T),B,out(FT)) :- frshn(T,B,FT).
frshnArg(inout(T),B,inout(FT)) :- frshn(T,B,FT).


freezeType(Tp,B,FrZ) :-
  freeze(Tp,B,FT),
  reQuant(B,B,FT,FrZ).

freeze(voidType,_,voidType) :- !.
freeze(topType,_,topType) :- !.
freeze(anonType,_,anonType) :- !.
freeze(thisType,_,thisType) :- !.
freeze(kVar(TV),_,kVar(TV)) :- !.
freeze(tVar(V),B,FzT) :- isBound(tVar(V),Tp), !, freeze(Tp,B,FzT).
freeze(tVar(V),B,kVar(TV)) :- is_member((TV,VV),B), deRef(VV,VVV), identicalVar(tVar(V),VVV), !.
freeze(tVar(V),_,tVar(V)) :- !.
freeze(type(Nm),_,type(Nm)).
freeze(funType(A,R),B,funType(FA,FR)) :-
  freezeArgTypes(A,B,FA),
  freeze(R,B,FR).
freeze(grammarType(A,R),B,grammarType(FA,FR)) :-
  freezeArgTypes(A,B,FA),
  freeze(R,B,FR).
freeze(classType(A,R),B,classType(FA,FR)) :-
  freezeTypes(A,B,FA),
  freeze(R,B,FR).
freeze(predType(A),B,predType(FA)) :- freezeArgTypes(A,B,FA).
freeze(tupleType(L),B,tupleType(FL)) :- freezeTypes(L,B,FL).
freeze(typeExp(O,A),B,typeExp(O,FA)) :- freezeTypes(A,B,FA).
freeze(univType(kVar(V),Tp),B,univType(kVar(V),FTp)) :-
  subtract((V,_),B,B0),
  freeze(Tp,B0,FTp).
freeze(faceType(L),B,faceType(FL)) :-
  freezeFields(L,B,FL).
freeze(typeRule(A,R),B,typeRule(FA,FR)) :-
  freeze(A,B,FA),
  freeze(R,B,FR).

freezeFields([],_,[]).
freezeFields([(Nm,A)|L],B,[(Nm,FA)|FL]) :- !, freeze(A,B,FA), freezeFields(L,B,FL).
freezeFields([A|L],B,[FA|FL]) :- freeze(A,B,FA), freezeFields(L,B,FL).

freezeTypes([],_,[]).
freezeTypes([A|L],B,[FA|FL]) :- freeze(A,B,FA), freezeTypes(L,B,FL).

freezeArgTypes([],_,[]).
freezeArgTypes([Tp|L],B,[FTp|FL]) :-
  copyFlowMode(Fl,InTp,Tp),
  freeze(InTp,B,FT),
  copyFlowMode(Fl,FT,FTp),
  freezeArgTypes(L,B,FL).

reQuant([],_,T,T).
reQuant([(Nm,Tp)|B],BB,T,FT) :- isUnbound(Tp),!, 
  bounds(Tp,Lw,Up),
  ( (Lw \= voidType ; Up \= topType),
    freeze(Lw,BB,FLw),
    freeze(Up,BB,FUp),
    reQuant(B,BB,univType(constrained(FLw,kVar(Nm),FUp),T),FT) ;
    reQuant(B,BB,univType(kVar(Nm),T),FT)).
reQuant([_|B],BB,T,FT) :- reQuant(B,BB,T,FT).

copyFlowMode(in(_),Tp,in(Tp)).
copyFlowMode(out(_),Tp,out(Tp)).
copyFlowMode(inout(_),Tp,inout(Tp)).

