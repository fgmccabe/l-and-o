:- module(freshen,[freshn/3,freshen/4,freshenConstraint/2,frshnConstraint/3,freshenContract/3,freshenContract/4,
  bindConstraint/1,contractedType/2,rewriteType/3,addThisType/3,
  evidence/2,evidence/4,contractEvidence/4,freeze/3,freezeType/3]).

:- use_module(misc).
:- use_module(types).

freshen(Tp,ThisType,B,FTp) :- 
  addThisType(ThisType,[],B0),
  deQuant(Tp,B0,B,T0),
  freshn(T0,B,FTp),!.

freshenConstraint(Con,FCon) :-
  deQuant(Con,[],B,C0),
  frshnConstraint(C0,B,FCon).

addThisType(voidType,B,B).
addThisType(Tp,B,[(thisType,Tp)|B]).

hasQuants(univType(_,_)).
  
deQuant(univType(kVar(V),Tp),B,BV,FTp) :- newTypeVar(V,TV),deQuant(Tp,[(V,TV)|B],BV,FTp).
deQuant(Tp,B,B,Tp).

evidence(Tp,FTp) :- skolemize(Tp,[],B,T0), freshn(T0,B,FTp).

evidence(Tp,ThisType,Q,ProgramType) :-
  addThisType(ThisType,[],B0),
  skolemize(Tp,B0,Q,SkTp),
  frshn(SkTp,Q,ProgramType).

freshn(Tp,[],Tp) :- !.
freshn(Tp,Binding,FTp) :- frshn(Tp,Binding,FTp),!.

skolemize(univType(kVar(V),Tp),B,BV,FTp) :- readOnlyTypeVar(V,TV),skolemize(Tp,[(V,TV)|B],BV,FTp).
skolemize(Tp,B,B,Tp).

rewriteType(Tp,Q,WTp) :-
  frshn(Tp,Q,WTp).

frshn(anonType,_,anonType).
frshn(voidType,_,voidType) :- !.
frshn(thisType,B,Tp) :- (is_member((thisType,Tp),B),! ; Tp=thisType).
frshn(kVar(TV),B,Tp) :- (is_member((TV,Tp),B),! ; Tp=kVar(TV)).
frshn(tVar(V),_,tVar(V)).
frshn(type(Nm),_,type(Nm)).
frshn(funType(A,R),B,funType(FA,FR)) :-
  frshnTypes(A,B,FA),
  frshn(R,B,FR).
frshn(grammarType(A,R),B,grammarType(FA,FR)) :-
  frshnTypes(A,B,FA),
  frshn(R,B,FR).
frshn(classType(A,R),B,classType(FA,FR)) :-
  frshnTypes(A,B,FA),
  frshn(R,B,FR).
frshn(predType(A),B,predType(FA)) :- frshnTypes(A,B,FA).
frshn(tupleType(L),B,tupleType(FL)) :- frshnTypes(L,B,FL).
frshn(typeExp(O,A),B,typeExp(O,FA)) :- frshnTypes(A,B,FA).
frshn(univType(kVar(V),Tp),B,univType(kVar(V),FTp)) :-
  subtract((V,_),B,B0),
  frshn(Tp,B0,FTp).
frshn(constrained(Tp,Con),B,constrained(FTp,FCon)) :-
  frshn(Tp,B,FTp),
  frshnConstraint(Con,B,FCon).
frshn(faceType(L),B,faceType(FL)) :-
  frshnFields(L,B,FL).
frshn(typeRule(A,R),B,typeRule(FA,FR)) :-
  frshn(A,B,FA),
  frshn(R,B,FR).

frshnConstraint(Con,[],Con) :- !.
frshnConstraint(conTract(Nm,Args,Deps),B,conTract(Nm,FArgs,FDeps)) :-
  frshnTypes(Args,B,FArgs),
  frshnTypes(Deps,B,FDeps).
frshnConstraint(implementsFace(Tp,Els),B,implementsFace(FTp,FL)) :-
  frshn(Tp,B,FTp),
  frshnFields(Els,B,FL).

frshnContract(conTract(Nm,Args,Deps),B,conTract(Nm,FArgs,FDeps)) :-
  frshnTypes(Args,B,FArgs),
  frshnTypes(Deps,B,FDeps).
frshnContract(constrained(Con,Other),B,constrained(FCon,FOther)) :-
  frshnConstraint(Other,B,FOther),
  frshnContract(Con,B,FCon).

freshenContract(Con,ThisType,Q,FCon) :-
  addThisType(ThisType,[],B0),
  deQuant(Con,B0,Q,CC),
  frshnContract(CC,Q,FCon),!.

freshenContract(Con,Q,FCon) :-
  deQuant(Con,[],Q,CC),
  frshnContract(CC,Q,FCon),!.

contractEvidence(Tp,ThisType,Q,Con) :-
  addThisType(ThisType,[],B0),
  skolemize(Tp,B0,Q,SkTp),
  frshnContract(SkTp,Q,Con).

contractedType(constrained(Tp,Con),contracted([Con|Contracts],ConTp)) :-
  contracts(Tp,Contracts,ConTp).

contracts(constrained(Tp,Con),[Con|M],ConTp) :-
  Con = typeExp(_,_),!,
  contracts(Tp,M,ConTp).
contracts(constrained(Tp,Con),Cons,ConTp) :-
  bindConstraint(Con),!,
  contracts(Tp,Cons,ConTp).
contracts(Tp,[],Tp).

bindConstraint(typeExp(Nm,Args)) :-
  bindContract(Args,typeExp(Nm,Args)).
bindConstraint(implementsFace(Tp,Els)) :- deRef(Tp,V), isUnbound(V),!,
  setConstraint(V,implementsFace(Tp,Els)).
bindConstraint(implementsFace(_,_)).

bindContract([],_).
bindContract([E|R],C) :- deRef(E,V), isUnbound(V),
  setConstraint(V,C),
  bindContract(R,C).
bindContract([_|R],C) :-
  bindContract(R,C).

frshnFields([],_,[]).
frshnFields([(Nm,A)|L],B,[(Nm,FA)|FL]) :- !, frshn(A,B,FA), frshnFields(L,B,FL).
frshnFields([A|L],B,[FA|FL]) :- frshn(A,B,FA), frshnFields(L,B,FL).

frshnTypes([],_,[]).
frshnTypes([A|L],B,[FA|FL]) :- frshn(A,B,FA), frshnTypes(L,B,FL).

freezeType(Tp,B,FrZ) :-
  freeze(Tp,B,FT),
  reQuant(B,B,FT,FrZ).

freeze(voidType,_,voidType) :- !.
freeze(anonType,_,anonType) :- !.
freeze(thisType,_,thisType) :- !.
freeze(kVar(TV),_,kVar(TV)) :- !.
freeze(tVar(V),B,FzT) :- isBound(tVar(V),Tp), !, freeze(Tp,B,FzT).
freeze(tVar(V),B,kVar(TV)) :- is_member((TV,VV),B), deRef(VV,VVV), identicalVar(tVar(V),VVV), !.
freeze(tVar(V),_,tVar(V)) :- !.
freeze(type(Nm),_,type(Nm)).
freeze(funType(A,R),B,funType(FA,FR)) :-
  freezeTypes(A,B,FA),
  freeze(R,B,FR).
freeze(grammarType(A,R),B,grammarType(FA,FR)) :-
  freezeTypes(A,B,FA),
  freeze(R,B,FR).
freeze(classType(A,R),B,classType(FA,FR)) :-
  freezeTypes(A,B,FA),
  freeze(R,B,FR).
freeze(predType(A),B,predType(FA)) :- freezeTypes(A,B,FA).
freeze(tupleType(L),B,tupleType(FL)) :- freezeTypes(L,B,FL).
freeze(typeExp(O,A),B,typeExp(O,FA)) :- freezeTypes(A,B,FA).
freeze(univType(kVar(V),Tp),B,univType(kVar(V),FTp)) :-
  subtract((V,_),B,B0),
  freeze(Tp,B0,FTp).
freeze(faceType(L),B,faceType(FL)) :-
  freezeFields(L,B,FL).
freeze(implementsFace(V,L),B,implementsFace(FV,FL)) :-
  freezeType(V,B,FV),
  freezeFields(L,B,FL).
freeze(typeRule(A,R),B,typeRule(FA,FR)) :-
  freeze(A,B,FA),
  freeze(R,B,FR).

freezeFields([],_,[]).
freezeFields([(Nm,A)|L],B,[(Nm,FA)|FL]) :- !, freeze(A,B,FA), freezeFields(L,B,FL).
freezeFields([A|L],B,[FA|FL]) :- freeze(A,B,FA), freezeFields(L,B,FL).

freezeTypes([],_,[]).
freezeTypes([A|L],B,[FA|FL]) :- freeze(A,B,FA), freezeTypes(L,B,FL).

reQuant([],_,T,T).
reQuant([(_,Tp)|R],BB,T,FZT) :- 
  deRef(Tp,V), isUnbound(V),!, 
  constraints(V,C),
  freezeConstraints(C,BB,T,FT),
  reQuant(R,BB,FT,FZT).
reQuant([_|B],BB,T,FT) :- reQuant(B,BB,T,FT).

freezeConstraints(C,_,T,T) :- var(C),!.
freezeConstraints([C|_],_,T,T) :- var(C),!.
freezeConstraints([C|R],BB,T,FT) :-
  freeze(C,BB,FC),
  freezeConstraints(R,BB,constrained(T,FC),FT).
