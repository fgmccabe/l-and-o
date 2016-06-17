:- module(macro,[macroRewrite/2]).

% Implement features like the algegraic type definition via macro-replacement
% used in checking a theta environment before dependency analysis

:- use_module(abstract).
:- use_module(wff).


% rewrite a sequence of statements into another sequence.

macroRewrite([],[]).
macroRewrite([St|More],Stmts) :-
  isBinary(St,"::=",_,_),!,
  convertAlgebraic(St,Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],[St|Stmts]) :-
  macroRewrite(More,Stmts).

% super simple conversion. Maybe later add support for auto generation of interfaces

convertAlgebraic(St,[InheritThing|Elements],Tail) :-
  isAlgebraicTypeDef(St,Lc,Head,Body),
  binary(Lc,"<~",Head,name(Lc,"thing"),InheritThing),
  convertConstructors(Body,Head,Elements,Tail).

convertConstructors(Pair,Head,Elements,Tail) :-
  isBinary(Pair,"|",L,R),
  convertConstructors(L,Head,Elements,L1),
  convertConstructors(R,Head,L1,Tail).
convertConstructors(Term,Head,Elements,Tail) :-
  convertConstructor(Term,Head,Elements,Tail).

convertConstructor(name(Lc,Nm),Tp,[TpRule,InheritRule,Body|Tail],Tail) :-
  hasType(Lc,Nm,Tp,TpRule),
  inheritThing(Lc,name(Lc,Nm),InheritRule),                  /* Inherit from thing */
  emptyBody(Lc,name(Lc,Nm),Body).
convertConstructor(Con,Tp,[TpRule,InheritRule,Body|Tail],Tail) :-
  locOfAst(Con,Lc),
  isRound(Con,Nm,ArgTypes), /* Construct con:(T1,..,Tn)<=>Tp */
  classType(Lc,ArgTypes,Tp,ClassType),
  hasType(Lc,Nm,ClassType,TpRule),
  genAnonArgs(ArgTypes,Args),       /* Construct con(_,..,_) <= thing */
  roundTerm(Lc,Nm,Args,Hd),
  inheritThing(Lc,Hd,InheritRule),
  emptyBody(Lc,Hd,Body).

genAnonArgs([],[]).
genAnonArgs([T|M],[name(Lc,"_")|A]) :- locOfAst(T,Lc), genAnonArgs(M,A).

hasType(Lc,Nm,Tp,St) :- binary(Lc,":",name(Lc,Nm),Tp,St).

classType(Lc,Args,Res,Tp) :- isTuple(A,Lc,Args),binary(Lc,"<=>",A,Res,Tp).
funType(Lc,Args,Res,Tp) :- isTuple(A,Lc,Args),binary(Lc,"=>",A,Res,Tp).
genericType(Lc,Nm,Args,Tp) :- squareTerm(Lc,Nm,Args,Tp).

inheritThing(Lc,Hd,Rule) :-
  binary(Lc,"<=",Hd,name(Lc,"thing"),Rule).

emptyBody(Lc,Hd,Body) :-
  isBraceTuple(Content,Lc,[]),
  binary(Lc,"..",Hd,Content,Body).

thingType(name(Lc,"thing"),Lc).