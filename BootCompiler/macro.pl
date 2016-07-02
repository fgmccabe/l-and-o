:- module(macro,[macroRewrite/2]).

% Implement features like the algegraic type definition via macro-replacement
% used in checking a theta environment before dependency analysis

:- use_module(abstract).
:- use_module(wff).


% rewrite a sequence of statements into another sequence.

macroRewrite([],[]).
macroRewrite([St|More],Stmts) :-
  isAlgebraicTypeDef(St,Lc,Quants,Head,Body),!,
  convertAlgebraic(Lc,Quants,Head,Body,Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],[St|Stmts]) :-
  macroRewrite(More,Stmts).

% super simple conversion. Maybe later add support for auto generation of interfaces

convertAlgebraic(Lc,Quants,Head,Body,[InheritThing|Elements],Tail) :-
  typeRule(Lc,Quants,Head,name(Lc,"thing"),InheritThing),
  convertConstructors(Body,Head,Quants,Elements,Tail).

convertConstructors(Pair,Head,Quants,Elements,Tail) :-
  isBinary(Pair,"|",L,R),
  convertConstructors(L,Head,Quants,Elements,L1),
  convertConstructors(R,Head,Quants,L1,Tail).
convertConstructors(Term,Head,Quants,Elements,Tail) :-
  convertConstructor(Term,Head,Quants,Elements,Tail).

convertConstructor(name(Lc,Nm),Tp,Quants,[TpRule,Body|Tail],Tail) :-
  hasType(Lc,Nm,Quants,Tp,TpRule),
  emptyBody(Lc,name(Lc,Nm),Body).
convertConstructor(Con,Tp,Quants,[TpRule,Body|Tail],Tail) :-
  locOfAst(Con,Lc),
  isRound(Con,Nm,ArgTypes), /* Construct con:(T1,..,Tn)<=>Tp */
  classType(Lc,ArgTypes,Tp,ClassType),
  hasType(Lc,Nm,Quants,ClassType,TpRule),
  genAnonArgs(ArgTypes,Args),       /* Construct con(_,..,_) <= thing */
  roundTerm(Lc,Nm,Args,Hd),
  emptyBody(Lc,Hd,Body).

typeRule(Lc,Quants,Hd,Body,Rule) :-
  binary(Lc,"<~",Hd,Body,Rl),
  wrapQuants(Quants,Lc,Rl,Rule).

wrapQuants([],_,Rule,Rule).
wrapQuants(Q,Lc,Rl,Rule) :-
  binary(Lc,"~~",Q,Rl,R1),
  unary(Lc,"all",R1,Rule).

genAnonArgs([],[]).
genAnonArgs([T|M],[name(Lc,"_")|A]) :- locOfAst(T,Lc), genAnonArgs(M,A).

hasType(Lc,Nm,Quants,Tp,St) :- 
  wrapQuants(Quants,Lc,Tp,QTp),
  binary(Lc,":",name(Lc,Nm),QTp,St).

classType(Lc,Args,Res,Tp) :- isTuple(A,Lc,Args),binary(Lc,"<=>",A,Res,Tp).
funType(Lc,Args,Res,Tp) :- isTuple(A,Lc,Args),binary(Lc,"=>",A,Res,Tp).
genericType(Lc,Nm,Args,Tp) :- squareTerm(Lc,Nm,Args,Tp).

inheritThing(Lc,Hd,Rule) :-
  binary(Lc,"<=",Hd,name(Lc,"thing"),Rule).

emptyBody(Lc,Hd,Body) :-
  isBraceTuple(Content,Lc,[]),
  binary(Lc,"..",Hd,Content,Body).

thingType(name(Lc,"thing"),Lc).