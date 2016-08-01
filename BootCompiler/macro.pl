:- module(macro,[macroRewrite/2]).

% Implement features like the algegraic type definition via macro-replacement
% used in checking a theta environment before dependency analysis

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).


% rewrite a sequence of statements into another sequence.

macroRewrite([],[]).
macroRewrite([St|More],Stmts) :-
  isUnary(St,"public",Inner),
  isAlgebraicTypeDef(Inner,Lc,Quants,Constraints,Head,Body),!,
  convertAlgebraic(Lc,macro:markPublic,Quants,Constraints,Head,Body,Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],Stmts) :-
  isUnary(St,"private",Inner),
  isAlgebraicTypeDef(Inner,Lc,Quants,Constraints,Head,Body),!,
  convertAlgebraic(Lc,macro:markPrivate,Quants,Constraints,Head,Body,Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],Stmts) :-
  isAlgebraicTypeDef(St,Lc,Quants,Constraints,Head,Body),!,
  convertAlgebraic(Lc,macro:noMark,Quants,Constraints,Head,Body,Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],[St|Stmts]) :-
  isContractSpec(St,_,Quants,Constraints,Con,Els),!,
  generateAnnotations(Els,Quants,[Con|Constraints],Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],[St|Stmts]) :-
  macroRewrite(More,Stmts).

markPublic(Lc,Stmt,PStmt) :-
  unary(Lc,"public",Stmt,PStmt).

markPrivate(Lc,Stmt,PStmt) :-
  unary(Lc,"private",Stmt,PStmt).

noMark(_,Stmt,Stmt).

% super simple conversion. Maybe later add support for auto generation of interfaces

convertAlgebraic(Lc,Mark,Quants,Constraints,Head,Body,[TypeRule|Elements],Tail) :-
  algebraicFace(Body,[],Els),
  isBraceTuple(Face,Lc,Els),
  typeRule(Lc,Quants,Constraints,Head,Face,FaceRule),
  call(Mark,Lc,FaceRule,TypeRule),
  convertConstructors(Body,Head,Mark,Quants,Constraints,Elements,Tail).

convertConstructors(Pair,Head,Mark,Quants,Constraints,Elements,Tail) :-
  isBinary(Pair,"|",L,R),
  convertConstructors(L,Head,Mark,Quants,Constraints,Elements,L1),
  convertConstructors(R,Head,Mark,Quants,Constraints,L1,Tail).
convertConstructors(Term,Head,Mark,Quants,Constraints,Elements,Tail) :-
  convertConstructor(Term,Head,Mark,Quants,Constraints,Elements,Tail).

convertConstructor(name(Lc,Nm),Tp,Mark,Quants,Constraints,[TpRule,BodyRule|Tail],Tail) :-
  wrapConstraints(Constraints,Lc,Tp,TpCon),
  wrapQuants(Quants,Lc,TpCon,QTp),
  hasType(Lc,Nm,QTp,TpRl),
  call(Mark,Lc,TpRl,TpRule),
  bodyRule(Lc,name(Lc,Nm),[],BodyRule).
convertConstructor(Con,Tp,Mark,Quants,Constraints,[TpRule,BodyRule|Tail],Tail) :-
  isRound(Con,Nm,ArgTypes),!, /* Construct con:(T1,..,Tn)<=>Tp */
  locOfAst(Con,Lc),
  classType(Lc,ArgTypes,Tp,ClassType),
  wrapConstraints(Constraints,Lc,ClassType,CTp),
  wrapQuants(Quants,Lc,CTp,QTp),
  hasType(Lc,Nm,QTp,TpRl),
  call(Mark,Lc,TpRl,TpRule),
  genAnonArgs(ArgTypes,Args),       /* Construct con(_,..,_) <= thing */
  roundTerm(Lc,Nm,Args,Hd),
  bodyRule(Lc,Hd,[],BodyRule).
convertConstructor(Con,Tp,Mark,Quants,Constraints,[TpRule,BodyRule|Tail],Tail) :-
  isBrace(Con,Op,FldTps),
  locOfAst(Con,Lc),
  isBraceTuple(ClArgs,Lc,FldTps),
  binary(Lc,"<=>",ClArgs,Tp,ClassType),
  wrapConstraints(Constraints,Lc,ClassType,CTp),
  wrapQuants(Quants,Lc,CTp,QTp),
  hasType(Lc,Op,QTp,TpRl),
  call(Mark,Lc,TpRl,TpRule),
  genFieldArgs(FldTps,Args,Defs),
  roundTerm(Lc,Op,Args,Hd),
  bodyRule(Lc,Hd,Defs,BodyRule).

algebraicFace(T,SoFar,Face) :-
  isBinary(T,"|",L,R),
  algebraicFace(L,SoFar,SF),
  algebraicFace(R,SF,Face).
algebraicFace(T,SoFar,SoFar) :-
  isRound(T,_,_),!.
algebraicFace(T,Face,Face) :-
  isIden(T,_,_),!.
algebraicFace(T,SoFar,Face) :-
  isBraceTerm(T,_,Args),
  pickupFields(Args,SoFar,Face).

pickupFields([],Face,Face).
pickupFields([T|M],SF,Face) :-
  isBinary(T,Lc,":",L,R),
  isIden(L,_,Nm),
  checkSoFar(Lc,Nm,R,SF,SF0),
  pickupFields(M,[T|SF0],Face).
pickupFields([T|M],SF,Face) :-
  locOfAst(T,Lc),
  reportError("invalid type field: %s",[T],Lc),
  pickupFields(M,SF,Face).

checkSoFar(_,_,_,[],[]).
checkSoFar(Lc,Nm,T,[P|L],L) :-
  isBinary(P,":",NN,RR),
  isIden(NN,_,Nm),
  (sameTerm(RR,T) ; reportError("field %s:%s must be identical to: %s",[Nm,T,RR],Lc)).
checkSoFar(Lc,Nm,T,[P|L],[P|LL]) :-
  checkSoFar(Lc,Nm,T,L,LL).

typeRule(Lc,Quants,Constraints,Hd,Body,Rule) :-
  binary(Lc,"<~",Hd,Body,Rl),
  wrapConstraints(Constraints,Lc,Rl,ConRl),
  wrapQuants(Quants,Lc,ConRl,Rule).

wrapQuants([],_,Rule,Rule).
wrapQuants(Q,Lc,Rl,Rule) :-
  listComma(Q,Lc,QV),
  binary(Lc,"~~",QV,Rl,R1),
  unary(Lc,"all",R1,Rule).

wrapConstraints([],_,Tp,Tp).
wrapConstraints(Con,Lc,Tp,ConTp) :-
  listComma(Con,Lc,CTp),
  binary(Lc,"|:",CTp,Tp,ConTp).

listComma([T],_,T).
listComma([T|R],Lc,CT) :-
  listComma(R,Lc,RR),
  binary(Lc,",",T,RR,CT).

genAnonArgs([],[]).
genAnonArgs([T|M],[name(Lc,"_")|A]) :- locOfAst(T,Lc), genAnonArgs(M,A).

genFieldArgs([],[],[]).
genFieldArgs([F|M],[name(Lc,V)|R],[FA|AR]) :-
  isBinary(F,Lc,":",L,_),
  genstr("_",V),
  binary(Lc,"=",L,name(Lc,V),FA),
  genFieldArgs(M,R,AR).

hasType(Lc,Nm,Tp,St) :- 
  binary(Lc,":",name(Lc,Nm),Tp,St).

classType(Lc,Args,Res,Tp) :- isTuple(A,Lc,Args),binary(Lc,"<=>",A,Res,Tp).
funType(Lc,Args,Res,Tp) :- isTuple(A,Lc,Args),binary(Lc,"=>",A,Res,Tp).
genericType(Lc,Nm,Args,Tp) :- squareTerm(Lc,Nm,Args,Tp).

inheritThing(Lc,Hd,Rule) :-
  binary(Lc,"<=",Hd,name(Lc,"thing"),Rule).

bodyRule(Lc,Hd,Els,Body) :-
  isBraceTuple(Content,Lc,Els),
  binary(Lc,"..",Hd,Content,Body).

thingType(name(Lc,"thing"),Lc).

generateAnnotations([],_,_,Stmts,Stmts).
generateAnnotations([Def|Els],Quants,Constraints,[Annot|Stmts],S0) :-
  isBinary(Def,Lc,":",N,Tp),
  wrapConstraints(Constraints,Lc,Tp,CTp),
  wrapQuants(Quants,Lc,CTp,MTp),
  binary(Lc,":",N,MTp,Annot),
  generateAnnotations(Els,Quants,Constraints,Stmts,S0).
generateAnnotations([_|Els],Quants,Constraints,Stmts,S0) :- % ignore things like assertions
  generateAnnotations(Els,Quants,Constraints,Stmts,S0).

