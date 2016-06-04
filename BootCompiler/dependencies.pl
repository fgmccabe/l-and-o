:- module(dependencies,[dependencies/6]).

:- use_module(topsort).
:- use_module(abstract).
:- use_module(errors).
:- use_module(misc).
:- use_module(keywords).

dependencies(Els,Groups,Private,Annots,Imports,Other) :-
  collectDefinitions(Els,Dfs,Private,Annots,Imports,Other),
  allRefs(Dfs,[],AllRefs),
  collectThetaRefs(Dfs,AllRefs,Defs),
  topsort(Defs,Groups).

collectDefinitions([St|Stmts],Defs,P,A,[St|I],Other) :-
  isUnary(St,"import",_),
  collectDefinitions(Stmts,Defs,P,A,I,Other).
collectDefinitions([St|Stmts],Defs,P,A,I,[St|Other]) :-
  isUnary(St,"assert",_),
  collectDefinitions(Stmts,Defs,P,A,I,Other).
collectDefinitions([St|Stmts],Defs,P,[(V,St)|A],I,Other) :-
  isBinary(St,":",L,_),
  isIden(L,V),
  collectDefinitions(Stmts,Defs,P,A,I,Other).
collectDefinitions([St|Stmts],Defs,[(Prvte,Kind)|P],A,I,O) :-
  isUnary(St,"private",Inner),
  ruleName(Inner,Prvte,Kind),
  collectDefinitions([Inner|Stmts],Defs,P,A,I,O).
collectDefinitions([St|Stmts],[(Nm,Lc,[St|Defn])|Defs],P,A,I,O) :-
  ruleName(St,Nm,Kind),
  locOfAst(St,Lc),
  collectDefines(Stmts,Kind,OS,Nm,Defn),
  collectDefinitions(OS,Defs,P,A,I,O).
collectDefinitions([],[],[],[],[],[]).

ruleName(St,Name,Mode) :-
  isQuantified(St,_,B),!,
  ruleName(B,Name,Mode).
ruleName(St,var(Nm),value) :-
  headOfRule(St,Hd),
  headName(Hd,Nm).
ruleName(St,tpe(Nm),type) :-
  isBinary(St,"<~",L,_),
  typeName(L,Nm).

collectDefines([St|Stmts],Kind,OSt,Nm,[St|Defn]) :-
  ruleName(St,Nm,Kind),
  collectDefines(Stmts,Kind,OSt,Nm,Defn).
collectDefines([St|Stmts],Kind,[St|OSt],Nm,Defn) :-
  collectDefines(Stmts,Kind,OSt,Nm,Defn).
collectDefines(Stmts,_,Stmts,_,[]).

headOfRule(St,Hd) :-
  isBinary(St,"=",Hd,_).
headOfRule(St,Hd) :-
  isBinary(St,"=>",Hd,_).
headOfRule(St,Hd) :-
  isBinary(St,":-",Hd,_).
headOfRule(St,Hd) :-
  isBinary(St,":--",Hd,_).
headOfRule(St,Hd) :-
  isBinary(St,"<=",Hd,_).
headOfRule(St,Hd) :-
  isBinary(St,"<<",Hd,_).
headOfRule(St,Hd) :-
  isBinary(St,"..",Hd,_).
headOfRule(St,Hd) :-
  isBinary(St,"-->",Hd,_).
headOfRule(St,St) :-
  isRound(St,Nm,_), \+ isRuleKeyword(Nm).

headName(Head,Nm) :-
  isBinary(Head,"::",H,_),
  headName(H,Nm).
headName(Head,Nm) :-
  isRoundTerm(Head,Op,_),
  headName(Op,Nm).
headName(Name,Nm) :-
  isName(Name,Nm).
headName(tuple(_,"()",[Name]),Nm) :-
  isName(Name,Nm).

typeName(Tp,Nm) :- isSquare(Tp,Nm,_).
typeName(Tp,Nm) :- isName(Tp,Nm).

isPrivate(St,Inner) :-
  isUnary(St,"private",Inner).

allRefs([(N,_,_)|Defs],SoFar,AllRefs) :-
  allRefs(Defs,[N|SoFar],AllRefs).
allRefs([],SoFar,SoFar).

collectThetaRefs([],_,[]).
collectThetaRefs([(Defines,Lc,Def)|Defs],AllRefs,[(Defines,Refs,Lc,Def)|Dfns]) :-
  collectStmtRefs(Def,AllRefs,Refs,[]),
  collectThetaRefs(Defs,AllRefs,Dfns).

collectStmtRefs([],_,Refs,Refs).
collectStmtRefs([St|Stmts],All,SoFar,Refs) :-
  collRefs(St,All,SoFar,S0),
  collectStmtRefs(Stmts,All,S0,Refs).

collRefs(St,All,SoFar,Refs) :-
  isQuantified(St,V,B),
  collectQuants(V,All,SoFar,R0),
  collRefs(B,All,R0,Refs).
collRefs(St,All,SoFar,Refs) :-
  isBinary(St,"=",H,Exp),
  collectHeadRefs(H,All,SoFar,R0),
  collectExpRefs(Exp,All,R0,Refs).
collRefs(St,All,SoFar,Refs) :-
  isBinary(St,"=>",H,Exp),
  collectHeadRefs(H,All,SoFar,R0),
  collectExpRefs(Exp,All,R0,Refs).
collRefs(St,All,SoFar,Refs) :-
  isBinary(St,"<=",H,Exp),
  collectHeadRefs(H,All,SoFar,R0),
  collectLabelRefs(Exp,All,R0,Refs).
collRefs(St,All,SoFar,Refs) :-
  isBinary(St,"<<",H,Exp),
  collectHeadRefs(H,All,SoFar,R0),
  collectLabelRefs(Exp,All,R0,Refs).
collRefs(St,All,SoFar,Refs) :-
  isBinary(St,"..",H,Exp),
  collectHeadRefs(H,All,SoFar,R0),
  collectClassRefs(Exp,All,R0,Refs).
collRefs(St,All,SoFar,Refs) :-
  isBinary(St,":-",H,Exp),
  collectHeadRefs(H,All,SoFar,R0),
  collectCondRefs(Exp,All,R0,Refs).
collRefs(St,All,SoFar,Refs) :-
  isBinary(St,":--",H,Exp),
  collectHeadRefs(H,All,SoFar,R0),
  collectCondRefs(Exp,All,R0,Refs).
collRefs(St,All,SoFar,Refs) :-
  isBinary(St,"-->",H,Exp),
  collectHeadRefs(H,All,SoFar,R0),
  collectCondRefs(Exp,All,R0,Refs).
collRefs(St,All,R0,Refs) :-
  isBinary(St,"<~",_,Tp),
  collectTypeRefs(Tp,All,R0,Refs).
collRefs(St,All,SoFar,Refs) :-
  collectHeadRefs(St,All,SoFar,Refs).

collectHeadRefs(Hd,All,R0,Refs) :-
  isBinary(Hd,"::",L,R),
  collectHeadRefs(L,All,R0,R1),
  collectCondRefs(R,All,R1,Refs).
collectHeadRefs(Hd,All,R0,Refs) :-
  isRoundTerm(Hd,_,A),
  collectPtnListRefs(A,All,R0,Refs).
collectHeadRefs(_,_,R,R).

collectClassRefs(Term,All,SoFar,Refs) :-
  isBraceTuple(Term,_,Defs),
  locallyDefined(Defs,All,Rest),
  collectStmtRefs(Defs,Rest,SoFar,Refs).

locallyDefined([],All,All).
locallyDefined([St|Stmts],All,Rest) :-
  removeLocalDef(St,All,A0),
  locallyDefined(Stmts,A0,Rest).

removeLocalDef(St,All,Rest) :-
  ruleName(St,Nm,_),
  subtract(Nm,All,Rest).
removeLocalDef(_,All,All).

collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,",",L,R),
  collectCondRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,"|",L,R),
  isBinary(L,"?",T,Th),
  collectCondRefs(T,A,R0,R1),
  collectCondRefs(Th,A,R1,R2),
  collectCondRefs(R,A,R2,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,"|",L,R),
  collectCondRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,"*>",L,R),
  collectCondRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isUnary(C,"!",R),
  collectCondRefs(R,A,R0,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isUnary(C,"\\+",R),
  collectCondRefs(R,A,R0,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isTuple(C,[Inner]),
  collectCondRefs(Inner,A,R0,Refs).
collectCondRefs(C,A,R0,Refs) :-
  collectExpRefs(C,A,R0,Refs).

collectExpRefs(E,A,R0,Refs) :-
  isBinary(E,":",L,R),
  collectExpRefs(L,A,R0,R1),
  collectTypeRefs(R,A,R1,Refs).
collectExpRefs(V,A,[var(Nm)|Refs],Refs) :-
  isName(V,Nm),
  is_member(var(Nm),A).
collectExpRefs(T,A,R0,Refs) :-
  isRoundTerm(T,O,Args),
  collectExpRefs(O,A,R0,R1),
  collectExpListRefs(Args,A,R1,Refs).
collectExpRefs(T,A,R0,Refs) :-
  isTuple(T,Els),
  collectExpListRefs(Els,A,R0,Refs).
collectExpRefs(T,A,R0,Refs) :-
  isSquareTuple(T,_,Els),
  collectExpListRefs(Els,A,R0,Refs).
collectExpRefs(_,_,Refs,Refs).

collectExpListRefs([],_,Refs,Refs).
collectExpListRefs([E|L],A,R0,Refs) :-
  collectExpRefs(E,A,R0,R1),
  collectExpListRefs(L,A,R1,Refs).

collectPtnListRefs([],_,Refs,Refs).
collectPtnListRefs([E|L],A,R0,Refs) :-
  collectPtnRefs(E,A,R0,R1),
  collectPtnListRefs(L,A,R1,Refs).

collectPtnRefs(P,A,R0,Refs) :-
  isBinary(P,"::",L,R),
  collectPtnRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectPtnRefs(P,A,R0,Refs) :-
  isBinary(P,":",L,R),
  collectPtnRefs(L,A,R0,R1),
  collectTypeRefs(R,A,R1,Refs).
collectPtnRefs(V,A,[var(Nm)|Refs],Refs) :-
  isIden(V,Nm),
  is_member(var(Nm),A).
collectPtnRefs(app(_,Op,Args),All,R0,Refs) :-
  collectExpRefs(Op,All,R0,R1),
  collectPtnRefs(Args,All,R1,Refs).
collectPtnRefs(tuple(_,_,Args),All,R0,Refs) :-
  collectPtnListRefs(Args,All,R0,Refs).
collectPtnRefs(_,Refs,Refs).

collectTypeRefs(V,All,SoFar,Refs) :-
  isIden(V,Nm),
  collectTypeName(Nm,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isSquare(T,Nm,A),
  collectTypeName(Nm,All,SoFar,R0),
  collectTypeList(A,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBinary(T,"=>",L,R),
  isTuple(L,A),
  collectTypeList(A,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBinary(T,"<=>",L,R),
  isTuple(L,A),
  collectTypeList(A,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBraceTerm(T,L,[]), isTuple(L,A),
  collectTypeList(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBraceTuple(T,_,A), 
  collectFaceTypes(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isQuantified(T,V,A),
  collectQuants(V,All,SoFar,R0),
  collectTypeRefs(A,All,R0,Refs).

collectQuants(T,All,SoFar,Refs) :-
  isIden(T,Nm),
  collectTypeName(Nm,All,SoFar,Refs).
collectQuants(T,All,SoFar,Refs) :-
  isBinary(T,",",L,R),
  collectQuants(L,All,SoFar,R0),
  collectQuants(R,All,R0,Refs).
collectQuants(T,All,SoFar,Refs) :-
  isBinary(T,"<~",L,Up),
  isBinary(L,"<~",Lw,_),
  collectTypeRefs(Lw,All,SoFar,R0),
  collectTypeRefs(Up,All,R0,Refs).
collectQuants(T,All,SoFar,Refs) :-
  isBinary(T,"<~",_,Up),
  collectTypeRefs(Up,All,SoFar,Refs).

collectTypeList([],_,Refs,Refs).
collectTypeList([Tp|List],All,SoFar,Refs) :-
  collectTypeRefs(Tp,All,SoFar,R0),
  collectTypeList(List,All,R0,Refs).

collectFaceTypes([],_,Refs,Refs).
collectFaceTypes([T|List],All,SoFar,Refs) :-
  isBinary(T,":",_,Tp),
  collectTypeRefs(Tp,All,SoFar,R0),
  collectFaceTypes(List,All,R0,Refs).

collectTypeName(Nm,All,[tpe(Nm)|SoFar],SoFar) :-
  is_member(tpe(Nm),All),!.
collectTypeName(_,_,Refs,Refs).


collectLabelRefs(Lb,All,R0,Refs) :- collectExpRefs(Lb,All,R0,Refs).
