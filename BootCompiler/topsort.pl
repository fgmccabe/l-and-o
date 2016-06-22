:-module(topsort,[topsort/2]).
:-use_module(misc).

topsort(Defs,Groups) :-
  analyseDefs(Defs,[],Grps),!,
  reverse(Grps,Groups).

analyseDefs([],Groups,Groups).
analyseDefs([Def|Defs],Groups,OGroups) :-
  analyseDef(Def,Defs,IDefs,[],_,Groups,G0,_),
  analyseDefs(IDefs,G0,OGroups).

analyseDef((Defines,Refs,Lc,Df),Defs,ODefs,Stack,OStack,G,OG,Pt) :-
  pushDef((Defines,Refs,Lc,Df),Stack,S0,SPt),
  analyseRefs(Refs,Defs,ODefs,S0,S1,G,G0,SPt,Pt),
  popGroups(S1,OStack,G0,OG,SPt,Pt).

analyseRefs([],Defs,Defs,Stk,Stk,G,G,Low,Low).
analyseRefs([Ref|Refs],Defs,ODefs,Stk,OStk,G,OG,Low,Pt) :-
  analyse(Ref,Defs,ID,Stk,S1,G,G1,Low,Low1),
  analyseRefs(Refs,ID,ODefs,S1,OStk,G1,OG,Low1,Pt).

analyse(Ref,Defs,Defs,Stack,Stack,G,G,Low,Pt) :- inStack(Ref,Stack,X), minPoint(X,Low,Pt).
analyse(Ref,Defs,ODefs,Stack,OStack,G,OG,Low,Pt) :- 
  pickDef(Ref,Defs,RDefs,Df), 
  analyseDef(Df,RDefs,ODefs,Stack,OStack,G,OG,DfPt),
  minPoint(Low,DfPt,Pt).
analyse(_,Defs,Defs,Stack,Stack,Groups,Groups,Low,Low).

pushDef((Defines,Refs,Lc,Def),Stack,[(Defines,Refs,Lc,Def,Sz)|Stack],Sz) :- length(Stack,Sz).

inStack(Ref,[(Ref,_,_,_,X)|_],X).
inStack(Ref,[_|Stack],X) :- inStack(Ref,Stack,X).

pickDef(Nm,[(Nm,Refs,Lc,Df)|Defs],Defs,(Nm,Refs,Lc,Df)).
pickDef(Nm,[D|Defs],[D|ODefs],Df) :- pickDef(Nm,Defs,ODefs,Df).

popGroups(Stack,Stack,Groups,Groups,Low,Pt) :- Pt > Low.
popGroups(Stack,OStack,Groups,OG,_,Pt) :-
  popGroup(Stack,OStack,Group,Pt),
  mkGroup(Group,Groups,OG).

popGroup([(Nm,_,Lc,Df,Pt)|Stack],OStack,[(Nm,Lc,Df)|Group],Low) :- Pt >= Low,!,
  popGroup(Stack,OStack,Group,Low).
popGroup(Stack,Stack,[],_).

mkGroup([],Groups,Groups).
mkGroup(Group,Groups,[Group|Groups]).

minPoint(X,Y,X) :- X =< Y.
minPoint(X,Y,Y) :- X>Y.