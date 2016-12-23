:- module(dependencies,[dependencies/6]).

:- use_module(topsort).
:- use_module(abstract).
:- use_module(errors).
:- use_module(misc).
:- use_module(keywords).
:- use_module(wff).

dependencies(Els,Groups,Public,Annots,Imports,Other) :-
  collectDefinitions(Els,Dfs,Public,Annots,Imports,Other),
  allRefs(Dfs,[],AllRefs),
  collectThetaRefs(Dfs,AllRefs,Annots,Defs),
  topsort(Defs,Groups).
  %showGroups(Groups).

collectDefinitions([St|Stmts],Defs,P,A,I,Other) :-
  collectDefinition(St,Stmts,S0,Defs,D0,P,P0,A,A0,I,I0,Other,O0,dependencies:nop),
  collectDefinitions(S0,D0,P0,A0,I0,O0).
collectDefinitions([],[],[],[],[],[]).

collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,[St|I],I,Other,Other,_) :-
  isImport(St).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,I,I,[St|Other],Other,_) :-
  isUnary(St,"assert",_).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,I,I,[St|Other],Other,_) :-
  isUnary(St,"show",_).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,Px,[(V,St)|A],A,I,I,Other,Other,Export) :-
  isBinary(St,":",L,_),
  isIden(L,V),
  call(Export,var(V),P,Px).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,P,A,Ax,I,Ix,O,Ox,_) :-
  isUnary(St,"private",Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,_,A,Ax,I,Ix,O,Ox,dependencies:nop).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,I,Ix,O,Ox,_) :-
  isUnary(St,"public",Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,I,Ix,O,Ox,dependencies:export).
collectDefinition(St,Stmts,Stmts,[(Nm,Lc,[St])|Defs],Defs,P,Px,A,A,I,I,O,O,Export) :-
  isUnary(St,Lc,"contract",Inner),
  contractName(Inner,Nm),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,[(Nm,Lc,[St])|Defs],Defs,P,Px,A,A,I,I,O,O,Export) :-
  isUnary(St,Lc,"implementation",Inner),
  isBinary(Inner,"..",Im,_),
  implementationName(Im,Nm),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stx,[(Nm,Lc,[St|Defn])|Defs],Defs,P,Px,A,A,I,I,O,O,Export) :-
  ruleName(St,Nm,Kind),
  locOfAst(St,Lc),
  collectDefines(Stmts,Kind,Stx,Nm,Defn),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,I,I,O,O,_) :-
  locOfAst(St,Lc),
  reportError("Cannot fathom %s",[St],Lc).

export(Nm,[Nm|P],P).
nop(_,P,P).

isImport(St) :-
  isUnary(St,"public",I),!,
  isImport(I).
isImport(St) :-
  isUnary(St,"private",I),!,
  isImport(I).
isImport(St) :-
  isUnary(St,"import",_).

ruleName(St,Name,Mode) :-
  isQuantified(St,_,B),!,
  ruleName(B,Name,Mode).
ruleName(St,Name,Mode) :-
  isBinary(St,"|:",_,R),
  ruleName(R,Name,Mode).
ruleName(St,Nm,con) :-
  isUnary(St,"contract",I),
  contractName(I,Nm),!.
ruleName(St,Nm,impl) :-
  isUnary(St,"implementation",I),
  isBinary(I,"..",L,_),
  implementationName(L,Nm),!.
ruleName(St,Nm,type) :-
  isUnary(St,"type",I),
  ruleName(I,Nm,type).
ruleName(St,tpe(Nm),type) :-
  isBinary(St,"<~",L,_),
  typeName(L,Nm).
ruleName(St,var(Nm),value) :-
  headOfRule(St,Hd),
  headName(Hd,Nm).

contractName(St,Nm) :-
  isQuantified(St,_,B),
  contractName(B,Nm).
contractName(St,Nm) :-
  isBinary(St,"|:",_,R),
  contractName(R,Nm).
contractName(St,Nm) :-
  isBinary(St,"<~",L,_),
  contractName(L,Nm).
contractName(St,con(Nm)) :-
  isSquare(St,Nm,_).

%% Thus must mirror the definition in types.pl 
implementationName(St,Nm) :-
  isQuantified(St,_,B),
  implementationName(B,Nm).
implementationName(St,Nm) :-
  isBinary(St,"|:",_,R),
  implementationName(R,Nm).
implementationName(St,imp(Nm)) :-
  implementedContractName(St,Nm).

implementedContractName(Sq,INm) :-
  isSquare(Sq,Nm,A),
  appStr(Nm,S0,S1),
  marker(conTract,M),
  surfaceNames(A,M,S1,[]),
  string_chars(INm,S0).

surfaceNames([],_,S,S).
surfaceNames([T|_],Sep,S0,Sx) :-
  isBinary(T,Lc,"->>",L,_),!,
  tupleize(L,"()",Lc,tuple(_,_,Els)),
  surfaceNames(Els,Sep,S0,Sx).
surfaceNames([T|L],Sep,S0,Sx) :-
  surfaceName(T,SN),
  appStr(Sep,S0,S1),
  appStr(SN,S1,S2),
  surfaceNames(L,Sep,S2,Sx).

surfaceName(N,Nm) :-
  isIden(N,Nm).
surfaceName(N,Nm) :-
  isSquare(N,Nm,_).
surfaceName(T,Nm) :-
  isTuple(T,_,A),
  length(A,Ar),
  swritef(Nm,"()%d",[Ar]).

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
  isBinary(St,":-",L,_),
  headOfRule(L,Hd).
headOfRule(St,Hd) :-
  isBinary(St,"<=",Hd,_).
headOfRule(St,Hd) :-
  isBraceTerm(St,_,Hd,_),!.
headOfRule(St,Hd) :-
  isBinary(St,"-->",H,_),
  (isBinary(H,",",Hd,_) ; H=Hd),!.
headOfRule(St,St) :-
  isRound(St,Nm,_), \+ isRuleKeyword(Nm).

headName(Head,Nm) :-
  isBinary(Head,"@@",H,_),
  headName(H,Nm).
headName(Head,Nm) :-
  isRoundTerm(Head,Op,_),
  headName(Op,Nm).
headName(Head,Nm) :-
  isBrace(Head,Nm,_).
headName(Name,Nm) :-
  isName(Name,Nm),
  \+isKeyword(Nm).
headName(tuple(_,"()",[Name]),Nm) :-
  headName(Name,Nm).

typeName(Tp,Nm) :-
  isBinary(Tp,"|:",_,R),
  typeName(R,Nm).
typeName(Tp,Nm) :- isSquare(Tp,Nm,_), \+ isKeyword(Nm).
typeName(Tp,Nm) :- isName(Tp,Nm), \+ isKeyword(Nm).

allRefs([(N,_,_)|Defs],SoFar,AllRefs) :-
  allRefs(Defs,[N|SoFar],AllRefs).
allRefs([],SoFar,SoFar).

collectThetaRefs([],_,_,[]).
collectThetaRefs([(Defines,Lc,Def)|Defs],AllRefs,Annots,[(Defines,Refs,Lc,Def)|Dfns]) :-
  collectStmtRefs(Def,AllRefs,Annots,Refs,[]),
  collectThetaRefs(Defs,AllRefs,Annots,Dfns).

collectStmtRefs([],_,_,Refs,Refs).
collectStmtRefs([St|Stmts],All,Annots,SoFar,Refs) :-
  collRefs(St,All,Annots,SoFar,S0),
  collectStmtRefs(Stmts,All,Annots,S0,Refs).

collRefs(St,All,Annots,SoFar,Refs) :-
  isQuantified(St,_,B),
  collRefs(B,All,Annots,SoFar,Refs).
collRefs(St,All,Annots,SoFar,Rest) :-
  isBinary(St,"|:",L,Inner),
  collConstraints(L,All,SoFar,R0),
  collRefs(Inner,All,Annots,R0,Rest).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,"=",H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectExpRefs(Exp,All,R1,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,"=>",H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectExpRefs(Exp,All,R1,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,":-",L,C),
  isBinary(L,"=>",H,R),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectCondRefs(C,All,R1,R2),
  collectExpRefs(R,All,R2,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,"-->",H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectGrHeadRefs(H,All,R0,R1),
  collectExpRefs(Exp,All,R1,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,"<=",H,Exp),
  collectAnnotRefs(St,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectLabelRefs(Exp,All,R1,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBraceTerm(St,_,H,Els),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectClassRefs(Els,All,R1,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,":-",H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectCondRefs(Exp,All,R1,Refs).
collRefs(St,All,_,R0,Refs) :-
  isBinary(St,"<~",_,Tp),
  collectTypeRefs(Tp,All,R0,Refs).
collRefs(St,All,_,R0,Refs) :-
  isUnary(St,"implementation",I),
  isBinary(I,"..",L,R),
  collectContractRefs(L,All,R0,R1),
  isBraceTuple(R,_,Defs),
  collectClassRefs(Defs,All,R1,Refs).

collRefs(St,All,Annots,SoFar,Refs) :-
  collectAnnotRefs(St,All,Annots,SoFar,R0),
  collectHeadRefs(St,All,R0,Refs).

collectHeadRefs(Hd,All,R0,Refs) :-
  isBinary(Hd,"@@",L,R),
  collectHeadRefs(L,All,R0,R1),
  collectCondRefs(R,All,R1,Refs).
collectHeadRefs(Hd,All,R0,Refs) :-
  isRoundTerm(Hd,_,A),
  collectPtnListRefs(A,All,R0,Refs).
collectHeadRefs(_,_,R,R).

collectGrHeadRefs(Hd,All,R0,Refs) :-
  isBinary(Hd,",",L,R),!,
  collectHeadRefs(L,All,R0,R1),
  collectPtnRefs(R,All,R1,Refs).
collectGrHeadRefs(Hd,All,R0,Refs) :-
  collectHeadRefs(Hd,All,R0,Refs).

collectClassRefs(Defs,All,SoFar,Refs) :-
  locallyDefined(Defs,All,Rest),
  collectStmtRefs(Defs,Rest,[],SoFar,Refs).

collectAnnotRefs(H,All,Annots,SoFar,Refs) :-
  headName(H,Nm),
  is_member((Nm,Annot),Annots),!,
  isBinary(Annot,":",_,Tp),
  collectTypeRefs(Tp,All,SoFar,Refs).
collectAnnotRefs(_,_,_,Refs,Refs).

collConstraints(C,All,SoFar,Refs) :-
  isBinary(C,",",L,R),
  collConstraints(L,All,SoFar,R0),
  collConstraints(R,All,R0,Refs).
collConstraints(C,All,[con(Nm)|Refs],Refs) :-
  isSquare(C,_,Nm,_),
  is_member(con(Nm),All).
collConstraints(_,_,Refs,Refs).

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
  isBinary(C,"%%",L,R),
  isBinary(R,"~",St,Re),
  collectExpRefs(L,A,R0,R1),
  collectExpRefs(St,A,R1,R2),
  collectExpRefs(Re,A,R2,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,"%%",L,R),
  collectExpRefs(L,A,R0,R1),
  collectExpRefs(R,A,R1,Refs).
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
  isSquareTuple(T,Lc,Els),
  collectExpRefs(name(Lc,"[]"),A,R0,R1),
  collectExpRefs(name(Lc,",.."),A,R1,R2), % special handling for list notation
  collectExpListRefs(Els,A,R2,Refs).
collectExpRefs(app(_,Op,Args),All,R,Refs) :-
  collectExpRefs(Op,All,R,R0),
  collectExpRefs(Args,All,R0,Refs).
collectExpRefs(T,All,R,Refs) :-
  isBraceTuple(T,_,Els),
  collectExpListRefs(Els,All,R,Refs).
collectExpRefs(T,All,R,Refs) :-
  isSquareTerm(T,Op,A),
  collectExpRefs(Op,All,R,R0),
  collectIndexRefs(A,All,R0,Refs).
collectExpRefs(_,_,Refs,Refs).

collectExpListRefs([],_,Refs,Refs).
collectExpListRefs([E|L],A,R0,Refs) :-
  collectExpRefs(E,A,R0,R1),
  collectExpListRefs(L,A,R1,Refs).

collectIndexRefs([A],All,R,Refs) :-
  isBinary(A,_,"->",Ky,Vl),!,
  collectExpRefs(Ky,All,R,R0),
  collectExpRefs(Vl,All,R0,Refs).
collectIndexRefs([A],All,R,Refs) :-
  isUnary(A,_,"\\+",Ky),!,
  collectExpRefs(Ky,All,R,Refs).
collectIndexRefs(A,All,R,Refs) :-
  collectExpListRefs(A,All,R,Refs).

collectPtnListRefs([],_,Refs,Refs).
collectPtnListRefs([E|L],A,R0,Refs) :-
  collectPtnRefs(E,A,R0,R1),
  collectPtnListRefs(L,A,R1,Refs).

collectPtnRefs(P,A,R0,Refs) :-
  isBinary(P,"@@",L,R),
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
collectPtnRefs(_,_,Refs,Refs).

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
  isBinary(T,"-->",L,R),
  isTuple(L,A),
  collectTypeList(A,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Rest) :-
  isBinary(T,"|:",L,R),
  collConstraints(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Rest).
collectTypeRefs(T,All,SoFar,Rest) :-
  isBinary(T,"->>",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Rest).
collectTypeRefs(C,All,SoFar,Refs) :-
  isBinary(C,",",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBraceTerm(T,_,L,[]), 
  isTuple(L,A),
  collectTypeList(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBraceTuple(T,_,A), 
  collectFaceTypes(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isQuantified(T,_,A),
  collectTypeRefs(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isTuple(T,Args),
  collectTypeList(Args,All,SoFar,Refs).

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

collectContractRefs(C,All,R0,Refs) :-
  isQuantified(C,_,B),
  collectContractRefs(B,All,R0,Refs).
collectContractRefs(C,All,R0,Refs) :-
  isBinary(C,"|:",L,R),
  collConstraints(L,All,R0,R1),
  collectTypeRefs(R,All,R1,Refs).
collectContractRefs(C,All,R0,Refs) :-
  collectTypeRefs(C,All,R0,Refs).

collectLabelRefs(Lb,All,R0,Refs) :- collectExpRefs(Lb,All,R0,Refs).

showGroups([]).
showGroups([G|M]) :- 
  reportMsg("Group:",[]),
  showGroup(G),
  showGroups(M).
showGroup([]).
showGroup([(Def,Lc,_)|M]) :-
  reportMsg("Def %s",[Def],Lc),
  showGroup(M).

showRefs(Msg,Refs) :-
  reportMsg("%s references: %s",[Msg,Refs]).
