:- module(checker,[checkProgram/3]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(dependencies).
:- use_module(freshen).
:- use_module(unify).
:- use_module(types).
:- use_module(parsetype).
:- use_module(dict).
:- use_module(misc).
:- use_module(canon).
:- use_module(errors).
:- use_module(keywords).
:- use_module(macro).
:- use_module(import).
:- use_module(transitive).

:- use_module(display).

checkProgram(Prog,Repo,prog(Pkg,Imports,Defs,Others,Exports,Types,Contracts,Impls)) :-
  stdDict(Base),
  isBraceTerm(Prog,Pk,Els),
  packageName(Pk,Pkg),
  pushScope(Base,Env),
  locOfAst(Prog,Lc),
  thetaEnv(Pkg,Repo,Lc,Els,[],Env,_,Defs,Public,Imports,Others),
  computeExport(Defs,[],Public,Exports,Types,Contracts,Impls),!.

thetaEnv(Pkg,Repo,Lc,Els,Fields,Base,TheEnv,Defs,Public,Imports,Others) :-
  macroRewrite(Els,Stmts),
  displayAll(Stmts),
  dependencies(Stmts,Groups,Public,Annots,Imps,Otrs),
  processImportGroup(Imps,Imports,Repo,Base,IBase),
  pushFace(Fields,Lc,IBase,Env),
  checkGroups(Groups,Fields,Annots,Defs,Env,TheEnv,Pkg),
  checkOthers(Otrs,Others,TheEnv,Pkg).

importDefs(spec(_,_,faceType(Exported),faceType(Types),_,_),Lc,Env,Ex) :-
  declareFields(Exported,Lc,Env,E0),
  importTypes(Types,Lc,E0,Ex).

declareFields([],_,Env,Env).
declareFields([(Nm,Tp)|More],Lc,Env,Ex) :-
  declareVar(Nm,vr(Nm,Lc,Tp),Env,E0),
  declareFields(More,Lc,E0,Ex).

importTypes([],_,Env,Env).
importTypes([(Nm,tupleType(Rules))|More],Lc,Env,Ex) :-
  pickTypeTemplate(Rules,Type),
  pickFaceRule(Rules,FaceRule),
  declareType(Nm,tpDef(Lc,Type,FaceRule),Env,E0),
  importTypes(More,Lc,E0,Ex).

findAllImports([],_,[]).
findAllImports([St|More],Lc,[Spec|Imports]) :-
  findImport(St,Lc,private,Spec),
  findAllImports(More,_,Imports).

findImport(St,Lc,_,Spec) :-
  isUnary(St,Lc,"private",I),
  findImport(I,_,private,Spec).
findImport(St,Lc,_,Spec) :-
  isUnary(St,Lc,"public",I),
  findImport(I,_,public,Spec).
findImport(St,Lc,Viz,import(Viz,Pkg,Version)) :-
  isUnary(St,Lc,"import",P),
  packageName(P,Pkg,Version).

processImportGroup(Stmts,ImportSpecs,Repo,Env,Ex) :-
  findAllImports(Stmts,Lc,Imports),
  importAll(Imports,Repo,AllImports),
  importAllDefs(AllImports,Lc,ImportSpecs,Repo,Env,Ex).

importAll(Imports,Repo,AllImports) :-
  closure(Imports,[],checker:notAlreadyImported,checker:importMore(Repo),AllImports).

importAllDefs([],_,[],_,Env,Env).
importAllDefs([import(Viz,Pkg,Vers)|More],Lc,[import(Viz,Pkg,Vers,Exported,Types,Classes)|Specs],Repo,Env,Ex) :-
  importPkg(Pkg,Vers,Repo,Spec),
  Spec = spec(_,_,Exported,Types,Classes,_),
  importDefs(Spec,Lc,Env,Ev0),
  importAllDefs(More,Lc,Specs,Repo,Ev0,Ex).

notAlreadyImported(import(_,Pkg,Vers),SoFar) :-
  \+ is_member(import(_,Pkg,Vers),SoFar),!.

importMore(Repo,import(Viz,Pkg,Vers),SoFar,[import(Viz,Pkg,Vers)|SoFar],Inp,More) :-
  importPkg(Pkg,Vers,Repo,spec(_,_,_,_,_,Imports)),
  addPublicImports(Imports,Inp,More).
importMore(_,import(_,Pkg,Vers),SoFar,SoFar,Inp,Inp) :-
  reportError("could not import package %s,%s",[Pkg,Vers]).

addPublicImports([],Imp,Imp).
addPublicImports([import(public,Pkg,Vers)|I],Rest,[import(public,Pkg,Vers)|Out]) :-
  addPublicImports(I,Rest,Out).
addPublicImports([import(private,_,_)|I],Rest,Out) :-
  addPublicImports(I,Rest,Out).

checkOthers([],[],_,_).
checkOthers([St|Stmts],Ass,Env,Path) :-
  checkOther(St,Ass,More,Env,Path),!,
  checkOthers(Stmts,More,Env,Path).
checkOthers([St|Stmts],Ass,Env,Path) :-
  locOfAst(St,Lc),
  reportError("cannot understand statement: %s",[St],[Lc]),
  checkOthers(Stmts,Ass,Env,Path).

checkOther(St,[assertion(Lc,Cond)|More],More,Env,_) :-
  isUnary(St,Lc,"assert",C),!,
  checkCond(C,Env,_,Cond).
checkOther(St,[show(Lc,Show)|More],More,Env,_) :-
  isUnary(St,Lc,"show",E),!,
  binary(Lc,".",E,name(Lc,"disp"),Op),
  unary(Lc,"formatSS",app(Lc,Op,tuple(Lc,"()",[])),FCall),
  findType("string",Lc,Env,StringTp),
  typeOfTerm(FCall,StringTp,Env,_,Show).

checkGroups([],_,_,[],E,E,_).
checkGroups([Gp|More],Fields,Annots,Defs,Env,E,Path) :-
  groupType(Gp,GrpType),
  checkGroup(Gp,GrpType,Fields,Annots,Defs,D0,Env,E0,Path),
  checkGroups(More,Fields,Annots,D0,E0,E,Path).

groupType([(var(_),_,_)|_],var).
groupType([(tpe(_),_,_)|_],tpe).
groupType([(con(_),_,_)|_],con).
groupType([(imp(_),_,_)|_],imp).

checkGroup(Grp,tpe,_,_,Defs,Dx,Env,Ex,Path) :-
  typeGroup(Grp,Defs,Dx,Env,Ex,Path).
checkGroup(Grp,var,Fields,Annots,Defs,Dx,Env,Ex,Path) :-
  varGroup(Grp,Fields,Annots,Defs,Dx,Env,Ex,Path).
checkGroup(Grp,con,_,_,Defs,Dx,Env,Ex,Path) :-
  contractGroup(Grp,Defs,Dx,Env,Ex,Path).
checkGroup(Grp,imp,_,_,Defs,Dx,Env,Ex,Path) :-
  implementationGroup(Grp,Defs,Dx,Env,Ex,Path).

contractGroup([(con(N),Lc,[ConStmt])|_],[Contract|Defs],Defs,Env,Ex,Path) :-
  parseContract(ConStmt,Env,Path,Contract),
  declareContract(N,Contract,Env,E0),
  declareMethods(Contract,Lc,E0,Ex).

declareMethods(contract(_,_,QCon,MtdsTp),Lc,Env,Ev) :-
  moveQuants(QCon,Q,Con),
  moveQuants(MtdsTp,_,faceType(Methods)),
  formMethods(Methods,Lc,Q,Con,Env,Ev).

formMethods([],_,_,_,Env,Env).
formMethods([(Nm,Tp)|M],Lc,Q,Con,Env,Ev) :-
  moveQuants(Tp,FQ,QTp),
  merge(FQ,Q,MQ),
  moveQuants(MTp,MQ,constrained(QTp,Con)),
  declareVar(Nm,mtd(Lc,Nm,MTp),Env,E0),
  formMethods(M,Lc,Q,Con,E0,Ev).

% This is very elaborate - to support mutual recursion amoung types.
typeGroup(Grp,Defs,Dx,Env,Ex,Path) :-
  defineTypes(Grp,Env,TmpEnv,Path),
  parseTypeDefs(Grp,TpDefs,[],TmpEnv,Path),
  declareTypes(TpDefs,TpDefs,Defs,Dx,Env,Ex).

defineTypes([],Env,Env,_).
defineTypes([(tpe(N),Lc,[Stmt])|More],Env,Ex,Path) :-
  defineType(N,Lc,Stmt,Env,E0,Path),
  defineTypes(More,E0,Ex,Path).
defineType([(tpe(N),Lc,[_|_])|More],Env,Ex,Path) :-
  reportError("multiple type definition statement for %s",[N],Lc),
  defineTypes(More,Env,Ex,Path).

defineType(N,Lc,_,Env,Env,_) :-
  isType(N,Env,tpDef(_,OLc,_)),!,
  reportError("type %s already defined at %s",[N,OLc],Lc).
defineType(N,Lc,St,Env,Ex,Path) :-
  parseTypeTemplate(St,[],Env,Type,Path),
  declareType(N,tpDef(Lc,Type,faceType([])),Env,Ex).

parseTypeDefs([],Defs,Defs,_,_).
parseTypeDefs([(tpe(N),Lc,[Stmt])|More],Defs,Dx,TmpEnv,Path) :-
  parseTypeDefinition(N,Lc,Stmt,Defs,D0,TmpEnv,Path),
  parseTypeDefs(More,D0,Dx,TmpEnv,Path).

parseTypeDefinition(N,Lc,St,[typeDef(Lc,N,Type,FaceRule)|Defs],Defs,Env,Path) :-
  parseTypeRule(St,Env,FaceRule,Path),
  isType(N,Env,tpDef(_,Type,_)).

declareTypes([],_,Defs,Defs,Env,Env).
declareTypes([typeDef(Lc,N,Type,FaceRule)|More],TpDefs,[typeDef(Lc,N,Type,FaceRule)|Defs],Dx,Env,Ex) :-
  declareType(N,tpDef(Lc,Type,FaceRule),Env,E0),
  declareTypes(More,TpDefs,Defs,Dx,E0,Ex).

faceOfType(Lc,TpDefs,Env,Rules,Tmplate,Face) :-
  moveQuants(Tmplate,Q,Plate),
  findAllFields(Rules,Q,Plate,[],Fields,TpDefs,Env,Lc),
  moveQuants(Face,Q,typeRule(Plate,faceType(Fields))).

isFaceRule(Rl) :-
  moveQuants(Rl,_,typeRule(_,faceType(_))).

findAllFields([],_,_,Fields,Fields,_,_,_).
findAllFields([Rl|Rules],Q,Plate,SoFar,Fields,TpDefs,Env,Lc) :-
  moveQuants(Rl,_,typeRule(Lhs,Rhs)),
  matchTypes(Plate,Lhs,Binding),
  freshn(Rhs,Binding,FRhs),
  findFields(FRhs,Q,TpDefs,TpDefs1,Env,SoFar,Flds,Lc),
  findAllFields(Rules,Q,Plate,Flds,Fields,TpDefs1,Env,Lc).

findFields(faceType(Flds),_,TpDefs,TpDefs,Env,SoFar,Fields,Lc) :-
  collectFace(faceType(Flds),Env,SoFar,Fields,Lc).
findFields(type(Nm),_,TpDefs,TpDefs,Env,SoFar,Fields,Lc) :-
  typeFaceRule(Nm,Env,FaceRule),!,
  mergeFields([FaceRule],[],type(Nm),SoFar,Fields,Env,Lc).
findFields(typeExp(Nm,Args),_,TpDefs,TpDefs,Env,SoFar,Fields,Lc) :-
  typeFaceRule(Nm,Env,FaceRule),
  mergeFields([FaceRule],[],typeExp(Nm,Args),SoFar,Fields,Env,Lc).
findFields(type(Nm),Q,TpDefs,TpDefs1,Env,SoFar,Fields,Lc) :-
  subtract(typeDef(_,_,type(Nm),Rules),TpDefs,TpDefs1),
  findAllFields(Rules,Q,type(Nm),SoFar,Fields,TpDefs1,Env,Lc).
findFields(typeExp(Nm,Args),Q,TpDefs,TpDefs1,Env,SoFar,Fields,Lc) :-
  subtract(typeDef(_,_,typeExp(Nm,Args),Rules),TpDefs,TpDefs1),
  findAllFields(Rules,Q,typExp(Nm,Args),SoFar,Fields,TpDefs1,Env,Lc).
findFields(anonType,_,TpDefs,TpDefs,_,SoFar,SoFar,_).

varGroup(Grp,Fields,Annots,Defs,Dx,Base,Env,Path) :-
  parseAnnotations(Grp,Fields,Annots,Base,Env,Path),!,
  checkVarRules(Grp,Env,Defs,Dx,Path).

parseAnnotations([],_,_,Env,Env,_) :-!.
parseAnnotations([(var(Nm),_,_)|More],Fields,Annots,Env,Ex,Path) :-
  is_member((Nm,Annot),Annots),!,
  isBinary(Annot,Lc,":",_,T),
  parseType(T,Env,Tp),
  declareVar(Nm,vr(Nm,Lc,Tp),Env,E0),
  parseAnnotations(More,Fields,Annots,E0,Ex,Path).
parseAnnotations([(var(N),Lc,_)|More],Fields,Annots,Env,Ex,Path) :-
  is_member((N,Tp),Fields),!,
  declareVar(N,vr(N,Lc,Tp),Env,E0),
  parseAnnotations(More,Fields,Annots,E0,Ex,Path).
parseAnnotations([(var(N),Lc,_)|More],Fields,Annots,Env,Ex,Path) :-
  reportError("no type annotation for variable %s",[N],Lc),
  parseAnnotations(More,Fields,Annots,Env,Ex,Path).

checkVarRules([],_,Defs,Defs,_).
checkVarRules([(var(N),Lc,Stmts)|More],Env,Defs,Dx,Path) :-
  pickupVarType(N,Lc,Env,Tp),
  pickupThisType(Env,ThisType),
  evidence(Tp,ThisType,Q,PT),
  declareTypeVars(Q,Lc,Env,StmtEnv),
  moveConstraints(PT,Cx,ProgramType),
  processStmts(Stmts,ProgramType,Rules,[],StmtEnv,Path),
  generalizeStmts(Rules,Env,Cx,Defs,D0),
  checkVarRules(More,Env,D0,Dx,Path).

pickupVarType(N,_,Env,Tp) :-
  isVar(N,Env,vr(_,_,Tp)),!.
pickupVarType(N,Lc,_,anonType) :- reportError("%s not declared",[N],Lc).

pickupThisType(Env,Tp) :-
  isVar("this",Env,vr(_,_,Tp)),!.
pickupThisType(_,voidType).

checkEvidenceBinding(_,_).

declareTypeVars([],_,Env,Env).
declareTypeVars([(thisType,_)|Vars],Lc,Env,Ex) :- !,
  declareTypeVars(Vars,Lc,Env,Ex).
declareTypeVars([(Nm,Tp)|Vars],Lc,Env,Ex) :-
  declareType(Nm,tpDef(Lc,Tp,voidType),Env,E0),
  declareTypeVars(Vars,Lc,E0,Ex).

findType(Nm,_,Env,Tp) :-
  isType(Nm,Env,tpDef(_,T,_)),
  pickupThisType(Env,ThisType),
  freshen(T,ThisType,_,Tp),!.
findType(Nm,Lc,_,anonType) :-
  reportError("type %s not known",[Nm],Lc).

processStmts([],_,Defs,Defs,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Env,Path) :-
  processStmt(St,ProgramType,Defs,D0,Env,Path),!,
  processStmts(More,ProgramType,D0,Dx,Env,Path).

processStmt(St,ProgramType,Defs,Defx,E,_) :-
  isBinary(St,Lc,"=>",L,R),!,
  checkEquation(Lc,L,R,ProgramType,Defs,Defx,E).
processStmt(St,predType(AT),[clause(Lc,Nm,Args,Cond,Body)|Defs],Defs,E,_) :-
  isBinary(St,Lc,":-",L,R),!,
  splitHead(L,Nm,A,C),
  pushScope(E,Env),
  typeOfTerms(A,AT,Env,E0,Lc,Args),
  checkCond(C,E0,E1,Cond),
  checkCond(R,E1,_,Body).
processStmt(St,predType(AT),[strong(Lc,Nm,Args,Cond,Body)|Defs],Defs,E,_) :-
  isBinary(St,Lc,":--",L,R),!,
  splitHead(L,Nm,A,C),
  pushScope(E,Env),
  typeOfTerms(A,AT,Env,E0,Lc,Args),
  checkCond(C,E0,E1,Cond),
  checkCond(R,E1,_,Body).
processStmt(St,predType(AT),[clause(Lc,Nm,Args,Cond,true(Lc))|Defs],Defs,E,_) :-
  splitHead(St,Nm,A,C),!,
  pushScope(E,Env),
  locOfAst(St,Lc),
  typeOfTerms(A,AT,Env,E0,Lc,Args),
  checkCond(C,E0,_,Cond).
processStmt(St,Tp,[Def|Defs],Defs,Env,_) :-
  isBinary(St,Lc,"=",L,R),!,
  checkDefn(Lc,L,R,Tp,Def,Env).
processStmt(St,Tp,[labelRule(Lc,Nm,Hd,Repl,SuperFace)|Defs],Defs,E,_) :-
  isBinary(St,Lc,"<=",L,R),
  checkClassHead(L,Tp,E,E1,Nm,Hd),!,
  typeOfTerm(R,SuperTp,E1,_,Repl),
  generateClassFace(SuperTp,E,SuperFace).
processStmt(St,Tp,[classBody(Lc,Nm,enum(Lc,Nm),Stmts,Others,Types)|Defs],Defs,E,Path) :-
  isBinary(St,Lc,"..",L,R),
  isIden(L,Nm),
  marker(class,Marker),
  subPath(Path,Marker,Nm,ClassPath),
  checkClassBody(Tp,R,E,Stmts,Others,Types,_,ClassPath).
processStmt(St,classType(AT,Tp),[classBody(Lc,Nm,Hd,Stmts,Others,Types)|Defs],Defs,E,Path) :-
  isBinary(St,Lc,"..",L,R),
  checkClassHead(L,classType(AT,Tp),E,E1,Nm,Hd),
  marker(class,Marker),
  subPath(Path,Marker,Nm,ClassPath),
  checkClassBody(Tp,R,E1,Stmts,Others,Types,_,ClassPath).
processStmt(St,Tp,Defs,Dx,E,Path) :-
  isBinary(St,Lc,"-->",L,R),
  processGrammarRule(Lc,L,R,Tp,Defs,Dx,E,Path).
processStmt(St,Tp,Defs,Defs,_,_) :-
  locOfAst(St,Lc),
  reportError("Statement %s not consistent with expected type %s",[St,Tp],Lc).

checkEquation(Lc,L,R,funType(AT,RT),[equation(Lc,Nm,Args,Cond,Exp)|Defs],Defs,E) :-
  splitHead(L,Nm,A,C),
  pushScope(E,Env),
  typeOfTerms(A,AT,Env,E0,Lc,Args),
  checkCond(C,E0,E1,Cond),
  typeOfTerm(R,RT,E1,_,Exp).
checkEquation(Lc,_,_,ProgramType,Defs,Defs,_) :-
  reportError("equation not consistent with expected type: %s",[ProgramType],Lc).

checkDefn(Lc,L,R,Tp,defn(Lc,Nm,Cond,Value),Env) :-
  splitHead(L,Nm,none,C),
  pushScope(Env,E),
  checkCond(C,E,E1,Cond),
  typeOfTerm(R,Tp,E1,_,Value).

checkClassHead(Term,_,Env,Env,Nm,enum(Lc,Nm)) :-
  isIden(Term,Lc,Nm),!.
checkClassHead(Term,classType(AT,_),Env,Ex,Nm,Ptn) :-
  splitHead(Term,Nm,A,C),!,
  locOfAst(Term,Lc),
  pushScope(Env,E0),
  typeOfTerms(A,AT,E0,E1,Lc,Args),
  checkCond(C,E1,Ex,Cond),
  Hd = apply(Lc,v(Lc,Nm),Args),
  (Cond=true(_), Ptn = Hd ; Ptn = where(Hd,Cond)),!.

checkClassBody(ClassTp,Body,Env,Defs,Others,Types,BodyDefs,ClassPath) :-
  isBraceTuple(Body,Lc,Els),
  getTypeFace(ClassTp,Env,Fields),
  pushScope(Env,Base),
  declareVar("this",vr("this",Lc,ClassTp),Base,ThEnv),
  thetaEnv(ClassPath,nullRepo,Lc,Els,Fields,ThEnv,_OEnv,Defs,Public,_Imports,Others),
  computeExport(Defs,Fields,Public,BodyDefs,Types,[],[]).

splitHead(tuple(_,"()",[A]),Nm,Args,Cond) :-!,
  splitHd(A,Nm,Args,Cond).
splitHead(Term,Nm,Args,Cond) :-
  splitHd(Term,Nm,Args,Cond).

splitHd(Term,Nm,Args,Cond) :-
  isBinary(Term,"::",L,Cond),!,
  splitHead(L,Nm,Args,_).
splitHd(Term,Nm,Args,name(Lc,"true")) :-
  isRound(Term,Nm,Args),
  locOfAst(Term,Lc).
splitHd(Id,Nm,none,name(Lc,"true")) :-
  isIden(Id,Lc,Nm),!.
splitHd(Term,"()",Args,name(Lc,"true")) :-
  locOfAst(Term,Lc),
  isTuple(Term,Args).

splitGrHead(tuple(_,"()",[A]),Nm,Args,Cond) :-!,
  splitGrHd(A,Nm,Args,Cond).
splitGrHead(Term,Nm,Args,Cond) :-
  splitGrHd(Term,Nm,Args,Cond).

splitGrHd(Term,Nm,Args,PB) :-
  isBinary(Term,",",L,tuple(_,"[]",PB)),!,
  splitHead(L,Nm,Args,_).
splitGrHd(Term,Nm,Args,[]) :-
  splitHead(Term,Nm,Args,name(_,"true")).

generalizeStmts([],_,_,Defs,Defs).
generalizeStmts([Eqn|Stmts],Env,Cx,[function(Lc,Nm,Tp,Cx,[Eqn|Eqns])|Defs],Dx) :-
  Eqn = equation(Lc,Nm,_,_,_),
  collectEquations(Stmts,S0,Nm,Eqns),
  pickupVarType(Nm,Lc,Env,Tp),
  generalizeStmts(S0,Env,Cx,Defs,Dx).
generalizeStmts([Cl|Stmts],Env,Cx,[predicate(Lc,Nm,Tp,Cx,[Cl|Clses])|Defs],Dx) :-
  (Cl = clause(Lc,Nm,_,_,_) ; Cl = strong(Lc,Nm,_,_,_)),!,
  collectClauses(Stmts,S0,Nm,Clses),
  pickupVarType(Nm,Lc,Env,Tp),
  generalizeStmts(S0,Env,Cx,Defs,Dx).
generalizeStmts([defn(Lc,Nm,Cond,Value)|Stmts],Env,Cx,[defn(Lc,Nm,Cx,Cond,Tp,Value)|Defs],Dx) :-
  pickupVarType(Nm,Lc,Env,Tp),!,
  generalizeStmts(Stmts,Env,Cx,Defs,Dx).
generalizeStmts([Cl|Stmts],Env,Cx,[enum(Lc,Nm,Tp,Cx,[Cl|Rules],Face)|Defs],Dx) :-
  isRuleForEnum(Cl,Lc,Nm),!,
  collectEnumRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  generateClassFace(Tp,Env,Face),
  generalizeStmts(S0,Env,Cx,Defs,Dx).
generalizeStmts([Cl|Stmts],Env,Cx,[class(Lc,Nm,Tp,Cx,[Cl|Rules],Face)|Defs],Dx) :-
  isRuleForClass(Cl,Lc,Nm),!,
  collectClassRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  generateClassFace(Tp,Env,Face),
  generalizeStmts(S0,Env,Cx,Defs,Dx).
generalizeStmts([Rl|Stmts],Env,Cx,[grammar(Lc,Nm,Tp,Cx,[Rl|Rules])|Defs],Dx) :-
  isGrammarRule(Rl,Lc,Nm),
  collectGrammarRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  generalizeStmts(S0,Env,Cx,Defs,Dx).

collectClauses([],[],_,[]).
collectClauses([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  Cl = clause(_,Nm,_,_,_),!,
  collectMoreClauses(Stmts,Sx,Nm,Ex).
collectClauses([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  Cl = strong(_,Nm,_,_,_),!,
  collectStrongClauses(Stmts,Sx,Nm,Ex).

collectMoreClauses([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  Cl = clause(_,Nm,_,_,_),!,
  collectMoreClauses(Stmts,Sx,Nm,Ex).
collectMoreClauses([Cl|Stmts],Sx,Nm,Clses) :-
  Cl = strong(Lc,Nm,_,_,_),!,
  reportError("not allowed to mix regular and strong clauses",[],Lc),
  collectMoreClauses(Stmts,Sx,Nm,Clses).
collectMoreClauses([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectMoreClauses(Stmts,Sx,Nm,Eqns).
collectMoreClauses([],[],_,[]).

collectStrongClauses([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  Cl = strong(_,Nm,_,_,_),!,
  collectStrongClauses(Stmts,Sx,Nm,Ex).
collectStrongClauses([Cl|Stmts],Sx,Nm,Clses) :-
  Cl = clause(Lc,Nm,_,_,_),!,
  reportError("not allowed to mix strong and regular clauses",[],Lc),
  collectStrongClauses(Stmts,Sx,Nm,Clses).
collectStrongClauses([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectMoreClauses(Stmts,Sx,Nm,Eqns).
collectStrongClauses([],[],_,[]).

collectEquations([Eqn|Stmts],Sx,Nm,[Eqn|Ex]) :-
  Eqn = equation(_,Nm,_,_,_),
  collectEquations(Stmts,Sx,Nm,Ex).
collectEquations([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectEquations(Stmts,Sx,Nm,Eqns).
collectEquations([],[],_,[]).

collectGrammarRules([Rl|Stmts],Sx,Nm,[Rl|Ex]) :-
  isGrammarRule(Rl,_,Nm),
  collectGrammarRules(Stmts,Sx,Nm,Ex).
collectGrammarRules([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectGrammarRules(Stmts,Sx,Nm,Eqns).
collectGrammarRules([],[],_,[]).

isGrammarRule(grammarRule(Lc,Nm,_,_,_),Lc,Nm).

collectClassRules([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  isRuleForClass(Cl,_,Nm),!,
  collectClassRules(Stmts,Sx,Nm,Ex).
collectClassRules([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectClassRules(Stmts,Sx,Nm,Eqns).
collectClassRules([],[],_,[]).

isRuleForClass(labelRule(Lc,Nm,_,_,_),Lc,Nm).
isRuleForClass(classBody(Lc,Nm,_,_,_,_),Lc,Nm).

collectEnumRules([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  isRuleForEnum(Cl,_,Nm),!,
  collectEnumRules(Stmts,Sx,Nm,Ex).
collectEnumRules([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectEnumRules(Stmts,Sx,Nm,Eqns).
collectEnumRules([],[],_,[]).

isRuleForEnum(labelRule(Lc,Nm,enum(_,_),_,_),Lc,Nm).
isRuleForEnum(classBody(Lc,Nm,enum(_,_),_,_,_),Lc,Nm).

implementationGroup([(imp(Nm),_,[Stmt])],[Impl|Defs],Defs,E,Env,Path) :-
  buildImplementation(Stmt,Nm,Impl,E,Path),
  declareImplementation(Nm,Impl,E,Env).

buildImplementation(Stmt,INm,implementation(Lc,INm,ImplName,ISpec,OCx,Cx,ThDefs,BodyDefs,Types,Others),Env,Path) :-
  isUnary(Stmt,Lc,"implementation",I),
  isBinary(I,"..",Sq,Body),
  isBraceTuple(Body,_,Els),
  parseContractConstraint(Sq,Env,Nm,Spec),
  getContract(Nm,Env,contract(_,CNm,ConTp,ConFace)),
  % We have to unify the implemented contract and the contract specification
  moveQuants(ConTp,_,Qcon),
  moveConstraints(Qcon,OC,conTract(ConNm,OArgs,ODeps)),
  moveQuants(Spec,SQ,ASpec),
  moveConstraints(ASpec,AC,conTract(ConNm,AArgs,ADeps)),
  sameLength(OArgs,AArgs,Lc),
  sameLength(ODeps,ADeps,Lc),
  % match up the type variables of the original contract with the actual implemented contract
  bindAT(OArgs,AArgs,[],AQ),
  bindAT(ODeps,ADeps,AQ,QQ),
  rewriteConstraints(OC,QQ,[],OCx),
  rewriteConstraints(AC,QQ,[],Cx), % Cx will become additional contract requirements
  moveQuants(ConFace,_,Face),
  rewriteType(Face,QQ,faceType(Fields)),
  pushScope(Env,ThEnv),
  thetaEnv(Path,nullRepo,Lc,Els,Fields,ThEnv,_,ThDefs,Public,_,Others),
  computeExport(ThDefs,Fields,Public,BodyDefs,Types,[],[]),
  moveQuants(ISpec,SQ,conTract(ConNm,AArgs,ADeps)),
  implementationName(conTract(CNm,AArgs,ADeps),ImplName).

bindAT([],_,Q,Q).
bindAT(_,[],Q,Q).
bindAT([kVar(N)|L1],[kVar(N)|L2],Q,Qx) :-
  bindAT(L1,L2,Q,Qx).
bindAT([kVar(V)|L],[Tp|TL],Q,Qx) :-
  bindAT(L,TL,[(V,Tp)|Q],Qx).

typeOfTerm(V,_,Env,Env,v(Lc,N)) :-
  isIden(V,Lc,"_"),!,
  genstr("_",N).
typeOfTerm(V,Tp,Env,Env,Term) :-
  isIden(V,Lc,N),
  isVar(N,Env,Spec),!,
  typeOfVar(Lc,N,Tp,Spec,Env,Term).
typeOfTerm(V,Tp,Ev,Env,v(Lc,N)) :-
  isIden(V,Lc,N),
  declareVar(N,vr(Lc,N,Tp),Ev,Env).
typeOfTerm(integer(Lc,Ix),Tp,Env,Env,intLit(Ix)) :- !,
  findType("integer",Lc,Env,IntTp),
  checkType(Lc,IntTp,Tp,Env).
typeOfTerm(float(Lc,Ix),Tp,Env,Env,floatLit(Ix)) :- !,
  findType("float",Lc,Env,FltTp),
  checkType(Lc,FltTp,Tp,Env).
typeOfTerm(string(Lc,Ix),Tp,Env,Env,stringLit(Lc,Ix)) :- !,
  findType("string",Lc,Env,StrTp),
  checkType(Lc,StrTp,Tp,Env).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isBinary(Term,Lc,":",L,R), !,
  parseType(R,Env,RT),
  checkType(Lc,RT,Tp,Env),
  typeOfTerm(L,RT,Env,Ev,Exp).
typeOfTerm(P,Tp,Env,Ex,where(Ptn,Cond)) :-
  isBinary(P,"::",L,R),
  typeOfTerm(L,Tp,Env,E0,Ptn),
  checkCond(R,E0,Ex,Cond).
typeOfTerm(Call,Tp,Env,Ev,where(V,Cond)) :-
  isUnary(Call,Lc,"@",Test), % @Test = NV :: NV.Test where NV is a new name
  isRoundTerm(Test,_,_,_),
  genstr("_",NV),
  typeOfTerm(name(Lc,NV),Tp,Env,E0,V),
  binary(Lc,".",name(Lc,NV),Test,TT),
  checkCond(TT,E0,Ev,Cond).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isBinary(Term,Lc,".",L,F), !,
  isIden(F,Fld),
  recordAccessExp(Lc,L,Fld,Tp,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Env,pkgRef(Lc,Pkg,Fld)) :-
  isBinary(Term,Lc,"#",L,F), !,
  isIden(F,FLc,Fld),
  packageName(L,Pkg,Version),
  isExported(Pkg,Version,Fld,ExTp),
  freshen(ExTp,voidType,_,FlTp), % replace with package type
  checkType(FLc,FlTp,Tp,Env).
typeOfTerm(Term,Tp,Env,Ev,conditional(Lc,Test,Then,Else)) :-
  isBinary(Term,Lc,"|",L,El),
  isBinary(L,"?",Tst,Th), !,
  checkCond(Tst,Env,E0,Test),
  typeOfTerm(Th,Tp,E0,E1,Then),
  typeOfTerm(El,Tp,E1,Ev,Else).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isSquareTuple(Term,Lc,Els), !,
  findType("list",Lc,Env,ListTp),
  ListTp = typeExp(_,[ElTp]),
  checkType(Lc,ListTp,Tp,Env),
  typeOfListTerm(Els,Lc,ElTp,ListTp,Env,Ev,Exp).
typeOfTerm(tuple(_,"()",[Inner]),Tp,Env,Ev,Exp) :-
  \+ isTuple(Inner,_), !,
  typeOfTerm(Inner,Tp,Env,Ev,Exp).
typeOfTerm(tuple(Lc,"()",A),Tp,Env,Ev,tuple(Lc,Els)) :-
  genTpVars(A,ArgTps),
  checkType(Lc,tupleType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els).
typeOfTerm(Term,Tp,Env,Ev,dict(Lc,Entries)) :-
  isBraceTuple(Term,Lc,Els),!,
  findType("map",Lc,Env,MapTp),
  MapTp = typeExp(_,[KyTp,ElTp]),
  checkType(Lc,MapTp,Tp,Env),
  typeMapEntries(Els,KyTp,ElTp,Env,Ev,Entries).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isUnary(Term,Lc,"-",Arg), % handle unary minus
  binary(Lc,"-",integer(Lc,0),Arg,Sub),
  typeOfTerm(Sub,Tp,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isRoundTerm(Term,Lc,F,A),
  newTypeVar("F",FnTp),
  typeOfKnown(F,FnTp,Env,E0,Fun),
  deRef(FnTp,FTp),
  typeOfCall(Lc,Fun,A,FTp,Tp,E0,Ev,Exp).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isSquareTerm(Term,Lc,F,[A]),
  binary(Lc,"find",F,A,Ix),
  typeOfTerm(Ix,Tp,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Env,void) :-
  locOfAst(Term,Lc),
  reportError("illegal expression: %s, expecting a %s",[Term,Tp],Lc).

typeOfCall(Lc,Fun,A,funType(ArgTps,FnTp),Tp,Env,Ev,apply(Lc,Fun,Args)) :-
  checkType(Lc,FnTp,Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Args).
typeOfCall(Lc,Fun,A,classType(ArgTps,FnTp),Tp,Env,Ev,apply(Lc,Fun,Args)) :-
  checkType(Lc,FnTp,Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Args). % small but critical difference

genTpVars([],[]).
genTpVars([_|I],[Tp|More]) :-
  newTypeVar("__",Tp),
  genTpVars(I,More).

recordAccessExp(Lc,Rc,Fld,ET,Env,Ev,dot(Lc,Rec,Fld)) :-
  typeOfTerm(Rc,AT,Env,Ev,Rec),
  getTypeFace(AT,Env,Face),
  fieldInFace(Face,AT,Fld,Lc,FTp),!,
  freshen(FTp,AT,_,Tp), % the record is this to the right of dot.
  checkType(Lc,Tp,ET,Env).

typeMapEntries([],_,_,Env,Env,[]).
typeMapEntries([En|Els],KyTp,ElTp,Env,Ev,[(Ky,Vl)|Entries]) :-
  isBinary(En,"->",Lhs,Rhs),
  typeOfTerm(Lhs,KyTp,Env,E0,Ky),
  typeOfTerm(Rhs,ElTp,E0,E1,Vl),
  typeMapEntries(Els,KyTp,ElTp,E1,Ev,Entries).
typeMapEntries([En|Els],KyTp,ElTp,Env,Ev,Entries) :-
  locOfAst(En,Lc),
  reportError("invalid entry '%s' in map",[En],Lc),
  typeMapEntries(Els,KyTp,ElTp,Env,Ev,Entries).

fieldInFace(Fields,_,Nm,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(_,Tp,Nm,Lc,anonType) :-
  reportError("field %s not declared in %s",[Nm,Tp],Lc).

typeOfVar(Lc,Nm,Tp,vr(_,_,VT),Env,Exp) :-
  pickupThisType(Env,ThisType),
  freshen(VT,ThisType,_,VrTp),
  manageConstraints(VrTp,[],v(Lc,Nm),MTp,Exp),
  checkType(Lc,MTp,Tp,Env).
typeOfVar(Lc,Nm,Tp,mtd(_,_,MTp),Env,Exp) :-
  pickupThisType(Env,ThisType),
  freshen(MTp,ThisType,_,VrTp),
  manageConstraints(VrTp,[],mtd(Lc,Nm),MtTp,Exp),
  checkType(Lc,MtTp,Tp,Env).

manageConstraints(constrained(Tp,Con),Cons,V,MTp,Exp) :- !,
  manageConstraints(Tp,[Con|Cons],V,MTp,Exp).
manageConstraints(Tp,[],V,Tp,V) :- !.
manageConstraints(Tp,Cons,V,Tp,over(V,Cons)).

typeOfKnown(T,Tp,Env,Env,Exp) :-
  isIden(T,Lc,Nm),
  isVar(Nm,Env,Spec),!,
  typeOfVar(Lc,Nm,Tp,Spec,Env,Exp).
typeOfKnown(T,Tp,Env,Env,v(Lc,Nm)) :-
  isIden(T,Lc,Nm),
  reportError("variable %s not declared, expecting a %s",[Nm,Tp],Lc).
typeOfKnown(T,Tp,Env,Ev,Exp) :-
  typeOfTerm(T,Tp,Env,Ev,Exp).

typeOfTerms([],[],Env,Env,_,[]).
typeOfTerms([],[T|_],Env,Env,Lc,[]) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfTerms([A|_],[],Env,Env,_,[]) :-
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfTerms([A|As],[ElTp|ElTypes],Env,Ev,_,[Term|Els]) :-
  typeOfTerm(A,ElTp,Env,E0,Term),
  locOfAst(A,Lc),
  typeOfTerms(As,ElTypes,E0,Ev,Lc,Els).

typeOfListTerm([],Lc,_,ListTp,Env,Ev,Exp) :-
  typeOfTerm(name(Lc,"[]"),ListTp,Env,Ev,Exp).
typeOfListTerm([Last],_,ElTp,ListTp,Env,Ev,apply(Lc,Op,[Hd,Tl])) :-
  isBinary(Last,Lc,",..",L,R),
  newTypeVar("_",LiTp),
  typeOfKnown(name(Lc,",.."),LiTp,Env,E0,Op),
  typeOfTerm(L,ElTp,E0,E1,Hd),
  typeOfTerm(R,ListTp,E1,Ev,Tl).
typeOfListTerm([El|More],_,ElTp,ListTp,Env,Ev,apply(Lc,Op,[Hd,Tl])) :-
  locOfAst(El,Lc),
  newTypeVar("_",LiTp),
  typeOfKnown(name(Lc,",.."),LiTp,Env,E0,Op),
  typeOfTerm(El,ElTp,E0,E1,Hd),
  typeOfListTerm(More,Lc,ElTp,ListTp,E1,Ev,Tl).

checkType(_,Actual,Expected,Env) :-
  sameType(Actual,Expected,Env).
checkType(Lc,S,T,_) :-
  reportError("%s not consistent with expected type %s",[S,T],Lc).

checkCond(Term,Env,Env,true(Lc)) :-
  isIden(Term,Lc,"true") ,!.
checkCond(Term,Env,Env,false(Lc)) :-
  isIden(Term,Lc,"false") ,!.
checkCond(Term,Env,Ex,conj(Lhs,Rhs)) :-
  isBinary(Term,",",L,R), !,
  checkCond(L,Env,E1,Lhs),
  checkCond(R,E1,Ex,Rhs).
checkCond(Term,Env,Ex,conditional(Lc,Test,Either,Or)) :-
  isBinary(Term,Lc,"|",L,R),
  isBinary(L,"?",T,Th),!,
  checkCond(T,Env,E0,Test),
  checkCond(Th,E0,E1,Either),
  checkCond(R,E1,Ex,Or).
checkCond(Term,Env,Ex,disj(Lc,Either,Or)) :-
  isBinary(Term,Lc,"|",L,R),!,
  checkCond(L,Env,E1,Either),
  checkCond(R,E1,Ex,Or).
checkCond(Term,Env,Ex,one(Lc,Test)) :-
  isUnary(Term,Lc,"!",N),!,
  checkCond(N,Env,Ex,Test).
checkCond(Term,Env,Env,neg(Lc,Test)) :-
  isUnary(Term,Lc,"\\+",N),!,
  checkCond(N,Env,_,Test).
checkCond(Term,Env,Env,forall(Lc,Gen,Test)) :-
  isBinary(Term,Lc,"*>",L,R),!,
  checkCond(L,Env,E0,Gen),
  checkCond(R,E0,_,Test).
checkCond(Term,Env,Ex,Cond) :-
  isTuple(Term,C),!,
  checkConds(C,Env,Ex,Cond).
checkCond(Term,Env,Ev,neg(Lc,unify(Lc,Lhs,Rhs))) :-
  isBinary(Term,Lc,"\\=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkCond(Term,Env,Ev,unify(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,"=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkCond(Term,Env,Ev,match(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,".=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkCond(Term,Env,Ev,match(Lc,Rhs,Lhs)) :-
  isBinary(Term,Lc,"=.",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(R,TV,Env,E0,Lhs),
  typeOfTerm(L,TV,E0,Ev,Rhs).
checkCond(Term,Env,Ev,phrase(Lc,NT,Strm,Rest)) :-
  isBinary(Term,Lc,"%%",L,R),
  isBinary(R,"~",S,M),!,
  newTypeVar("_S",StrmTp),
  newTypeVar("_E",ElTp),
  findContract("stream",Lc,Env,[StrmTp],[ElTp],StreamType),
  setConstraint(StrmTp,StreamType),
  typeOfTerm(S,StrmTp,Env,E0,Strm),
  typeOfTerm(M,StrmTp,E0,E1,Rest),
  checkNonTerminals(L,StrmTp,E1,Ev,NT).
checkCond(Term,Env,Ev,Goal) :-
  isBinary(Term,Lc,"%%",L,R),
  checkInvokeGrammar(Lc,L,R,Env,Ev,Goal).
checkCond(Term,Env,Ev,Call) :-
  isRoundTerm(Term,Lc,F,A),
  newTypeVar("_P",PrTp),
  typeOfKnown(F,PrTp,Env,E0,Pred),
  checkCondCall(Lc,Pred,A,PrTp,Call,E0,Ev).

checkCondCall(Lc,Pred,A,predType(ArgTps),Call,Env,Ev) :-
  checkCallArgs(Lc,Pred,A,ArgTps,Env,Ev,Call).
checkCondCall(Lc,Pred,_,Tp,true(Lc),Env,Env) :-
  reportError("type of %s:%s not a predicate",[Pred,Tp],Lc).

checkCallArgs(Lc,Pred,A,ArgTps,Env,Ev,call(Lc,Pred,Args)) :-
  typeOfTerms(A,ArgTps,_,Env,Ev,Lc,Args).
checkCallArgs(Lc,Pred,A,ArgTps,Env,Env,true(Lc)) :-
  reportError("arguments %s of %s not consistent with expected types %s",[A,Pred,tupleType(ArgTps)],Lc).

checkInvokeGrammar(Lc,L,R,Env,Ev,phrase(Lc,NT,Strm)) :-
  findType("stream",Lc,Env,StreamType),
  typeOfTerm(R,in(StreamType),StrType,Env,E1,Strm),
  StreamType = typeExp(_,[ElTp]),
  checkNonTerminals(L,inout(StrType),in(ElTp),E1,Ev,NT).

checkConds([C],Env,Ex,Cond) :-
  checkCond(C,Env,Ex,Cond).
checkConds([C|More],Env,Ex,conj(L,R)) :-
  checkCond(C,Env,E0,L),
  checkConds(More,E0,Ex,R).

processGrammarRule(Lc,L,R,grammarType(AT,Tp),[grammarRule(Lc,Nm,Args,PB,Body)|Defs],Defs,E,_) :-
  splitGrHead(L,Nm,A,P),
  pushScope(E,Env),
  newTypeVar("_S",StrmTp),
  newTypeVar("_E",ElTp),
  findContract("stream",Lc,Env,[StrmTp],[ElTp],StreamType),
  setConstraint(StrmTp,StreamType),
  checkType(Lc,Tp,StrmTp),
  typeOfTerms(A,AT,Env,E0,Lc,Args),!,
  checkNonTerminals(R,StrmTp,ElTp,E0,E1,Body),
  checkTerminals(P,PB,StrmTp,ElTp,E1,_).

checkNonTerminals(tuple(Lc,"[]",Els),_,ElTp,E,Env,terminals(Lc,Terms)) :- !,
  checkTerminals(Els,Terms,ElTp,E,Env).
checkNonTerminals(string(Lc,Text),_,ElTp,Env,Env,stringLit(Lc,Text)) :- !,
  findType("integer",Lc,Env,IntTp),     % strings are exploded into code points
  checkType(Lc,IntTp,ElTp).
checkNonTerminals(tuple(_,"()",[NT]),Tp,ElTp,Env,Ex,GrNT) :-
  checkNonTerminals(NT,Tp,ElTp,Env,Ex,GrNT).
checkNonTerminals(Term,Tp,ElTp,Env,Ex,conj(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,",",L,R), !,
  checkNonTerminals(L,Tp,ElTp,Env,E1,Lhs),
  checkNonTerminals(R,Tp,ElTp,E1,Ex,Rhs).
checkNonTerminals(Term,Tp,ElTp,Env,Ex,conditional(Lc,Test,Either,Or)) :-
  isBinary(Term,Lc,"|",L,R),
  isBinary(L,"?",T,Th),!,
  checkNonTerminals(T,Tp,ElTp,Env,E0,Test),
  checkNonTerminals(Th,Tp,ElTp,E0,E1,Either),
  checkNonTerminals(R,Tp,ElTp,E1,Ex,Or).
checkNonTerminals(Term,Tp,ElTp,Env,Ex,disj(Lc,Either,Or)) :-
  isBinary(Term,Lc,"|",L,R),!,
  checkNonTerminals(L,Tp,ElTp,Env,E1,Either),
  checkNonTerminals(R,Tp,ElTp,E1,Ex,Or).
checkNonTerminals(Term,Tp,ElTp,Env,Ex,guard(Lc,NonTerm,Cond)) :-
  isBinary(Term,Lc,"::",L,R),!,
  checkNonTerminals(L,Tp,ElTp,Env,E1,NonTerm),
  checkCond(R,E1,Ex,Cond).
checkNonTerminals(Term,Tp,ElTp,Env,Ex,one(Lc,Test)) :-
  isUnary(Term,Lc,"!",N),!,
  checkNonTerminals(N,Tp,ElTp,Env,Ex,Test).
checkNonTerminals(Term,Tp,ElTp,Env,Env,neg(Lc,Test)) :-
  isUnary(Term,Lc,"\\+",N),!,
  checkNonTerminals(N,Tp,ElTp,Env,_,Test).
checkNonTerminals(Term,Tp,ElTp,Env,Env,ahead(Lc,Test)) :-
  isUnary(Term,Lc,"+",N),!,
  checkNonTerminals(N,Tp,ElTp,Env,_,Test).
checkNonTerminals(Term,_,_,Env,Ev,goal(Lc,unify(Lc,Lhs,Rhs))) :-
  isBinary(Term,Lc,"=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkNonTerminals(Term,_,_,Env,Ev,goal(Lc,neg(Lc,unify(Lc,Lhs,Rhs)))) :-
  isBinary(Term,Lc,"\\=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkNonTerminals(Term,_,_,Env,Ev,goal(Lc,match(Lc,Lhs,Rhs))) :-
  isBinary(Term,Lc,".=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkNonTerminals(Term,_,_,Env,Ev,goal(Lc,match(Lc,Rhs,Lhs))) :-
  isBinary(Term,Lc,"=.",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkNonTerminals(Term,Tp,_,Env,Ev,dip(Lc,v(Lc,NV),Cond)) :-
  isUnary(Term,Lc,"@",Test),
  isRoundTerm(Test,Op,Args),
  genstr("_",NV),
  declareVar(NV,vr(NV,Lc,Tp),Env,E0),
  binary(Lc,".",name(Lc,NV),Op,NOp),
  checkCond(app(Lc,NOp,tuple(Lc,"()",Args)),E0,Ev,Cond).
checkNonTerminals(Term,Tp,_,Env,Ev,NT) :-
  isRoundTerm(Term,Lc,F,A),
  newTypeVar("_G",GrTp),
  typeOfKnown(F,GrTp,Env,E0,Pred),
  ( deRef(GrTp,grammarType(ArgTps,_)), % must be done out of call to typeOfKnown
    typeOfTerms(A,ArgTps,E0,Ev,Lc,Args),
    NT = call(Lc,Pred,Args) ,!;
    reportError("type of %s:%s not consistent with expected stream type: %s",[F,GrTp,Tp],Lc),
    NT = terminals(Lc,[]),
    Env=Ev).
checkNonTerminals(Term,_,_,Env,Env,eof(Lc)) :-
  isIden(Term,Lc,"eof").
checkNonTerminals(Term,_,_,Env,Ex,goal(Lc,Cond)) :-
  isBraceTuple(Term,Lc,Els),
  checkConds(Els,Env,Ex,Cond).

checkTerminals([],[],_,Env,Env) :- !.
checkTerminals([T|More],[TT|Out],ElTp,Env,Ex) :-
  typeOfTerm(T,ElTp,Env,E0,TT),
  checkTerminals(More,Out,ElTp,E0,Ex).

computeExport([],_,_,[],[],[],[]).
computeExport([Def|Defs],Fields,Public,Exports,Types,Contracts,Impls) :-
  exportDef(Def,Fields,Public,Exports,Ex,Types,Tx,Contracts,Cx,Impls,Ix),!,
  computeExport(Defs,Fields,Public,Ex,Tx,Cx,Ix).

exportDef(function(_,Nm,Tp,_,_),Fields,Public,[var(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(predicate(_,Nm,Tp,_,_),Fields,Public,[var(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(class(_,Nm,Tp,_,_,_),Fields,Public,[var(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(enum(_,Nm,Tp,_,_,_),Fields,Public,[var(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(typeDef(_,Nm,Tp,_),_,Public,Exports,Exports,[tpe(Nm,Tp)|Tx],Tx,Cons,Cons,Impl,Impl) :-
  isPublicType(Nm,Public).
exportDef(defn(_,Nm,_,Tp,_,_),Fields,Public,[var(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(grammar(_,Nm,Tp,_,_),Fields,Public,[var(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(Con,_,Public,Ex,Ex,Types,Types,[Con|Cons],Cons,Impl,Impl) :-
  isPublicContract(Con,Public).
exportDef(Impl,_,Public,Ex,Ex,Tps,Tps,Cons,Cons,[Impl|Ix],Ix) :-
  isPublicImplementation(Impl,Public).
exportDef(_,_,_,Ex,Ex,Tps,Tps,Cons,Cons,Impls,Impls).

isPublicVar(Nm,_,Public) :-
  is_member(var(Nm),Public),!.
isPublicVar(Nm,Fields,_) :-
  is_member((Nm,_),Fields),!.

isPublicType(Nm,Public) :-
  is_member(tpe(Nm),Public),!.

isPublicContract(contract(Nm,_,_,_),Public) :-
  is_member(con(Nm),Public),!.

isPublicImplementation(implementation(_,ConNm,_,_,_,_,_,_,_,_),Public) :-
  is_member(imp(ConNm),Public),!.

isPublicImplementation(Nm,Con,Public,[impl(Nm,Con)|Exx],Exx) :-
  is_member(imp(Nm),Public),!.
isPublicImplementation(_,_,_,Ex,Ex).

mergeFields([],_,_,Fields,Fields,_,_).
mergeFields([Rule|More],Q,Plate,SoFar,Fields,Env,Lc) :-
  mergeFromTypeRule(Rule,Plate,SoFar,Flds,Env,Lc),
  mergeFields(More,Q,Plate,Flds,Fields,Env,Lc).

mergeFromTypeRule(Rule,Plate,SoFar,Fields,Env,Lc) :-
  moveQuants(Rule,_,typeRule(Lhs,Rhs)),
  matchTypes(Plate,Lhs,Binding),
  freshn(Rhs,Binding,FRhs),
  collectFace(FRhs,Env,SoFar,Fields,Lc).

matchTypes(type(Nm),type(Nm),[]) :-!.
matchTypes(typeExp(Nm,L),typeExp(Nm,R),Binding) :-
  matchArgTypes(L,R,Binding).

matchArgTypes([],[],[]).
matchArgTypes([kVar(Nm)|L],[kVar(Nm)|R],Binding) :- !,
  matchArgTypes(L,R,Binding).
matchArgTypes([Tp|L],[kVar(Ot)|R],[(Ot,Tp)|Binding]) :-
  matchArgTypes(L,R,Binding).

collectFace(faceType(Fs),_,SoFar,Flds,Lc) :-
  collectFields(Fs,SoFar,Flds,Lc).
collectFace(type(Nm),Env,SoFar,Fields,Lc) :-
  typeFaceRule(Nm,Env,FaceRule),
  mergeFields([FaceRule],[],type(Nm),SoFar,Fields,Env,Lc). % some laziness here: how do we know no quants?
collectFace(typeExp(Nm,_),Env,SoFar,Fields,Lc) :-
  typeFaceRule(Nm,Env,FaceRule),
  mergeFields([FaceRule],[],type(Nm),SoFar,Fields,Env,Lc). % some laziness here: how do we know no quants?

collectFields([],Flds,Flds,_).
collectFields([(Nm,_)|More],SoFar,Flds,Lc) :-
  is_member((Nm,_),SoFar),!,
  reportError("multiple declaration of field %s",[Nm],Lc),
  collectFields(More,SoFar,Flds,Lc).
collectFields([(Nm,Tp)|More],SoFar,Flds,Lc) :-
  collectFields(More,[(Nm,Tp)|SoFar],Flds,Lc).

pickTypeTemplate([Rl|_],Type) :-
  deRule(Rl,Type).

pickFaceRule(Rules,Rl) :-
  is_member(Rl,Rules),
  isFaceRule(Rl),!.

deRule(univType(B,Tp),univType(B,XTp)) :-
  deRule(Tp,XTp).
deRule(typeRule(Lhs,_),Lhs).

generateClassFace(Tp,Env,Face) :-
  freshen(Tp,voidType,Q,Plate),
  (Plate = classType(_,T); T=Plate),
  getTypeFace(T,Env,F),
  freezeType(faceType(F),Q,Face),!.
