:- module(checker,[checkProgram/4]).

:- use_module(abstract).
:- use_module(dependencies).
:- use_module(freshen).
:- use_module(subsume).
:- use_module(types).
:- use_module(parsetype).
:- use_module(dict).
:- use_module(misc).
:- use_module(canon).
:- use_module(errors).
:- use_module(keywords).
:- use_module(macro).
:- use_module(catalog).
:- use_module(import).

:- use_module(display).

checkProgram(Prog,Cat,Opts,prog(Pkg,Imports,Defs,Others,Fields,Types)) :-
  stdDict(Base),
  isBraceTerm(Prog,Pk,Els),
  packageName(Pk,Pkg),
  pushScope(Base,TEnv),
  declareCatalog(Pkg,Cat,TEnv,Env),
  thetaEnv(Pkg,Opts,Els,[],Env,_,Defs,Private,Imports,Others),
  computeExport(Defs,Private,Fields,Types),!.

packageName(T,Pkg) :- isIden(T,Pkg).
packageName(T,Pkg) :- isString(T,Pkg).
packageName(T,Pkg) :- isBinary(T,".",L,R), 
  packageName(L,LP),
  packageName(R,RP),
  string_concat(LP,".",I),
  string_concat(I,RP,Pkg).

thetaEnv(Pkg,Opts,Els,Fields,Base,TheEnv,Defs,Private,Imports,Others) :-
  macroRewrite(Els,Stmts),
  dependencies(Stmts,Groups,Private,Annots,Imps,Otrs),
  checkImports(Imps,Imports,Pkg,Opts,Base,IBase),
  pushFace(Fields,IBase,Env),
  checkGroups(Groups,Fields,Annots,Defs,Env,TheEnv,Pkg),
  checkOthers(Otrs,Others,TheEnv,Pkg).

checkImports([],[],_,_,Env,Env) :- !.
checkImports([St|Stmts],Imports,Pkg,Opts,Env,Ex) :-
  checkImport(St,private,Imports,IM,Pkg,Opts,Env,E0),
  checkImports(Stmts,IM,Pkg,Opts,E0,Ex).

checkImport(St,_,Imports,More,Pkg,Opts,Env,Ex) :-
  isUnary(St,"private",I),
  checkImport(I,private,Imports,More,Pkg,Opts,Env,Ex).
checkImport(St,_,Imports,More,Pkg,Opts,Env,Ex) :-
  isUnary(St,"public",I),
  checkImport(I,public,Imports,More,Pkg,Opts,Env,Ex).
checkImport(St,Viz,[import(Lc,PkgName,Viz,PkgSpec)|More],More,Pkg,Opts,Env,Ex) :-
  isUnary(St,Lc,"import",P),
  packageName(P,PkgName),
  getCatalog(Pkg,Env,Cat),
  (locatePackage(PkgName,Cat,Opts,PkgSpec),
   importDefs(PkgSpec,Lc,Env,Ex) ;
   reportError("cannot locate package %s",[PkgName],Lc),
   Env=Ex).

% For now, we stub these out.
locatePackage(Pkg,Cat,Opts,PkgSpec) :-
  resolveCatalog(Cat,Pkg,Uri),
  catalogBase(Cat,Base),
  importPkg(Pkg,Uri,Base,Opts,PkgSpec).
importDefs(spec(_,faceType(Exported),faceType(Types)),Lc,Env,Ex) :-
  declareFields(Exported,Lc,Env,E0),
  importTypes(Types,Lc,E0,Ex).

importTypes([],_,Env,Env).
importTypes([(Nm,tupleType(Rules))|More],Lc,Env,Ex) :-
  pickTypeTemplate(Rules,Type),
  pickFaceRule(Rules,FaceRule),
  declareType(Nm,Lc,Type,FaceRule,Rules,Env,E0),
  importTypes(More,Lc,E0,Ex).

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

checkGroups([],_,_,[],E,E,_).
checkGroups([Gp|More],Fields,Annots,Defs,Env,E,Path) :-
  groupType(Gp,GrpType),
  checkGroup(Gp,GrpType,Fields,Annots,Defs,D0,Env,E0,Path),
  checkGroups(More,Fields,Annots,D0,E0,E,Path).

displayGroup([]).
displayGroup([(T,_,Stmts)|More]) :-
  writef("Group %w: ",[T]),
  displayAll(Stmts),
  displayGroup(More).

groupType([(var(_),_,_)|_],var).
groupType([(tpe(_),_,_)|_],tpe).

checkGroup(Grp,tpe,_,_,Defs,Dx,Env,Ex,Path) :-
  typeGroup(Grp,Defs,Dx,Env,Ex,Path).
checkGroup(Grp,var,Fields,Annots,Defs,Dx,Env,Ex,Path) :-
  varGroup(Grp,Fields,Annots,Defs,Dx,Env,Ex,Path).

% This is very elaborate - to support mutual recursion amoung types.
typeGroup(Grp,Defs,Dx,Env,Ex,Path) :-
  defineTypes(Grp,Env,TmpEnv,Path),
  parseTypeDefs(Grp,TpDefs,[],TmpEnv,Path),
  declareTypes(TpDefs,TpDefs,Defs,Dx,Env,Ex).

defineTypes([],Env,Env,_).
defineTypes([(tpe(N),Lc,Stmts)|More],Env,Ex,Path) :-
  defineType(N,Lc,Stmts,Env,E0,Path),
  defineTypes(More,E0,Ex,Path).

defineType(N,Lc,_,Env,Env,_) :-
  typeInDict(N,Env,OLc,_),!,
  reportError("type %s already defined at %s",[N,OLc],Lc).
defineType(N,Lc,[St|_],Env,Ex,Path) :- 
  parseTypeTemplate(St,[],Env,Type,Path),
  declareType(N,Lc,Type,Env,Ex).

parseTypeDefs([],Defs,Defs,_,_).
parseTypeDefs([(tpe(N),Lc,Stmts)|More],Defs,Dx,TmpEnv,Path) :-
  parseTypeDefinition(N,Lc,Stmts,Defs,D0,TmpEnv,Path),
  parseTypeDefs(More,D0,Dx,TmpEnv,Path).

parseTypeDefinition(N,Lc,Stmts,[typeDef(Lc,N,Type,Rules)|Defs],Defs,TmpEnv,Path) :- 
  parseTypeDef(Stmts,TmpEnv,Rules,[],Path),
  pickTypeTemplate(Rules,Type).

parseTypeDef([],_,TpDefs,TpDefs,_).
parseTypeDef([St|More],Env,[Rl|Defs],Dx,Path) :-
  parseTypeRule(St,Env,Rl,Path),
  parseTypeDef(More,Env,Defs,Dx,Path).

declareTypes([],_,Defs,Defs,Env,Env).
declareTypes([typeDef(Lc,N,Type,Rules)|More],TpDefs,[typeDef(Lc,N,Type,NRules)|Defs],Dx,Env,Ex) :-
  faceOfType(Lc,TpDefs,Env,Rules,Type,Face),
  replaceFaceRule(Rules,Face,NRules),
  declareType(N,Lc,Type,Face,NRules,Env,E0),
  declareTypes(More,TpDefs,Defs,Dx,E0,Ex).

faceOfType(Lc,TpDefs,Env,Rules,Tmplate,Face) :-
  moveQuants(Tmplate,Q,Plate),
  findAllFields(Rules,Q,Plate,[],Fields,TpDefs,Env,Lc),
  moveQuants(Face,Q,typeRule(Plate,faceType(Fields))).

replaceFaceRule([],Face,[Face]).
replaceFaceRule([Rl|Rules],Face,[Face|Rules]) :-
  isFaceRule(Rl),!.
replaceFaceRule([Rl|Rules],Face,[Rl|NRules]) :-
  replaceFaceRule(Rules,Face,NRules).

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

varGroup(Grp,Fields,Annots,Defs,Dx,Base,Env,Path) :-
  parseAnnotations(Grp,Fields,Annots,Base,Env,Path),
  checkVarRules(Grp,D0,Env,Path),
  generalizeStmts(D0,Env,Defs,Dx).

parseAnnotations([],_,_,Env,Env,_) :-!.
parseAnnotations([(var(N),_,_)|More],Fields,Annots,Env,Ex,Path) :-
  is_member((N,Annot),Annots),!,
  annotateVar(N,Annot,Env,E0),
  parseAnnotations(More,Fields,Annots,E0,Ex,Path).
parseAnnotations([(var(N),Lc,_)|More],Fields,Annots,Env,Ex,Path) :-
  is_member((N,Tp),Fields),!,
  declareVar(N,Lc,vr(N,Tp),Env,E0),
  parseAnnotations(More,Fields,Annots,E0,Ex,Path).
parseAnnotations([(var(N),Lc,_)|More],Fields,Annots,Env,Ex,Path) :-
  reportError("no type annotation for variable %s",[N],Lc),
  parseAnnotations(More,Fields,Annots,Env,Ex,Path).

annotateVar(Nm,St,Env,Ex) :-
  isBinary(St,Lc,":",_,T),
  parseType(T,Env,Tp),
  declareVar(Nm,Lc,vr(Nm,Tp),Env,Ex).

checkVarRules([],[],_,_).
checkVarRules([(var(N),Lc,Stmts)|More],Defs,Env,Path) :-
  pickupVarType(N,Lc,Env,Tp),
  pickupThisType(Env,ThisType),
  freshen(Tp,ThisType,Q,ProgramType),
  declareTypeVars(Q,Lc,Env,StmtEnv),
  processStmts(Stmts,ProgramType,Defs,D0,StmtEnv,Path),
  checkEvidenceBinding(Lc,Q),
  checkVarRules(More,D0,Env,Path).

pickupVarType(N,_,Env,Tp) :-
  isVar(N,Env,vr(_,Tp)),!.
pickupVarType(N,Lc,_,_) :- reportError("%s not declared",[N],Lc).

pickupThisType(Env,Tp) :-
  isVar("this",Env,vr(_,Tp)),!.
pickupThisType(_,voidType).

checkEvidenceBinding(_,_).

declareTypeVars([],_,Env,Env).
declareTypeVars([(thisType,_)|Vars],Lc,Env,Ex) :- !,
  declareTypeVars(Vars,Lc,Env,Ex).
declareTypeVars([(Nm,Tp)|Vars],Lc,Env,Ex) :-
  declareType(Nm,Lc,Tp,Env,E0),
  declareTypeVars(Vars,Lc,E0,Ex).

findType(Nm,Env,Tp) :-
  typeInDict(Nm,Env,_,T),
  pickupThisType(Env,ThisType),
  freshen(T,ThisType,_,Tp).

processStmts([],_,Defs,Defs,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Env,Path) :-
  processStmt(St,ProgramType,Defs,D0,Env,Path),!,
  processStmts(More,ProgramType,D0,Dx,Env,Path).

processStmt(St,funType(AT,RT),[equation(Lc,Nm,Args,Cond,Exp)|Defs],Defs,E,_) :- 
  isBinary(St,Lc,"=>",L,R),!,
  splitHead(L,Nm,A,C),
  pushScope(E,Env),
  typeOfPtns(A,AT,Lc,Env,E0,Args),
  checkCond(C,E0,E1,Cond),
  typeOfExp(R,RT,_,E1,Exp).
processStmt(St,predType(AT),[clause(Lc,Nm,Args,Cond,Body)|Defs],Defs,E,_) :- 
  isBinary(St,Lc,":-",L,R),!,
  splitHead(L,Nm,A,C),
  pushScope(E,Env),
  typeOfPtns(A,AT,Lc,Env,E0,Args),
  checkCond(C,E0,E1,Cond),
  checkCond(R,E1,_,Body).
processStmt(St,predType(AT),[strong(Lc,Nm,Args,Cond,Body)|Defs],Defs,E,_) :- 
  isBinary(St,Lc,":--",L,R),!,
  splitHead(L,Nm,A,C),
  pushScope(E,Env),
  typeOfPtns(A,AT,Lc,Env,E0,Args),
  checkCond(C,E0,E1,Cond),
  checkCond(R,E1,_,Body).
processStmt(St,predType(AT),[clause(Lc,Nm,Args,Cond,true(Lc))|Defs],Defs,E,_) :- 
  splitHead(St,Nm,A,C),!,
  pushScope(E,Env),
  locOfAst(St,Lc),
  typeOfPtns(A,AT,Lc,Env,E0,Args),
  checkCond(C,E0,_,Cond).
processStmt(St,Tp,[Def|Defs],Defs,Env,_) :-
  isBinary(St,Lc,"=",L,R),!,
  checkDefn(Lc,L,R,Tp,Def,Env).
processStmt(St,Tp,[labelRule(Lc,Nm,Hd,Repl,SuperFace)|Defs],Defs,E,_) :- 
  isBinary(St,Lc,"<=",L,R),
  checkClassHead(L,Tp,E,E1,Nm,Hd),!,
  typeOfExp(R,topType,SuperTp,E1,Repl),
  generateClassFace(SuperTp,E,SuperFace).
processStmt(St,Tp,[overrideRule(Lc,Nm,Hd,Repl,SuperFace)|Defs],Defs,E,_) :- 
  isBinary(St,Lc,"<<",L,R),
  checkClassHead(L,Tp,E,E1,Nm,Hd),!,
  typeOfExp(R,topType,SuperTp,E1,Repl),
  generateClassFace(SuperTp,E,SuperFace).
processStmt(St,Tp,[classBody(Lc,Nm,enum(Lc,Nm),Stmts,Others,Types)|Defs],Defs,E,Path) :-
  isBinary(St,Lc,"..",L,R),
  isName(L,Nm),!,
  pushScope(E,Env),
  marker(class,Marker),
  subPath(Path,Marker,Nm,ClassPath),
  checkClassBody(Tp,R,Env,Stmts,Others,Types,_,ClassPath).
processStmt(St,classType(AT,Tp),[classBody(Lc,Nm,Hd,Stmts,Others,Types)|Defs],Defs,E,Path) :-
  isBinary(St,Lc,"..",L,R),
  checkClassHead(L,classType(AT,Tp),E,E1,Nm,Hd),
  marker(class,Marker),
  subPath(Path,Marker,Nm,ClassPath),
  checkClassBody(Tp,R,E1,Stmts,Others,Types,_,ClassPath).
processStmt(St,Tp,Defs,Dx,E,Path) :-
  isBinary(St,"-->",_,_),
  processGrammarRule(St,Tp,Defs,Dx,E,Path).

checkDefn(Lc,L,R,Tp,defn(Lc,Nm,Cond,Value),Env) :-
  splitHead(L,Nm,none,C),
  pushScope(Env,E),
  checkCond(C,E,E1,Cond),
  typeOfExp(R,Tp,_,E1,Value).

checkClassHead(Term,_,Env,Env,Nm,enum(Lc,Nm)) :-
  isIden(Term,Lc,Nm),!.
checkClassHead(Term,classType(AT,_),Env,Ex,Nm,Ptn) :-
  splitHead(Term,Nm,A,C),!,
  locOfAst(Term,Lc),
  pushScope(Env,E0),
  typeOfPtns(A,AT,Lc,E0,E1,Args),
  checkCond(C,E1,Ex,Cond),
  Hd = apply(Lc,v(Lc,Nm),Args),
  (Cond=true(_), Ptn = Hd ; Ptn = where(Hd,Cond)),!.

checkClassBody(ClassTp,Body,Env,Defs,Others,Types,BodyDefs,ClassPath) :-
  isBraceTuple(Body,Lc,Els),
  getTypeFace(ClassTp,Env,Fields),
  pushScope(Env,Base),
  declareVar("this",Lc,vr("this",ClassTp),Base,ThEnv),
  thetaEnv(ClassPath,[],Els,Fields,ThEnv,_OEnv,Defs,Private,_Imports,Others),
  computeExport(Defs,Private,BodyDefs,Types).

declareFields([],_,Env,Env).
declareFields([(Nm,Tp)|More],Lc,Env,Ex) :-
  declareVar(Nm,Lc,vr(Nm,Tp),Env,E0),
  declareFields(More,Lc,E0,Ex).

splitHead(Term,Nm,Args,Cond) :-
  isBinary(Term,"::",L,Cond),!,
  splitHead(L,Nm,Args,_).
splitHead(Term,Nm,Args,name(Lc,"true")) :-
  isRound(Term,Nm,Args),
  locOfAst(Term,Lc),
  \+ is_member(Nm,["=>",":-",":--","<=","..","=",":="]).
splitHead(Id,Nm,none,name(Lc,"true")) :-
  isIden(Id,Lc,Nm),!.
splitHead(Term,"()",Args,name(Lc,"true")) :-
  locOfAst(Term,Lc),
  isTuple(Term,Args).

splitGrHead(Term,Nm,Args,PB) :-
  isBinary(Term,",",L,tuple(_,"[]",PB)),!,
  splitHead(L,Nm,Args,_).
splitGrHead(Term,Nm,Args,[]) :-
  splitHead(Term,Nm,Args,name(_,"true")).

generalizeStmts([],_,Defs,Defs).
generalizeStmts([Eqn|Stmts],Env,[function(Lc,Nm,Tp,[Eqn|Eqns])|Defs],Dx) :-
  Eqn = equation(Lc,Nm,_,_,_),
  collectEquations(Stmts,S0,Nm,Eqns),
  pickupVarType(Nm,Lc,Env,Tp),
  generalizeStmts(S0,Env,Defs,Dx).
generalizeStmts([Cl|Stmts],Env,[predicate(Lc,Nm,Tp,[Cl|Clses])|Defs],Dx) :-
  (Cl = clause(Lc,Nm,_,_,_) ; Cl = strong(Lc,Nm,_,_,_)),!,
  collectClauses(Stmts,S0,Nm,Clses),
  pickupVarType(Nm,Lc,Env,Tp),
  generalizeStmts(S0,Env,Defs,Dx).
generalizeStmts([defn(Lc,Nm,Cond,Value)|Stmts],Env,[defn(Lc,Nm,Cond,Tp,Value)|Defs],Dx) :-
  pickupVarType(Nm,_,Env,Tp),!,
  generalizeStmts(Stmts,Env,Defs,Dx).
generalizeStmts([Cl|Stmts],Env,[enum(Lc,Nm,Tp,[Cl|Rules],Face)|Defs],Dx) :-
  isRuleForEnum(Cl,Lc,Nm),!,
  collectEnumRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  generateClassFace(Tp,Env,Face),
  generalizeStmts(S0,Env,Defs,Dx).
generalizeStmts([Cl|Stmts],Env,[class(Lc,Nm,Tp,[Cl|Rules],Face)|Defs],Dx) :-
  isRuleForClass(Cl,Lc,Nm),!,
  collectClassRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  generateClassFace(Tp,Env,Face),
  generalizeStmts(S0,Env,Defs,Dx).
generalizeStmts([Rl|Stmts],Env,[grammar(Lc,Nm,Tp,[Rl|Rules])|Defs],Dx) :-
  isGrammarRule(Rl,Lc,Nm),
  collectGrammarRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  generalizeStmts(S0,Env,Defs,Dx).

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
  reportError("not allowed to mix strong and regular clauses",[],Lc),
  collectMoreClauses(Stmts,Sx,Nm,Clses).
collectMoreClauses([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectMoreClauses(Stmts,Sx,Nm,Eqns).
collectMoreClauses([],[],_,[]).

collectStrongClauses([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  Cl = strong(_,Nm,_,_,_),!,
  collectMoreClauses(Stmts,Sx,Nm,Ex).
collectStrongClauses([Cl|Stmts],Sx,Nm,Clses) :-
  Cl = clause(Lc,Nm,_,_,_),!,
  reportError("not allowed to mix strong and regular clauses",[],Lc),
  collectMoreClauses(Stmts,Sx,Nm,Clses).
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
isRuleForClass(overrideRule(Lc,Nm,_,_,_),Lc,Nm).
isRuleForClass(classBody(Lc,Nm,_,_,_,_),Lc,Nm).

collectEnumRules([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  isRuleForEnum(Cl,_,Nm),!,
  collectEnumRules(Stmts,Sx,Nm,Ex).
collectEnumRules([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectEnumRules(Stmts,Sx,Nm,Eqns).
collectEnumRules([],[],_,[]).

isRuleForEnum(labelRule(Lc,Nm,enum(_,_),_,_),Lc,Nm).
isRuleForEnum(overrideRule(Lc,Nm,enum(_,_),_,_),Lc,Nm).
isRuleForEnum(classBody(Lc,Nm,enum(_,_),_,_,_),Lc,Nm).

typeOfPtn(N,Tp,Env,Ex,Term) :- 
  isIden(N,Lc,Nm),
  typeOfVarPtn(Lc,Nm,Tp,Env,Ex,Term).
typeOfPtn(integer(_,Ix),Tp,Env,Env,intLit(Ix)) :-
  findType("integer",Env,IntegerTp),
  sameType(Tp,IntegerTp,Env).
typeOfPtn(float(_,Ix),Tp,Env,Env,floatLit(Ix)) :- !,
  findType("float",Env,FloatTp),
  sameType(Tp,FloatTp,Env).
typeOfPtn(string(Lc,Sx),Tp,Env,Env,stringLit(Lc,Sx)) :- !,
  findType("string",Env,StringTp),
  sameType(Tp,StringTp,Env).
typeOfPtn(P,Tp,Env,Ex,where(Ptn,Cond)) :-
  isBinary(P,"::",L,R),
  typeOfPtn(L,Tp,Env,E0,Ptn),
  checkCond(R,E0,Ex,Cond).
typeOfPtn(Term,ET,Env,Env,pkgRef(Lc,Pkg,Fld)) :-
  isBinary(Term,Lc,"#",L,F), !,
  isIden(F,FLc,Fld),
  packageName(L,Pkg),
  isExported(Pkg,Fld,ExTp),
  freshen(ExTp,voidType,_,Tp), % replace with package type
  sameType(FLc,Tp,ET,Env).
typeOfPtn(tuple(Lc,"()",[A]),Tp,Env,Ex,tuple(Lc,Args)) :-
  genTpVars(A,ArgTps),
  sameType(Tp,tupleType(ArgTps),Env),
  typeOfPtns(A,ArgTps,Lc,Env,Ex,Args).
typeOfPtn(tuple(Lc,"()",A),Tp,Env,Ex,tuple(Lc,Args)) :-
  genTpVars(A,ArgTps),
  sameType(Tp,tupleType(ArgTps),Env),
  typeOfPtns(A,ArgTps,Lc,Env,Ex,Args).
typeOfPtn(tuple(Lc,"[]",A),Tp,Env,Ex,Term) :-
  findType("list",Env,ListTp),
  ListTp = typeExp(_,[ElTp]),
  sameType(ListTp,Tp,Env),
  typeOfListPtn(A,Lc,ElTp,ListTp,Env,Ex,Term).
typeOfPtn(Call,Tp,Env,Ex,where(V,Cond)) :-
  isUnary(Call,Lc,"@",Test), % @Test = NV :: NV.Test where NV is a new name
  isRoundTerm(Test,_,_,_),
  gensym("_",NV),
  typeOfVarPtn(Lc,NV,Tp,Env,E0,V),
  V = v(Lc,Tp),
  binary(Lc,".",name(Lc,NV),Test,TT),
  checkCond(TT,E0,Ex,Cond).
typeOfPtn(Term,Tp,Env,Ex,apply(Lc,Op,Args)) :-
  isRoundTerm(Term,Lc,F,A), !,
  \+ (isIden(F,N), isKeyword(N)),
  typeOfExp(F,topType,classType(ArgTps,ClassT),Env,Op),
  sameType(Tp,ClassT,Env),
  typeOfPtns(A,ArgTps,Lc,Env,Ex,Args).

typeOfPtn(T,Tp,Env,Env,pVoid) :-
  locOfAst(T,Lc),
  reportError("pattern %s not consistent with expected type %s",[T,Tp],Lc).

genTpVars([],[]).
genTpVars([_|I],[V|More]) :- 
  newTypeVar("__",V),
  genTpVars(I,More).

typeOfVarPtn(Lc,"_",_,Env,Env,v(Lc,"_")) :- !.
typeOfVarPtn(Lc,Nm,Tp,Env,Env,Term) :-
  isVar(Nm,Env,Vr),!,
  varPttrn(Lc,Nm,Vr,Tp,Env,Term).
typeOfVarPtn(Lc,Nm,Tp,Env,Ex,v(Lc,Nm)) :-
  declareVar(Nm,Lc,vr(Nm,Tp),Env,Ex).

varPttrn(Lc,_,vr(VNm,VT),Tp,Env,v(Lc,VNm)) :-
  pickupThisType(Env,ThisType),
  freshen(VT,ThisType,_,T),
  sameType(Tp,T,Env).
varPttrn(Lc,Nm,V,Tp,_,pVoid) :-
  reportError("illegal variable: %s/%s : %s",[Nm,V,Tp],Lc).

typeOfPtns([],[],_,Env,Env,[]).
typeOfPtns([A|_],[],_,Env,Env,[]) :-
  locOfAst(A,Lc),!,
  reportError("too many arguments: %s",[A],Lc).
typeOfPtns([],[T|_],Lc,Env,Env,[]) :-!,
  reportError("not enough arguments, expecting a %s",[T],Lc).
typeOfPtns([A|Els],[ATp|Tps],_,Env,Ex,[Ptn|More]) :-
  typeOfPtn(A,ATp,Env,E0,Ptn),
  locOfAst(A,Lc),
  typeOfPtns(Els,Tps,Lc,E0,Ex,More).

typeOfListPtn([],Lc,_,Tp,Env,Env,Term) :-
  typeOfExp(name(Lc,"[]"),Tp,_,Env,Term).
typeOfListPtn([Last],_,ElTp,Tp,Env,Ex,apply(Lc,Op,[Hd,Tl])) :-
  isBinary(Last,Lc,",..",L,R),
  typeOfExp(name(Lc,",.."),topType,_,Env,Op),
  typeOfPtn(L,ElTp,Env,E0,Hd),
  typeOfPtn(R,Tp,E0,Ex,Tl).
typeOfListPtn([El|More],_,ElTp,ListTp,Env,Ex,apply(Lc,Op,[Hd,Tl])) :-
  locOfAst(El,Lc),
  typeOfExp(name(Lc,",.."),topType,_,Env,Op),
  typeOfPtn(El,ElTp,Env,E0,Hd),
  typeOfListPtn(More,Lc,ElTp,ListTp,E0,Ex,Tl).

typeOfExp(V,ET,Tp,Env,Term) :-
  isIden(V,Lc,N),!,
  typeOfVar(Lc,N,ET,Tp,Env,Term).
typeOfExp(integer(Lc,Ix),ET,Tp,Env,intLit(Ix)) :- !,
  findType("integer",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfExp(float(Lc,Ix),ET,Tp,Env,floatLit(Ix)) :- !,
  findType("float",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfExp(string(Lc,Ix),ET,Tp,Env,stringLit(Lc,Ix)) :- !,
  findType("string",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfExp(interString(Lc,Segs),ET,Tp,Env,Exp) :- !,
  findType("string",Env,Tp),
  checkType(Lc,Tp,ET,Env),
  typeOfStringSegments(Segs,Lc,Env,Exp).
typeOfExp(Term,ET,RT,Env,Exp) :-
  isBinary(Term,Lc,":",L,R), !,
  parseType(R,Env,RT),
  checkType(Lc,RT,ET,Env),
  typeOfExp(L,RT,_,Env,Exp).
typeOfExp(Term,ET,Tp,Env,Exp) :-
  isBinary(Term,Lc,".",L,F), !,
  isIden(F,Fld),
  recordAccessExp(Lc,L,Fld,ET,Tp,Env,Exp).
typeOfExp(Term,ET,Tp,Env,pkgRef(Lc,Pkg,Fld)) :-
  isBinary(Term,Lc,"#",L,F), !,
  isIden(F,FLc,Fld),
  packageName(L,Pkg),
  isExported(Pkg,Fld,ExTp),
  freshen(ExTp,voidType,_,Tp), % replace with package type
  checkType(FLc,Tp,ET,Env).
typeOfExp(Term,ET,Tp,Env,conditional(Lc,Test,Then,Else)) :-
  isBinary(Term,Lc,"|",L,El),
  isBinary(L,"?",Tst,Th), !,
  checkCond(Tst,Env,ET,Test),
  typeOfExp(Th,ET,T1,Env,Then),
  typeOfExp(El,ET,T2,Env,Else),
  glb(T1,T2,Env,Tp).
typeOfExp(Term,ET,funType(ArgTps,RTp),Env,lambda(Lc,Args,Cond,Res)) :-
  isBinary(Term,"=>",Lc,L,R),
  splitHead(L,"()",A,C), !,
  genTpVars(A,ArgTps),
  newTypeVar("_R",ResType),
  checkType(Lc,funType(ArgTps,ResType),ET,Env),
  typeOfPtns(A,ArgTps,Lc,Env,E0,Args),
  checkCond(C,E0,E1,Cond),
  typeOfExp(R,ResType,RTp,E1,Res).
typeOfExp(Term,ET,predType(ArgTps),Env,clause(Lc,Args,Cond,Body)) :-
  isBinary(Term,":-",Lc,L,R),
  splitHead(L,"()",A,C), !,
  genTpVars(A,ArgTps),
  checkType(Lc,predType(ArgTps),ET,Env),
  typeOfPtns(A,ArgTps,Lc,Env,E0,Args),
  checkCond(C,E0,E1,Cond),
  checkCond(R,E1,_,Body).
typeOfExp(Term,ET,ListTp,Env,Exp) :-
  isSquareTuple(Term,Lc,Els), !,
  findType("list",Env,ListTp),
  ListTp = typeExp(_,[ElTp]),
  checkType(Lc,ListTp,ET,Env),
  typeOfListExp(Els,Lc,ElTp,ListTp,Env,Exp).
typeOfExp(tuple(_,"()",[Inner]),ET,Tp,Env,Exp) :-
  \+ isTuple(Inner,_), !,
  typeOfExp(Inner,ET,Tp,Env,Exp).
typeOfExp(tuple(Lc,A),ET,tupleType(ElTypes),Env,tuple(Lc,Els)) :-
  genTpVars(A,ArgTps),
  checkType(Lc,tupleType(ArgTps),ET,Env),
  typeOfExps(A,ArgTps,ElTypes,Env,Els).
typeOfExp(Term,ET,Tp,Env,Record) :-
  isBraceTuple(Term,Lc,_),!,
  typeOfRecord(Lc,Term,ET,Tp,Env,Record).
typeOfExp(Term,ET,Tp,Env,apply(Lc,Fun,Args)) :-
  isRoundTerm(Term,Lc,F,A), 
  genTpVars(A,ArgTps),
  typeOfExp(F,funType(ArgTps,ET),funType(_,Tp),Env,Fun),!,
  typeOfExps(A,ArgTps,_,Env,Args).
typeOfExp(Term,ET,Tp,Env,apply(Lc,Fun,Args)) :-
  isRoundTerm(Term,Lc,F,A),!,
  genTpVars(A,ArgTps),
  typeOfExp(F,classType(ArgTps,ET),classType(_,Tp),Env,Fun),
  typeOfExps(A,ArgTps,_,Env,Args).
typeOfExp(Term,ET,ET,_,void) :-
  locOfAst(Term,Lc),
  reportError("illegal expression: %s, expecting %s",[Term,ET],Lc).

recordAccessExp(Lc,Rc,Fld,ET,Tp,Env,dot(Lc,Rec,Fld)) :-
  typeOfExp(Rc,topType,AT,Env,Rec),
  getTypeFace(AT,Env,Face),
  fieldInFace(Face,Fld,Lc,FTp),!,
  freshen(FTp,AT,_,Tp), % the record is this to the right of dot.
  checkType(Lc,ET,Tp,Env). % dot is contra variant!

typeOfRecord(Lc,Rec,ClassTp,Tp,Env,record(Lc,Defs,Others,Types)) :-
  gensym("anonClass",ClassPath),
  checkClassBody(ClassTp,Rec,Env,Defs,Others,Types,Fields,ClassPath),
  Tp = faceType(Fields),
  (subType(Tp,ClassTp,Env),!; reportError("record type %s not consistent with %s",[Tp,ClassTp],Lc)).

fieldInFace(Fields,Nm,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(_,Nm,Lc,V) :-
  reportError("field %s not declared",[Nm],Lc),
  newTypeVar(Nm,V).

typeOfVar(Lc,"true",ET,Tp,Env,v(Lc,"true")) :-
  findType("logical",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfVar(Lc,"false",ET,Tp,Env,v(Lc,"false")) :-
  findType("logical",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfVar(Lc,"_",_,anonType,_,v(Lc,"_")) :-!.
typeOfVar(Lc,Nm,ET,Tp,Env,Term) :-
  isVar(Nm,Env,Vr),!,
  varExp(Lc,Nm,Vr,ET,Tp,Env,Term).
typeOfVar(Lc,Nm,_,voidType,_,_,tVoid) :-
  reportError("variable %s not declared",[v(Lc,Nm)],Lc).

varExp(Lc,_,vr(VNm,VT),ET,Tp,Env,v(Lc,VNm)) :- !,
  pickupThisType(Env,ThisType),
  freshen(VT,ThisType,_,Tp),
  checkType(Lc,Tp,ET,Env).
varExp(Lc,Nm,V,Tp,voidType,_,tVoid) :-
  reportError("variable not understood: %s/%s : %s",[Nm,V,Tp],Lc).

typeOfExps([],[],[],_,[]).
typeOfExps([A|As],[T|Ts],[ElTp|ElTypes],Env,[Term|Els]) :-
  typeOfExp(A,T,ElTp,Env,Term),
  typeOfExps(As,Ts,ElTypes,Env,Els).

typeOfListExp([],Lc,_,ListTp,Env,Exp) :-
  typeOfExp(name(Lc,"[]"),ListTp,_,Env,Exp).
typeOfListExp([Last],_,ElTp,ListTp,Env,apply(Lc,Op,[Hd,Tl])) :-
  isBinary(Last,Lc,",..",L,R),
  typeOfExp(name(Lc,",.."),topType,_,Env,Op),
  typeOfExp(L,ElTp,_,Env,Hd),
  typeOfExp(R,ListTp,_,Env,Tl).
typeOfListExp([El|More],_,ElTp,ListTp,Env,apply(Lc,Op,[Hd,Tl])) :-
  locOfAst(El,Lc),
  typeOfExp(name(Lc,",.."),topType,_,Env,Op),
  typeOfExp(El,ElTp,_,Env,Hd),
  typeOfListExp(More,Lc,ElTp,ListTp,Env,Tl).

typeOfStringSegments([],Lc,_Env,stringLit(Lc,"")) :- 
  reportError("string interpolation not implemented",[],Lc),!.


checkType(_,S,T,Env) :-
  subType(S,T,Env),!.
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
checkCond(Term,Env,Env,neg(Lc,equals(Lc,Lhs,Rhs))) :-
  isBinary(Term,Lc,"\\=",L,R),!,
  newTypeVar("_#",TV),
  typeOfExp(L,TV,Tp,Env,Lhs),
  typeOfExp(R,Tp,_,Env,Rhs).
checkCond(Term,Env,Ex,unify(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,"==",L,R),!,
  newTypeVar("_#",TV),
  typeOfPtn(L,TV,Env,E0,Lhs),
  typeOfPtn(R,TV,E0,Ex,Rhs).
checkCond(Term,Env,Env,equals(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,"=",L,R),!,
  newTypeVar("_#",TV),
  typeOfExp(L,TV,Tp,Env,Lhs),
  typeOfExp(R,Tp,_,Env,Rhs).
checkCond(Term,Env,Ex,match(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,".=",L,R),!,
  newTypeVar("_#",TV),
  typeOfPtn(L,TV,Env,Ex,Lhs),
  typeOfExp(R,TV,_,Ex,Rhs).
checkCond(Term,Env,Ex,match(Lc,Rhs,Lhs)) :-
  isBinary(Term,Lc,"=.",L,R),!,
  newTypeVar("_#",TV),
  typeOfPtn(L,TV,Env,Ex,Lhs),
  typeOfExp(R,TV,_,Ex,Rhs).
checkCond(Term,Env,Ex,call(Lc,Pred,Args)) :-
  isRoundTerm(Term,Lc,F,A),
  genTpVars(A,ArgTps),
  typeOfExp(F,predType(ArgTps),_,Env,Pred),
  typeOfPtns(A,ArgTps,Lc,Env,Ex,Args).

checkConds([C],Env,Ex,Cond) :-
  checkCond(C,Env,Ex,Cond).
checkConds([C|More],Env,Ex,(L,R)) :-
  checkCond(C,Env,E0,L),
  checkConds(More,E0,Ex,R).

processGrammarRule(St,grammarType(AT,Tp),[grammarRule(Lc,Nm,Args,PB,Body)|Defs],Defs,E,_) :-
  isBinary(St,Lc,"-->",L,R),!,
  splitGrHead(L,Nm,A,P),
  pushScope(E,Env),
  findType("stream",Env,StreamType),
  StreamType = typeExp(_,[ElTp]),
  subType(Tp,StreamType,Env),
  typeOfPtns(A,AT,Lc,Env,E0,Args),
  checkNonTerminals(R,Tp,ElTp,E0,E1,Body),
  checkPushBack(P,PB,ElTp,E1).

checkNonTerminals(tuple(Lc,"[]",Els),_,ElTp,E,Env,terminals(Lc,Terms)) :- !,
  checkTerminals(Els,Terms,ElTp,E,Env).
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
checkNonTerminals(Term,_,_,Env,Env,neg(Lc,equals(Lc,Lhs,Rhs))) :-
  isBinary(Term,Lc,"\\=",L,R),!,
  newTypeVar("_#",TV),
  typeOfExp(L,TV,Tp,Env,Lhs),
  typeOfExp(R,Tp,_,Env,Rhs).
checkNonTerminals(Term,_,_,Env,Ex,unify(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,"==",L,R),!,
  newTypeVar("_#",TV),
  typeOfPtn(L,TV,Env,E0,Lhs),
  typeOfPtn(R,TV,E0,Ex,Rhs).
checkNonTerminals(Term,_,_,Env,Env,equals(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,"=",L,R),!,
  newTypeVar("_#",TV),
  typeOfExp(L,TV,Tp,Env,Lhs),
  typeOfExp(R,Tp,_,Env,Rhs).
checkNonTerminals(Term,_,_,Env,Ex,match(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,".=",L,R),!,
  newTypeVar("_#",TV),
  typeOfPtn(L,TV,Env,Ex,Lhs),
  typeOfExp(R,TV,_,Ex,Rhs).
checkNonTerminals(Term,_,_,Env,Ex,match(Lc,Rhs,Lhs)) :-
  isBinary(Term,Lc,"=.",L,R),!,
  newTypeVar("_#",TV),
  typeOfPtn(L,TV,Env,Ex,Lhs),
  typeOfExp(R,TV,_,Ex,Rhs).
checkNonTerminals(Term,Tp,_,Env,Ex,call(Lc,Pred,Args)) :-
  isRoundTerm(Term,Lc,F,A),
  genTpVars(A,ArgTps),
  typeOfExp(F,grammarType(ArgTps,Tp),_,Env,Pred),
  typeOfPtns(A,ArgTps,Lc,Env,Ex,Args).
checkNonTerminals(Term,_,_,Env,Env,eof(Lc)) :-
  isIden(Term,Lc,"eof").
checkNonTerminals(Term,_,_,Env,Ex,goal(Lc,Cond)) :-
  isBraceTuple(Term,Lc,Els),
  checkConds(Els,Env,Ex,Cond).

checkPushBack([],[],_,_) :- !.
checkPushBack([T|More],[TT|Out],Tp,Env) :-
  typeOfExp(T,Tp,_,Env,TT),
  checkPushBack(More,Out,Tp,Env).

checkTerminals([],[],_,Env,Env) :- !.
checkTerminals([T|More],[TT|Out],Tp,Env,Ex) :-
  typeOfPtn(T,Tp,Env,E0,TT),
  checkTerminals(More,Out,Tp,E0,Ex).

computeExport([],_,[],[]).
computeExport([Def|Defs],Private,Fields,Types) :-
  exportDef(Def,Private,Fields,Fx,Types,Tx),!,
  computeExport(Defs,Private,Fx,Tx).

exportDef(function(_,Nm,Tp,_),Private,Fields,Fx,Types,Types) :-
  (is_member((Nm,value),Private), Fields=Fx ; Fields = [(Nm,Tp)|Fx] ).
exportDef(predicate(_,Nm,Tp,_),Private,Fields,Fx,Types,Types) :-
  (is_member((Nm,value),Private), Fields=Fx ; Fields = [(Nm,Tp)|Fx] ).
exportDef(class(_,Nm,Tp,_,_),Private,Fields,Fx,Types,Types) :-
  (is_member((Nm,value),Private), Fields=Fx ; Fields = [(Nm,Tp)|Fx] ).
exportDef(enum(_,Nm,Tp,_,_),Private,Fields,Fx,Types,Types) :-
  (is_member((Nm,value),Private), Fields=Fx ; Fields = [(Nm,Tp)|Fx] ).
exportDef(typeDef(_,Nm,_,Rules),Private,Fields,Fields,Types,Tx) :-
  (is_member((Nm,type),Private), Types=Tx ; Types = [(Nm,Rules)|Tx]).
exportDef(defn(_,Nm,_,Tp,_),Private,Fields,Fx,Types,Types) :-
  (is_member((Nm,type),Private),Fields=Fx ; Fields = [(Nm,Tp)|Fx]).
exportDef(grammar(_,Nm,Tp,_),Private,Fields,Fx,Types,Types) :-
  (is_member((Nm,type),Private),Fields=Fx ; Fields = [(Nm,Tp)|Fx]).

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
matchArgTypes([kVar(Nm)|L],[kVar(Ot)|R],[(Ot,kVar(Nm))|Binding]) :-
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

getTypeFace(T,Env,Face) :-
  deRef(T,Tp),
  getFace(Tp,Env,Face).

getFace(type(Nm),Env,Face) :- !,
  typeFaceRule(Nm,Env,FaceRule),
  freshen(FaceRule,voidType,_,typeRule(Lhs,faceType(Face))),
  sameType(type(Nm),Lhs,Env).
getFace(typeExp(Nm,Args),Env,Face) :- !,
  typeFaceRule(Nm,Env,FaceRule),
  freshen(FaceRule,voidType,_,typeRule(Lhs,faceType(Face))),
  sameType(Lhs,typeExp(Nm,Args),Env).
getFace(T,Env,Face) :- isUnbound(T), !,
  upperBoundOf(T,Up),
  getTypeFace(Up,Env,Face).
getFace(faceType(Face),_,Face) :- !.
getFace(_,_,[]).
