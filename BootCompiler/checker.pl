:- module(checker,[checkProgram/2]).

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

checkProgram(Prog,prog(Pkg,Imports,Defs,Others,Fields,Types)) :-
  stdDict(Base),
  isBraceTerm(Prog,Pk,Els),
  packageName(Pk,Pkg),
  pushScope(Base,TEnv),
  thetaEnv(Pkg,Els,[],TEnv,_,Defs,Private,Imports,Others),
  computeExport(Defs,Private,Fields,Types),!.

packageName(T,Pkg) :- isIden(T,Pkg).
packageName(T,Pkg) :- isString(T,Pkg).

thetaEnv(Path,Els,Fields,Base,TheEnv,Defs,Private,Imports,Others) :-
  dependencies(Els,Groups,Private,Annots,Imps,Otrs),
  checkImports(Imps,Imports,Base,Env),
  checkGroups(Groups,Fields,Annots,Defs,Env,TheEnv,Path),
  checkOthers(Otrs,Others,TheEnv,Path).

checkImports([],[],Env,Env) :- !.
checkImports([St|Stmts],Imports,Env,Ex) :-
  checkImport(St,Imports,IM,Env,E0),
  checkImports(Stmts,IM,E0,Ex).

checkImport(St,[import(Lc,PkgSpec)|More],More,Env,Ex) :-
  isUnary(St,Lc,"import",P),
  isIden(P,_,PkgName),
  locatePackage(PkgName,PkgSpec),
  importDefs(PkgSpec,Env,Ex).

% For now, e stub these out.
locatePackage(Pkg,Pkg).
importDefs(_,Env,Env).

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

groupType([(var(_),_,_)|_],var).
groupType([(tpe(_),_,_)|_],tpe).

checkGroup(Grp,tpe,_,_,Defs,Dx,Env,Ex,Path) :-
  typeGroup(Grp,Defs,Dx,Env,Ex,Path).
checkGroup(Grp,var,Fields,Annots,Defs,Dx,Env,Ex,Path) :-
  varGroup(Grp,Fields,Annots,Defs,Dx,Env,Ex,Path).

typeGroup(Grp,Defs,Dx,Env,Ex,Path) :-
  defineTypes(Grp,Defs,Dx,Env,Ex,Path).

defineTypes([],Defs,Defs,Env,Env,_).
defineTypes([(tpe(N),Lc,Stmts)|More],Defs,Dx,Env,Ex,Path) :-
  defineType(N,Lc,Stmts,Defs,D0,Env,E0,Path),
  defineTypes(More,D0,Dx,E0,Ex,Path).

defineType(N,Lc,_,Defs,Defs,Env,Env,_) :-
  typeInDict(N,Env,OLc,_),!,
  reportError("type %s already defined at %s",[N,OLc],Lc).
defineType(N,Lc,Stmts,[typeDef(N,Lc,Rules)|Defs],Defs,Env,Ex,Path) :- 
  parseTypeDef(Stmts,Env,Rules,[],Path),
  pickTypeTemplate(Rules,Type),
  faceOfType(Lc,Env,Rules,Face),
  declareType(N,Lc,Type,Face,Rules,Env,Ex).

parseTypeDef([],_,TpDefs,TpDefs,_).
parseTypeDef([St|More],Env,[Rl|Defs],Dx,Path) :-
  parseTypeRule(St,Env,Rl,Path),
  parseTypeDef(More,Env,Defs,Dx,Path).

varGroup(Grp,Fields,Annots,Defs,Dx,Base,Env,Path) :-
  parseAnnotations(Grp,Fields,Annots,Base,Env,Path),
  checkVarRules(Grp,D0,Env,Path),
  generalizeStmts(D0,Env,Defs,Dx).

parseAnnotations([],_,_,Env,Env,_).
parseAnnotations([(var(N),_,_)|More],Fields,Annots,Env,Ex,Path) :-
  is_member((N,Annot),Annots),!,
  annotateVar(N,Annot,Env,E0),
  parseAnnotations(More,Fields,Annots,E0,Ex,Path).
parseAnnotations([(var(N),Lc,_)|More],Fields,Annots,Env,Ex,Path) :-
  is_member((N,Tp),Fields),!,
  declareVar(N,vr(N,Tp),Lc,Env,E0),
  parseAnnotations(More,Fields,Annots,E0,Ex,Path).
parseAnnotations([(var(N),Lc,_)|More],Fields,Annots,Env,Ex,Path) :-
  reportError("no type annotation for variable %s",[N],Lc),
  parseAnnotations(More,Fields,Annots,Env,Ex,Path).

annotateVar(Nm,St,Env,Ex) :-
  isBinary(St,Lc,":",_,T),
  parseType(T,Env,Tp),
  declareVar(Nm,vr(Nm,Tp),Lc,Env,Ex).

checkVarRules([],[],_,_).
checkVarRules([(var(N),Lc,Stmts)|More],Defs,Env,Path) :-
  pickupVarType(N,Lc,Env,Tp),
  pickupThisType(Env,ThisType),
  freshen(Tp,ThisType,Q,ProgramType),
  processStmts(Stmts,ProgramType,Defs,D0,Env,Path),
  checkEvidenceBinding(Lc,Q),
  checkVarRules(More,D0,Env,Path).

pickupVarType(N,_,Env,Tp) :-
  isVar(N,Env,vr(_,Tp)),!.
pickupVarType(N,Lc,_,_) :- reportError("%s not declared",[N],Lc).

pickupThisType(Env,Tp) :-
  isVar("this",Env,vr(_,Tp)),!.
pickupThisType(_,voidType).

checkEvidenceBinding(_,_).

getTypeOfVar(N,Env,Nm,Tp) :-
  isVar(N,Env,vr(Nm,T)),
  pickupThisType(Env,ThisType),
  freshen(T,ThisType,_,Tp).

findType(Nm,Env,Tp) :-
  typeInDict(Nm,Env,T),
  pickupThisType(Env,ThisType),
  freshen(T,ThisType,_,Tp).

processStmts([],_,Defs,Defs,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Env,Path) :-
  processStmt(St,ProgramType,Defs,D0,Env,Path),!,
  processStmts(More,ProgramType,D0,Dx,Env,Path).

processStmt(St,funType(AT,RT),[equation(Lc,Nm,Ptn,Cond,Exp)|Defs],Defs,E,_) :- 
  pushScope(E,Env),
  isBinary(St,Lc,"=>",L,R),!,
  splitHead(L,Nm,Args,C),
  typeOfPtns(Args,AT,Env,E0,Ptn),
  checkCond(C,E0,E1,Cond),
  typeOfExp(R,RT,_,E1,Exp).
processStmt(St,predType(AT),[clause(Lc,Nm,Ptn,Cond,Body)|Defs],Defs,E,_) :- 
  pushScope(E,Env),
  isBinary(St,Lc,":-",L,R),!,
  splitHead(L,Nm,Args,C),
  typeOfPtns(Args,AT,Env,E0,Ptn),
  checkCond(C,E0,E1,Cond),
  checkCond(R,E1,_,Body).
processStmt(St,predType(AT),[strong(Lc,Nm,Ptn,Cond,Body)|Defs],Defs,E,_) :- 
  pushScope(E,Env),
  isBinary(St,Lc,":--",L,R),!,
  splitHead(L,Nm,Args,C),
  typeOfPtns(Args,AT,Env,E0,Ptn),
  checkCond(C,E0,E1,Cond),
  checkCond(R,E1,_,Body).
processStmt(St,predType(AT),[clause(Lc,Nm,Ptn,Cond,true(Lc))|Defs],Defs,E,_) :- 
  pushScope(E,Env),
  splitHead(St,Nm,Args,C),!,
  typeOfPtns(Args,AT,Env,E0,Ptn),
  checkCond(C,E0,_,Cond),
  locOfAst(St,Lc).
processStmt(St,Tp,[labelRule(Lc,Nm,Hd,Repl)|Defs],Defs,E,_) :- 
  isBinary(St,Lc,"<=",L,R),
  checkClassHead(L,Tp,E,E1,Nm,Hd),!,
  typeOfExp(R,E1,topType,_,Repl).
processStmt(St,Tp,[overrideRule(Lc,Nm,Hd,Repl)|Defs],Defs,E,_) :- 
  isBinary(St,Lc,"<<",L,R),
  checkClassHead(L,Tp,E,E1,Nm,Hd),!,
  typeOfExp(R,E1,topType,_,Repl).
processStmt(St,Tp,[classBody(Lc,Nm,Op,Stmts,Others,Types)|Defs],Defs,E,Path) :-
  isBinary(St,Lc,"..",L,R),
  isName(L,Nm),!,
  pushScope(E,Env),
  typeOfPtn(L,Tp,Env,E0,Op),
  checkClassBody(Nm,Tp,R,E0,Stmts,Others,Types,Path).
processStmt(St,classType(AT,Tp),[classBody(Lc,Nm,Hd,Stmts,Others,Types)|Defs],Defs,E,Path) :-
  isBinary(St,Lc,"..",L,R),
  checkClassHead(L,classType(AT,Tp),E,E1,Nm,Hd),
  checkClassBody(Nm,Tp,R,E1,Stmts,Others,Types,Path).

checkClassHead(Term,Tp,Env,Ex,Nm,Ptn) :-
  isName(Term,Nm),!,
  pushScope(Env,E0),
  typeOfPtn(Term,Tp,E0,Ex,Ptn).
checkClassHead(Term,classType(AT,_),Env,Ex,Nm,Ptn) :-
  splitHead(Term,Nm,A,C),!,
  locOfAst(Term,Lc),
  pushScope(Env,E0),
  typeOfPtns(A,AT,E0,E1,Args),
  checkCond(C,E1,Ex,Cond),
  Hd = apply(Lc,con(Lc,Nm),Args),
  (Cond=true(_), Ptn = Hd ; Ptn = where(Hd,Cond)),!.

checkClassBody(Nm,ClassTp,Body,Env,Defs,Others,Types,Path) :-
  isBraceTuple(Body,Lc,Els),
  getTypeFace(ClassTp,Env,Fields),
  pushScope(Env,Base),
  subPath(Path,Nm,ClassPath),
  declareVar("this",vr("this",ClassTp),Lc,Base,ThEnv),
  thetaEnv(ClassPath,Els,Fields,ThEnv,_OEnv,Defs,Private,_Imports,Others),
  computeExport(Defs,Private,_,Types).

declareFields([],_,Env,Env).
declareFields([(Nm,Tp)|More],Lc,Env,Ex) :-
  declareVar(Nm,vr(Nm,Tp),Lc,Env,E0),
  declareFields(More,Lc,E0,Ex).

splitHead(Term,Nm,Args,Cond) :-
  isBinary(Term,"::",L,Cond),!,
  splitHead(L,Nm,Args,_).
splitHead(Term,Nm,Args,name(Lc,"true")) :-
  isRound(Term,Nm,Args),
  locOfAst(Term,Lc),
  \+ is_member(Nm,["=>",":-",":--","<=","..","=",":="]).
splitHead(Term,"()",Args,name(Lc,"true")) :-
  locOfAst(Term,Lc),
  isTuple(Term,Args).

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
generalizeStmts([Cl|Stmts],Env,[class(Lc,Nm,Tp,[Cl|Rules])|Defs],Dx) :-
  isRuleForClass(Cl,Lc,Nm),!,
  collectClassRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  generalizeStmts(S0,Env,Defs,Dx).

collectClauses([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  (Cl = clause(_,Nm,_,_,_) ; Cl = strong(_,Nm,_,_,_)),!,
  collectClauses(Stmts,Sx,Nm,Ex).
collectClauses([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectClauses(Stmts,Sx,Nm,Eqns).
collectClauses([],[],_,[]).

collectEquations([Eqn|Stmts],Sx,Nm,[Eqn|Ex]) :-
  Eqn = equation(_,Nm,_,_,_),
  collectEquations(Stmts,Sx,Nm,Ex).
collectEquations([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectEquations(Stmts,Sx,Nm,Eqns).
collectEquations([],[],_,[]).

collectClassRules([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  isRuleForClass(Cl,_,Nm),!,
  collectClassRules(Stmts,Sx,Nm,Ex).
collectClassRules([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectClassRules(Stmts,Sx,Nm,Eqns).
collectClassRules([],[],_,[]).


isRuleForClass(labelRule(Lc,Nm,_,_),Lc,Nm).
isRuleForClass(overrideRule(Lc,Nm,_,_),Lc,Nm).
isRuleForClass(classBody(Lc,Nm,_,_,_,_),Lc,Nm).

typeOfPtn(N,Tp,Env,Ex,Term) :- 
  isIden(N,Lc,Nm),
  typeOfVarPtn(Lc,Nm,Tp,Env,Ex,Term).
typeOfPtn(integer(Lc,Ix),Tp,Env,Env,intLit(Lc,Ix)) :-
  findType("integer",Env,IntegerTp),
  sameType(Tp,IntegerTp,Env).
typeOfPtn(long(Lc,Ix),Tp,Env,Env,longLit(Lc,Ix)) :- !,
  findType("long",Env,LongTp),
  sameType(Tp,LongTp,Env).
typeOfPtn(float(Lc,Ix),Tp,Env,Env,floatLit(Lc,Ix)) :- !,
  findType("float",Env,FloatTp),
  sameType(Tp,FloatTp,Env).
typeOfPtn(string(Lc,Sx),Tp,Env,Env,stringLit(Lc,Sx)) :- !,
  findType("string",Env,StringTp),
  sameType(Tp,StringTp,Env).
typeOfPtn(P,Tp,Env,Ex,where(Ptn,Cond)) :-
  isBinary(P,"::",L,R),
  typeOfPtn(L,Tp,Env,E0,Ptn),
  checkCond(R,E0,Ex,Cond).
typeOfPtn(tuple(Lc,"()",[A]),Tp,Env,Ex,tuple(Lc,Args)) :-
  genTpVars(A,ArgTps),
  sameType(Tp,tupleType(ArgTps),Env),
  typeOfPtns(A,ArgTps,Env,Ex,Args).
typeOfPtn(tuple(Lc,"()",A),Tp,Env,Ex,tuple(Lc,Args)) :-
  genTpVars(A,ArgTps),
  sameType(Tp,tupleType(ArgTps),Env),
  typeOfPtns(A,ArgTps,Env,Ex,Args).
typeOfPtn(tuple(Lc,"[]",A),Tp,Env,Ex,Term) :-
  findType("list",Env,ListTp),
  ListTp = typeExp(_,[ElTp]),
  sameType(ListTp,Tp,Env),
  typeOfListPtn(A,Lc,ElTp,ListTp,Env,Ex,Term).
typeOfPtn(Term,Tp,Env,Ex,apply(Lc,Op,Args)) :-
  isRoundTerm(Term,Lc,F,A), !,
  \+ (isIden(F,N), isKeyword(N)),
  typeOfExp(F,topType,classType(ArgTps,ClassT),Env,Op),
  sameType(Tp,ClassT,Env),
  typeOfPtns(A,ArgTps,Env,Ex,Args).

typeOfPtn(T,Tp,Env,Env,pVoid) :-
  locOfAst(T,Lc),
  reportError("pattern %s not consistent with expected type %s",[T,Tp],Lc).

genTpVars([],[]).
genTpVars([_|I],[V|More]) :- 
  newTypeVar("__",V),
  genTpVars(I,More).

typeOfVarPtn(Lc,Nm,Tp,Env,Env,Term) :-
  isVar(Nm,Env,Vr),!,
  varPttrn(Lc,Nm,Vr,Tp,Env,Term).
typeOfVarPtn(Lc,Nm,Tp,Env,Ex,vr(Lc,Nm)) :-
  declareVar(Nm,vr(Nm,Tp),Lc,Env,Ex).

varPttrn(Lc,_,vr(VNm,VT),Tp,Env,vr(Lc,VNm)) :-
  pickupThisType(Env,ThisType),
  freshen(VT,ThisType,_,T),
  sameType(Tp,T,Env).
varPttrn(Lc,_,enum(VNm,VT),Tp,Env,enum(Lc,VNm)) :-
  pickupThisType(Env,ThisType),
  freshen(VT,ThisType,_,T),
  sameType(Tp,T,Env).
varPttrn(Lc,Nm,V,Tp,_,pVoid) :-
  reportError("illegal variable: %s/%s : %s",[Nm,V,Tp],Lc).

typeOfPtns([],[],Env,Env,[]).
typeOfPtns([A|Els],[ATp|Tps],Env,Ex,[Ptn|More]) :-
  typeOfPtn(A,ATp,Env,E0,Ptn),
  typeOfPtns(Els,Tps,E0,Ex,More).

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
typeOfExp(integer(Lc,Ix),ET,Tp,Env,intLit(Lc,Ix)) :- !,
  findType("integer",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfExp(long(Lc,Ix),ET,Tp,Env,longLit(Lc,Ix)) :- !,
  findType("long",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfExp(float(Lc,Ix),ET,Tp,Env,floatLit(Lc,Ix)) :- !,
  findType("float",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfExp(string(Lc,Ix),ET,Tp,Env,stringLit(Lc,Ix)) :- !,
  findType("string",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfExp(interString(Lc,Segs),ET,Tp,Env,Exp) :- !,
  findType("string",Env,Tp),
  checkType(Lc,Tp,ET,Env),
  typeOfStringSegments(Segs,Env,Exp).
typeOfExp(Term,ET,RT,Env,Exp) :-
  isBinary(Term,Lc,":",L,R), !,
  parseType(R,Env,RT),
  checkType(Lc,RT,ET,Env),
  typeOfExp(L,RT,_,Env,Exp).
typeOfExp(Term,ET,Tp,Env,Exp) :-
  isBinary(Term,Lc,".",L,F), !,
  isIden(F,Fld),
  recordAccessExp(Lc,L,Fld,ET,Tp,Env,Exp).
typeOfExp(Term,ET,Tp,Env,pkgDot(Lc,Pkg,Fld)) :-
  isBinary(Term,Lc,"#",L,F), !,
  isIden(F,FLc,Fld),
  packageName(L,Pkg),
  isExported(Pkg,Fld,ExTp),
  freshen(ExTp,voidType,_,Tp), % replace with package type
  checkType(FLc,Tp,ET,Env).
typeOfExp(Term,ET,Tp,Env,conditional(Lc,Test,Then,Else)) :-
  isBinary(Term,Lc,"|",L,El),
  isBinary(L,"?",Tst,Th), !,
  checkCond(Tst,Env,Test),
  typeOfExp(Th,ET,T1,Env,Then),
  typeOfExp(El,ET,T2,Env,Else),
  glb(T1,T2,Env,Tp).
typeOfExp(Term,ET,funType(ArgTps,RTp),Env,lambda(Lc,Args,Cond,Res)) :-
  isBinary(Term,"=>",Lc,L,R),
  splitHead(L,"()",A,C), !,
  genTpVars(A,ArgTps),
  newTypeVar("_R",ResType),
  checkType(Lc,funType(ArgTps,ResType),ET,Env),
  typeOfPtns(A,ArgTps,Env,E0,Args),
  checkCond(C,E0,E1,Cond),
  typeOfExp(R,ResType,RTp,E1,Res).
typeOfExp(Term,ET,predType(ArgTps),Env,clause(Lc,Args,Cond,Body)) :-
  isBinary(Term,":-",Lc,L,R),
  splitHead(L,"()",A,C), !,
  genTpVars(A,ArgTps),
  checkType(Lc,predType(ArgTps),ET,Env),
  typeOfPtns(A,ArgTps,Env,E0,Args),
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
typeOfExp(Term,ET,Tp,Env,record(Lc,Fields)) :-
  isBraceTuple(Term,Lc,Els),!,
  typeOfRecord(Els,ET,Tp,Env,Fields).
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

fieldInFace(Fields,Nm,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(_,Nm,Lc,V) :-
  reportError("field %s not declared",[Nm],Lc),
  newTypeVar(Nm,V).

typeOfVar(Lc,"true",ET,Tp,Env,enum(Lc,"true")) :-
  findType("logical",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfVar(Lc,"false",ET,Tp,Env,enum(Lc,"false")) :-
  findType("logical",Env,Tp),
  checkType(Lc,Tp,ET,Env).
typeOfVar(Lc,Nm,ET,Tp,Env,Term) :-
  isVar(Nm,Env,Vr),!,
  varExp(Lc,Nm,Vr,ET,Tp,Env,Term).
typeOfVar(Lc,Nm,_,voidType,_,_,tVoid) :-
  reportError("variable %s not declared",[enum(Lc,Nm)],Lc).

varExp(Lc,_,vr(VNm,VT),ET,Tp,Env,vr(Lc,VNm)) :- !,
  pickupThisType(Env,ThisType),
  freshen(VT,ThisType,_,Tp),
  checkType(Lc,Tp,ET,Env).
varExp(Lc,_,enum(VNm,VT),ET,Tp,Env,enum(Lc,VNm)) :- !,
  pickupThisType(Env,ThisType),
  freshen(VT,ThisType,_,Tp),
  checkType(Lc,Tp,ET,Env).
varExp(Lc,_,con(VNm,VT),ET,Tp,Env,con(Lc,VNm)) :- !,
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
checkCond(Term,Env,Ex,conditional(Test,Either,Or)) :-
  isBinary(Term,"|",L,R),
  isBinary(L,"?",T,Th),!,
  checkCond(T,Env,E0,Test),
  checkCond(Th,E0,E1,Either),
  checkCond(R,E1,Ex,Or).
checkCond(Term,Env,Ex,disj(Either,Or)) :-
  isBinary(Term,"|",L,R),!,
  checkCond(L,Env,E1,Either),
  checkCond(R,E1,Ex,Or).
checkCond(Term,Env,Ex,one(Test)) :-
  isUnary(Term,"!",N),!,
  checkCond(N,Env,Ex,Test).
checkCond(Term,Env,Env,neg(Test)) :-
  isUnary(Term,"\\+",N),!,
  checkCond(N,Env,_,Test).
checkCond(Term,Env,Env,forall(Gen,Test)) :-
  isBinary(Term,"*>",L,R),!,
  checkCond(L,Env,E0,Gen),
  checkCond(R,E0,_,Test).
checkCond(Term,Env,Ex,Cond) :-
  isTuple(Term,C),!,
  checkConds(C,Env,Ex,Cond).
checkCond(Term,Env,Env,neg(equals(Lc,Lhs,Rhs))) :-
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
  typeOfPtns(A,ArgTps,Env,Ex,Args).

checkConds([C],Env,Ex,Cond) :-
  checkCond(C,Env,Ex,Cond).
checkConds([C|More],Env,Ex,(L,R)) :-
  checkCond(C,Env,E0,L),
  checkConds(More,E0,Ex,R).

computeExport([],_,[],[]).
computeExport([Def|Defs],Private,Fields,Types) :-
  exportDef(Def,Private,Fields,Fx,Types,Tx),
  computeExport(Defs,Private,Fx,Tx).

exportDef(function(_,Nm,Tp,_),Private,Fields,Fx,Types,Types) :-
  (is_member((Nm,value),Private), Fields=Fx ; Fields = [(Nm,Tp)|Fx] ).
exportDef(predicate(_,Nm,Tp,_),Private,Fields,Fx,Types,Types) :-
  (is_member((Nm,value),Private), Fields=Fx ; Fields = [(Nm,Tp)|Fx] ).
exportDef(class(_,Nm,Tp,_),Private,Fields,Fx,Types,Types) :-
  (is_member((Nm,value),Private), Fields=Fx ; Fields = [(Nm,Tp)|Fx] ).
exportDef(typeDef(_,Nm,Tp),Private,Fields,Fields,Types,Tx) :-
  (is_member((Nm,type),Private), Types=Tx ; Types = [(Nm,Tp)|Tx]).

faceOfType(Lc,Env,Rules,Face) :-
  pickTypeTemplate(Rules,Tmplate),
  moveQuants(Tmplate,Q,Plate),
  mergeFields(Rules,Q,Plate,[],Fields,Env,Lc),
  moveQuants(Face,Q,typeRule(Plate,faceType(Fields))).

moveQuants(univType(B,Tp),[B|Q],Tmpl) :- !,
  moveQuants(Tp,Q,Tmpl).
moveQuants(Tp,[],Tp).

mergeFields([],_,_,Fields,Fields,_,_).
mergeFields([Rule|More],Q,Plate,SoFar,Fields,Env,Lc) :-
  moveQuants(Rule,_,typeRule(Lhs,Rhs)),
  matchTypes(Plate,Lhs,Binding),
  freshn(Rhs,Binding,FRhs),
  collectFace(FRhs,Env,SoFar,Flds,Lc),
  mergeFields(More,Q,Plate,Flds,Fields,Env,Lc).

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

deRule(univType(B,Tp),univType(B,XTp)) :-
  deRule(Tp,XTp).
deRule(typeRule(Lhs,_),Lhs).

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
