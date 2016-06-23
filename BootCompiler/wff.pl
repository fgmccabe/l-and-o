:-module(wff,[wffModule/1,isAlgebraicTypeDef/4]).
:-use_module(errors).
:-use_module(abstract).
:-use_module(misc).
:-use_module(keywords).

wffModule(Term) :- 
    isBraceTerm(Term,P,Els),
    wffPackageName(P),
    wffThetaEnv(Els),!.
wffModule(Term) :-
    locOfAst(Term,Lc),
    reportError("Invalid module: %s",[Term],Lc).

wffPackageName(Term) :- isIden(Term).
wffPackageName(Term) :- isString(Term,_).
wffPackageName(Term) :- isBinary(Term,".",L,R), wffPackageName(L), wffPackageName(R).
wffPackageName(Term) :- isBinary(Term,"#",L,R), wffPackageName(L), wffPackageVersion(R).
wffPackageName(Name) :- 
  locOfAst(Name,Lc),
  reportError("Module %s not valid",[Name],Lc).

wffPackageVersion(Term) :- isIden(Term).
wffPackageVersion(Term) :- isString(Term,_).
wffPackageVersion(Term) :- isInteger(Term,_).
wffPackageVersion(Term) :- isBinary(Term,".",L,R), wffPackageVersion(L), wffPackageVersion(R).

wffThetaEnv([]).
wffThetaEnv([St|Stmts]) :-
    wffStmt(St),
    wffThetaEnv(Stmts).

wffStmt(St) :-
  isPrivate(St,El,_),!,
  wffStmt(El).
wffStmt(St) :-
  isPublic(St,El,_),!,
  wffStmt(El).
wffStmt(St) :-
  wffImportSt(St).
wffStmt(St) :-
  isAlgebraicTypeDef(St).
wffStmt(St) :-
  isTypeAssertion(St).
wffStmt(St) :-
  isTypeAnnotation(St).
wffStmt(St) :-
  isAssertion(St).
wffStmt(St) :-
  wffDefinition(St).
wffStmt(St) :-
  wffEquation(St).
wffStmt(St) :-
  wffGrammarRule(St).
wffStmt(St) :- wffLabelRule(St).
wffStmt(St) :- wffClass(St).
wffStmt(St) :- wffClause(St).
wffStmt(St) :- locOfAst(St,Lc), reportError("Cannot understand statement %s",[St],Lc).

wffImportSt(St) :- isUnary(St,"private",P), wffImportSt(P).
wffImportSt(St) :- isUnary(St,"public",P), wffImportSt(P).
wffImportSt(St) :- isUnary(St,"import",P), wffPackageName(P).

wffTypeExp(T) :- wffIden(T).
wffTypeExp(T) :- isSquare(T,_,A), wffTypeExps(A).
wffTypeExp(T) :- isBinary(T,"=>",L,R), isTuple(L,A), wffTypeExps(A), wffTypeExp(R).
wffTypeExp(T) :- isBinary(T,"-->",L,R), isTuple(L,A), wffTypeExps(A), wffTypeExp(R).
wffTypeExp(T) :- isBinary(T,"<=>",L,R), isTuple(L,A), wffTypeExps(A), wffTypeExp(R).
wffTypeExp(T) :- isBraceTerm(T,L,[]),isTuple(L,A), wffTypeExps(A).
wffTypeExp(T) :- isBraceTuple(T,_,A), wffFaceTypes(A).
wffTypeExp(T) :- isTuple(T,A), wffTypeExps(A).
wffTypeExp(T) :- isQuantified(T,V,Tp), wffTypeQuants(V,[],_), wffTypeExp(Tp).
wffTypeExp(T) :- locOfAst(T,Lc), reportError("Cannot understand type %s",[T],Lc).

wffTypeQuants(P,Q,QQ) :- isBinary(P,",",L,R), wffTypeQuants(L,Q,Q0), wffTypeQuants(R,Q0,QQ).
wffTypeQuants(V,Q,[Nm|Q]) :- isBinary(V,"<~",L,Up), isBinary(L,"<~",Lw,Vr), isName(Vr,Nm), wffTypeExp(Lw), wffTypeExp(Up).
wffTypeQuants(V,Q,[Nm|Q]) :- isBinary(V,"<~",Vr,Up), isName(Vr,Nm), wffTypeExp(Up).
wffTypeQuants(N,Q,[Nm|Q]) :- isName(N,Nm).

wffFaceTypes([]).
wffFaceTypes([F|A]) :- isTypeAnnotation(F), wffFaceTypes(A).

wffTypeExps([]).
wffTypeExps([T|L]) :- wffTypeExp(T), wffTypeExps(L).

wffArgType(T) :- isUnary(T,"+",TT), !, wffTypeExp(TT).
wffArgType(T) :- isUnary(T,"++",TT), !, wffTypeExp(TT).
wffArgType(T) :- isUnary(T,"-",TT), !, wffTypeExp(TT).
wffArgType(T) :- isUnary(T,"+-",TT), !, wffTypeExp(TT).
wffArgType(T) :- isUnary(T,"-+",TT), !, wffTypeExp(TT).
wffArgType(T) :- isIden(T).

isPrivate(Term,T,Lc) :- isUnary(Term,"private",T), locOfAst(T,Lc).

isPublic(Term,T,Lc) :- isUnary(Term,"public",T), locOfAst(T,Lc).

isAlgebraicTypeDef(Term,Lc,Head,Body) :- isBinary(Term,Lc,"::=",Head,Body),
   wffNamedType(Head),
   wffTypeConstructors(Body).

isAlgebraicTypeDef(Term) :- isAlgebraicTypeDef(Term,_,_,_).

wffNamedType(T) :- wffIden(T).
wffNamedType(T) :- isSquare(T,_,A), wffTypeArgs(A).

wffTypeArgs([]).
wffTypeArgs([T|L]) :- isIden(T), wffTypeArgs(L).

wffTypeConstructors(T) :- isBinary(T,"|",L,R), wffTypeConstructors(L), wffTypeConstructors(R).
wffTypeConstructors(T) :- isIden(T).
wffTypeConstructors(T) :- isRound(T,Op,Args), \+ isKeyword(Op), wffTypeExps(Args).

isTypeAnnotation(Term) :-
  isBinary(Term,":",L,R),
  wffIden(L),
  wffTypeExp(R).

isTypeAssertion(St) :-
  isQuantified(St,V,Inner),!,
  wffTypeQuants(V,[],Q),
  isBinary(Inner,"<~",L,R),
  wffTypeHead(L,Q),
  wffTypeExp(R).
isTypeAssertion(St) :-
  isBinary(St,"<~",L,R),
  wffTypeHead(L,[]),
  wffTypeExp(R).

wffTypeHead(T,Q) :-
  isIden(T,Lc,_),
  (Q=[] ; reportError("quantifiers not permitted for type %s",[T],Lc)).
wffTypeHead(T,Q) :-
  isSquare(T,_,A),
  locOfAst(T,Lc),
  checkHeadTypes(A,Q,Lc).
wffTypeHead(T,_) :-
  locOfAst(T,Lc),
  reportError("invalid type in definition: %s",[T],Lc).

checkHeadTypes([],[],_) :- !.
checkHeadTypes([],_,Lc) :- reportError("too many quantifiers",[],Lc).
checkHeadTypes([V|L],Q,Lc) :-
  isIden(V,VLc,VN),!,
  (is_member(VN,Q) ; reportError("type arg %s not quantified",[V],VLc)),
  subtract(VN,Q,QQ),
  checkHeadTypes(L,QQ,Lc).
checkHeadTypes([V|L],Q,Lc) :-
  locOfAst(V,VLc),
  reportError("invalid type arg in definition %s",[V],VLc),
  checkHeadTypes(L,Q,Lc).

isAssertion(St) :-
  isUnary(St,"assert",A),
  wffCond(A).

wffDefinition(Term) :-
  isBinary(Term,"=",Hd,Result),
  wffHead(Hd),
  wffExp(Result).

wffEquation(Term) :-
  isBinary(Term,"=>",Hd,Result),
  wffHead(Hd),
  wffExp(Result).

wffClause(Term) :- 
  isBinary(Term,":-",Head,Body),
  wffHead(Head),
  wffCond(Body).
wffClause(Term) :- 
  isBinary(Term,":--",Head,Body),
  wffHead(Head),
  wffCond(Body).
wffClause(Head) :- wffHead(Head).

wffHead(Head) :-
  isBinary(Head,"::",H,C),
  wffHead(H),
  wffCond(C).
wffHead(Head) :-
  isRound(Head,Op,Args),
  \+ isKeyword(Op),
  wffPtns(Args).
wffHead(Head) :- wffIden(Head).

wffLabelRule(Term) :-
  isBinary(Term,"<=",L,R),
  wffHead(L),
  wffLabelReplacement(R).

wffLabelReplacement(Term) :-
  isBinary(Term,"~",L,R),
  wffLabelReplacement(L),
  wffExclusions(R).
wffLabelReplacement(Term) :- 
  wffExp(Term).
wffLabelReplacement(T) :- locOfAst(T,Lc), reportError("Cannot understand label expression %s",[T],Lc).

wffExclusions(Term) :- 
  isSquareTuple(Term,_,A),
  wffPtns(A).

wffClass(Term) :-
  isBinary(Term,"..",L,R),
  wffPtn(L),
  isBraceTuple(R,_,Els),
  wffThetaEnv(Els).

wffExps([]).
wffExps([P|T]) :- wffExp(P), wffExps(T).

wffExpList([]).
wffExpList([T]) :-
  isBinary(T,".,,",L,R),
  wffExp(L),
  wffExp(R).
wffExpList([P|T]) :- wffExp(P), wffExpList(T).

wffExp(name(_,"this")).
wffExp(name(_,"true")).
wffExp(name(_,"false")).
wffExp(name(_,K)) :- \+isKeyword(K).
wffExp(name(Lc,K)) :- isKeyword(K), reportError("unexpected keyword: %s",[K],Lc).
wffExp(integer(_,_)).
wffExp(float(_,_)).
wffExp(string(_,_)).
wffExp(tuple(_,"()",A)) :- wffExps(A).
wffExp(tuple(_,"[]",A)) :- wffExpList(A).
wffExp(tuple(_,"{}",A)) :- wffThetaEnv(A).
wffExp(T) :- isBinary(T,":",L,R), wffExp(L), wffTypeExp(R).
wffExp(T) :- isBinary(T,".",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"#",L,R), wffPackageName(L), wffExp(R).
wffExp(T) :- isBinary(T,"|",L,R), isBinary(L,"?",Tst,Th), wffCond(Tst), wffExp(Th), wffExp(R).
wffExp(T) :- isBinary(T,"=>",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,":-",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"+",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"-",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"*",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"/",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"%",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"%%",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isRoundTerm(T,Op,Args), wffExp(Op), wffExps(Args).
wffExp(T) :- locOfAst(T,Lc), reportError("expression %s not well formed",[T],Lc).

wffStringSegments([]).
wffStringSegments([S|M]) :- wffStringSegment(S), wffStringSegments(M).

wffStringSegment(string(_,_)).
wffStringSegment(display(_,T)) :- wffExp(T).
wffStringSegment(format(_,T,_)) :- wffExp(T).

wffCond(name(_,"true")).
wffCond(name(_,"false")).
wffCond(C) :- isBinary(C,",",L,R), wffCond(L), wffCond(R).
wffCond(C) :- isBinary(C,"|",L,R), isBinary(L,"?",T,Th), wffCond(T),wffCond(Th),wffCond(R).
wffCond(C) :- isBinary(C,"|",L,R), wffCond(L), wffCond(R).
wffCond(C) :- isUnary(C,"!",R), wffCond(R).
wffCond(C) :- isUnary(C,"\\+",R), wffCond(R).
wffCond(C) :- isBinary(C,"*>",L,R), wffCond(L), wffCond(R).
wffCond(C) :- isTuple(C,[Cx]), wffCond(Cx).
wffCond(C) :- isBinary(C,"=",L,R), wffExp(L),wffExp(R).
wffCond(C) :- isBinary(C,"==",L,R), wffExp(L),wffExp(R).
wffCond(C) :- isBinary(C,"\\=",L,R), wffExp(L),wffExp(R).
wffCond(C) :- isBinary(C,"!=",L,R), wffExp(L),wffExp(R).
wffCond(C) :- isBinary(C,"<",L,R), wffExp(L),wffExp(R).
wffCond(C) :- isBinary(C,"=<",L,R), wffExp(L),wffExp(R).
wffCond(C) :- isBinary(C,">",L,R), wffExp(L),wffExp(R).
wffCond(C) :- isBinary(C,">=",L,R), wffExp(L),wffExp(R).
wffCond(C) :- isBinary(C,".=",L,R), wffPtn(L),wffExp(R).
wffCond(C) :- isBinary(C,"=.",L,R), wffExp(L),wffPtn(R).
wffCond(C) :- isBinary(C,"in",L,R), wffPtn(L),wffExp(R).
wffCond(C) :- isRoundTerm(C,Op,Args), wffExp(Op), wffExps(Args).
wffCond(T) :- locOfAst(T,Lc), reportError("Cannot understand condition %s",[T],Lc).

wffPtn(name(_,"this")).
wffPtn(name(_,"true")).
wffPtn(name(_,"false")).
wffPtn(name(_,O)) :- \+ isKeyword(O).
wffPtn(integer(_,_)).
wffPtn(float(_,_)).
wffPtn(string(_,_)).
wffPtn(tuple(_,"()",A)) :- wffPtns(A).
wffPtn(tuple(_,"[]",A)) :- wffPtns(A).
wffPtn(tuple(Lc,"{}",_)) :- reportError("not permitted as pattern",[],Lc).
wffPtn(T) :- isBinary(T,"::",L,R), wffPtn(L), wffCond(R).
wffPtn(T) :- isUnary(T,"@",R), wffCond(R). /* Tau pattern */
wffPtn(T) :- isBinary(T,"#",L,R), wffPackageName(L), wffPtn(R).
wffPtn(T) :- isRoundTerm(T,Op,Args), wffExp(Op), wffPtns(Args).
wffPtn(T) :- locOfAst(T,Lc), reportError("Cannot understand pattern %s",[T],Lc).

wffPtns([]).
wffPtns([T]) :-
  isBinary(T,",..",L,R),
  wffPtn(L),
  wffPtn(R).
wffPtns([P|T]) :- wffPtn(P), wffPtns(T).

wffIden(Nm) :- isName(Nm,_),\+isKeyword(Nm).
wffIden(Id) :- isTuple(Id,[Nm]), isName(Nm,_), \+ isKeyword(Nm).

ifthen(T,Th,_) :- T, !, Th.
ifthen(_,_,El) :- El.

wffGrammarRule(St) :-
  isBinary(St,"-->",L,R),!,
  wffGrammarHead(L),
  wffGrammarNonTermimal(R).

wffGrammarHead(H) :-
  isBinary(H,",",L,R),!,
  wffHead(L),
  wffTerminal(R).
wffGrammarHead(H) :-
  wffHead(H).

wffTerminal(T) :-
  isString(T,_).
wffTerminal(tuple(_,"[]",Els)) :-
  wffExps(Els).

wffGrammarNonTermimal(T) :-
  isTuple(T,[NT]),!,
  wffGrammarNonTermimal(NT).
wffGrammarNonTermimal(T) :-
  wffTerminal(T).
wffGrammarNonTermimal(T) :-
  isBinary(T,",",L,R),
  wffGrammarNonTermimal(L),
  wffGrammarNonTermimal(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"|",L,R),
  isBinary(L,"?",Ts,Th),
  wffGrammarNonTermimal(Ts),
  wffGrammarNonTermimal(Th),
  wffGrammarNonTermimal(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"|",L,R),
  wffGrammarNonTermimal(L),
  wffGrammarNonTermimal(R).
wffGrammarNonTermimal(T) :-
  isUnary(T,"!",R),
  wffGrammarNonTermimal(R).
wffGrammarNonTermimal(T) :-
  isUnary(T,"\\+",R),
  wffGrammarNonTermimal(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"=",L,R),
  wffExp(L),
  wffExp(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"\\=",L,R),
  wffExp(L),
  wffExp(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"==",L,R),
  wffExp(L),
  wffExp(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,".=",L,R),
  wffPtn(L),
  wffExp(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"=.",L,R),
  wffExp(L),
  wffPtn(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"::",L,R),
  wffGrammarNonTermimal(L),
  wffCond(R).
wffGrammarNonTermimal(T) :-
  isBraceTuple(T,_,[El]),
  wffCond(El).
wffGrammarNonTermimal(T) :-
  isRoundTerm(T,_,F,A),
  wffExp(F),
  wffPtns(A).
wffGrammarNonTermimal(T) :-
  isIden(T,"eof").
