:-module(wff,[wffModule/1,isAlgebraicTypeDef/5]).
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
  isBinary(St,"-->",_,_),
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
wffTypeExp(T) :- isBinary(T,"=>",L,R), isTuple(L,A), wffArgTypes(A), wffTypeExp(R).
wffTypeExp(T) :- isBinary(T,"-->",L,R), isTuple(L,A), wffArgTypes(A), wffTypeExp(R).
wffTypeExp(T) :- isBinary(T,"<=>",L,R), isTuple(L,A), wffTypeExps(A), wffTypeExp(R).
wffTypeExp(T) :- isBraceTerm(T,L,[]),isTuple(L,A), wffArgTypes(A).
wffTypeExp(T) :- isBraceTuple(T,_,A), wffFaceTypes(A).
wffTypeExp(T) :- isTuple(T,A), wffTypeExps(A).
wffTypeExp(T) :- isQuantified(T,V,Tp), wffTypeQuants(V,[],_), wffTypeExp(Tp).
wffTypeExp(T) :- locOfAst(T,Lc), reportError("Cannot understand type %s",[T],Lc).

wffTypeQuants(V,Q,Qx) :- isBinary(V,",",L,R), wffTypeQuants(L,Q,Q0), wffTypeQuants(R,Q0,Qx).
wffTypeQuants(V,Q,Qx) :- isBinary(V,"<~",L,R), wffTypeQuants(L,Q,Qx), wffTypeExp(R).
wffTypeQuants(V,Q,Qx) :- isBinary(V,"::",L,R), wffTypeQuants(L,Q,Qx), wffTypeExp(R).
wffTypeQuants(N,Q,[Nm|Q]) :- isIden(N,Nm).

wffFaceTypes([]).
wffFaceTypes([F|A]) :- 
  isTypeAnnotation(F),
  wffFaceTypes(A).
wffFaceTypes([F|A]) :- 
  locOfAst(F,Lc),
  reportError("%s not a valid type annotation",[F],Lc),
  wffFaceTypes(A).

wffTypeExps([]).
wffTypeExps([T|L]) :- wffTypeExp(T), wffTypeExps(L).


wffArgTypes([]).
wffArgTypes([T|L]) :- wffArgType(T), wffArgTypes(L).

wffArgType(T) :- isUnary(T,"+",TT), !, wffTypeExp(TT).
wffArgType(T) :- isUnary(T,"-",TT), !, wffTypeExp(TT).
wffArgType(T) :- wffTypeExp(T).

isPrivate(Term,T,Lc) :- isUnary(Term,"private",T), locOfAst(T,Lc).

isPublic(Term,T,Lc) :- isUnary(Term,"public",T), locOfAst(T,Lc).

isAlgebraicTypeDef(Term,Lc,Quants,Head,Body) :- 
  isQuantified(Term,Quants,Inner),!,
  isBinary(Inner,Lc,"::=",Head,Body),
  wffNamedType(Head),
  wffTypeConstructors(Body).
isAlgebraicTypeDef(Term,Lc,[],Head,Body) :- 
  isBinary(Term,Lc,"::=",Head,Body),
  wffNamedType(Head),
  wffTypeConstructors(Body).

isAlgebraicTypeDef(Term) :- isAlgebraicTypeDef(Term,_,_,_,_).

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

isShow(St) :-
  isUnary(St,"show",A),
  wffTerm(A).


wffDefinition(Term) :-
  isBinary(Term,"=",Hd,Result),
  wffHead(Hd),
  wffTerm(Result).

wffEquation(Term) :-
  isBinary(Term,"=>",Hd,Result),
  wffHead(Hd),
  wffTerm(Result).

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
  wffTerms(Args).
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
  wffTerm(Term).
wffLabelReplacement(T) :- locOfAst(T,Lc), reportError("Cannot understand label expression %s",[T],Lc).

wffExclusions(Term) :- 
  isSquareTuple(Term,_,A),
  wffTerms(A).

wffClass(Term) :-
  isBinary(Term,"..",L,R),
  wffTerm(L),
  isBraceTuple(R,_,Els),
  wffThetaEnv(Els).

wffTerms([]).
wffTerms([P|T]) :- wffTerm(P), wffTerms(T).

wffTermList([]).
wffTermList([T]) :-
  isBinary(T,".,,",L,R),
  wffTerm(L),
  wffTerm(R).
wffTermList([P|T]) :- wffTerm(P), wffTermList(T).

wffTerm(name(_,"this")).
wffTerm(name(_,"true")).
wffTerm(name(_,"false")).
wffTerm(name(_,K)) :- \+isKeyword(K).
wffTerm(name(Lc,K)) :- isKeyword(K), reportError("unexpected keyword: %s",[K],Lc).
wffTerm(integer(_,_)).
wffTerm(float(_,_)).
wffTerm(string(_,_)).
wffTerm(tuple(_,"()",A)) :- wffTerms(A).
wffTerm(tuple(_,"[]",A)) :- wffTermList(A).
wffTerm(tuple(_,"{}",A)) :- wffThetaEnv(A).
wffTerm(T) :- isBinary(T,":",L,R), wffTerm(L), wffTypeExp(R).
wffTerm(T) :- isBinary(T,"::",L,R), wffTerm(L), wffCond(R).
wffTerm(T) :- isBinary(T,".",L,R), wffTerm(L), wffTerm(R).
wffTerm(T) :- isBinary(T,"#",L,R), wffPackageName(L), wffIden(R).
wffTerm(T) :- isUnary(T,"@",R), wffCond(R).
wffTerm(T) :- isBinary(T,"|",L,R), isBinary(L,"?",Tst,Th), wffCond(Tst), wffTerm(Th), wffTerm(R).
wffTerm(T) :- isRoundTerm(T,Op,Args), wffTerm(Op), wffTerms(Args).
wffTerm(T) :- locOfAst(T,Lc), reportError("term %s not well formed",[T],Lc).

wffCond(name(_,"true")).
wffCond(name(_,"false")).
wffCond(C) :- isBinary(C,",",L,R), wffCond(L), wffCond(R).
wffCond(C) :- isBinary(C,"|",L,R), isBinary(L,"?",T,Th), wffCond(T),wffCond(Th),wffCond(R).
wffCond(C) :- isBinary(C,"|",L,R), wffCond(L), wffCond(R).
wffCond(C) :- isUnary(C,"!",R), wffCond(R).
wffCond(C) :- isUnary(C,"\\+",R), wffCond(R).
wffCond(C) :- isBinary(C,"*>",L,R), wffCond(L), wffCond(R).
wffCond(C) :- isTuple(C,Cx), wffConds(Cx).
wffCond(C) :- isBinary(C,"=",L,R), wffTerm(L),wffTerm(R).
wffCond(C) :- isBinary(C,"\\=",L,R), wffTerm(L),wffTerm(R).
wffCond(C) :- isBinary(C,"!=",L,R), wffTerm(L),wffTerm(R).
wffCond(C) :- isBinary(C,"<",L,R), wffTerm(L),wffTerm(R).
wffCond(C) :- isBinary(C,"=<",L,R), wffTerm(L),wffTerm(R).
wffCond(C) :- isBinary(C,">",L,R), wffTerm(L),wffTerm(R).
wffCond(C) :- isBinary(C,">=",L,R), wffTerm(L),wffTerm(R).
wffCond(C) :- isBinary(C,".=",L,R), wffTerm(L),wffTerm(R).
wffCond(C) :- isBinary(C,"=.",L,R), wffTerm(L),wffTerm(R).
wffCond(C) :- isBinary(C,"in",L,R), wffTerm(L),wffTerm(R).
wffCond(T) :- isBinary(T,"%%",L,R), isBinary(R,"~",S,M), wffTerm(S), wffGrammarNonTermimal(L), wffTerm(M).
wffCond(T) :- isBinary(T,"%%",L,R), wffTerm(L), wffTerm(R).
wffCond(C) :- isRoundTerm(C,Op,Args), wffTerm(Op), wffTerms(Args).
wffCond(T) :- locOfAst(T,Lc), reportError("Condition not well formed: %s",[T],Lc).

wffConds([]).
wffConds([C|M]) :- wffCond(C), wffConds(M).

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
  wffTerms(Els).

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
  isUnary(T,"+",R),
  wffGrammarNonTermimal(R).
wffGrammarNonTermimal(T) :-
  isUnary(T,"@",R),
  wffCond(R).
wffGrammarNonTermimal(T) :-
  isUnary(T,"\\+",R),
  wffGrammarNonTermimal(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"=",L,R),
  wffTerm(L),
  wffTerm(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"\\=",L,R),
  wffTerm(L),
  wffTerm(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,".=",L,R),
  wffTerm(L),
  wffTerm(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"=.",L,R),
  wffTerm(L),
  wffTerm(R).
wffGrammarNonTermimal(T) :-
  isBinary(T,"::",L,R),
  wffGrammarNonTermimal(L),
  wffCond(R).
wffGrammarNonTermimal(T) :-
  isBraceTuple(T,_,[El]),
  wffCond(El).
wffGrammarNonTermimal(T) :-
  isRoundTerm(T,_,F,A),
  wffTerm(F),
  wffTerms(A).
wffGrammarNonTermimal(T) :-
  isIden(T,"eof").
wffGrammarNonTermimal(T) :-
  locOfAst(T,Lc),
  reportError("%s not well formed grammar condition",[T],Lc).
