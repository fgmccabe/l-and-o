:- module(canon,[displayType/1,displayCanon/1,showCanon/3,showCanonTerm/3,isCanon/1,isAssertion/1,isShow/1]).

:- use_module(misc).
:- use_module(operators).
:- use_module(types).
:- use_module(location).
:- use_module(uri).

isCanon(prog(_,_,_,_)).
isCanon(v(_,_)).
isCanon(intLit(_)).
isCanon(floatLit(_)).
isCanon(stringLit(_,_)).
isCanon(apply(_,_,_)).
isCanon(call(_,_,_)).
isCanon(dot(_,_,_)).
isCanon(dict(_,_)).
isCanon(pkgRef(_,_,_)).
isCanon(enum(_,_)).
isCanon(record(_,_,_,_)).
isCanon(true(_)).
isCanon(false(_)).
isCanon(where(_,_)).
isCanon(conj(_,_)).
isCanon(disj(_,_)).
isCanon(conditional(_,_,_)).
isCanon(unify(_,_,_)).
isCanon(match(_,_,_)).
isCanon(one(_)).
isCanon(neg(_)).
isCanon(forall(_,_)).

isAssertion(assertion(_,_)).

isShow(show(_,_)).

displayCanon(Term) :- showCanon(Term,Chrs,[]), string_chars(Res,Chrs), write(Res).

displayType(Tp) :- showType(Tp,Chrs,[]), string_chars(Res,Chrs), write(Res).

showCanon(prog(Pkg,Imports,Defs,Others,_Fields,Types),O,Ox) :-
  appStr(Pkg,O,O1),
  appStr("{\n",O1,O2),
  showImports(Imports,O2,O3),!,
  showTypeDefs(Types,O3,O4),!,
  showDefs(Defs,O4,O5),!,
  showOthers(Others,O5,O6),!,
  appStr("}.\n",O6,Ox),!.

showCanonTerm(v(_,Nm),O,Ox) :- appStr(Nm,O,Ox).
showCanonTerm(intLit(Ix),O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(floatLit(Ix),O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(stringLit(_,Str),O,Ox) :- 
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,Ox).
showCanonTerm(apply(_,Op,Args),O,Ox) :- 
  showCanonTerm(Op,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,Ox).
showCanonTerm(call(_,Op,Args),O,Ox) :- 
  showCanonTerm(Op,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,Ox).
showCanonTerm(dot(_,Rc,Fld),O,Ox) :-
  showCanonTerm(Rc,O,O1),
  appStr(".",O1,O2),
  appStr(Fld,O2,Ox).
showCanonTerm(pkgRef(_,Rc,Fld),O,Ox) :-
  showCanonTerm(Rc,O,O1),
  appStr("#",O1,O2),
  appStr(Fld,O2,Ox).
showCanonTerm(enum(_,Nm),O,Ox) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,Ox).
showCanonTerm(record(_,Defs,Others,Types),O,Ox) :-
  appStr("{ ",O,O1),
  showTypeDefs(Types,O1,O2),
  showDefs(Defs,O2,O3),
  showOthers(Others,O3,O4),
  appStr(" }",O4,Ox).
showCanonTerm(tuple(_,Els),O,Ox) :-
  appStr("(",O,O1),
  showTerms(Els,O1,O2),
  appStr(")",O2,Ox).
showCanonTerm(dict(_,Els),O,Ox) :-
  appStr("{",O,O1),
  showEntries(Els,O1,O2),
  appStr("}",O2,Ox).
showCanonTerm(true(_),O,Ox) :-
  appStr("true",O,Ox).
showCanonTerm(false(_),O,Ox) :-
  appStr("false",O,Ox).
showCanonTerm(where(Ptn,Cond),O,Ox) :-
  appStr("(",O,O0),
  showCanonTerm(Ptn,O0,O1),
  appStr(" :: ",O1,O2),
  showCanonTerm(Cond,O2,O3),
  appStr(")",O3,Ox).
showCanonTerm(conj(L,R),O,Ox) :-
  showCanonTerm(L,O,O1),
  appStr(", ",O1,O2),
  showCanonTerm(R,O2,Ox).
showCanonTerm(disj(_,Either,Or),O,Ox) :-
  appStr("(",O,O0),
  showCanonTerm(Either,O0,O1),
  appStr(" | ",O1,O2),
  showCanonTerm(Or,O2,O3),
  appStr(")",O3,Ox).
showCanonTerm(conditional(_,Test,Either,Or),O,Ox) :-
  appStr("(",O,O1),
  showCanonTerm(Test,O1,O2),
  appStr("?",O2,O3),
  showCanonTerm(Either,O3,O4),
  appStr(" | ",O4,O5),
  showCanonTerm(Or,O5,O6),
  appStr(")",O6,Ox).
showCanonTerm(unify(_,L,R),O,Ox) :-
  showCanonTerm(L,O,O1),
  appStr(" = ",O1,O2),
  showCanonTerm(R,O2,Ox).
showCanonTerm(match(_,L,R),O,Ox) :-
  showCanonTerm(L,O,O1),
  appStr(" .= ",O1,O2),
  showCanonTerm(R,O2,Ox).
showCanonTerm(one(_,L),O,Ox) :-
  showCanonTerm(L,O,O1),
  appStr("!",O1,Ox).
showCanonTerm(neg(_,R),O,Ox) :-
  appStr("\\+",O,O1),
  showCanonTerm(R,O1,Ox).
showCanonTerm(forall(_,Gen,Test),O,Ox) :-
  appStr("(",O,O0),
  showCanonTerm(Gen,O0,O1),
  appStr(" *> ",O1,O2),
  showCanonTerm(Test,O2,O3),
  appStr(")",O3,Ox).
showCanonTerm(phrase(_,NT,Strm),O,Ox) :-
  showNonTerminal(NT,O,O1),
  appStr("%%",O1,O2),
  showCanonTerm(Strm,O2,Ox).
showCanonTerm(phrase(_,NT,Strm,Rem),O,Ox) :-
  showCanonTerm(NT,O,O1),
  appStr("%%",O1,O2),
  showCanonTerm(Strm,O2,O3),
  appStr("~~",O3,O4),
  showCanonTerm(Rem,O4,Ox).

showTerms([],O,O).
showTerms([T|More],O,Ox) :-
  showCanonTerm(T,O,O1),
  showMoreTerms(More,O1,Ox).

showMoreTerms([],O,O).
showMoreTerms([T|More],O,Ox) :-
  appStr(", ",O,O1),
  showCanonTerm(T,O1,O2),
  showMoreTerms(More,O2,Ox).

showEntries([],O,O).
showEntries([(Ky,Vl)|M],O,Ox) :-
  showCanonTerm(Ky,O,O1),
  appStr(" -> ",O1,O2),
  showCanonTerm(Vl,O2,O3),
  appStr(". ",O3,O4),
  showEntries(M,O4,Ox).

showImports([],O,O).
showImports([import(Viz,pkg(Pkg),Version,_,_,_)|Imports],O,Ox) :-
  showVisibility(Viz,O,O0),
  appStr("import ",O0,O1),
  appStr(Pkg,O1,O2),
  showVersion(Version,O2,O3),
  appStr(".\n",O3,O4),
  showImports(Imports,O4,Ox).

showVisibility(private,O,Ox) :-
  appStr("private ",O,Ox).
showVisibility(public,O,Ox) :-
  appStr("public ",O,Ox).

showVersion(defltVersion,O,O).
showVersion(v(V),O,Ox) :-
  appStr(",",O,O1),
  appStr(V,O1,Ox).

showTypeDefs([],O,O).
showTypeDefs([(_,Rules)|More],O,Ox) :-
  showTypeDef(Rules,O,O1),
  appStr("\n",O1,O2),
  showTypeDefs(More,O2,Ox).

showTypeDef([],O,O) :- !.
showTypeDef([Rl|Rules],O,Ox) :-
  showTypeRule(Rl,O,O1),
  appStr(".\n",O1,O2),
  showTypeDef(Rules,O2,Ox).  

showDefs([],O,O).
showDefs([Stmt|Stmts],O,Ox) :-
  showDef(Stmt,O,O2),
  appStr("\n",O2,O3),
  showDefs(Stmts,O3,Ox).

showDef(function(Lc,Nm,Type,Eqns),O,Ox) :-
  appStr("function: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showEquations(Eqns,O7,Ox),!.
showDef(predicate(Lc,Nm,Type,Clauses),O,Ox) :-
  appStr("predicate: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showClauses(Clauses,O7,Ox),!.
showDef(defn(Lc,Nm,Cond,Tp,Value),O,Ox) :-
  appStr("var definition: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  appStr("Nm",O7,O7a),
  showGuard(Cond,O7a,O8),
  appStr(" = ",O8,O9),
  showCanonTerm(Value,O9,O10),
  appStr(".\n",O10,Ox).
showDef(enum(Lc,Nm,_Type,Rules,Face),O,Ox) :-
  appStr("enum: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Face,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O6a),
  showClassRules(Rules,O6a,Ox).
showDef(class(Lc,Nm,_Type,Rules,Face),O,Ox) :-
  appStr("class: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Face,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O6a),
  showClassRules(Rules,O6a,Ox).
showDef(grammar(Lc,Nm,Tp,Rules),O,Ox) :-
  appStr("grammar: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O6a),
  showGrammarRules(Rules,O6a,Ox).

showDef(typeDef(Lc,Nm,Tp,Rules),O,Ox) :-
  appStr("type: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showTypeDef(Rules,O7,O8),
  appStr("\n",O8,Ox).

showClassRules([],O,O).
showClassRules([Rl|Rules],O,Ox) :-
  showClassRule(Rl,O,O1),
  showClassRules(Rules,O1,Ox).

showClassRule(labelRule(_,_,Hd,Repl,_),O,Ox) :-
  showCanonTerm(Hd,O,O1),
  appStr(" <= ",O1,O2),
  showCanonTerm(Repl,O2,O3),
  appStr(".\n",O3,Ox).
showClassRule(classBody(_,_,Hd,Stmts,Others,Types),O,Ox) :-
  showCanonTerm(Hd,O,O1),
  appStr(" .. {\n",O1,O2),
  showTypeDefs(Types,O2,O3),
  showDefs(Stmts,O3,O4),
  showOthers(Others,O4,O5),
  appStr("}\n",O5,Ox).

showEquations([],O,O).
showEquations([Eq|Rest],O,Ox) :-
  showEq(Eq,O,O1),
  showEquations(Rest,O1,Ox).

showEq(equation(_,Nm,Args,Cond,Value),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") ",O3,O4),
  showGuard(Cond,O4,O5),
  appStr(" => ",O5,O6),
  showCanonTerm(Value,O6,O7),
  appStr(".\n",O7,Ox).

showGuard(true(_),O,O) :- !.
showGuard(C,O,Ox) :-
  appStr(" :: ",O,O1),
  showCanonTerm(C,O1,Ox).

showClauses([],O,O).
showClauses([Cl|Rest],O,Ox) :-
  showClause(Cl,O,O1),
  showClauses(Rest,O1,Ox).

showClause(clause(_,Nm,Args,Cond,Body),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") ",O3,O4),
  showGuard(Cond,O4,O5),
  appStr(" :- ",O5,O6),
  showCanonTerm(Body,O6,O7),
  appStr(".\n",O7,Ox).
showClause(strong(_,Nm,Args,Cond,Body),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,O4),
  showGuard(Cond,O4,O5),
  appStr(" :-- ",O5,O6),
  showCanonTerm(Body,O6,O7),
  appStr(".\n",O7,Ox).

showGrammarRules([],O,O).
showGrammarRules([Rl|Rules],O,Ox) :-
  showGrammarRule(Rl,O,O1),
  showGrammarRules(Rules,O1,Ox).

showGrammarRule(grammarRule(_,Nm,Args,PB,Body),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") ",O3,O4),
  showPushBack(PB,O4,O5),
  appStr(" --> ",O5,O6),
  showNonTerminal(Body,O6,O7),
  appStr(".\n",O7,Ox).

showPushBack([],O,O).
showPushBack(Els,O,Ox) :-
  appStr("[",O,O1),
  showTerms(Els,O1,O2),
  appStr("]",O2,Ox).

showNonTerminal(terminals(_,Terms),O,Ox) :-
  showPushBack(Terms,O,Ox).
showNonTerminal(conj(_,Lhs,Rhs),O,Ox) :-
  showNonTerminal(Lhs,O,O1),
  appStr(", ",O1,O2),
  showNonTerminal(Rhs,O2,Ox).
showNonTerminal(guard(_,Lhs,Rhs),O,Ox) :-
  showNonTerminal(Lhs,O,O1),
  appStr(" :: ",O1,O2),
  showCanonTerm(Rhs,O2,Ox).
showNonTerminal(disj(_,Lhs,Rhs),O,Ox) :-
  appStr("(",O,O0),
  showNonTerminal(Lhs,O0,O1),
  appStr(" | ",O1,O2),
  showNonTerminal(Rhs,O2,O3),
  appStr(")",O3,Ox).
showNonTerminal(conditional(_,Test,Lhs,Rhs),O,Ox) :-
  appStr("(",O,O0),
  showNonTerminal(Test,O0,O1),
  appStr("?",O1,O2),
  showNonTerminal(Lhs,O2,O3),
  appStr(" | ",O3,O4),
  showNonTerminal(Rhs,O4,O5),
  appStr(")",O5,Ox).
showNonTerminal(one(_,Rhs),O,Ox) :-
  appStr("(",O,O1),
  appStr("!",O1,O2),
  showNonTerminal(Rhs,O2,O3),
  appStr(")",O3,Ox).
showNonTerminal(neg(_,Rhs),O,Ox) :-
  appStr("(",O,O1),
  appStr("\\+",O1,O2),
  showNonTerminal(Rhs,O2,O3),
  appStr(")",O3,Ox).
showNonTerminal(ahead(_,Rhs),O,Ox) :-
  appStr("(",O,O1),
  showNonTerminal(Rhs,O1,O2),
  appStr(")+",O2,Ox).
showNonTerminal(dip(_,_,Rhs),O,Ox) :-
  appStr("@ ",O,O1),
  showNonTerminal(Rhs,O1,Ox).
showNonTerminal(goal(_,Rhs),O,Ox) :-
  appStr("{",O,O1),
  showCanonTerm(Rhs,O1,O2),
  appStr(")",O2,Ox).
showNonTerminal(eof(_),O,Ox) :-
  appStr("eof",O,Ox).
showNonTerminal(NT,O,Ox) :-
  showCanonTerm(NT,O,Ox).

showOthers([],O,O).
showOthers([Stmt|Stmts],O,Ox) :-
  showStmt(Stmt,O,O2),
  appStr(".\n",O2,O3),
  showOthers(Stmts,O3,Ox).

showStmt(assertion(_,Cond),O,Ox) :-
  appStr("  assert ",O,O1),
  showCanonTerm(Cond,O1,Ox).

showStmt(show(_,Exp),O,Ox) :-
  appStr("  show ",O,O1),
  showCanonTerm(Exp,O1,Ox).

