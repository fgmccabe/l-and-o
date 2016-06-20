:- module(canon,[displayType/1,displayCanon/1,showCanon/3,isCanon/1,isAssertion/1]).

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
isCanon(pkgRef(_,_,_)).
isCanon(enum(_,_)).
isCanon(record(_,_,_,_)).
isCanon(true(_)).
isCanon(false(_)).
isCanon(where(_,_)).
isCanon(conj(_,_)).
isCanon(disj(_,_)).
isCanon(conditional(_,_,_)).
isCanon(equals(_,_,_)).
isCanon(unify(_,_,_)).
isCanon(match(_,_,_)).
isCanon(one(_)).
isCanon(neg(_)).
isCanon(forall(_,_)).

isAssertion(assertion(_,_)).

displayCanon(Term) :- showCanon(Term,Chrs,[]), string_chars(Res,Chrs), write(Res).

displayType(Tp) :- showType(Tp,Chrs,[]), string_chars(Res,Chrs), write(Res).

showCanon(prog(Pkg,Imports,Defs,Others,_Fields,Types),O,E) :-
  appStr(Pkg,O,O1),
  appStr("{\n",O1,O2),
  showImports(Imports,O2,O3),
  showTypeDefs(Types,O3,O4),
  showDefs(Defs,O4,O5),
  showOthers(Others,O5,O6),
  appStr("}.\n",O6,E),!.

showTerm(v(_,Nm),O,E) :- appStr(Nm,O,E).
showTerm(intLit(Ix),O,E) :- appInt(Ix,O,E).
showTerm(floatLit(Ix),O,E) :- appInt(Ix,O,E).
showTerm(stringLit(_,Str),O,E) :- 
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,E).
showTerm(apply(_,Op,Args),O,E) :- 
  showTerm(Op,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,E).
showTerm(call(_,Op,Args),O,E) :- 
  showTerm(Op,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,E).
showTerm(dot(_,Rc,Fld),O,E) :-
  showTerm(Rc,O,O1),
  appStr(".",O1,O2),
  appStr(Fld,O2,E).
showTerm(pkgRef(_,Rc,Fld),O,E) :-
  showTerm(Rc,O,O1),
  appStr("#",O1,O2),
  appStr(Fld,O2,E).
showTerm(enum(_,Nm),O,E) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,E).
showTerm(record(_,Defs,Others,Types),O,E) :-
  appStr("{ ",O,O1),
  showTypeDefs(Types,O1,O2),
  showDefs(Defs,O2,O3),
  showOthers(Others,O3,O4),
  appStr(" }",O4,E).
showTerm(true(_),O,E) :-
  appStr("true",O,E).
showTerm(false(_),O,E) :-
  appStr("false",O,E).
showTerm(where(Ptn,Cond),O,E) :-
  appStr("(",O,O0),
  showTerm(Ptn,O0,O1),
  appStr(" :: ",O1,O2),
  showTerm(Cond,O2,O3),
  appStr(")",O3,E).
showTerm(conj(L,R),O,E) :-
  showTerm(L,O,O1),
  appStr(", ",O1,O2),
  showTerm(R,O2,E).
showTerm(disj(_,Either,Or),O,E) :-
  appStr("(",O,O0),
  showTerm(Either,O0,O1),
  appStr(" | ",O1,O2),
  showTerm(Or,O2,O3),
  appStr(")",O3,E).
showTerm(conditional(_,Test,Either,Or),O,E) :-
  appStr("(",O,O1),
  showTerm(Test,O1,O2),
  appStr("?",O2,O3),
  showTerm(Either,O3,O4),
  appStr(" | ",O4,O5),
  showTerm(Or,O5,O6),
  appStr(")",O6,E).
showTerm(equals(_,L,R),O,E) :-
  showTerm(L,O,O1),
  appStr(" = ",O1,O2),
  showTerm(R,O2,E).
showTerm(unify(_,L,R),O,E) :-
  showTerm(L,O,O1),
  appStr(" == ",O1,O2),
  showTerm(R,O2,E).
showTerm(match(_,L,R),O,E) :-
  showTerm(L,O,O1),
  appStr(" .= ",O1,O2),
  showTerm(R,O2,E).
showTerm(one(_,L),O,E) :-
  showTerm(L,O,O1),
  appStr("!",O1,E).
showTerm(neg(_,R),O,E) :-
  appStr("\\+",O,O1),
  showTerm(R,O1,E).
showTerm(forall(_,Gen,Test),O,E) :-
  appStr("(",O,O0),
  showTerm(Gen,O0,O1),
  appStr(" *> ",O1,O2),
  showTerm(Test,O2,O3),
  appStr(")",O3,E).

showTerms([],O,O).
showTerms([T|More],O,E) :-
  showTerm(T,O,O1),
  showMoreTerms(More,O1,E).

showMoreTerms([],O,O).
showMoreTerms([T|More],O,E) :-
  appStr(", ",O,O1),
  showTerm(T,O1,O2),
  showMoreTerms(More,O2,E).

showImports([],O,O).
showImports([import(_,_,Viz,spec(Pkg,Version,_,_,_))|Imports],O,E) :-
  showVisibility(Viz,O,O0),
  appStr("import ",O0,O1),
  appStr(Pkg,O1,O2),
  showVersion(Version,O2,O3),
  appStr(".\n",O3,O4),
  showImports(Imports,O4,E).

showVisibility(private,O,Ox) :-
  appStr("private ",O,Ox).
showVisibility(public,O,Ox) :-
  appStr("public ",O,Ox).

showVersion(defltVersion,O,O).
showVersion(v(V),O,Ox) :-
  appStr(",",O,O1),
  appStr(V,O1,Ox).

showTypeDefs([],O,O).
showTypeDefs([(_,Rules)|More],O,E) :-
  showTypeDef(Rules,O,O1),
  appStr("\n",O1,O2),
  showTypeDefs(More,O2,E).

showTypeDef([],O,O) :- !.
showTypeDef([Rl|Rules],O,E) :-
  showTypeRule(Rl,O,O1),
  appStr(".\n",O1,O2),
  showTypeDef(Rules,O2,E).  

showDefs([],O,O).
showDefs([Stmt|Stmts],O,E) :-
  showDef(Stmt,O,O2),
  appStr("\n",O2,O3),
  showDefs(Stmts,O3,E).

showDef(function(Lc,Nm,Type,Eqns),O,E) :-
  appStr("function: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showEquations(Eqns,O7,E),!.
showDef(predicate(Lc,Nm,Type,Clauses),O,E) :-
  appStr("predicate: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showClauses(Clauses,O7,E),!.
showDef(defn(Lc,Nm,Cond,Tp,Value),O,E) :-
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
  showTerm(Value,O9,O10),
  appStr(".\n",O10,E).
showDef(enum(Lc,Nm,_Type,Rules,Face),O,E) :-
  appStr("enum: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Face,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O6a),
  showClassRules(Rules,O6a,E).
showDef(class(Lc,Nm,_Type,Rules,Face),O,E) :-
  appStr("class: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Face,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O6a),
  showClassRules(Rules,O6a,E).
showDef(grammar(Lc,Nm,Tp,Rules),O,E) :-
  appStr("grammar: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O6a),
  showGrammarRules(Rules,O6a,E).

showDef(typeDef(Lc,Nm,Tp,Rules),O,E) :-
  appStr("type: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showTypeDef(Rules,O7,O8),
  appStr("\n",O8,E).

showClassRules([],O,O).
showClassRules([Rl|Rules],O,E) :-
  showClassRule(Rl,O,O1),
  showClassRules(Rules,O1,E).

showClassRule(labelRule(_,_,Hd,Repl,_),O,E) :-
  showTerm(Hd,O,O1),
  appStr(" <= ",O1,O2),
  showTerm(Repl,O2,O3),
  appStr(".\n",O3,E).
showClassRule(classBody(_,_,Hd,Stmts,Others,Types),O,E) :-
  showTerm(Hd,O,O1),
  appStr(" .. {\n",O1,O2),
  showTypeDefs(Types,O2,O3),
  showDefs(Stmts,O3,O4),
  showOthers(Others,O4,O5),
  appStr("}\n",O5,E).

showEquations([],O,O).
showEquations([Eq|Rest],O,E) :-
  showEq(Eq,O,O1),
  showEquations(Rest,O1,E).

showEq(equation(_,Nm,Args,Cond,Value),O,E) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") ",O3,O4),
  showGuard(Cond,O4,O5),
  appStr(" => ",O5,O6),
  showTerm(Value,O6,O7),
  appStr(".\n",O7,E).

showGuard(true(_),E,E) :- !.
showGuard(C,E,O) :-
  appStr(" :: ",E,E1),
  showTerm(C,E1,O).

showClauses([],O,O).
showClauses([Cl|Rest],O,E) :-
  showClause(Cl,O,O1),
  showClauses(Rest,O1,E).

showClause(clause(_,Nm,Args,Cond,Body),O,E) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") ",O3,O4),
  showGuard(Cond,O4,O5),
  appStr(" :- ",O5,O6),
  showTerm(Body,O6,O7),
  appStr(".\n",O7,E).
showClause(strong(_,Nm,Args,Cond,Body),O,E) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,O4),
  showGuard(Cond,O4,O5),
  appStr(" :-- ",O5,O6),
  showTerm(Body,O6,O7),
  appStr(".\n",O7,E).

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
  showTerm(Rhs,O2,Ox).
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
showNonTerminal(goal(_,Rhs),O,Ox) :-
  appStr("{",O,O1),
  showTerm(Rhs,O1,O2),
  appStr(")",O2,Ox).
showNonTerminal(eof(_),O,Ox) :-
  appStr("eof",O,Ox).
showNonTerminal(NT,O,Ox) :-
  showTerm(NT,O,Ox).

showOthers([],O,O).
showOthers([Stmt|Stmts],O,E) :-
  showStmt(Stmt,O,O2),
  appStr(".\n",O2,O3),
  showOthers(Stmts,O3,E).

showStmt(assertion(_,Cond),O,E) :-
  appStr("  assert ",O,O1),
  showTerm(Cond,O1,E).

