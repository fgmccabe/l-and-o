:- module(canon,[displayType/1,displayCanon/1,showCanon/3,isCanon/1]).

:- use_module(misc).
:- use_module(operators).
:- use_module(types).
:- use_module(location).

isCanon(prog(_,_,_,_)).
isCanon(con(_,_)).

displayCanon(Term) :- showCanon(Term,Chrs,[]), string_chars(Res,Chrs), write(Res).

displayType(Tp) :- showType(Tp,Chrs,[]), string_chars(Res,Chrs), write(Res).

showCanon(prog(Pkg,Imports,Defs,Others,_Fields,Types),O,E) :-
  appStr(Pkg,O,O1),
  appStr("{\n",O1,O2),
  showImports(Imports,O2,O3),
  showTypeDefs(Types,O3,O4),
  showDefs(Defs,O4,O5),
  showOthers(Others,O5,O6),
  appStr("}",O6,E).

showTerm(vr(_,Nm),O,E) :- appStr(Nm,O,E).
showTerm(intLit(_,Ix),O,E) :- appInt(Ix,O,E).
showTerm(longLit(_,Ix),O,E) :- appInt(Ix,O,E).
showTerm(floatLit(_,Ix),O,E) :- appInt(Ix,O,E).
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
showTerm(con(_,Nm),O,E) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,E).
showTerm(enum(_,Nm),O,E) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,E).
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
showTerm(disj(Either,Or),O,E) :-
  appStr("(",O,O0),
  showTerm(Either,O0,O1),
  appStr(" | ",O1,O2),
  showTerm(Or,O2,O3),
  appStr(")",O3,E).
showTerm(conditional(Test,Either,Or),O,E) :-
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
showTerm(one(L),O,E) :-
  showTerm(L,O,O1),
  appStr("!",O1,E).
showTerm(neg(R),O,E) :-
  appStr("\\+",O,O1),
  showTerm(R,O1,E).
showTerm(forall(Gen,Test),O,E) :-
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
showImports([import(_,Pkg)|Imports],O,E) :-
  appStr("  import ",O,O1),
  appStr(Pkg,O1,O2),
  appStr(".\n",O2,O3),
  showImports(Imports,O3,E).

showTypeDefs([],O,O).
showTypeDefs([(_,Rules)|More],O,E) :-
  showTypeDef(Rules,O,O1),
  showTypeDefs(More,O1,E).

showTypeDef([],O,O) :- !.
showTypeDef([Rl|Rules],O,E) :-
  showTypeRule(Rl,O,O1),
  showTypeDef(Rules,O1,E).  

showDefs([],O,O).
showDefs([Stmt|Stmts],O,E) :-
  showDef(Stmt,O,O2),
  appStr("\n",O2,O3),
  showDefs(Stmts,O3,E).

showDef(function(Lc,Nm,Type,Eqns),O,E) :-
  appStr("function: ",O,O1),
  appStr(Nm,O1,O2),
  appStr("|:",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showEquations(Eqns,O7,E),!.
showDef(predicate(Lc,Nm,Type,Clauses),O,E) :-
  appStr("predicate: ",O,O1),
  appStr(Nm,O1,O2),
  appStr("|:",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showClauses(Clauses,O7,E),!.
showDef(class(Lc,Nm,Type,Rules),O,E) :-
  appStr("class: ",O,O1),
  appStr(Nm,O1,O2),
  appStr("|:",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O6a),
  showClassRules(Rules,O6a,E).
showDef(typeDef(Nm,Lc,Rules),O,E) :-
  appStr("type: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(" @ ",O2,O3),
  showLocation(Lc,O3,O4),
  showTypeDef(Rules,O4,E).

showClassRules([],O,O).
showClassRules([Rl|Rules],O,E) :-
  showClassRule(Rl,O,O1),
  showClassRules(Rules,O1,E).

showClassRule(labelRule(_,Nm,Args,Cond,Repl),O,E) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") :: ",O3,O4),
  showTerm(Cond,O4,O5),
  appStr(" <= ",O5,O6),
  showTerm(Repl,O6,O7),
  appStr(".\n",O7,E).
showClassRule(enumRule(_,Nm,_,Repl),O,E) :-
  appStr(Nm,O,O1),
  appStr(" <= ",O1,O2),
  showTerm(Repl,O2,O3),
  appStr(".\n",O3,E).
showClassRule(enumBody(_,Nm,_,Stmts,Others,Types),O,E) :-
  appStr(Nm,O,O1),
  appStr(" .. {\n",O1,O2),
  showTypeDefs(Types,O2,O3),
  showDefs(Stmts,O3,O4),
  showOthers(Others,O4,O5),
  appStr("}\n",O5,E).
showClassRule(classBody(_,Nm,Args,Cond,Stmts,Others,Types),O,E) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") :: ",O3,O4),
  showTerm(Cond,O4,O5),
  appStr(" .. {\n",O5,O6),
  showTypeDefs(Types,O6,O7),
  showDefs(Stmts,O7,O8),
  showOthers(Others,O8,O9),
  appStr("}\n",O9,E).

showEquations([],O,O).
showEquations([Eq|Rest],O,E) :-
  showEq(Eq,O,O1),
  showEquations(Rest,O1,E).

showEq(equation(_,Nm,Args,Cond,Value),O,E) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") :: ",O3,O4),
  showTerm(Cond,O4,O5),
  appStr(" => ",O5,O6),
  showTerm(Value,O6,O7),
  appStr(".\n",O7,E).

showClauses([],O,O).
showClauses([Cl|Rest],O,E) :-
  showClause(Cl,O,O1),
  showClauses(Rest,O1,E).

showClause(clause(_,Nm,Args,Cond,Body),O,E) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") :: ",O3,O4),
  showTerm(Cond,O4,O5),
  appStr(" :- ",O5,O6),
  showTerm(Body,O6,O7),
  appStr(".\n",O7,E).
showClause(strong(_,Nm,Args,Cond,Body),O,E) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") :: ",O3,O4),
  showTerm(Cond,O4,O5),
  appStr(" :-- ",O5,O6),
  showTerm(Body,O6,O7),
  appStr(".\n",O7,E).


showOthers([],O,O).
showOthers([Stmt|Stmts],O,E) :-
  showStmt(Stmt,O,O2),
  appStr(".\n",O2,O3),
  showOthers(Stmts,O3,E).

showStmt(assertion(_,Cond),O,E) :-
  appStr("  assert ",O,O1),
  showTerm(Cond,O1,E).

