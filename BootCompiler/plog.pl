:- module(plog,[displayPlRules/1]).

:- use_module(misc).
:- use_module(types).

displayPlRules(export(_,Imports,Fields,Types,Rules)) :-
  showImports(Imports,Chrs,O1),
  showType(faceType(Fields),O1,O2),
  appStr(".\n",O2,O2a),
  showTypes(Types,O2a,O3),
  showPlRules(Rules,O3,[]),
  string_chars(Text,Chrs),
  write(Text).

showImports([],O,O).
showImports([I|More],O,Ox) :-
  showImport(I,O,O1),
  showImports(More,O1,Ox).

showImport(import(Viz,pkg(Pkg),_,_,_),O,Ox) :-
  showViz(Viz,O,O1),
  appStr(Pkg,O1,O2),
  appStr(".\n",O2,Ox).

showViz(private,O,Ox) :-
  appStr("private import ",O,Ox).
showViz(public,O,Ox) :-
  appStr("public import ",O,Ox).

showTypes(Types,O,Ox) :-
  formatTypeRules(Types,Fields),
  showType(faceType(Fields),O,O1),
  appStr(".\n",O1,Ox).

formatTypeRules([],[]).
formatTypeRules([(Nm,Rules)|More],[(Nm,tupleType(Rules))|Out]) :-
  formatTypeRules(More,Out).

showPlRules(Rls,O,Ox) :-
  rfold(Rls,plog:showPlRule,O,Ox).

showPlRule(clse(Q,Nm,Args,Body),O,Ox) :-
  showQuants(Q,O,O1),
  showTerm(Nm,O1,O2),
  appStr("(",O2,O3),
  showTerms(Args,O3,O4),
  appStr("):-",O4,O5),
  showBody(Body,O5,O6),
  appStr(".\n",O6,Ox).

showQuants([],O,O) :- !.
showQuants([V|Q],O,Ox) :-
  appStr("all ",O,O1),
  showTerm(V,O1,O2),
  rfold(Q,plog:showMoreQuant,O2,O3),
  appStr(" ~~ ",O3,Ox).

showMoreQuant(V,O,Ox) :-
  appStr(", ",O,O0),
  showTerm(V,O0,Ox).

showBody([],O,O).
showBody([G|Body],O,Ox) :-
  showGoal(G,O,O0),
  rfold(Body,plog:showMoreGoal,O0,Ox).

showMoreGoal(G,O,Ox) :-
  appStr(", ",O,O0),
  showGoal(G,O0,Ox).

showGoal(call(Pr,Args),O,Ox) :-
  showTerm(Pr,O,O0),
  appStr("(",O0,O1),
  showTerms(Args,O1,O2),
  appStr(")",O2,Ox).
showGoal(ecall(Pr,Args),O,Ox) :-
  appStr("escape ",O,O1),
  appStr(Pr,O1,O2),
  appStr("(",O2,O3),
  showTerms(Args,O3,O4),
  appStr(")",O4,Ox).
showGoal(ocall(Pr,Lbl,This),O,Ox) :-
  appStr("ocall(",O,O1),
  showTerm(Pr,O1,O2),
  appStr(",",O2,O3),
  showTerm(Lbl,O3,O4),
  appStr(",",O4,O5),
  showTerm(This,O5,O6),
  appStr(")",O6,Ox).
showGoal(neck,O,Ox) :-
  appStr("!",O,Ox).
showGoal(fail,O,Ox) :-
  appStr("fail",O,Ox).
showGoal(unify(L,R),O,Ox) :-
  showTerm(L,O,O1),
  appStr(" = ",O1,O2),
  showTerm(R,O2,Ox).
showGoal(equals(L,R),O,Ox) :-
  showTerm(L,O,O1),
  appStr(" == ",O1,O2),
  showTerm(R,O2,Ox).
showGoal(match(L,R),O,Ox) :-
  showTerm(L,O,O1),
  appStr(" .= ",O1,O2),
  showTerm(R,O2,Ox).
showGoal(raise(T),O,Ox) :-
  appStr("raise ",O,O2),
  showTerm(T,O2,Ox).

showTerm(prg(Nm,Ar),O,Ox) :-
  appStr(Nm,O,O0),
  appStr("/",O0,O1),
  appInt(Ar,O1,Ox).
showTerm(strct(Nm,Ar),O,Ox) :-
  appStr("[",O,O0),
  appStr(Nm,O0,O1),
  appStr("/",O1,O2),
  appInt(Ar,O2,O3),
  appStr("]",O3,Ox).
showTerm(intgr(Ix),O,Ox) :-
  appInt(Ix,O,Ox).
showTerm(float(Dx),O,Ox) :-
  appFlt(Dx,O,Ox).
showTerm(strg(Str),O,Ox) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,Ox).
showTerm(enum(Nm),O,Ox) :-
  appStr(Nm,O,Ox).
showTerm(idnt(Nm),O,Ox) :-
  appStr("?",O,O0),
  appStr(Nm,O0,Ox).
showTerm(cons(Op,Args),O,Ox) :-
  showTerm(Op,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,Ox).
showTerm(tpl(Args),O,Ox) :-
  appStr("(",O,O1),
  showTerms(Args,O1,O2),
  appStr(")",O2,Ox).
showTerm(anon,O,Ox) :-
  appStr("_",O,Ox).

showTerms([],O,O).
showTerms([T|Rest],O,Ox) :-
  showTerm(T,O,O0),
  rfold(Rest,plog:showMoreTerm,O0,Ox).

showMoreTerm(G,O,Ox) :-
  appStr(", ",O,O0),
  showTerm(G,O0,Ox).
