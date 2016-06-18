:- module(plog,[displayPlRules/1]).

:- use_module(misc).
:- use_module(types).

displayPlRules(export(_,Imports,Fields,_,Rules)) :-
  showImports(Imports,Chrs,O1),
  showType(faceType(Fields),O1,O2),
  showPlRules(Rules,O2,[]),
  string_chars(Text,Chrs),
  write(Text).

showImports([],O,O).
showImports([I|More],O,Ox) :-
  showImport(I,O,O1),
  showImports(More,O1,Ox).

showImport(import(_,Pkg,Mode,_),O,Ox) :-
  showMode(Mode,O,O1),
  appStr(Pkg,O1,Ox).

showMode(private,O,Ox) :-
  appStr("private ",O,Ox).
showMode(public,O,Ox) :-
  appStr("public ",O,Ox).  


showPlRules(Rls,O,E) :-
  rfold(Rls,plog:showPlRule,O,E).

showPlRule(clse(Q,Nm,Args,Body),O,E) :-
  showQuants(Q,O,O1),
  showTerm(Nm,O1,O2),
  appStr("(",O2,O3),
  showTerms(Args,O3,O4),
  appStr("):-",O4,O5),
  showBody(Body,O5,O6),
  appStr(".\n",O6,E).

showQuants([],O,O) :- !.
showQuants([V|Q],O,E) :-
  appStr("all ",O,O1),
  showTerm(V,O1,O2),
  rfold(Q,plog:showMoreQuant,O2,O3),
  appStr(" ~~ ",O3,E).

showMoreQuant(V,O,E) :-
  appStr(", ",O,O0),
  showTerm(V,O0,E).

showBody([],O,O).
showBody([G|Body],O,E) :-
  showGoal(G,O,O0),
  rfold(Body,plog:showMoreGoal,O0,E).

showMoreGoal(G,O,E) :-
  appStr(", ",O,O0),
  showGoal(G,O0,E).

showGoal(call(Pr,Args),O,E) :-
  showTerm(Pr,O,O0),
  appStr("(",O0,O1),
  showTerms(Args,O1,O2),
  appStr(")",O2,E).
showGoal(ocall(Pr,Lbl,This),O,E) :-
  appStr("ocall(",O,O1),
  showTerm(Pr,O1,O2),
  appStr(",",O2,O3),
  showTerm(Lbl,O3,O4),
  appStr(",",O4,O5),
  showTerm(This,O5,O6),
  appStr(")",O6,E).
showGoal(neck,O,E) :-
  appStr("!",O,E).
showGoal(fail,O,E) :-
  appStr("fail",O,E).
showGoal(equals(L,R),O,E) :-
  showTerm(L,O,O1),
  appStr(" = ",O1,O2),
  showTerm(R,O2,E).
showGoal(raise(T),O,E) :-
  appStr("raise ",O,O2),
  showTerm(T,O2,E).

showTerm(prg(Nm,Ar),O,E) :-
  appStr(Nm,O,O0),
  appStr("/",O0,O1),
  appInt(Ar,O1,E).
showTerm(strct(Nm,Ar),O,E) :-
  appStr("[",O,O0),
  appStr(Nm,O0,O1),
  appStr("/",O1,O2),
  appInt(Ar,O2,O3),
  appStr("]",O3,E).
showTerm(intgr(Ix),O,E) :-
  appInt(Ix,O,E).
showTerm(strg(Str),O,E) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,E).
showTerm(enum(Nm),O,E) :-
  appStr(Nm,O,E).
showTerm(idnt(Nm),O,E) :-
  appStr("?",O,O0),
  appStr(Nm,O0,E).
showTerm(cons(Op,Args),O,E) :-
  showTerm(Op,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,E).
showTerm(tpl(Args),O,E) :-
  appStr("(",O,O1),
  showTerms(Args,O1,O2),
  appStr(")",O2,E).

showTerms([],O,O).
showTerms([T|Rest],O,E) :-
  showTerm(T,O,O0),
  rfold(Rest,plog:showMoreTerm,O0,E).

showMoreTerm(G,O,E) :-
  appStr(", ",O,O0),
  showTerm(G,O0,E).

