:- module(genprolog,[genRules/2]).

:- use_module(misc).

genRules(Out,Rls) :-
  genPlRules(Rls,Chrs,[]),
  string_chars(Res,Chrs), 
  write(Out,Res).

genPlRules(Rls,O,E) :-
  rfold(Rls,genprolog:genPlRule,O,E).

genPlRule(clse(_,Nm,Args,Body),O,E) :-
  genTerm(Nm,O,O2),
  appStr("(",O2,O3),
  genTerms(Args,O3,O4),
  appStr(")",O4,O5),
  genBody(Body,O5,O6),
  appStr(".\n",O6,E).

genQuants([],O,O) :- !.
genQuants([V|Q],O,E) :-
  appStr("all ",O,O1),
  genTerm(V,O1,O2),
  rfold(Q,genprolog:genMoreQuant,O2,O3),
  appStr(" ~~ ",O3,E).

genMoreQuant(V,O,E) :-
  appStr(", ",O,O0),
  genTerm(V,O0,E).

genBody([],O,O).
genBody([G|Body],O,E) :-
  appStr(" :- ",O,O0),
  genGoal(G,O0,O1),
  rfold(Body,genprolog:genMoreGoal,O1,E).

genMoreGoal(G,O,E) :-
  appStr(",\n    ",O,O0),
  genGoal(G,O0,E).

genGoal(call(Pr,Args),O,E) :-
  genTerm(Pr,O,O0),
  appStr("(",O0,O1),
  genTerms(Args,O1,O2),
  appStr(")",O2,E).
genGoal(ocall(Pr,Lbl,This),O,E) :-
  appStr("ocall(",O,O1),
  genTerm(Pr,O1,O2),
  appStr(",",O2,O3),
  genTerm(Lbl,O3,O4),
  appStr(",",O4,O5),
  genTerm(This,O5,O6),
  appStr(")",O6,E).
genGoal(neck,O,E) :-
  appStr("!",O,E).
genGoal(fail,O,E) :-
  appStr("fail",O,E).
genGoal(equals(L,R),O,E) :-
  genTerm(L,O,O1),
  appStr(" = ",O1,O2),
  genTerm(R,O2,E).
genGoal(raise(_),O,E) :-
  appStr("abort",O,E).

genTerm(prg(Nm,_),O,E) :-
  appStr("'",O,O0),
  appStr(Nm,O0,O1),
 % appStr("/",O1,O2),
 % appInt(Ar,O2,O3),
  appStr("'",O1,E).
genTerm(strct(Nm,_),O,E) :-
  appStr("'",O,O0),
  appStr(Nm,O0,O1),
  appStr("'",O1,E).
genTerm(intgr(Ix),O,E) :-
  appInt(Ix,O,E).
genTerm(strg(Str),O,E) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,E).
genTerm(anon,O,E) :-
  appStr("_",O,E).
genTerm(enum("lo.std#[]"),O,E) :- !,
  appStr("[]",O,E).
genTerm(enum(Nm),O,E) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,E).
genTerm(idnt(Nm),O,E) :-
  appStr("X",O,O0),
  appStr(Nm,O0,E).
genTerm(cons(strct("lo.std#,..",2),[A,B]),O,E) :- !, genList(cons(strct("lo.std#,..",2),[A,B]),O,E).
genTerm(cons(Op,Args),O,E) :-
  genTerm(Op,O,O1),
  appStr("(",O1,O2),
  genTerms(Args,O2,O3),
  appStr(")",O3,E).
genTerm(tpl(Args),O,E) :-
  appStr("(",O,O5),
  genTerms(Args,O5,O6),
  appStr(")",O6,E).

genList(cons(strct("lo.std#,..",2),[A,B]),O,E) :-
  appStr("[",O,O1),
  genTerm(A,O1,O2),
  genMoreList(B,O2,E).

genMoreList(enum("lo.std#[]"),O,E) :-
  appStr("]",O,E).
genMoreList(cons(strct("lo.std#,..",2),[A,B]),O,E) :- !,
  appStr(",",O,O1),
  genTerm(A,O1,O2),
  genMoreList(B,O2,E).
genMoreList(A,O,E) :-
  appStr(" | ",O,O1),
  genTerm(A,O1,O2),
  appStr("]",O2,E).

genTerms([],O,O).
genTerms([T|Rest],O,E) :-
  genTerm(T,O,O0),
  rfold(Rest,genprolog:genMoreTerm,O0,E).

genMoreTerm(G,O,E) :-
  appStr(", ",O,O0),
  genTerm(G,O0,E).

