:- module(genprolog,[genRules/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genRules(export(Pkg,Imports,Fields,Types,Rules),Text) :-
  genImports(Imports,Pkg,Chrs,O1),
  genFieldTypes(Fields,Pkg,O1,O2),
  genTypes(Types,Pkg,O2,O3),
  genPlRules(Rules,O3,[]),
  string_chars(Text,Chrs).

genImports([],_,O,O).
genImports([I|More],Pkg,O,Ox) :-
  genImport(I,Pkg,O,O1),
  genImports(More,Pkg,O1,Ox).

genImport(import(Viz,pkg(PkgImp),Vers,_,_),Pkg,O,Ox) :-
  localName(Pkg,"#","import",Imp),
  appQuoted(Imp,"'",O,O1),
  appStr("(",O1,O2),
  genViz(Viz,O2,O3),
  appStr(",",O3,O4),
  appQuoted(PkgImp,"""",O4,O6),
  appStr(",",O6,O7),
  genVer(Vers,O7,O8),
  appStr(").\n",O8,Ox).

genVer(defltVersion,O,Ox) :-
  appStr("'*'",O,Ox).
genVer(v(V),O,Ox) :-
  appQuoted(V,"""",O,Ox).

genViz(private,O,Ox) :-
  appStr("private",O,Ox).
genViz(public,O,Ox) :-
  appStr("public",O,Ox).

genFieldTypes(Fields,Pkg,O,Ox) :-
  localName(Pkg,"#","export",Ex),
  appQuoted(Ex,"'",O,O0),
  appStr("(""",O0,O1),
  encodeType(faceType(Fields),O1,O2),
  appStr(""").\n",O2,Ox).

genTypes(Types,Pkg,O,Ox) :-
  localName(Pkg,"#","types",Ex),
  appQuoted(Ex,"'",O,O0),
  appStr("(""",O0,O1),
  formatTypeRules(Types,Fields),
  encodeType(faceType(Fields),O1,O2),
  appStr(""").\n",O2,Ox).

formatTypeRules([],[]).
formatTypeRules([(Nm,Rules)|More],[(Nm,tupleType(Rules))|Out]) :-
  formatTypeRules(More,Out).

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
genGoal(ecall(Escape,Args),O,E) :-
  genQuoted(Escape,O,O0),
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
  genQuoted(Nm,O,E).
genTerm(intgr(Ix),O,E) :-
  appInt(Ix,O,E).
genTerm(strg(Str),O,Ox) :-
  appQuoted(Str,"""",O,Ox).
genTerm(anon,O,E) :-
  appStr("_",O,E).
genTerm(enum(Nm),O,E) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,E).
genTerm(idnt(Nm),O,E) :-
  appStr("X",O,O0),
  appStr(Nm,O0,E).
genTerm(cons(Op,Args),O,E) :-
  genTerm(Op,O,O1),
  appStr("(",O1,O2),
  genTerms(Args,O2,O3),
  appStr(")",O3,E).
genTerm(tpl(Args),O,E) :-
  appStr("(",O,O5),
  genTerms(Args,O5,O6),
  appStr(")",O6,E).

genQuoted(Str,O,Ox) :-
  appStr("'",O,O1),
  appStr(Str,O1,O2),
  appStr("'",O2,Ox).

genTerms([],O,O).
genTerms([T|Rest],O,E) :-
  genTerm(T,O,O0),
  rfold(Rest,genprolog:genMoreTerm,O0,E).

genMoreTerm(G,O,E) :-
  appStr(", ",O,O0),
  genTerm(G,O0,E).
