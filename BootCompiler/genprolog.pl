:- module(genprolog,[genRules/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genRules(export(Pkg,Imports,Fields,Types,Classes,Rules,Contracts,Impls),Text) :-
  genImports(Imports,Pkg,Chrs,O1),
  genFieldTypes(Fields,Pkg,O1,O2),
  genTypes(Types,Pkg,O2,O3),
  genClasses(Classes,Pkg,O3,O4),
  genContracts(Contracts,Pkg,O4,O5),
  genImpls(Impls,Pkg,O5,O6),
  genPlRules(Rules,O6,[]),
  string_chars(Text,Chrs).

genImports([],_,O,O).
genImports([I|More],Pkg,O,Ox) :-
  genImport(I,Pkg,O,O1),
  genImports(More,Pkg,O1,Ox).

genImport(import(Viz,pkg(PkgImp),Vers,_,_,_,_,_),Pkg,O,Ox) :-
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

genTypes(Fields,Pkg,O,Ox) :-
  localName(Pkg,"#","types",Ex),
  appQuoted(Ex,"'",O,O0),
  appStr("(""",O0,O1),
  encodeType(faceType(Fields),O1,O2),
  appStr(""").\n",O2,Ox).


genClasses(Classes,Pkg,O,Ox) :-
  localName(Pkg,"#","classes",C),
  appQuoted(C,"'",O,O0),
  appStr("(",O0,O1),
  formatClassStructures(Classes,Struct),
  encodeTerm(tpl(Struct),B,[]),
  string_codes(Txt,B),
  appQuoted(Txt,"\"",O1,O2),
  appStr(").\n",O2,Ox).

formatClassStructures([],[]).
formatClassStructures([(Nm,Access,Tp)|M],[tpl([strg(Nm),Access,strg(EncType)])|R]) :-
  encodeType(Tp,Chars,[]),
  string_chars(EncType,Chars),
  formatClassStructures(M,R).

genContracts(Contracts,Pkg,O,Ox) :-
  localName(Pkg,"#","contracts",C),
  appQuoted(C,"'",O,O0),
  appStr("(",O0,O1),
  formatContracts(Contracts,Struct),
  encodeTerm(tpl(Struct),B,[]),
  string_codes(Txt,B),
  appQuoted(Txt,"\"",O1,O2),
  appStr(").\n",O2,Ox).

formatContracts([],[]).
formatContracts([contract(Nm,CnNm,_,FullSpec,Face)|M],[tpl([strg(Nm),strg(CnNm),strg(CSig),strg(FSig)])|R]) :-
  encodeConstraint(FullSpec,CChars,[]),
  string_chars(CSig,CChars),
  encodeType(Face,FChars,[]),
  string_chars(FSig,FChars),
  formatContracts(M,R).

genImpls(Impls,Pkg,O,Ox) :-
  localName(Pkg,"#","implementations",C),
  appQuoted(C,"'",O,O0),
  appStr("(",O0,O1),
  formatImpls(Impls,Struct),
  encodeTerm(tpl(Struct),B,[]),
  string_codes(Txt,B),
  appQuoted(Txt,"\"",O1,O2),
  appStr(").\n",O2,Ox).
  
formatImpls([],[]).
formatImpls([imp(Nm,Spec)|M],[tpl([strg(Nm),strg(Sig)])|R]) :-
  encodeConstraint(Spec,Chars,[]),
  string_chars(Sig,Chars),
  formatImpls(M,R).

genPlRules(Rls,O,Ox) :-
  rfold(Rls,genprolog:genPlRule,O,Ox).

genPlRule(clse(_,Nm,Args,Body),O,Ox) :-
  genTerm(Nm,O,O2),
  appStr("(",O2,O3),
  genTerms(Args,O3,O4),
  appStr(")",O4,O5),
  genBody(Body,O5,O6),
  appStr(".\n",O6,Ox).

genQuants([],O,O) :- !.
genQuants([V|Q],O,Ox) :-
  appStr("all ",O,O1),
  genTerm(V,O1,O2),
  rfold(Q,genprolog:genMoreQuant,O2,O3),
  appStr(" ~~ ",O3,Ox).

genMoreQuant(V,O,Ox) :-
  appStr(", ",O,O0),
  genTerm(V,O0,Ox).

genBody([],O,O).
genBody([G|Body],O,Ox) :-
  appStr(" :- ",O,O0),
  genGoal(G,O0,O1),
  rfold(Body,genprolog:genMoreGoal,O1,Ox).

genMoreGoal(G,O,Ox) :-
  appStr(",\n    ",O,O0),
  genGoal(G,O0,Ox).

genGoal(call(Pr,Args),O,Ox) :-
  genTerm(Pr,O,O0),
  appStr("(",O0,O1),
  genTerms(Args,O1,O2),
  appStr(")",O2,Ox).
genGoal(ecall(Escape,Args),O,Ox) :-
  genQuoted(Escape,O,O0),
  appStr("(",O0,O1),
  genTerms(Args,O1,O2),
  appStr(")",O2,Ox).
genGoal(ocall(Pr,Lbl,This),O,Ox) :-
  appStr("ocall(",O,O1),
  genTerm(Pr,O1,O2),
  appStr(",",O2,O3),
  genTerm(Lbl,O3,O4),
  appStr(",",O4,O5),
  genTerm(This,O5,O6),
  appStr(")",O6,Ox).
genGoal(neck,O,Ox) :-
  appStr("!",O,Ox).
genGoal(fail,O,Ox) :-
  appStr("fail",O,Ox).
genGoal(unify(L,R),O,Ox) :-
  genTerm(L,O,O1),
  appStr(" = ",O1,O2),
  genTerm(R,O2,Ox).
genGoal(match(L,R),O,Ox) :-
  genTerm(L,O,O1),
  appStr(" = ",O1,O2),
  genTerm(R,O2,Ox).
genGoal(raise(_),O,Ox) :-
  appStr("abort",O,Ox).

genTerm(prg(Nm,_),O,Ox) :-
  appQuoted(Nm,'''',O,Ox).
genTerm(strct(Nm,_),O,Ox) :-
  genQuoted(Nm,O,Ox).
genTerm(intgr(Ix),O,Ox) :-
  appInt(Ix,O,Ox).
genTerm(float(Dx),O,Ox) :-
  appFlt(Dx,O,Ox).
genTerm(strg(Str),O,Ox) :-
  appQuoted(Str,"""",O,Ox).
genTerm(anon,O,Ox) :-
  appStr("_",O,Ox).
genTerm(enum(Nm),O,Ox) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,Ox).
genTerm(idnt(Nm),O,Ox) :-
  appStr("X",O,O0),
  genVar(Nm,O0,Ox).
genTerm(cons(Op,Args),O,Ox) :-
  genTerm(Op,O,O1),
  appStr("(",O1,O2),
  genTerms(Args,O2,O3),
  appStr(")",O3,Ox).
genTerm(tpl([]),O,Ox) :-!,
  appQuoted("()","'",O,Ox).
genTerm(tpl(Args),O,Ox) :-
  appStr("(",O,O5),
  genTerms(Args,O5,O6),
  appStr(")",O6,Ox).

genVar(Nm,O,Ox) :-
  string_chars(Nm,Chars),
  filterChars(Chars,O,Ox).

filterChars([],O,O).
filterChars([Ch|M],[Ch|O],Ox) :-
  isLetter(Ch),
  filterChars(M,O,Ox).
filterChars([Ch|M],['_'|O],Ox) :-
  \+isLetter(Ch),
  filterChars(M,O,Ox).

isLetter('_').
isLetter(Ch) :- char_type(Ch,alnum).

genQuoted(Str,O,Ox) :-
  appStr("'",O,O1),
  appStr(Str,O1,O2),
  appStr("'",O2,Ox).

genTerms([],O,O).
genTerms([T|Rest],O,Ox) :-
  genTerm(T,O,O0),
  rfold(Rest,genprolog:genMoreTerm,O0,Ox).

genMoreTerm(G,O,Ox) :-
  appStr(", ",O,O0),
  genTerm(G,O0,Ox).
