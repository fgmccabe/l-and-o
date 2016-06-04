:- module(dict,[typeInDict/3,typeInDict/4,typeFaceRule/3,
    typeRuleInDict/3,typeRules/3,
    declareVar/5,isVar/3,isVar/4,
    declareType/5,declareType/7,
    pushScope/2,makeKey/2,stdDict/1]).

:- use_module(misc).

typeInDict(Nm,Env,Tp) :- pathSuffix(Nm,Id),tpInDict(Id,Env,tpDef(_,_,Tp,_)).

typeInDict(Nm,Env,Lc,Tp) :- pathSuffix(Nm,Id), tpInDict(Id,Env,tpDef(_,Lc,Tp,_)).

tpInDict(Nm,Env,Tp) :- makeKey(Nm,Key), typeInD(Key,Env,Tp).

typeInD(Key,[scope(Types,_,_)|_],Tp) :- get_dict(Key,Types,Tp),!.
typeInD(Key,[_|Env],Tp) :- typeInD(Key,Env,Tp).

typeRuleInDict(Nm,Env,Rule) :- pathSuffix(Nm,Id),makeKey(Id,Key), typeRuleInD(Key,Env,Rule).

typeRuleInD(Key,[scope(_,_,Rules)|_],Rl) :- get_dict(Key,Rules,Rls),is_member(Rl,Rls).
typeRuleInD(Key,[_|Env],Rl) :- typeRuleInD(Key,Env,Rl).

typeRules(Nm,Env,Rules) :- pathSuffix(Nm,Id),makeKey(Id,Key), findRules(Key,Env,Rules).

findRules(Key,[scope(_,_,Rules)|_],Rls) :-
  get_dict(Key,Rules,Rls),!.
findRules(Key,[_|Env],Rls) :-
  findRules(Key,Env,Rls).

typeFaceRule(Id,Env,Rule) :-
  pathSuffix(Id,Suffix),
  tpInDict(Suffix,Env,tpDef(_,_,_,Rule)).

declareVar(Nm,Vr,Lc,[scope(Types,Names,Rules)|Outer],[scope(Types,Names1,Rules)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Names,vEntry(Vr,Lc),Names1).

isVar(Nm,Env,Vr,Lc) :- makeKey(Nm,Key), isVr(Key,Env,Vr,Lc).
isVar(Nm,Env,Vr) :- isVar(Nm,Env,Vr,_).

isVr(Key,[scope(_,Names,_)|_],Vr,Lc) :- get_dict(Key,Names,vEntry(Vr,Lc)),!.
isVr(Key,[_|Outer],Vr,Lc) :- isVr(Key,Outer,Vr,Lc).

declareType(Nm,Lc,Tp,Face,TpRules,[scope(Types,Nms,Rls)|Outer],[scope(Types1,Nms,Rls1)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Types,tpDef(Nm,Lc,Tp,Face),Types1),
  put_dict(Key,Rls,TpRules,Rls1).

moveQuants(univType(B,Tp),[B|Q],Tmpl) :- !,
  moveQuants(Tp,Q,Tmpl).
moveQuants(Tp,[],Tp).

declareType(Nm,Lc,Tp,[scope(Types,Nms,Rls)|Outer],[scope(Types1,Nms,Rls)|Outer]) :-
  makeKey(Nm,Key),
  moveQuants(Tp,B,Inner),
  moveQuants(FaceRule,B,typeRule(Inner,faceType([]))),
  put_dict(Key,Types,tpDef(Nm,Lc,Tp,FaceRule),Types1).

pushScope(Env,[scope(types{},vars{},rules{})|Env]).

makeKey(Id,Key) :-
  atom_string(Key,Id).

stdDict(Dict) :-
  pushScope([],Base),
  declareType("integer",'',type("lo.std#integer"),Base,B1),
  declareType("long",'',type("lo.std#long"),B1,B2),
  declareType("float",'',type("lo.std#float"),B2,B3),
  declareType("string",'',type("lo.std#string"),B3,B4),
  declareType("logical",'',type("lo.std#logical"),B4,B4a),
  declareType("thing",'',type("lo.std#thing"),B4a,B5),
  declareType("list",'',univType("t",typeExp("lo.std#list",[kVar("t")])),B5,B6),
  declareVar(",..",con("lo.std#,..",univType("t",
    classType([kVar("t"),typeExp("lo.std#list",[kVar("t")])],typeExp("lo.std#list",[kVar("t")])))),'',B6,B7),
  declareVar("[]",enum("lo.std#[]",univType("t",typeExp("lo.std#list",[kVar("t")]))),'',B7,Dict).

