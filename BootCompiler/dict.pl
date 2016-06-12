:- module(dict,[typeInDict/4,typeFaceRule/3,typeRules/3,declareType/5,declareType/7,
    declareVar/5,isVar/3,isVar/4,
    processNames/3,processTypes/3,
    pushScope/2,pushFace/3,makeKey/2,stdDict/1,marker/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(escapes).

typeInDict(Nm,Env,Tp) :- tpInDict(Nm,Env,tpDef(_,_,Tp,_,_)).

typeInDict(Nm,Env,Lc,Tp) :- tpInDict(Nm,Env,tpDef(_,Lc,Tp,_,_)).

typeFaceRule(Nm,Env,Rule) :- tpInDict(Nm,Env,tpDef(_,_,_,Rule,_)).

typeRules(Nm,Env,Rules) :- tpInDict(Nm,Env,tpDef(_,_,_,_,Rules)).

tpInDict(Nm,Env,Tp) :- 
  marker(type,M),
  pathSuffix(Nm,M,Id),
  makeKey(Id,Key),
  typeInD(Key,Env,Tp).

typeInD(Key,[scope(Types,_,_)|_],Tp) :- get_dict(Key,Types,Tp),!.
typeInD(Key,[_|Env],Tp) :- typeInD(Key,Env,Tp).

declareType(Nm,Lc,Tp,Face,Rules,[scope(Types,Nms,Rls)|Outer],[scope(Types1,Nms,Rls1)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Types,tpDef(Nm,Lc,Tp,Face,Rules),Types1),
  put_dict(Key,Rls,Rules,Rls1).

declareType(Nm,Lc,Tp,[scope(Types,Nms,Rls)|Outer],[scope(Types1,Nms,Rls)|Outer]) :-
  makeKey(Nm,Key),
  moveQuants(Tp,B,Inner),
  moveQuants(FaceRule,B,typeRule(Inner,faceType([]))),
  put_dict(Key,Types,tpDef(Nm,Lc,Tp,FaceRule,[FaceRule]),Types1).

declareVar(Nm,Lc,Vr,[scope(Types,Names,Rules)|Outer],[scope(Types,Names1,Rules)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Names,vEntry(Vr,Lc),Names1).

isVar(Nm,_,vr(Nm,Tp),std) :- escapeType(Nm,Tp),!.
isVar(Nm,Env,Vr,Lc) :- makeKey(Nm,Key), isVr(Key,Env,Vr,Lc).
isVar(Nm,Env,Vr) :- isVar(Nm,Env,Vr,_).

marker(type,"*").
marker(value,"@").
marker(class,"#").

isVr(Key,[scope(_,Names,_)|_],Vr,Lc) :- get_dict(Key,Names,vEntry(Vr,Lc)),!.
isVr(Key,[_|Outer],Vr,Lc) :- isVr(Key,Outer,Vr,Lc).

pushScope(Env,[scope(types{},vars{},rules{})|Env]).

pushFace([],Env,Env).
pushFace([(Nm,Tp)|Fields],Env,ThEnv) :-
  declareVar(Nm,'',vr(Nm,Tp),Env,Env0),
  pushFace(Fields,Env0,ThEnv).

processNames(Dict,P,Result) :-
  processNames(Dict,P,[],Result).

processNames([],_,SoFar,SoFar).
processNames([scope(_,Names,_)|Outer],P,SoFar,Result) :-
  dict_pairs(Names,_,Pairs),
  procNames(Pairs,P,SoFar,S0),
  processNames(Outer,P,S0,Result).

procNames([],_,SoFar,SoFar).
procNames([K-vEntry(Vr,_)|More],P,SoFar,Result) :-
  call(P,K,Vr,SoFar,S0),
  procNames(More,P,S0,Result).

processTypes(Dict,P,Result) :-
  processTypes(Dict,P,[],Result).

processTypes([],_,SoFar,SoFar).
processTypes([scope(_,Names,_)|Outer],P,SoFar,Result) :-
  dict_pairs(Names,_,Pairs),
  procTypes(Pairs,P,SoFar,S0),
  processTypes(Outer,P,S0,Result).

procTypes([],_,SoFar,SoFar).
procTypes([K-V|More],P,SoFar,Result) :-
  call(P,K,V,SoFar,S0),
  procTypes(More,P,S0,Result).

makeKey(Id,Key) :-
  atom_string(Key,Id).

stdVar(Nm,Vr,Env,Ex) :- declareVar(Nm,'std',Vr,Env,Ex).

stdDict(Dict) :-
  pushScope([],Base),
  declareType("integer",std,type("lo.std*integer"),Base,B1),
  declareType("float",std,type("lo.std*float"),B1,B3),
  declareType("string",std,type("lo.std*string"),B3,B4),
  declareType("logical",std,type("lo.std*logical"),B4,B4a),
  declareType("thing",std,type("lo.std*thing"),B4a,B5),
  declareType("list",std,univType(kVar("t"),typeExp("lo.std*list",[kVar("t")])),B5,B6),
  stdVar(",..",vr("lo.std#,..",univType(kVar("t"),
    classType([kVar("t"),typeExp("lo.std*list",[kVar("t")])],typeExp("lo.std*list",[kVar("t")])))),B6,B7),
  stdVar("[]",vr("lo.std#[]",univType(kVar("t"),typeExp("lo.std*list",[kVar("t")]))),B7,B8),
  Dict=B8.


