:- module(dict,[typeInDict/4,typeFaceRule/3,typeRules/3,declareType/5,declareType/7,
    declareVar/5,isVar/3,isVar/4,
    processNames/3,processTypes/3,
    pushScope/2,pushFace/3,makeKey/2,stdDict/1,marker/2,
    getCatalog/3,declareCatalog/4]).

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

typeInD(Key,[scope(Types,_,_,_)|_],Tp) :- get_dict(Key,Types,Tp),!.
typeInD(Key,[_|Env],Tp) :- typeInD(Key,Env,Tp).

declareType(Nm,Lc,Tp,Face,Rules,[scope(Types,Nms,Rls,Cats)|Outer],[scope(Types1,Nms,Rls1,Cats)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Types,tpDef(Nm,Lc,Tp,Face,Rules),Types1),
  put_dict(Key,Rls,Rules,Rls1).

declareType(Nm,Lc,Tp,[scope(Types,Nms,Rls,Cats)|Outer],[scope(Types1,Nms,Rls,Cats)|Outer]) :-
  makeKey(Nm,Key),
  moveQuants(Tp,B,Inner),
  moveQuants(FaceRule,B,typeRule(Inner,faceType([]))),
  put_dict(Key,Types,tpDef(Nm,Lc,Tp,FaceRule,[FaceRule]),Types1).

declareVar(Nm,Lc,Vr,[scope(Types,Names,Rules,Cats)|Outer],[scope(Types,Names1,Rules,Cats)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Names,vEntry(Vr,Lc),Names1).

isVar(Nm,_,vr(Nm,Tp),std) :- escapeType(Nm,Tp),!.
isVar(Nm,Env,Vr,Lc) :- makeKey(Nm,Key), isVr(Key,Env,Vr,Lc).
isVar(Nm,Env,Vr) :- isVar(Nm,Env,Vr,_).

marker(type,"*").
marker(value,"@").
marker(class,"#").

isVr(Key,[scope(_,Names,_,_)|_],Vr,Lc) :- get_dict(Key,Names,vEntry(Vr,Lc)),!.
isVr(Key,[_|Outer],Vr,Lc) :- isVr(Key,Outer,Vr,Lc).

pushScope(Env,[scope(types{},vars{},rules{},catalogs{})|Env]).

pushFace([],Env,Env).
pushFace([(Nm,Tp)|Fields],Env,ThEnv) :-
  declareVar(Nm,'',vr(Nm,Tp),Env,Env0),
  pushFace(Fields,Env0,ThEnv).

processNames(Dict,P,Result) :-
  processNames(Dict,P,[],Result).

processNames([],_,SoFar,SoFar).
processNames([scope(_,Names,_,_)|Outer],P,SoFar,Result) :-
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
processTypes([scope(_,Names,_,_)|Outer],P,SoFar,Result) :-
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

getCatalog(Nm,Env,Cat) :-
  makeKey(Nm,Ky),
  is_member(scope(_,_,_,C),Env),
  get_dict(Ky,C,Cat).

declareCatalog(Nm,Ct,[scope(Types,Nms,Rls,Cats)|Outer],[scope(Types,Nms,Rls,Cats1)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Cats,Ct,Cats1).

stdDict(Base) :-
  pushScope([],Base).


