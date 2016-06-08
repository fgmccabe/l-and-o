:- module(transUtils,[trCons/3,localName/4,labelAccess/5,extraVars/2,thisVar/2,
          lookupVarName/3,lookupRelName/3,lookupFunName/3,lookupClassName/3,lookupTypeName/3,
          makePkgMap/4,genNewName/4,
          pushOpt/3, isOption/2,
          trCons/3]).

:- use_module(misc).
:- use_module(dict).
:- use_module(types).

trCons(Nm,Arity,strct(Name,Arity)) :-
  integer(Arity),!,
  number_string(Arity,Sz),
  string_concat(Nm,"%",N1),
  string_concat(N1,Sz,Name).
trCons(Nm,Args,strct(Name,Arity)) :-
  length(Args,Arity),
  number_string(Arity,Sz),
  string_concat(Nm,"%",N1),
  string_concat(N1,Sz,Name).

localName(_,Glue,Nm,Nm) :- 
  sub_string(Nm,_,_,_,Glue),!.
localName(Pkg,Glue,Nm,LclName) :-
  string_concat(Pkg,Glue,T),
  string_concat(T,Nm,LclName).

genNewName(Map,Variant,Args,prg(Nm,Ar)) :-
  layerName(Map,Prefix),
  gensym(Variant,V),
  length(Args,Ar),
  localName(Prefix,"@",V,Nm).


/*
 * Each element in Layers defines a scope. It is a tuple of the form:
 * lyr(Prefix,Defs:(Name,Class)[],Loc,Label,Clvr,Thvr)
 * Where Prefix is the current prefix
 * Defs is the set of local programs and other names defined in this scope
 * Loc is the file location of the defining label
 * Label is the full form of the label term.
 * Clvr is the variable holding the label
 * Thvr is the variable holding this
 *
 * E.g., in
 * pk{
 *  foo(A)..{
 *   ...
 *    bar(B)..{
 *      X = $anon..{}
 *   ...
 *    }
 *  }
 * }
 *
 * The inner-most layer will look like:
 *    lyr(pk#foo#bar#anon23,Defs,Lc,anon23(Free,bar(B,foo(A))),ClVar,ThVar)
 * the outermost class layer will look like
 *    lyr(pk#foo,Defs,Lc,foo(A),clVar,thVar)
 * the package layer will look like
 *    lyr(pk,Defs,Lc,vdel,vdel,vdel)
 */

stdMap([lyr(std,Defs,'',vdel,vdel,vdel)]) :-
  stdDict(Dict),
  processNames(Dict,transUtils:stdMapEntry,Defs).

stdMapEntry(_,vr(Nm,Tp),SoFar,[(Nm,moduleFun("",prg(LclName,Arity)))|SoFar]) :-
  isFunctionType(Tp,Ar),!,
  Arity is Ar+1,
  localName("","@",Nm,LclName).
stdMapEntry(_,vr(Nm,Tp),SoFar,[(Nm,moduleRel("",prg(LclName,Arity)))|SoFar]) :-
  isPredType(Tp,Arity),!,
  localName("","@",Nm,LclName).
stdMapEntry(_,vr(Nm,Tp),SoFar,[(Nm,moduleClass("",strct(LclName,Arity),prg(LclName,3)))|SoFar]) :-
  isClassType(Tp,Arity),!,
  localName("","#",Nm,LclName).
stdMapEntry(_,vr(Nm,Tp),SoFar,[(Nm,moduleClass("",enum(LclName),prg(LclName,3)))|SoFar]) :- % this is a hack
  \+isFunctionType(Tp,_),\+isPredType(Tp,_),\+isClassType(Tp,_),!,
  localName("","#",Nm,LclName).
stdMapEntry(_,vr(Nm,Tp),SoFar,SoFar) :-
  reportMsg("cannot understand standard name %s:%s",[Nm,Tp]).

makePkgMap(Pkg,Defs,Types,Map) :-
  stdMap(StdMap),
  makeModuleMap(Pkg,Defs,DfList,Rest),
  makeTypesMap(Pkg,Types,Rest,[]),
  pushMap(Pkg,DfList,StdMap,Map).

pushMap(PkgName,Defs,Std,[lyr(PkgName,Defs,'',vdel,vdel,vdel)|Std]).

makeModuleMap(Pkg,[Def|Rest],Map,Mx) :-
  makeMdkEntry(Pkg,Def,Map,M0),
  makeModuleMap(Pkg,Rest,M0,Mx).
makeModuleMap(_,[],Map,Map).

makeMdkEntry(Pkg,function(_,Nm,Tp,_),[(Nm,moduleFun(Pkg,prg(LclName,Arity)))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  typeArity(Tp,Ar),
  Arity is Ar+1.
makeMdkEntry(Pkg,predicate(_,Nm,Tp,_),[(Nm,moduleRel(Pkg,prg(LclName,Arity)))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  typeArity(Tp,Arity).
makeMdkEntry(Pkg,defn(_,Nm,_,_,_),[(Nm,moduleVar(Pkg,prg(LclName,1)))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName).
makeMdkEntry(Pkg,class(_,Nm,Tp,_),[(Nm,moduleClass(Pkg,strct(LclName,Ar),prg(LclName,3)))|Mx],Mx) :-
  localName(Pkg,"#",Nm,LclName),
  typeArity(Tp,Ar).
makeMdkEntry(Pkg,enum(_,Nm,_,_),[(Nm,moduleClass(Pkg,enum(LclName),prg(LclName,3)))|Mx],Mx) :-
  localName(Pkg,"#",Nm,LclName).
makeMdkEntry(Pkg,typeDef(_,Nm,Tp,_),[(Nm,moduleType(Pkg,LclName,Tp))|Mx],Mx) :-
  localName(Pkg,"*",Nm,LclName).

makeTypesMap(_,_,List,List).

lookup([],_,_,notInMap).
lookup([lyr(_Prefix,Defns,_Lc,_Lbl,_LbVr,_ThVr)|_Layers],Nm,Filter,Reslt) :-
  filteredSearch(Defns,Filter,Nm,Reslt),!.
lookup([_|Layers],Nm,Filter,Reslt) :-
  lookup(Layers,Nm,Filter,Reslt).

filteredSearch(Defns,Filter,Nm,Defn) :-
  is_member((Nm,Defn),Defns),
  call(Filter,Defn),!.

lookupVarName(Map,Nm,V) :-
  lookup(Map,Nm,anyDef,V).

anyDef(moduleVar(_Pkg,_Vn)).
anyDef(localVar(_Vn,_ClVr,_TVr)).
anyDef(labelArg(_N,_ClVr,_TVr)).
anyDef(localClass(_,_,_,_)).
anyDef(moduleClass(_,_,_)).
anyDef(inherit(_,_,_,_)).
anyDef(inheritField(_,_,_,_)).

lookupRelName(Map,Nm,V) :-
  lookup(Map,Nm,relDef,V).

relDef(localRel(_,_,_)).
relDef(moduleRel(_,_)).
relDef(inherit(_,_,_,_)).
relDef(inheritField(_,_,_)).

lookupFunName(Map,Nm,V) :-
  lookup(Map,Nm,funDef,V).

funDef(localFun(_,_,_)).
funDef(moduleFun(_,_)).
funDef(inherit(_,_,_,_)).
funDef(inheritField(_,_,_)).
funDef(localClass(_,_,_,_)).
funDef(moduleClass(_,_,_)).

lookupClassName(Map,Nm,V) :-
  lookup(Map,Nm,classDef,V).

classDef(localClass(_,_,_,_)).
classDef(moduleClass(_,_,_)).
classDef(inherit(_,_,_,_)).
classDef(inheritField(_,_,_)).

lookupTypeName(Map,Nm,T) :-
  lookup(Map,Nm,tpDef,T).

tpDef(localType(_)).
tpDef(inherit(_,_,_,_)).
tpDef(moduleType(_,_)).

lookupDefn(Map,Nm,Df) :-
  lookup(Map,Nm,nonType,Df).

nonType(Df) :- Df \= moduleType(_,_), Df \= localType(_).

lookupPackageRef(Map,Pkg,Nm,V) :-
  lookup(Map,Nm,pkgRef(Pkg),V).

pkgRef(Pkg,moduleClass(Pkg,_,_)).
pkgRef(Pkg,moduleFun(Pkg,_)).
pkgRef(Pkg,moduleRel(Pkg,_)).
pkgRef(Pkg,moduleVar(Pkg,_)).
pkgRef(Pkg,moduleType(Pkg,_)).

extraVars([lyr(_,_,_,_,vdel,vdel)|_],[]) :- !.
extraVars([lyr(_,_,_,_,LbVr,ThVr)|_],[LbVr,ThVr]).

thisVar([lyr(_,_,_,_,_,ThVr)|_],ThVr) :- ThVr \= vdel.

labelAccess(Q,Q,[lyr(_,_,_,_Lbl,vdel,vdel)|_],G,G) :- !.
labelAccess(Q,Qx,[lyr(_,_,_,Lbl,LbVr,_)|_],[equals(LbVr,Lbl)|G],G) :- merge([LbVr],Q,Qx).

pushOpt(Opts,Opt,[Opt|Opts]).

isOption(Opt,Opts) :- is_member(Opt,Opts),!.

layerName([lyr(Nm,_,_,_,_,_)|_],Nm).
