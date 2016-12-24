:- module(transUtils,[trCons/3,className/3,labelAccess/5,extraVars/2,thisVar/2,
          lookupVarName/3,lookupRelName/3,lookupFunName/3,lookupClassName/3,lookupTypeName/3,
          makePkgMap/6,mapName/3,
          genNewName/4,genVar/2,
          pushOpt/3, isOption/2,layerName/2,
          trCons/3,trPrg/3,typeTrArity/2,
          genAnons/2,genVars/2]).

:- use_module(misc).
:- use_module(dict).
:- use_module(types).
:- use_module(freshen).

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

className(Outer,Name,Nm) :-
  sub_string(Outer,_,_,_,"#"),!,
  string_concat(Outer,".",O),
  string_concat(O,Name,Nm).
className(Outer,Name,Nm) :-
  localName(Outer,"#",Name,Nm).

trPrg(Nm,Arity,prg(Name,Arity)) :-
  integer(Arity),!,
  number_string(Arity,Sz),
  string_concat(Nm,"%",N1),
  string_concat(N1,Sz,Name).
trPrg(Nm,Args,prg(Name,Arity)) :-
  length(Args,Arity),
  number_string(Arity,Sz),
  string_concat(Nm,"%",N1),
  string_concat(N1,Sz,Name).

genNewName(Map,Variant,Ar,prg(Nm,Ar)) :-
  layerName(Map,Prefix),
  genstr(Variant,V),
  localName(Prefix,"@",V,Nm).

/*
 * Each element in Layers defines a scope. It is a tuple of the form:
 * lyr(Prefix,Defs:list[(Name,Class),Loc,Label,Clvr,Thvr)
 * Where Prefix is the current prefix
 * Defs is the set of local programs and other names defined in this scope
 * Loc is the file location of the defining label
 * Label is the full form of the label term.
 * Clvr is the variable holding the label
 * Thvr is the variable holding this
 *
 * E.g., in
 * pk{
 *  foo(A){
 *   ...
 *    bar(B){
 *      X = $anon{}
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
 *    lyr(pk,Defs,Lc,void,void,void)
 */

stdMap([lyr(std,Defs,'',void,void,void)]) :-
  stdDict(Dict),
  processNames(Dict,transUtils:stdMapEntry,Defs).

stdMapEntry(_,vr(Nm,Tp),SoFar,[(Nm,moduleFun("",prg(LclName,Arity),strct(AccessName,Arity)))|SoFar]) :-
  isFunctionType(Tp,Ar),!,
  Arity is Ar+1,
  localName("","@",Nm,LclName),
  localName("","%",Nm,AccessName).
stdMapEntry(_,vr(Nm,Tp),SoFar,[(Nm,moduleRel("",prg(LclName,Arity)))|SoFar]) :-
  isPredType(Tp,Arity),!,
  localName("","@",Nm,LclName).
stdMapEntry(_,vr(Nm,Tp),SoFar,[(Nm,moduleClass(prg(AccessName,1),strct(LclName,Arity),prg(LclName,3)))|SoFar]) :-
  isClassType(Tp,Arity),!,
  localName("","#",Nm,LclName),
  localName("","@",Nm,AccessName).
stdMapEntry(_,vr(Nm,Tp),SoFar,[(Nm,moduleClass(prg(AccessName,1),enum(LclName),prg(LclName,3)))|SoFar]) :- % this is a hack
  \+isFunctionType(Tp,_),\+isPredType(Tp,_),\+isClassType(Tp,_),!,
  localName("","#",Nm,LclName),
  localName("","@",Nm,AccessName).
stdMapEntry(_,vr(Nm,Tp),SoFar,SoFar) :-
  reportMsg("cannot understand standard name %s:%s",[Nm,Tp]).

makePkgMap(Pkg,Defs,Types,Imports,Classes,Map) :-
  stdMap(StdMap),
  makeModuleMap(Pkg,Defs,DfList,Rest,Classes),
  makeImportsMap(Imports,Rest,R0),
  makeTypesMap(Pkg,Types,R0,[]),
  pushMap(Pkg,DfList,StdMap,Map).

pushMap(PkgName,Defs,Std,[lyr(PkgName,Defs,'',void,void,void)|Std]).

makeModuleMap(Pkg,[Def|Rest],Map,Mx,Classes) :-
  makeMdlEntry(Pkg,Def,Map,M0,Classes,Clx),
  makeModuleMap(Pkg,Rest,M0,Mx,Clx).
makeModuleMap(_,[],Map,Map,[]).

makeMdlEntry(Pkg,function(_,Nm,Tp,_,_),[(Nm,moduleFun(Pkg,prg(LclName,Arity),strct(AccessName,Arity)))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName),
  typeArity(Tp,Ar),
  Arity is Ar+1,
  localName(Pkg,"%",Nm,AccessName).
makeMdlEntry(Pkg,grammar(_,Nm,Tp,_,_),[(Nm,moduleRel(Pkg,prg(LclName,Arity)))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName),
  typeArity(Tp,Ar),
  Arity is Ar+2.
makeMdlEntry(Pkg,predicate(_,Nm,Tp,_,_),[(Nm,moduleRel(Pkg,prg(LclName,Arity)))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName),
  typeArity(Tp,Arity).
makeMdlEntry(Pkg,defn(_,Nm,_,_,_,_),[(Nm,moduleVar(Pkg,prg(LclName,1)))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName).
makeMdlEntry(Pkg,class(_,Nm,Tp,_,_,_),[(Nm,moduleClass(prg(AccessName,1),strct(LclName,Ar),prg(LclName,3)))|Mx],Mx,[(Nm,strct(LclName,Ar),Tp)|Clx],Clx) :-
  localName(Pkg,"#",Nm,LclName),
  typeArity(Tp,Ar),
  localName(Pkg,"@",Nm,AccessName).
makeMdlEntry(Pkg,enum(_,Nm,Tp,_,_,_),[(Nm,moduleClass(prg(AccessName,1),enum(LclName),prg(LclName,3)))|Mx],Mx,[(Nm,enum(LclName),Tp)|Clx],Clx) :-
  localName(Pkg,"#",Nm,LclName),
  localName(Pkg,"@",Nm,AccessName).
makeMdlEntry(Pkg,typeDef(_,Nm,Tp,_),[(Nm,moduleType(Pkg,LclName,Tp))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"*",Nm,LclName).
makeMdlEntry(Pkg,contract(Nm,CNm,ConTp,_,_),[(Nm,moduleContract(Pkg,CNm,ConTp))|Mx],Mx,Clx,Clx).
makeMdlEntry(_,impl(_,_,ImplNm,0,_,_,_,_,_),[(ImplNm,moduleImpl(prg(ImplNm,1),enum(ImplNm),prg(ImplNm,3)))|Mx],Mx,Clx,Clx).
makeMdlEntry(_,impl(_,_,ImplNm,Arity,_,_,_,_,_),[(ImplNm,moduleImpl(prg(ImplNm,1),strct(ImplNm,Arity),prg(ImplNm,3)))|Mx],Mx,Clx,Clx).
  
makeImportsMap([Import|Rest],Map,Mx) :-
  makeImportMap(Import,Map,M0),
  makeImportsMap(Rest,M0,Mx).
makeImportsMap([],Map,Map).

makeImportMap(import(_,pkg(Pkg,_),faceType(Fields),_,Classes,_,Impls),Map,Mx) :-
  importFields(Pkg,Classes,Fields,Map,M0),
  importImplementations(Impls,M0,Mx).

importFields(_,_,[],Map,Map).
importFields(Pkg,Classes,[(Nm,Tp)|Fields],Map,Mx) :-
  moveQuants(Tp,_,QTp),
  moveConstraints(QTp,_,Template),
  makeImportEntry(Template,Classes,Pkg,Nm,Map,M0),
  importFields(Pkg,Classes,Fields,M0,Mx).

importImplementations([],Map,Map).
importImplementations([imp(Nm,Con)|L],[(Nm,moduleImpl(prg(Nm,1),Struct,prg(Nm,3)))|M],Mx) :-
  contractArity(Con,Ar),
  contractStruct(Ar,Nm,Struct),
  importImplementations(L,M,Mx).

contractArity(univType(_,Con),Ar) :- contractArity(Con,Ar).
contractArity(constrained(Con,_),Ar) :- contractArity(Con,A), Ar is A+1.
contractArity(_,0).

contractStruct(0,Nm,enum(Nm)).
contractStruct(Ar,Nm,strct(Nm,Ar)).

makeImportEntry(funType(A,_),_,Pkg,Nm,[(Nm,moduleFun(Pkg,prg(LclName,Arity),strct(AccessName,Arity)))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  length(A,Ar),
  Arity is Ar+1,
  localName(Pkg,"%",Nm,AccessName).
makeImportEntry(grammarType(A,_),_,Pkg,Nm,[(Nm,moduleRel(Pkg,prg(LclName,Arity)))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  length(A,Ar),
  Arity is Ar+2.
makeImportEntry(predType(A),_,Pkg,Nm,[(Nm,moduleRel(Pkg,prg(LclName,Arity)))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  length(A,Arity).
makeImportEntry(classType(A,_),_,Pkg,Nm,[(Nm,moduleClass(prg(AccessName,1),strct(LclName,Ar),prg(LclName,3)))|Mx],Mx) :-
  localName(Pkg,"#",Nm,LclName),
  length(A,Ar),
  localName(Pkg,"@",Nm,AccessName).
makeImportEntry(_,Classes,Pkg,Nm,[(Nm,moduleClass(prg(AccessName,1),enum(LclName),prg(LclName,3)))|Mx],Mx) :-
  is_member((Nm,enum(LclName),_),Classes),
  localName(Pkg,"@",Nm,AccessName).
makeImportEntry(_,_,Pkg,Nm,[(Nm,moduleVar(Pkg,prg(LclName,1)))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName).

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
anyDef(localClass(_,_,_,_,_)).
anyDef(moduleClass(_,_,_)).
anyDef(inherit(_,_,_,_)).
anyDef(inheritField(_,_,_)).
anyDef(moduleContract(_,_,_)).
anyDef(moduleImpl(_,_,_)).
anyDef(moduleFun(_,_,_)).
anyDef(localFun(_,_,_,_)).

lookupRelName(Map,Nm,V) :-
  lookup(Map,Nm,relDef,V).

relDef(localRel(_,_,_)).
relDef(moduleRel(_,_)).
relDef(inherit(_,_,_,_)).
relDef(inheritField(_,_,_)).

lookupFunName(Map,Nm,V) :-
  lookup(Map,Nm,funDef,V).

funDef(localFun(_,_,_,_)).
funDef(moduleFun(_,_,_)).
funDef(inherit(_,_,_,_)).
funDef(inheritField(_,_,_)).
funDef(localClass(_,_,_,_,_)).
funDef(moduleClass(_,_,_)).
funDef(moduleImpl(_,_,_)).

lookupClassName(Map,Nm,V) :-
  lookup(Map,Nm,classDef,V).

classDef(localClass(_,_,_,_,_)).
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
pkgRef(Pkg,moduleFun(Pkg,_,_)).
pkgRef(Pkg,moduleRel(Pkg,_)).
pkgRef(Pkg,moduleVar(Pkg,_)).
pkgRef(Pkg,moduleType(Pkg,_)).

mapName(moduleFun(_,Prog,Access),Prog,Access).
mapName(localFun(Prog,Access,_,_),Prog,Access).

extraVars([lyr(_,_,_,_,void,void)|_],[]) :- !.
extraVars([lyr(_,_,_,_,LbVr,ThVr)|_],[LbVr,ThVr]).

thisVar([lyr(_,_,_,_,_,ThVr)|_],ThVr) :- ThVr \= void.

labelAccess(Q,Q,[lyr(_,_,_,_,void,void)|_],G,G) :- !.
labelAccess(Q,Qx,[lyr(_,_,_,LblGl,LbVr,_)|_],G,Gx) :- concat(LblGl,Gx,G),merge([LbVr],Q,Qx).

pushOpt(Opts,Opt,[Opt|Opts]).

isOption(Opt,Opts) :- is_member(Opt,Opts),!.

layerName([lyr(Nm,_,_,_,_,_)|_],Nm).

genVar(Prefix,idnt(V)) :-
  genstr(Prefix,V).

genAnons(0,[]).
genAnons(K,[anon|Rest]) :-
  K>0,
  K1 is K-1,
  genAnons(K1,Rest).

genVars(0,[]).
genVars(K,[V|Rest]) :-
  K>0,
  K1 is K-1,
  genVar("V",V),
  genVars(K1,Rest).

typeTrArity(Tp,Ar) :-
  isFunctionType(Tp,A),
  Ar is A+1.
typeTrArity(Tp,Ar) :-
  isPredType(Tp,Ar).
typeTrArity(Tp,Ar) :-
  isClassType(Tp,A),
  Ar is A+1.
typeTrArity(_,1).
