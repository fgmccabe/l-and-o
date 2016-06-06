:- module(transUtils,[trCons/3,localName/3,labelAccess/4,extraVars/2,thisVar/2
          lookupVarName/3,lookupRelName/3,lookupFunName/3,lookupClassName/3,lookupTypeName/3,
          moduleMap/3,
          trCons/3]).

:- use_module(misc).

trCons(Name,Arity,strct(ConsName,Arity)) :- 
  string_concat(Name,"%",N),
  number_string(Arity,NN),
  string_concat(N,NN,ConsName).

localName(Pkg,Glue,Nm,LclName) :-
  string_concat(Pkg,Glue,T),
  string_concat(T,Nm,LclName).

labelAccess(_Q,_Map,Body,Body). % temp

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

moduleMap(PkgName,Defs,[lyr(PkgName,Defs,'',vdel,vdel,vdel)]).

lookup([],_,_,notInMap).
lookup([lyr(_Prefix,Defns,_Lc,_Lbl,_LbVr,_ThVr)|_Layers],Nm,Filter,Reslt) :-
  filteredSearch(Defns,Filter,Nm,Defn),!.
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

extraVars([lyr(_Prefix,Defns,_Lc,_Lbl,vdel,vdel)|_],[]) :- !.
extraVars([lyr(_Prefix,Defns,_Lc,_Lbl,LbVr,ThVr)|_],[LbVr,ThVr]).

thisVar(lyr(_Prefix,Defns,_Lc,_Lbl,LbVr,ThVr)|_],ThVr) :- ThVr \= vdel.

trCons(Nm,Args,strct(Name,Arity)) :-
  length(Args,Arity),
  number_string(Arity,Sz),
  string_concat(Nm,"%",N1),
  string_concat(N1,Sz,Name).

