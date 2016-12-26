:- module(import, [importPkg/3,loadPkg/4]).

% This is specific to the Prolog translation of L&O code
 
:- use_module(resource).
:- use_module(types).
:- use_module(misc).
:- use_module(uri).
:- use_module(transutils).
:- use_module(decode).
:- use_module(repository).

importPkg(Pkg,Repo,spec(Act,Export,Types,Classes,Contracts,Implementations,Imports)) :-
  openPackageAsStream(Repo,Pkg,Act,_,Strm),
  read(Strm,SigTerm),
  close(Strm),
  pickupPkgSpec(SigTerm,Pkg,Imports,Export,Types,Classes,Contracts,Implementations).

pickupPkgSpec('#pkg'(Enc),Pkg,Imports,Export,Types,Classes,Contracts,Implementations) :-
  decodeValue(Enc,tpl([Pk,tpl(Imps),FTps,TTps,tpl(ClsSigs),tpl(ConSigs),tpl(ImplSigs)])),
  pickupPkg(Pk,Pkg),
  pickupImports(Imps,Imports),
  pickupFace(FTps,Export),
  pickupFace(TTps,Types),
  pickupClasses(ClsSigs,Classes,[]),
  pickupContracts(ConSigs,Contracts),
  pickupImplementations(ImplSigs,Implementations,[]).

pickupPkg(cons(strct("pkg",2),[strg(Nm),V]),pkg(Nm,Vers)) :-
  pickupVersion(V,Vers).

pickupVersion(enum("*"),defltVersion).
pickupVersion(strg(V),v(V)).

pickupImports([],[]).
pickupImports([cons(strct("import",2),[V,P])|L],[import(Viz,Pkg)|M]) :-
  pickupViz(V,Viz),
  pickupPkg(P,Pkg),
  pickupImports(L,M).

pickupViz(enum("private"),private).
pickupViz(enum("public"),public).

pickupFace(strg(Sig),Type) :-
  decodeSignature(Sig,Type).

pickupClasses([],Cls,Cls).
pickupClasses([tpl([strg(Nm),Cl,strg(Sig)])|Rest],[(Nm,Cl,Tp)|More],Cls):-
  decodeSignature(Sig,Tp),
  pickupClasses(Rest,More,Cls).

processContracts(Enc,C,Cx) :-
  decodeValue(Enc,tpl(Cons)),
  findContracts(Cons,C,Cx).

pickupContracts(C,Cons) :-
  findContracts(C,Cons,[]).

findContracts([],C,C).
findContracts([tpl([strg(Nm),strg(CnNm),strg(Sig),strg(FSig)])|M],[contract(Nm,CnNm,Spec,FullSpec,Face)|C],Cx) :-
  decodeConstraint(Sig,FullSpec),
  moveQuants(FullSpec,Q,FS),
  moveConstraints(FS,_,S),
  moveQuants(Spec,Q,S),
  decodeSignature(FSig,Face),
  findContracts(M,C,Cx).

processImplementations(Env,Impls,MoreImpls) :-
  decodeValue(Env,tpl(Els)),
  pickupImplementations(Els,Impls,MoreImpls).

pickupImplementations([],I,I).
pickupImplementations([tpl([strg(Nm),strg(Sig)])|M],[imp(Nm,Spec)|I],RI) :-
  decodeConstraint(Sig,Spec),
  pickupImplementations(M,I,RI).

loadPkg(Pkg,Repo,Code,Imports) :-
  openPackageAsStream(Repo,Pkg,_,_,Strm),
  read(Strm,SigTerm),
  pickupPkgSpec(SigTerm,Pkg,Imports,_,_,_,_,_),
  loadCode(Strm,Code),
  close(Strm).

loadCode(Strm,[]) :-
  at_end_of_stream(Strm).
loadCode(Strm,[Term|M]) :-
  read(Strm,Term),
  loadCode(Strm,M).
