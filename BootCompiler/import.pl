:- module(import, [importPkg/3,importPkg/4,loadPkg/5]).

% This is specific to the Prolog translation of L&O code
 
:- use_module(resource).
:- use_module(types).
:- use_module(misc).
:- use_module(uri).
:- use_module(transutils).
:- use_module(decode).
:- use_module(repository).

importPkg(Pkg,Repo,Spec) :-
  importPkg(Pkg,defltVersion,Repo,Spec).

importPkg(Pkg,Vers,Repo,spec(Pkg,Vers,Export,Types,Classes,Contracts,Impls,Imports)) :-
  openPackageAsStream(Repo,Pkg,Vers,Strm),
  pickupPieces(Strm,Pkg,[export,types,classes,contracts,implementations],Pieces),
  close(Strm),
  processPieces(Pieces,Export,Types,Imports,Classes,Contracts,Impls).

pickupPieces(_,_,[],[]).
pickupPieces(Strm,_,_,[]) :-
  at_end_of_stream(Strm).
pickupPieces(Strm,Pkg,Lookfor,Pieces) :-
  read(Strm,Term),
  Term =.. [F|Args],
  isAPiece(F,Args,Pkg,Lookfor,Rest,Pieces,More),
  pickupPieces(Strm,Pkg,Rest,More).

isAPiece(F,Args,pkg(Pkg),L,L,[Term|Pieces],Pieces) :-
  localName(Pkg,"#","import",F),!,
  Term =..['import'|Args].
isAPiece(F,Args,pkg(Pkg),[L|Rest],Rest,[Term|Pieces],Pieces) :-
  localName(Pkg,"#",L,F),!,
  Term =..[L|Args].
isAPiece(F,Args,Pkg,[L|Lookfor],[L|Rest],Pieces,More) :-
  isAPiece(F,Args,Pkg,Lookfor,Rest,Pieces,More).
isAPiece(_,_,_,[],[],Pieces,Pieces).

processPieces([],_,_,[],[],[],[]).
processPieces([export(Sig)|More],Export,Types,Imports,Classes,Contracts,Impls) :-
  decodeSignature(Sig,Export),
  processPieces(More,_,Types,Imports,Classes,Contracts,Impls).
processPieces([types(Sig)|More],Export,Types,Imports,Classes,Contracts,Impls) :-
  decodeSignature(Sig,Types),
  processPieces(More,Export,_,Imports,Classes,Contracts,Impls).
processPieces([import(Viz,Pkg,Version)|More],Export,Types,[Import|Imports],Classes,Contracts,Impls) :-
  massageImport([Viz,Pkg,Version],Import),
  processPieces(More,Export,Types,Imports,Classes,Contracts,Impls).
processPieces([classes(Enc)|More],Export,Types,Imports,Classes,Contracts,Impls) :-
  massageClasses(Enc,Classes,Cls),
  processPieces(More,Export,Types,Imports,Cls,Contracts,Impls).
processPieces([contracts(Enc)|More],Export,Types,Imports,Classes,Contracts,Impls) :-
  processContracts(Enc,Contracts,MoreCons),
  processPieces(More,Export,Types,Imports,Classes,MoreCons,Impls).
processPieces([implementations(Enc)|More],Export,Types,Imports,Classes,Contracts,Impls) :-
  processImplementations(Enc,Impls,MoreImpls),
  processPieces(More,Export,Types,Imports,Classes,Contracts,MoreImpls).

massageClasses(Enc,Classes,Cls) :-
  decodeValue(Enc,Term),
  findClasses(Term,Classes,Cls).

findClasses(tpl(Entries),Classes,Cls) :-
  pickupClasses(Entries,Classes,Cls).

pickupClasses([],Cls,Cls).
pickupClasses([tpl([strg(Nm),Cl,Sig])|Rest],[(Nm,Cl,Sig)|More],Cls):-
  pickupClasses(Rest,More,Cls).

processContracts(Enc,C,Cx) :-
  decodeValue(Enc,tpl(Cons)),
  findContracts(Cons,C,Cx).

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

loadPkg(Pkg,Vers,Repo,Code,Imports) :-
  openPackageAsStream(Repo,Pkg,Vers,Strm),
  loadPieces(Strm,Pkg,Code,Imports),
  close(Strm).

loadPieces(Strm,_,[],[]) :-
  at_end_of_stream(Strm).
loadPieces(Strm,Pkg,Code,Imports) :-
  read(Strm,Term),
  Term =.. [F|Args],
  ( sub_string(F,_,_,0,"#import"), !,
    massageImport(Args,I),
    Imports = [I|MoreImports], loadPieces(Strm,Pkg,Code,MoreImports) ;
    Code = [Term|MoreCode], loadPieces(Strm,Pkg,MoreCode,Imports)).

massageImport([Viz,Pkg,'*'],import(Viz,pkg(Pkg),defltVersion)).
massageImport([Viz,Pkg,Vers],import(Viz,pkg(Pkg),v(Vers))).

