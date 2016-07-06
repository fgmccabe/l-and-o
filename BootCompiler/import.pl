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

importPkg(Pkg,Vers,Repo,spec(Pkg,Vers,Export,Types,Classes,Imports)) :-
  openPackageAsStream(Repo,Pkg,Vers,Strm),
  pickupPieces(Strm,Pkg,[export,types,classes],Pieces),
  close(Strm),
  processPieces(Pieces,Export,Types,Imports,Classes).

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

processPieces([],_,_,[],[]).
processPieces([export(Sig)|More],Export,Types,Imports,Classes) :-
  decodeSignature(Sig,Export),
  processPieces(More,_,Types,Imports,Classes).
processPieces([types(Sig)|More],Export,Types,Imports,Classes) :-
  decodeSignature(Sig,Types),
  processPieces(More,Export,_,Imports,Classes).
processPieces([import(Viz,Pkg,Version)|More],Export,Types,[Import|Imports],Classes) :-
  massageImport([Viz,Pkg,Version],Import),
  processPieces(More,Export,Types,Imports,Classes).
processPieces([classes(Enc)|More],Export,Types,Imports,Classes) :-
  massageClasses(Enc,Classes,Cls),
  processPieces(More,Export,Types,Imports,Cls).

massageClasses(Enc,Classes,Cls) :-
  decodeValue(Enc,Term),
  findClasses(Term,Classes,Cls).

findClasses(tpl(Entries),Classes,Cls) :-
  pickupClasses(Entries,Classes,Cls).

pickupClasses([],Cls,Cls).
pickupClasses([tpl([strg(Nm),Cl,Sig])|Rest],[(Nm,Cl,Sig)|More],Cls):-
  pickupClasses(Rest,More,Cls).

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

