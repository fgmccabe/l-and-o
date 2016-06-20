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

importPkg(Pkg,Vers,Repo,spec(Pkg,Vers,Export,Types,Imports)) :-
  openPackageAsStream(Repo,Pkg,Vers,Strm),
  pickupPieces(Strm,Pkg,[export,types,import],Pieces), % todo: imports
  processPieces(Pieces,Export,Types,Imports),
  close(Strm).

pickupPieces(_,_,[],[]).
pickupPieces(Strm,_,_,[]) :-
  at_end_of_stream(Strm).
pickupPieces(Strm,Pkg,Lookfor,Pieces) :-
  read(Strm,Term),
  Term =.. [F|Args],
  isAPiece(F,Args,Pkg,Lookfor,Rest,Pieces,More),
  pickupPieces(Strm,Pkg,Rest,More).

isAPiece(F,Args,Pkg,[L|Rest],Rest,[Term|Pieces],Pieces) :-
  localName(Pkg,"#",L,F),!,
  Term =..[L|Args].
isAPiece(F,Args,Pkg,[L|Lookfor],[L|Rest],Pieces,More) :-
  isAPiece(F,Args,Pkg,Lookfor,Rest,Pieces,More).
isAPiece(_,_,_,[],[],Pieces,Pieces).

processPieces([],_,_,[]).
processPieces([export(Sig)|More],Export,Types,Imports) :-
  decodeSignature(Sig,Export),
  processPieces(More,_,Types,Imports).
processPieces([types(Sig)|More],Export,Types,Imports) :-
  decodeSignature(Sig,Types),
  processPieces(More,Export,_,Imports).
processPieces([import(Viz,Pkg,Version)|More],Export,Types,[import(Viz,Pkg,Version)|Imports]) :-
  processPieces(More,Export,Types,Imports).

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

