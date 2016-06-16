:- module(import, [importPkg/5,makeOutputUri/4]).
 
:- use_module(catalog).
:- use_module(resource).
:- use_module(types).
:- use_module(misc).
:- use_module(uri).
:- use_module(transutils).
:- use_module(decode).

importPkg(Pkg,Uri,Base,Opts,spec(CodeUri,Export,Types)) :-
  uriPath(Uri,Pth),
  makeOutputUri(Base,Opts,Pth,CodeUri),
  openResource(CodeUri,Strm),
  pickupPieces(Strm,Pkg,[export,types],Pieces), % todo: imports
  processPieces(Pieces,Export,Types),
  close(Strm).

pickupPieces(_,_,[],[]).
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

processPieces([],_,_).
processPieces([export(Sig)|More],Export,Types) :-
  decodeSignature(Sig,Export),
  processPieces(More,_,Types).
processPieces([types(Sig)|More],Export,Types) :-
  decodeSignature(Sig,Types),
  processPieces(More,Export,_).

makeOutputUri(_,Opts,Fn,Out) :-
  is_member(build(B),Opts),
  string_concat(Prefix,".lo",Fn),
  string_concat(Prefix,".pl",OFn),
  split_string(OFn,"/","",Els),
  last(Els,Tail),
  parseURI(Tail,OU),
  resolveURI(B,OU,Out).
makeOutputUri(Base,_,Fn,Out) :-
  string_concat(Prefix,".lo",Fn),
  string_concat(Prefix,".pl",OFn),
  parseURI(OFn,OU),
  resolveURI(Base,OU,Out).

