:- module(import, [import/3]).
 
:- use_module(catalog).
:- use_module(resource).
:- use_module(types).
:- use_module(misc).

import(Pkg,Fl,Spec) :-
  open(Fl,read,Str),
  pickupPieces(Str,Pkg,[export,types],Pieces), % todo: imports
  processPieces(Pieces,Spec).

pickupPieces(_,_,[],[]).
pickupPieces(Strm,Pkg,Lookfor,Pieces) :-
  read(Strm,Term),
  functor(Term,F,_),
  isAPiece(Term,F,Pkg,Lookfor,Rest,Pieces,More),
  pickupPieces(Strm,Pkg,Rest,More).

isAPiece(Term,F,Pkg,[L|Rest],Rest,[(L,Term)|Pieces],Pieces) :-
  localName(Pkg,"#",L,F),!.
isAPiece(Term,F,Pkg,[L|Lookfor],[L|Rest],Pieces,More) :-
  isAPiece(Term,F,Pkg,Lookfor,Rest,Pieces,More).
isAPiece(_,_,_,[],[],Pieces,Pieces).


