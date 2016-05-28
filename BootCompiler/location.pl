:- module(location,[locOf/2, mergeLoc/3]).

locOf(idTok(_,Lc),Lc).
locOf(integerTok(_,Lc),Lc).
locOf(longTok(_,Lc),Lc).
locOf(floatTok(_,Lc),Lc).
locOf(stringTok(_,Lc),Lc).
locOf(terminal,missing).

mergeLoc(loc(Ln,LnOff,Co1,_),loc(_,_,Co2,Len),loc(Ln,LnOff,Co1,Len1)) :- Len1 is Co2-Co1+Len.
