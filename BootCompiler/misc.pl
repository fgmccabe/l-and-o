:-module(misc,[concat/3,segment/3,last/2,reverse/2,is_member/2,merge/3,intersect/3,subtract/3,
        appStr/3,appInt/3,appSym/3,
        subPath/4,pathSuffix/3,starts_with/2,ends_with/2]).

concat([],X,X).
concat([E|X],Y,[E|Z]) :- concat(X,Y,Z).

reverse(X,Y) :- reverse(X,[],Y).
reverse([],X,X).
reverse([E|R],X,Y) :- reverse(R,[E|X],Y).

segment(Str,Ch,Segments) :- split_string(Str,Ch,"",Segments).

last([El],El).
last([_|Rest],El) :- last(Rest,El).

is_member(X,[X|_]).
is_member(X,[_|Y]) :- is_member(X,Y).

merge([],X,X).
merge([E|X],Y,Z) :- 
  is_member(E,Y),
  merge(X,Y,Z).
merge([E|X],Y,Z) :-
  merge(X,[E|Y],Z).

subtract(_,[],[]).
subtract(E,[E|O],O).
subtract(E,[X|I],[X|O]) :-
  subtract(E,I,O).

intersect([],_,[]).
intersect([E|X],Y,[E|Z]) :- is_member(E,Y), intersect(X,Y,Z).
intersect([_|X],Y,Z) :- intersect(X,Y,Z).

appStr(Str,O,E) :- string_chars(Str,Chrs), concat(Chrs,E,O).

appSym(Sym,O,E) :- atom_chars(Sym,Chrs), concat(Chrs,E,O).

appInt(Ix,O,E) :- number_string(Ix,Str), string_chars(Str,Chrs), concat(Chrs,E,O).

subPath(Path,Marker,Suffix,Name) :-
  sub_string(Path,_,_,After,Marker),
  (After=0, string_concat(Path,Suffix,Name) ; string_concat(Path,".",P0),string_concat(P0,Suffix,Name)).
subPath(Path,Marker,Suffix,Name) :-
  string_concat(Path,Marker,P0),
  string_concat(P0,Suffix,Name).

ends_with(String,Tail) :- 
  string_concat(_,Tail,String).

starts_with(String,Front) :-
  string_concat(Front,_,String).

pathSuffix(String,Marker,Tail) :-
  split_string(String,Marker,"",[_,Local]),
  split_string(Local,".","",Segs),
  last(Segs,Tail).
pathSuffix(String,_,String).