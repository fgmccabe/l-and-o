:-module(misc,[concat/3,segment/3,last/2,reverse/2]).

concat([],X,X).
concat([E|X],Y,[E|Z]) :- concat(X,Y,Z).

reverse(X,Y) :- reverse(X,[],Y).
reverse([],X,X).
reverse([E|R],X,Y) :- reverse(R,[E|X],Y).

segment(Str,Ch,Segments) :- split_string(Str,Ch,"",Segments).

last([El],El).
last([_|Rest],El) :- last(Rest,El).
