:- module(ocall,[ocall/3]).

ocall(Call,Lbl,ThVr) :- integer(Lbl),!, call('lo.arith#integer',Call,'lo.arith#integer',ThVr).
ocall(Call,Lbl,ThVr) :- functor(Lbl,P,_), call(P,Call,Lbl,ThVr).
