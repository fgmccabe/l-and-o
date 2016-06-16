:- module(ocall,[ocall/3]).

:- use_module(polyfill).

ocall(Call,Lbl,ThVr) :- integer(Lbl),!, call('lo.arith#integer',Call,'lo.arith#integer',ThVr).
ocall(Call,Lbl,ThVr) :- functor(Lbl,P,_), call(P,Call,Lbl,ThVr).
