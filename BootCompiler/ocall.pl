:- module(ocall,[ocall/3]).

ocall(Call,Lbl,ThVr) :- float(Lbl),!, call('lo.core#float',Call,'lo.core#float',ThVr).
ocall(Call,Lbl,ThVr) :- integer(Lbl),!, call('lo.core#integer',Call,'lo.core#integer',ThVr).
ocall(Call,Lbl,ThVr) :- string(Lbl),!, call('lo.core#string',Call,'lo.core#string',ThVr).
ocall(Call,Lbl,ThVr) :- functor(Lbl,P,_), call(P,Call,Lbl,ThVr).
