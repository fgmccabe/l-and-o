:- module(ocall,[ocall/3]).

ocall(Call,Lbl,ThVr) :- float(Lbl),!, call('lo.arith#float',Call,'lo.arith#float',ThVr).
ocall(Call,Lbl,ThVr) :- integer(Lbl),!, call('lo.arith#integer',Call,'lo.arith#integer',ThVr).
ocall(Call,Lbl,ThVr) :- string(Lbl),!, call('lo.string#string',Call,'lo.string#string',ThVr).
ocall(Call,Lbl,ThVr) :- functor(Lbl,P,_), call(P,Call,Lbl,ThVr).
