:- module(p,['px@assert'/0]).

'px@assert'() :- 'px@fred'(XX51),
    ocall('name%1'(XXV4),XX51,XX51),
    XXV4 = "fred",
    'px@fred'(XX52),
    ocall('first%1'(XXV5),XX52,XX52),
    XXV5 = "fred".
'px#p'('name%1'(XV12), XLbl12, XThis12) :- !,
    'px#p@name'(XV12, XLbl12, XThis12).
'px#p@name'(XNm, XLbV7, XThV7) :- XLbV7 = 'px#p'(XNm),
    !.
'px#s'('name%1'(XV13), XLbl13, XThis13) :- !,
    'px#s^super4'('name%1'(XV13), XLbl13, XThis13).
'px#s'('studies%1'(XV14), XLbl14, XThis14) :- !,
    'px#s@studies'(XV14, XLbl14, XThis14).
'px#s'('first%1'(XV15), XLbl15, XThis15) :- !,
    'px#s@first'(XV15, XLbl15, XThis15).
'px#s@studies'(XS, XLbV8, XThV8) :- XLbV8 = 'px#s'(XS, X_),
    !.
'px#s@first'(XX49, XLbV8, XThV8) :- XLbV8 = 'px#s'(XS, X_),
    !,
    'px#s^super4'('name%1'(XX49), XLbV8, XThV8).
'px@fred'('px#s'("math", "fred")) :- !.
'px#s^super4'(XCV4, 'px#s'(X_, XN), XThV8) :- ocall(XCV4,'px#p'(XN),XThV8).

ocall(Call,Lbl,ThVr) :- functor(Lbl,P,_), call(P,Call,Lbl,ThVr).