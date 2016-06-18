:- use_module(ocall).
:-['/Users/fgm/Projects/LandO/LO/Build/arith.pl'].
:-['/Users/fgm/Projects/LandO/LO/Build/thing.pl'].
'inEnums#export'("I3'outer't'inEnums*better''O't'inEnums*better''I't'inEnums*basic'").
'inEnums#types'("I2'basic'T1Yt'inEnums*basic'I1'id'i'better'T2Yt'inEnums*better't'inEnums*basic'Yt'inEnums*better'I2'inner't'inEnums*basic''id'i").
'inEnums@assert'() :- 'inEnums@I'(XX112),
    ocall('id%1'(XXV78),XX112,XX112),
    XXV78 = 47.
'inEnums#outer'('o%1'(XV170), XLbl114, XThis114) :- !,
    'inEnums#outer@o'(XV170, XLbl114, XThis114).
'inEnums#outer'('inner%1'('inEnums#outer@inner'(XLbV65, XThV65)), XLbV65, XThV65) :- !.
'inEnums#outer'('inner%3'(XV172, XV173, XV174), XLbl116, XThis116) :- !,
    'inEnums#outer@inner'(XV172, XV173, XV174, XLbl116, XThis116).
'inEnums#outer'('id%1'(XV175), XLbl117, XThis117) :- !,
    'inEnums#outer@id'(XV175, XLbl117, XThis117).
'inEnums#outer@o'(47, XLbV65, XThV65) :- !.
'inEnums#outer@inner'('id%1'(XV171), XLbl115, XThis115) :- !,
    'inEnums#outer@inner@id'(XV171, XLbl115, XThis115).
'inEnums#outer@inner@id'(XX108, XLbV66, XThV66) :- XLbV66 = 'inEnums#outer@inner'(XLbV65, XThV65),
    !,
    'inEnums#outer@o'(XX108, XLbV65, XThV65).
'inEnums#outer@id'(XXV76, XLbV65, XThV65) :- !,
    ocall('id%1'(XXV76),'inEnums#outer@inner'(XLbV65, XThV65),'inEnums#outer@inner'(XLbV65, XThV65)).
'inEnums@O'('inEnums#outer') :- !.
'inEnums@I'(XXV77) :- !,
    'inEnums@O'(XX111),
    ocall('inner%1'(XXV77),XX111,XX111).
