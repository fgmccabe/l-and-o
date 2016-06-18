:- use_module(ocall).
:-['/Users/fgm/Projects/LandO/LO/Build/arith.pl'].
'inEnums#import'("file:/Users/fgm/Projects/LandO/LO/Build/arith.pl").
:-['/Users/fgm/Projects/LandO/LO/Build/thing.pl'].
'inEnums#import'("file:/Users/fgm/Projects/LandO/LO/Build/thing.pl").
'inEnums#export'("I3'outer't'inEnums*better''O't'inEnums*better''I't'inEnums*basic'").
'inEnums#types'("I2'basic'T1Yt'inEnums*basic'I1'id'i'better'T2Yt'inEnums*better't'inEnums*basic'Yt'inEnums*better'I2'inner't'inEnums*basic''id'i").
'inEnums@assert'() :- 'inEnums@I'(XX10),
    ocall('id%1'(XXV6),XX10,XX10),
    XXV6 = 47.
'inEnums#outer'('o%1'(XV7), XLbl5, XThis5) :- !,
    'inEnums#outer@o'(XV7, XLbl5, XThis5).
'inEnums#outer'('inner%1'('inEnums#outer@inner'(XLbV3, XThV3)), XLbV3, XThV3) :- !.
'inEnums#outer'('inner%3'(XV9, XV10, XV11), XLbl7, XThis7) :- !,
    'inEnums#outer@inner'(XV9, XV10, XV11, XLbl7, XThis7).
'inEnums#outer'('id%1'(XV12), XLbl8, XThis8) :- !,
    'inEnums#outer@id'(XV12, XLbl8, XThis8).
'inEnums#outer@o'(47, XLbV3, XThV3) :- !.
'inEnums#outer@inner'('id%1'(XV8), XLbl6, XThis6) :- !,
    'inEnums#outer@inner@id'(XV8, XLbl6, XThis6).
'inEnums#outer@inner@id'(XX6, XLbV4, XThV4) :- XLbV4 = 'inEnums#outer@inner'(XLbV3, XThV3),
    !,
    'inEnums#outer@o'(XX6, XLbV3, XThV3).
'inEnums#outer@id'(XXV4, XLbV3, XThV3) :- !,
    ocall('id%1'(XXV4),'inEnums#outer@inner'(XLbV3, XThV3),'inEnums#outer@inner'(XLbV3, XThV3)).
'inEnums@O'('inEnums#outer') :- !.
'inEnums@I'(XXV5) :- !,
    'inEnums@O'(XX9),
    ocall('inner%1'(XXV5),XX9,XX9).
