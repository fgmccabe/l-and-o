:- use_module(ocall).
:-['/Users/fgm/Projects/LandO/LO/Build/arith.pl'].
'innerClass#import'("file:/Users/fgm/Projects/LandO/LO/Build/arith.pl").
:-['/Users/fgm/Projects/LandO/LO/Build/thing.pl'].
'innerClass#import'("file:/Users/fgm/Projects/LandO/LO/Build/thing.pl").
'innerClass#export'("I3'outer'C1it'innerClass*better''O't'innerClass*better''I't'innerClass*basic'").
'innerClass#types'("I2'basic'T1Yt'innerClass*basic'I1'id'i'better'T2Yt'innerClass*better't'innerClass*basic'Yt'innerClass*better'I2'inner't'innerClass*basic''id'i").
'innerClass@assert'() :- 'innerClass@I'(XX33),
    ocall('id%1'(XXV9),XX33,XX33),
    XXV9 = 47.
'innerClass@outer'('outer%1'('innerClass#outer'())) :- !.
'innerClass#outer'('inner%1'('innerClass#outer@inner'(XLbV7, XThV7)), XLbV7, XThV7) :- XLbV7 = 'innerClass#outer'(XId),
    !.
'innerClass#outer'('inner%3'(XV22, XV23, XV24), XLbl16, XThis16) :- !,
    'innerClass#outer@inner'(XV22, XV23, XV24, XLbl16, XThis16).
'innerClass#outer'('id%1'(XV25), XLbl17, XThis17) :- !,
    'innerClass#outer@id'(XV25, XLbl17, XThis17).
'innerClass#outer@inner'('id%1'(XV21), XLbl15, XThis15) :- !,
    'innerClass#outer@inner@id'(XV21, XLbl15, XThis15).
'innerClass#outer@inner@id'(XId, XLbV8, XThV8) :- XLbV8 = 'innerClass#outer@inner'(XLbV7, XThV7),
    !.
'innerClass#outer@id'(XXV7, XLbV7, XThV7) :- XLbV7 = 'innerClass#outer'(XId),
    !,
    ocall('id%1'(XXV7),'innerClass#outer@inner'(XLbV7, XThV7),'innerClass#outer@inner'(XLbV7, XThV7)).
'innerClass@O'('innerClass#outer'(47)) :- !.
'innerClass@I'(XXV8) :- !,
    'innerClass@O'(XX32),
    ocall('inner%1'(XXV8),XX32,XX32).
