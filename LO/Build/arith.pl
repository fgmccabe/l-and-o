:- use_module(ocall).
:-['/Users/fgm/Projects/LandO/LO/Build/thing.pl'].
'lo.arith#import'("file:/Users/fgm/Projects/LandO/LO/Build/thing.pl").
'lo.arith#export'("I5'+':cvk't't'lo.arith*arith'F2k't'k't'k't''*':cvk't't'lo.arith*arith'F2k't'k't'k't''-':cvk't't'lo.arith*arith'F2k't'k't'k't''/':cvk't't'lo.arith*arith'F2k't'k't'k't''integer'i").
'lo.arith#types'("I3'arith'T1Yt'lo.arith*arith'I4'dv'F1hh'mns'F1hh'tms'F1hh'pl'F1hh'integer'T2Yit'lo.arith*arith'YiI4'pl'F1hh'tms'F1hh'mns'F1hh'dv'F1hh'float'T2Yft'lo.arith*arith'YfI4'pl'F1hh'tms'F1hh'mns'F1hh'dv'F1hh").
'lo.arith@assert'() :- 'lo.arith@*'(2, 3, XX247),
    'lo.arith@+'(XX247, 4, XX248),
    XX248 = 10.
'lo.arith@+'(Xx, Xy, XX218) :- !,
    ocall('pl%2'(XX218, Xy),Xx,Xx).
'lo.arith@+'(_, _, _) :- abort.
'lo.arith@*'(Xx, Xy, XX223) :- !,
    ocall('tms%2'(XX223, Xy),Xx,Xx).
'lo.arith@*'(_, _, _) :- abort.
'lo.arith@-'(Xx, Xy, XX228) :- !,
    ocall('mns%2'(XX228, Xy),Xx,Xx).
'lo.arith@-'(_, _, _) :- abort.
'lo.arith@/'(Xx, Xy, XX233) :- !,
    ocall('dv%2'(XX233, Xy),Xx,Xx).
'lo.arith@/'(_, _, _) :- abort.
'lo.arith#integer'('pl%2'(XV61, XV62), XLbl33, XThis33) :- !,
    'lo.arith#integer@pl'(XV61, XV62, XLbl33, XThis33).
'lo.arith#integer'('tms%2'(XV63, XV64), XLbl34, XThis34) :- !,
    'lo.arith#integer@tms'(XV63, XV64, XLbl34, XThis34).
'lo.arith#integer'('mns%2'(XV65, XV66), XLbl35, XThis35) :- !,
    'lo.arith#integer@mns'(XV65, XV66, XLbl35, XThis35).
'lo.arith#integer'('dv%2'(XV67, XV68), XLbl36, XThis36) :- !,
    'lo.arith#integer@dv'(XV67, XV68, XLbl36, XThis36).
'lo.arith#integer@pl'(XY, XX237, XLbV12, XThV12) :- !,
    '_int_plus'(XX237, XThV12, XY).
'lo.arith#integer@pl'(_, _, _, _) :- abort.
'lo.arith#integer@tms'(XY, XX240, XLbV12, XThV12) :- !,
    '_int_times'(XX240, XThV12, XY).
'lo.arith#integer@tms'(_, _, _, _) :- abort.
'lo.arith#integer@mns'(XY, XX243, XLbV12, XThV12) :- !,
    '_int_minus'(XX243, XThV12, XY).
'lo.arith#integer@mns'(_, _, _, _) :- abort.
'lo.arith#integer@dv'(XY, XX246, XLbV12, XThV12) :- !,
    '_int_div'(XX246, XThV12, XY).
'lo.arith#integer@dv'(_, _, _, _) :- abort.
