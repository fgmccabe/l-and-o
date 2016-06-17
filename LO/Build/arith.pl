:- use_module(ocall).
:-['/Users/fgm/Projects/LandO/LO/Build/thing.pl'].
'lo.arith#export'("I5'+':cvk't't'lo.arith*arith'F2k't'k't'k't''*':cvk't't'lo.arith*arith'F2k't'k't'k't''-':cvk't't'lo.arith*arith'F2k't'k't'k't''/':cvk't't'lo.arith*arith'F2k't'k't'k't''integer'i").
'lo.arith#types'("I3'arith'T1Yt'lo.arith*arith'I4'dv'F1hh'mns'F1hh'tms'F1hh'pl'F1hh'integer'T2Yit'lo.arith*arith'YiI4'pl'F1hh'tms'F1hh'mns'F1hh'dv'F1hh'float'T2Yft'lo.arith*arith'YfI4'pl'F1hh'tms'F1hh'mns'F1hh'dv'F1hh").
'lo.arith@assert'() :- 'lo.arith@*'(2, 3, XX33),
    'lo.arith@+'(XX33, 4, XX34),
    XX34 = 10.
'lo.arith@+'(Xx, Xy, XX4) :- !,
    ocall('pl%2'(XX4, Xy),Xx,Xx).
'lo.arith@+'(_, _, _) :- abort.
'lo.arith@*'(Xx, Xy, XX9) :- !,
    ocall('tms%2'(XX9, Xy),Xx,Xx).
'lo.arith@*'(_, _, _) :- abort.
'lo.arith@-'(Xx, Xy, XX14) :- !,
    ocall('mns%2'(XX14, Xy),Xx,Xx).
'lo.arith@-'(_, _, _) :- abort.
'lo.arith@/'(Xx, Xy, XX19) :- !,
    ocall('dv%2'(XX19, Xy),Xx,Xx).
'lo.arith@/'(_, _, _) :- abort.
'lo.arith#integer'('pl%2'(XV1, XV2), XLbl1, XThis1) :- !,
    'lo.arith#integer@pl'(XV1, XV2, XLbl1, XThis1).
'lo.arith#integer'('tms%2'(XV3, XV4), XLbl2, XThis2) :- !,
    'lo.arith#integer@tms'(XV3, XV4, XLbl2, XThis2).
'lo.arith#integer'('mns%2'(XV5, XV6), XLbl3, XThis3) :- !,
    'lo.arith#integer@mns'(XV5, XV6, XLbl3, XThis3).
'lo.arith#integer'('dv%2'(XV7, XV8), XLbl4, XThis4) :- !,
    'lo.arith#integer@dv'(XV7, XV8, XLbl4, XThis4).
'lo.arith#integer@pl'(XY, XX23, XLbV1, XThV1) :- !,
    '_int_plus'(XX23, XThV1, XY).
'lo.arith#integer@pl'(_, _, _, _) :- abort.
'lo.arith#integer@tms'(XY, XX26, XLbV1, XThV1) :- !,
    '_int_times'(XX26, XThV1, XY).
'lo.arith#integer@tms'(_, _, _, _) :- abort.
'lo.arith#integer@mns'(XY, XX29, XLbV1, XThV1) :- !,
    '_int_minus'(XX29, XThV1, XY).
'lo.arith#integer@mns'(_, _, _, _) :- abort.
'lo.arith#integer@dv'(XY, XX32, XLbV1, XThV1) :- !,
    '_int_div'(XX32, XThV1, XY).
'lo.arith#integer@dv'(_, _, _, _) :- abort.
