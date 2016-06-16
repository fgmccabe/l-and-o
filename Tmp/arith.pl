:- use_module(ocall).
:-['/Users/fgm/Projects/LandO/Tmp/thing.pl'].
'lo.arith#export'("I5'+':cvk't't'lo.arith*arith'F2k't'k't'k't''*':cvk't't'lo.arith*arith'F2k't'k't'k't''-':cvk't't'lo.arith*arith'F2k't'k't'k't''/':cvk't't'lo.arith*arith'F2k't'k't'k't''integer'i").
'lo.arith#types'("I3'arith'T1Yt'lo.arith*arith'I4'dv'F1hh'mns'F1hh'tms'F1hh'pl'F1hh'integer'T2Yit'lo.arith*arith'YiI4'pl'F1hh'tms'F1hh'mns'F1hh'dv'F1hh'float'T2Yft'lo.arith*arith'YfI4'pl'F1hh'tms'F1hh'mns'F1hh'dv'F1hh").
'lo.arith@assert'() :- 'lo.arith@*'(2, 3, XX136),
    'lo.arith@+'(XX136, 4, XX137),
    XX137 = 10.
'lo.arith@+'(Xx, Xy, XX107) :- !,
    ocall('pl%2'(XX107, Xy),Xx,Xx).
'lo.arith@+'(_, _, _) :- abort.
'lo.arith@*'(Xx, Xy, XX112) :- !,
    ocall('tms%2'(XX112, Xy),Xx,Xx).
'lo.arith@*'(_, _, _) :- abort.
'lo.arith@-'(Xx, Xy, XX117) :- !,
    ocall('mns%2'(XX117, Xy),Xx,Xx).
'lo.arith@-'(_, _, _) :- abort.
'lo.arith@/'(Xx, Xy, XX122) :- !,
    ocall('dv%2'(XX122, Xy),Xx,Xx).
'lo.arith@/'(_, _, _) :- abort.
'lo.arith#integer'('pl%2'(XV34, XV35), XLbl30, XThis30) :- !,
    'lo.arith#integer@pl'(XV34, XV35, XLbl30, XThis30).
'lo.arith#integer'('tms%2'(XV36, XV37), XLbl31, XThis31) :- !,
    'lo.arith#integer@tms'(XV36, XV37, XLbl31, XThis31).
'lo.arith#integer'('mns%2'(XV38, XV39), XLbl32, XThis32) :- !,
    'lo.arith#integer@mns'(XV38, XV39, XLbl32, XThis32).
'lo.arith#integer'('dv%2'(XV40, XV41), XLbl33, XThis33) :- !,
    'lo.arith#integer@dv'(XV40, XV41, XLbl33, XThis33).
'lo.arith#integer@pl'(XY, XX126, XLbV28, XThV28) :- !,
    '_int_plus'(XX126, XThV28, XY).
'lo.arith#integer@pl'(_, _, _, _) :- abort.
'lo.arith#integer@tms'(XY, XX129, XLbV28, XThV28) :- !,
    '_int_times'(XX129, XThV28, XY).
'lo.arith#integer@tms'(_, _, _, _) :- abort.
'lo.arith#integer@mns'(XY, XX132, XLbV28, XThV28) :- !,
    '_int_minus'(XX132, XThV28, XY).
'lo.arith#integer@mns'(_, _, _, _) :- abort.
'lo.arith#integer@dv'(XY, XX135, XLbV28, XThV28) :- !,
    '_int_div'(XX135, XThV28, XY).
'lo.arith#integer@dv'(_, _, _, _) :- abort.
