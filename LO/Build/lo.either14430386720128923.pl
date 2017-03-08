'#pkg'("n7o7'()7'n2o2'pkg's'lo.either's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I2'either':k'a':k'b'CT1k'a'Uz2'lo.either*either'2k'a'k'b''alternate':k'a':k'b'CT1k'b'Uz2'lo.either*either'2k'a'k'b'\"s\"I1'either':k'a':k'b':k'a':k'b'YUz2'lo.either*either'2k'a'k'b'I0\"n2o2'()2's'either's'alternate'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.either*either's\":k'x':k'y'||c'lo.core$display'T1Uz2'lo.either*either'2k'x'k'y'T0c'lo.core$display'T1k'x'T0c'lo.core$display'T1k'y'T0\"n2o2'()2's'lo.core$equality$lo.either*either's\":k'x':k'y'||c'lo.core$equality'T1Uz2'lo.either*either'2k'x'k'y'T0c'lo.core$equality'T1k'x'T0c'lo.core$equality'T1k'y'T0\"").
'lo.either@init'():- !.
'lo.either#either'('either%1'('lo.either@either'())):- !.
'lo.either#alternate'('alternate%1'('lo.either@alternate'())):- !.
'lo.either@dispEither'(Xdisplay239, Xdisplay240, 'lo.either#either'(Xa), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("either("), 'lo.core#,..'(XXe1813, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]'))))):- !,
    ocall('disp%1'(XXV1836),Xdisplay239,Xdisplay239),
    ocall('_call%2'(Xa, XXe1813),XXV1836,XXV1836).
'lo.either@dispEither'(Xdisplay239, Xdisplay240, 'lo.either#alternate'(Xb), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("or("), 'lo.core#,..'(XXe1814, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]'))))):- !,
    ocall('disp%1'(XXV1837),Xdisplay240,Xdisplay240),
    ocall('_call%2'(Xb, XXe1814),XXV1837,XXV1837).
'lo.either@dispEither'(_, _, _, _):- raise_exception('error'("lo.either@dispEither", 13, 3, 63)).
'lo.core$display$lo.either*either'('lo.core$display$lo.either*either%1'('lo.core$display$lo.either*either')):- !.
'lo.core$display$lo.either*either'('disp%2'(XV17548, XV17549), XLbl3657, XThis3657):- !,
    'lo.core$display$lo.either*either@disp'(XV17548, XV17549, XLbl3657, XThis3657).
'lo.core$display$lo.either*either'('disp%1'('lo.core$display$lo.either*either^disp'(XLbl3658, XThis3658)), XLbl3658, XThis3658).
'lo.core$display$lo.either*either@disp'(XT, XXd8438, XLbV1594, XThV1594):- XLbV1594 = 'lo.core$display$lo.either*either'(Xdisplay241, Xdisplay242),
    !,
    'lo.either@dispEither'(Xdisplay242, Xdisplay241, XT, XXd8438).
'lo.core$display$lo.either*either@disp'(_, _):- raise_exception('error'("lo.core$display$lo.either*either@disp", 9, 5, 24)).
'lo.either@eitherHash'(Xequality742, Xequality743, 'lo.either#either'(XA), XXe1816):- !,
    ocall('hash%1'(XXV1838),Xequality742,Xequality742),
    ocall('*%1'(XXV1839),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('_call%2'(XA, XXe1815),XXV1838,XXV1838),
    ocall('_call%3'(XXe1815, 37, XXe1816),XXV1839,XXV1839).
'lo.either@eitherHash'(Xequality742, Xequality743, 'lo.either#alternate'(XB), XXe1818):- !,
    ocall('hash%1'(XXV1840),Xequality743,Xequality743),
    ocall('*%1'(XXV1841),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('_call%2'(XB, XXe1817),XXV1840,XXV1840),
    ocall('_call%3'(XXe1817, 41, XXe1818),XXV1841,XXV1841).
'lo.either@eitherHash'(_, _, _, _):- raise_exception('error'("lo.either@eitherHash", 26, 3, 35)).
'lo.either@eitherEquals'(Xequality744, Xequality745, 'lo.either#either'(XA), 'lo.either#either'(XB)):- ocall('==%2'(XA, XB),Xequality744,Xequality744).
'lo.either@eitherEquals'(Xequality744, Xequality745, 'lo.either#alternate'(XA), 'lo.either#alternate'(XB)):- ocall('==%2'(XA, XB),Xequality745,Xequality745).
'lo.core$equality$lo.either*either'('lo.core$equality$lo.either*either%1'('lo.core$equality$lo.either*either')):- !.
'lo.core$equality$lo.either*either'('==%2'(XV17562, XV17563), XLbl3659, XThis3659):- !,
    'lo.core$equality$lo.either*either@=='(XV17562, XV17563, XLbl3659, XThis3659).
'lo.core$equality$lo.either*either'('==%1'('lo.core$equality$lo.either*either^=='(XLbl3660, XThis3660)), XLbl3660, XThis3660).
'lo.core$equality$lo.either*either'('hash%2'(XV17566, XV17567), XLbl3661, XThis3661):- !,
    'lo.core$equality$lo.either*either@hash'(XV17566, XV17567, XLbl3661, XThis3661).
'lo.core$equality$lo.either*either'('hash%1'('lo.core$equality$lo.either*either^hash'(XLbl3662, XThis3662)), XLbl3662, XThis3662).
'lo.core$equality$lo.either*either@=='(XX, XY, XLbV1595, XThV1595):- XLbV1595 = 'lo.core$equality$lo.either*either'(Xequality746, Xequality747),
    'lo.either@eitherEquals'(Xequality747, Xequality746, XX, XY).
'lo.core$equality$lo.either*either@hash'(XX, XXd8439, XLbV1595, XThV1595):- XLbV1595 = 'lo.core$equality$lo.either*either'(Xequality746, Xequality747),
    !,
    'lo.either@eitherHash'(Xequality747, Xequality746, XX, XXd8439).
'lo.core$equality$lo.either*either@hash'(_, _):- raise_exception('error'("lo.core$equality$lo.either*either@hash", 18, 5, 24)).
'lo.either^dispEither'('_call%4'(XV17542, XV17543, XV17544, XV17545), 'lo.either^dispEither', _):- 'lo.either@dispEither'(XV17542, XV17543, XV17544, XV17545).
'lo.core$display$lo.either*either^disp'('_call%2'(XV17546, XV17547), 'lo.core$display$lo.either*either^disp'(XLbV1594, XThV1594), _):- 'lo.core$display$lo.either*either@disp'(XV17546, XV17547, XLbV1594, XThV1594).
'lo.either^eitherHash'('_call%4'(XV17550, XV17551, XV17552, XV17553), 'lo.either^eitherHash', _):- 'lo.either@eitherHash'(XV17550, XV17551, XV17552, XV17553).
'lo.either^eitherEquals'('_call%4'(XV17554, XV17555, XV17556, XV17557), 'lo.either^eitherEquals', _):- 'lo.either@eitherEquals'(XV17554, XV17555, XV17556, XV17557).
'lo.core$equality$lo.either*either^=='('_call%4'(XV17558, XV17559, XV17560, XV17561), 'lo.core$equality$lo.either*either^=='(XLbV1595, XThV1595), _):- 'lo.core$equality$lo.either*either@=='(XV17558, XV17559, XV17560, XV17561, XLbV1595, XThV1595).
'lo.core$equality$lo.either*either^hash'('_call%2'(XV17564, XV17565), 'lo.core$equality$lo.either*either^hash'(XLbV1595, XThV1595), _):- 'lo.core$equality$lo.either*either@hash'(XV17564, XV17565, XLbV1595, XThV1595).
