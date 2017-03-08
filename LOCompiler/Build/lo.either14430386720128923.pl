'#pkg'("n7o7'()7'n2o2'pkg's'lo.either's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I2'either':k'a':k'b'CT1k'a'Uz2'lo.either*either'2k'a'k'b''alternate':k'a':k'b'CT1k'b'Uz2'lo.either*either'2k'a'k'b'\"s\"I1'either':k'a':k'b':k'a':k'b'YUz2'lo.either*either'2k'a'k'b'I0\"n2o2'()2's'either's'alternate'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.either*either's\":k'x':k'y'||c'lo.core$display'T1Uz2'lo.either*either'2k'x'k'y'T0c'lo.core$display'T1k'x'T0c'lo.core$display'T1k'y'T0\"n2o2'()2's'lo.core$equality$lo.either*either's\":k'x':k'y'||c'lo.core$equality'T1Uz2'lo.either*either'2k'x'k'y'T0c'lo.core$equality'T1k'x'T0c'lo.core$equality'T1k'y'T0\"").
'lo.either@init'():- !.
'lo.either#either'('either%1'('lo.either@either'())):- !.
'lo.either#alternate'('alternate%1'('lo.either@alternate'())):- !.
'lo.either@dispEither'(Xdisplay105, Xdisplay106, 'lo.either#either'(Xa), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("either("), 'lo.core#,..'(XXe3414, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]'))))):- !,
    ocall('disp%1'(XXV3689),Xdisplay105,Xdisplay105),
    ocall('_call%2'(Xa, XXe3414),XXV3689,XXV3689).
'lo.either@dispEither'(Xdisplay105, Xdisplay106, 'lo.either#alternate'(Xb), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("or("), 'lo.core#,..'(XXe3415, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]'))))):- !,
    ocall('disp%1'(XXV3690),Xdisplay106,Xdisplay106),
    ocall('_call%2'(Xb, XXe3415),XXV3690,XXV3690).
'lo.either@dispEither'(_, _, _, _):- raise_exception('error'("lo.either@dispEither", 13, 3, 63)).
'lo.core$display$lo.either*either'('lo.core$display$lo.either*either%1'('lo.core$display$lo.either*either')):- !.
'lo.core$display$lo.either*either'('disp%2'(XV23064, XV23065), XLbl1909, XThis1909):- !,
    'lo.core$display$lo.either*either@disp'(XV23064, XV23065, XLbl1909, XThis1909).
'lo.core$display$lo.either*either'('disp%1'('lo.core$display$lo.either*either^disp'(XLbl1910, XThis1910)), XLbl1910, XThis1910).
'lo.core$display$lo.either*either@disp'(XT, XXd28976, XLbV1925, XThV1925):- XLbV1925 = 'lo.core$display$lo.either*either'(Xdisplay107, Xdisplay108),
    !,
    'lo.either@dispEither'(Xdisplay108, Xdisplay107, XT, XXd28976).
'lo.core$display$lo.either*either@disp'(_, _):- raise_exception('error'("lo.core$display$lo.either*either@disp", 9, 5, 24)).
'lo.either@eitherHash'(Xequality271, Xequality272, 'lo.either#either'(XA), XXe3417):- !,
    ocall('hash%1'(XXV3691),Xequality271,Xequality271),
    ocall('*%1'(XXV3692),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('_call%2'(XA, XXe3416),XXV3691,XXV3691),
    ocall('_call%3'(XXe3416, 37, XXe3417),XXV3692,XXV3692).
'lo.either@eitherHash'(Xequality271, Xequality272, 'lo.either#alternate'(XB), XXe3419):- !,
    ocall('hash%1'(XXV3693),Xequality272,Xequality272),
    ocall('*%1'(XXV3694),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('_call%2'(XB, XXe3418),XXV3693,XXV3693),
    ocall('_call%3'(XXe3418, 41, XXe3419),XXV3694,XXV3694).
'lo.either@eitherHash'(_, _, _, _):- raise_exception('error'("lo.either@eitherHash", 26, 3, 35)).
'lo.either@eitherEquals'(Xequality273, Xequality274, 'lo.either#either'(XA), 'lo.either#either'(XB)):- ocall('==%2'(XA, XB),Xequality273,Xequality273).
'lo.either@eitherEquals'(Xequality273, Xequality274, 'lo.either#alternate'(XA), 'lo.either#alternate'(XB)):- ocall('==%2'(XA, XB),Xequality274,Xequality274).
'lo.core$equality$lo.either*either'('lo.core$equality$lo.either*either%1'('lo.core$equality$lo.either*either')):- !.
'lo.core$equality$lo.either*either'('==%2'(XV23078, XV23079), XLbl1911, XThis1911):- !,
    'lo.core$equality$lo.either*either@=='(XV23078, XV23079, XLbl1911, XThis1911).
'lo.core$equality$lo.either*either'('==%1'('lo.core$equality$lo.either*either^=='(XLbl1912, XThis1912)), XLbl1912, XThis1912).
'lo.core$equality$lo.either*either'('hash%2'(XV23082, XV23083), XLbl1913, XThis1913):- !,
    'lo.core$equality$lo.either*either@hash'(XV23082, XV23083, XLbl1913, XThis1913).
'lo.core$equality$lo.either*either'('hash%1'('lo.core$equality$lo.either*either^hash'(XLbl1914, XThis1914)), XLbl1914, XThis1914).
'lo.core$equality$lo.either*either@=='(XX, XY, XLbV1926, XThV1926):- XLbV1926 = 'lo.core$equality$lo.either*either'(Xequality275, Xequality276),
    'lo.either@eitherEquals'(Xequality276, Xequality275, XX, XY).
'lo.core$equality$lo.either*either@hash'(XX, XXd28977, XLbV1926, XThV1926):- XLbV1926 = 'lo.core$equality$lo.either*either'(Xequality275, Xequality276),
    !,
    'lo.either@eitherHash'(Xequality276, Xequality275, XX, XXd28977).
'lo.core$equality$lo.either*either@hash'(_, _):- raise_exception('error'("lo.core$equality$lo.either*either@hash", 18, 5, 24)).
'lo.either^dispEither'('_call%4'(XV23058, XV23059, XV23060, XV23061), 'lo.either^dispEither', _):- 'lo.either@dispEither'(XV23058, XV23059, XV23060, XV23061).
'lo.core$display$lo.either*either^disp'('_call%2'(XV23062, XV23063), 'lo.core$display$lo.either*either^disp'(XLbV1925, XThV1925), _):- 'lo.core$display$lo.either*either@disp'(XV23062, XV23063, XLbV1925, XThV1925).
'lo.either^eitherHash'('_call%4'(XV23066, XV23067, XV23068, XV23069), 'lo.either^eitherHash', _):- 'lo.either@eitherHash'(XV23066, XV23067, XV23068, XV23069).
'lo.either^eitherEquals'('_call%4'(XV23070, XV23071, XV23072, XV23073), 'lo.either^eitherEquals', _):- 'lo.either@eitherEquals'(XV23070, XV23071, XV23072, XV23073).
'lo.core$equality$lo.either*either^=='('_call%4'(XV23074, XV23075, XV23076, XV23077), 'lo.core$equality$lo.either*either^=='(XLbV1926, XThV1926), _):- 'lo.core$equality$lo.either*either@=='(XV23074, XV23075, XV23076, XV23077, XLbV1926, XThV1926).
'lo.core$equality$lo.either*either^hash'('_call%2'(XV23080, XV23081), 'lo.core$equality$lo.either*either^hash'(XLbV1926, XThV1926), _):- 'lo.core$equality$lo.either*either@hash'(XV23080, XV23081, XLbV1926, XThV1926).
