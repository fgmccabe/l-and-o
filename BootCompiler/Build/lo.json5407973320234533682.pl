'#pkg'("n7o7'()7'n2o2'pkg's'lo.json'e'*'n10o10'()10'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I8'jTrue't'lo.json*json''jFalse't'lo.json*json''jNull't'lo.json*json''jTxt'CT1St'lo.json*json''jColl'CT1Uz2'lo.index*map'2St'lo.json*json't'lo.json*json''jSeq'CT1Lt'lo.json*json't'lo.json*json''jNum'CT1ft'lo.json*json''parseJson':k's'|GT1t'lo.json*json'k's'c'lo.core$stream'T1k's'T1i\"s\"I1'json'Yt'lo.json*json'I0\"n3o3'()3's'jTrue's'jFalse's'jNull'n0o0'()0'n5o5'()5'n2o2'()2's'lo.core$display$lo.json*json's\"c'lo.core$display'T1t'lo.json*json'T0\"n2o2'()2's'lo.core$equality$lo.json*json's\"c'lo.core$equality'T1t'lo.json*json'T0\"n2o2'()2's'lo.coerce$coercion$lo.core*string$lo.json*json's\"c'lo.coerce$coercion'T2St'lo.json*json'T0\"n2o2'()2's'lo.coerce$coercion$lo.index*map$lo.json*json's\":k'v'|c'lo.coerce$coercion'T2Uz2'lo.index*map'2Sk'v't'lo.json*json'T0c'lo.coerce$coercion'T2k'v't'lo.json*json'T0\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.json*json's\":k'v'|c'lo.coerce$coercion'T2Lk'v't'lo.json*json'T0c'lo.coerce$coercion'T2k'v't'lo.json*json'T0\"").
'lo.json@init'() :- !.
'lo.json#jTxt'('jTxt%1'('lo.json@jTxt'())) :- !.
'lo.json#jColl'('jColl%1'('lo.json@jColl'())) :- !.
'lo.json#jSeq'('jSeq%1'('lo.json@jSeq'())) :- !.
'lo.json#jNum'('jNum%1'('lo.json@jNum'())) :- !.
'lo.json@spaces'(0, 'lo.core#[]') :- !.
'lo.json@spaces'(XX, 'lo.core#,..'('lo.core#ss'(" "), XX888)) :- !,
    ocall('-%3'(XX, 1, XX886),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.json@spaces'(XX886, XX888).
'lo.json@spaces'(_, _) :- raise_exception('error'("spaces", 35, 3, 15)).
'lo.json@break'(0, 'lo.core#ssSeq'('lo.core#[]')) :- !.
'lo.json@break'(XX, 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("
"), XX895))) :- !,
    'lo.json@spaces'(XX, XX895).
'lo.json@break'(_, _) :- raise_exception('error'("break", 31, 3, 21)).
'lo.json@dispSeq'('lo.core#[]', X_12, X_13, 'lo.core#[]') :- !.
'lo.json@dispSeq'('lo.core#,..'(Xe, Xl), XSp, Xs, 'lo.core#,..'('lo.core#ss'(Xs), 'lo.core#,..'(XX911, XX914))) :- !,
    'lo.json@dispJson'(Xe, XSp, XX911),
    'lo.json@dispSeq'(Xl, XSp, ",", XX914).
'lo.json@dispSeq'(_, _, _, _) :- raise_exception('error'("dispSeq", 27, 3, 21)).
'lo.json@dispColl'('lo.core#[]', X_14, X_15, 'lo.core#[]') :- !.
'lo.json@dispColl'('lo.core#,..'((Xf, Xe), Xl), XSp, Xs, 'lo.core#,..'('lo.core#ss'(Xs), 'lo.core#,..'(XX930, 'lo.core#,..'(XX932, 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XX937, XX940)))))) :- !,
    'lo.json@break'(XSp, XX930),
    ocall('disp%2'(Xf, XX932),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo.json@dispJson'(Xe, XSp, XX937),
    'lo.json@dispColl'(Xl, XSp, ",", XX940).
'lo.json@dispColl'(_, _, _, _) :- raise_exception('error'("dispColl", 23, 3, 22)).
'lo.json@dispJson'('lo.json#jTrue', X_16, 'lo.core#ss'("true")) :- !.
'lo.json@dispJson'('lo.json#jFalse', X_17, 'lo.core#ss'("false")) :- !.
'lo.json@dispJson'('lo.json#jNull', X_18, 'lo.core#ss'("null")) :- !.
'lo.json@dispJson'('lo.json#jTxt'(XT), X_19, XX959) :- !,
    ocall('disp%2'(XT, XX959),'lo.core$display$lo.core*string','lo.core$display$lo.core*string').
'lo.json@dispJson'('lo.json#jNum'(XD), X_20, XX965) :- !,
    ocall('disp%2'(XD, XX965),'lo.core$display$lo.core*float','lo.core$display$lo.core*float').
'lo.json@dispJson'('lo.json#jColl'(XM), XSp, 'lo.core#ssSeq'('lo.core#,..'('lo.core#sc'(123), 'lo.core#,..'('lo.core#ssSeq'(XX978), 'lo.core#,..'('lo.core#sc'(125), 'lo.core#[]'))))) :- !,
    ocall('pairs%2'(XM, XX972),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('+%3'(XSp, 2, XX976),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.json@dispColl'(XX972, XX976, "", XX978).
'lo.json@dispJson'('lo.json#jSeq'(XL), XSp, 'lo.core#ssSeq'('lo.core#,..'('lo.core#sc'(91), 'lo.core#,..'('lo.core#ssSeq'(XX992), 'lo.core#,..'('lo.core#sc'(93), 'lo.core#[]'))))) :- !,
    'lo.json@dispSeq'(XL, XSp, "", XX992).
'lo.json@dispJson'(_, _, _) :- raise_exception('error'("dispJson", 14, 3, 31)).
'lo.core$display$lo.json*json'('lo.core$display$lo.json*json%1'('lo.core$display$lo.json*json')) :- !.
'lo.core$display$lo.json*json'('disp%2'(XV401, XV402), XLbl101, XThis101) :- !,
    'lo.core$display$lo.json*json@disp'(XV401, XV402, XLbl101, XThis101).
'lo.core$display$lo.json*json'('disp%1'('lo.core$display$lo.json*json^disp'(XLbl102, XThis102)), XLbl102, XThis102).
'lo.core$display$lo.json*json@disp'(Xj, XX1002, XLbV49, XThV49) :- !,
    'lo.json@dispJson'(Xj, 0, XX1002).
'lo.core$display$lo.json*json@disp'(_, _, _, _) :- raise_exception('error'("disp", 10, 5, 24)).
'lo.json@equalJson'('lo.json#jTrue', 'lo.json#jTrue').
'lo.json@equalJson'('lo.json#jFalse', 'lo.json#jFalse').
'lo.json@equalJson'('lo.json#jNull', 'lo.json#jNull').
'lo.json@equalJson'('lo.json#jTxt'(XS1), 'lo.json#jTxt'(XS2)) :- ocall('==%2'(XS1, XS2),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.json@equalJson'('lo.json#jNum'(XD1), 'lo.json#jNum'(XD2)) :- ocall('==%2'(XD1, XD2),'lo.core$equality$lo.core*float','lo.core$equality$lo.core*float').
'lo.json@equalJson'('lo.json#jColl'(XC1), 'lo.json#jColl'(XC2)) :- ocall('==%2'(XC1, XC2),'lo.core$equality$lo.index*map'('lo.core$equality$lo.core*string', 'lo.core$equality$lo.json*json'),'lo.core$equality$lo.index*map'('lo.core$equality$lo.core*string', 'lo.core$equality$lo.json*json')).
'lo.json@equalJson'('lo.json#jSeq'(XL1), 'lo.json#jSeq'(XL2)) :- ocall('==%2'(XL1, XL2),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json'),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json')).
'lo.json@hashJson'('lo.json#jTrue', XX1041) :- !,
    ocall('hash%2'("true", XX1041),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.json@hashJson'('lo.json#jFalse', XX1044) :- !,
    ocall('hash%2'("false", XX1044),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.json@hashJson'('lo.json#jNull', XX1047) :- !,
    ocall('hash%2'("null", XX1047),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.json@hashJson'('lo.json#jNum'(XD), XX1052) :- !,
    ocall('hash%2'(XD, XX1052),'lo.core$equality$lo.core*float','lo.core$equality$lo.core*float').
'lo.json@hashJson'('lo.json#jTxt'(XS), XX1057) :- !,
    ocall('hash%2'(XS, XX1057),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.json@hashJson'('lo.json#jSeq'(XL), XX1062) :- !,
    ocall('hash%2'(XL, XX1062),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json'),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json')).
'lo.json@hashJson'('lo.json#jColl'(XC), XX1068) :- !,
    ocall('hash%2'(XC, XX1068),'lo.core$equality$lo.index*map'('lo.core$equality$lo.core*string', 'lo.core$equality$lo.json*json'),'lo.core$equality$lo.index*map'('lo.core$equality$lo.core*string', 'lo.core$equality$lo.json*json')).
'lo.json@hashJson'(_, _) :- raise_exception('error'("hashJson", 53, 3, 31)).
'lo.core$equality$lo.json*json'('lo.core$equality$lo.json*json%1'('lo.core$equality$lo.json*json')) :- !.
'lo.core$equality$lo.json*json'('==%2'(XV411, XV412), XLbl103, XThis103) :- !,
    'lo.core$equality$lo.json*json@=='(XV411, XV412, XLbl103, XThis103).
'lo.core$equality$lo.json*json'('==%1'('lo.core$equality$lo.json*json^=='(XLbl104, XThis104)), XLbl104, XThis104).
'lo.core$equality$lo.json*json'('hash%2'(XV417, XV418), XLbl105, XThis105) :- !,
    'lo.core$equality$lo.json*json@hash'(XV417, XV418, XLbl105, XThis105).
'lo.core$equality$lo.json*json'('hash%1'('lo.core$equality$lo.json*json^hash'(XLbl106, XThis106)), XLbl106, XThis106).
'lo.core$equality$lo.json*json@=='(XT1, XT2, XLbV50, XThV50) :- 'lo.json@equalJson'(XT1, XT2).
'lo.core$equality$lo.json*json@hash'(XT, XX1078, XLbV50, XThV50) :- !,
    'lo.json@hashJson'(XT, XX1078).
'lo.core$equality$lo.json*json@hash'(_, _, _, _) :- raise_exception('error'("hash", 40, 5, 22)).
'lo.json@skipBlanks'(XStIn1, XStx1, Xlo_core_stream_s1) :- ocall('_hdtl%3'(XStIn1, 32, XNStrm1),Xlo_core_stream_s1,Xlo_core_stream_s1),
    'lo.json@skipBlanks'(XNStrm1, XStx1, Xlo_core_stream_s1).
'lo.json@skipBlanks'(XStIn2, XStx2, Xlo_core_stream_s1) :- ocall('_hdtl%3'(XStIn2, 9, XNStrm2),Xlo_core_stream_s1,Xlo_core_stream_s1),
    'lo.json@skipBlanks'(XNStrm2, XStx2, Xlo_core_stream_s1).
'lo.json@skipBlanks'(XStIn3, XStx3, Xlo_core_stream_s1) :- ocall('_hdtl%3'(XStIn3, 10, XNStrm3),Xlo_core_stream_s1,Xlo_core_stream_s1),
    'lo.json@skipBlanks'(XNStrm3, XStx3, Xlo_core_stream_s1).
'lo.json@skipBlanks'(XStIn4, XStx4, Xlo_core_stream_s1) :- ocall('_hdtl%3'(XStIn4, 13, XNStrm4),Xlo_core_stream_s1,Xlo_core_stream_s1),
    'lo.json@skipBlanks'(XNStrm4, XStx4, Xlo_core_stream_s1).
'lo.json@skipBlanks'(XStIn5, XStIn5, Xlo_core_stream_s1).
'lo.json@digit'(XStIn6, XNStrm5, Xlo_core_stream_s2, 0) :- ocall('_hdtl%3'(XStIn6, 48, XNStrm5),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@digit'(XStIn7, XNStrm6, Xlo_core_stream_s2, 1) :- ocall('_hdtl%3'(XStIn7, 49, XNStrm6),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@digit'(XStIn8, XNStrm7, Xlo_core_stream_s2, 2) :- ocall('_hdtl%3'(XStIn8, 50, XNStrm7),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@digit'(XStIn9, XNStrm8, Xlo_core_stream_s2, 3) :- ocall('_hdtl%3'(XStIn9, 51, XNStrm8),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@digit'(XStIn10, XNStrm9, Xlo_core_stream_s2, 4) :- ocall('_hdtl%3'(XStIn10, 52, XNStrm9),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@digit'(XStIn11, XNStrm10, Xlo_core_stream_s2, 5) :- ocall('_hdtl%3'(XStIn11, 53, XNStrm10),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@digit'(XStIn12, XNStrm11, Xlo_core_stream_s2, 6) :- ocall('_hdtl%3'(XStIn12, 54, XNStrm11),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@digit'(XStIn13, XNStrm12, Xlo_core_stream_s2, 7) :- ocall('_hdtl%3'(XStIn13, 55, XNStrm12),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@digit'(XStIn14, XNStrm13, Xlo_core_stream_s2, 8) :- ocall('_hdtl%3'(XStIn14, 56, XNStrm13),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@digit'(XStIn15, XNStrm14, Xlo_core_stream_s2, 9) :- ocall('_hdtl%3'(XStIn15, 57, XNStrm14),Xlo_core_stream_s2,Xlo_core_stream_s2).
'lo.json@readNatural'(XStIn16, XStx6, Xlo_core_stream_s3, XSoFar, XInt) :- 'lo.json@digit'(XStIn16, XStx5, Xlo_core_stream_s3, XD),
    ocall('*%3'(XSoFar, 10, XX1147),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%3'(XX1147, XD, XX1150),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.json@readNatural'(XStx5, XStx6, Xlo_core_stream_s3, XX1150, XInt).
'lo.json@readNatural'(XStIn17, XStIn17, Xlo_core_stream_s3, XSoFar, XSoFar) :- 'lo.json@Neg1'(XStIn17, X_21, Xlo_core_stream_s3).
'lo.json@fraction'(XStIn18, XStx9, Xlo_core_stream_s4, XScale, XSoFar, XResult) :- 'lo.json@digit'(XStIn18, XStx8, Xlo_core_stream_s4, XD),
    ocall('*%3'(XScale, 0.1, XX1166),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    '_int2flt'(XD, XX1170),
    ocall('*%3'(XX1170, XScale, XX1172),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    ocall('+%3'(XSoFar, XX1172, XX1174),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    'lo.json@fraction'(XStx8, XStx9, Xlo_core_stream_s4, XX1166, XX1174, XResult).
'lo.json@fraction'(XStIn19, XStIn19, Xlo_core_stream_s4, X_22, XFract, XFract).
'lo.json@readDecimal'(XStIn20, XStx10, Xlo_core_stream_s5, XIn) :- ocall('_hdtl%3'(XStIn20, 45, XNStrm15),Xlo_core_stream_s5,Xlo_core_stream_s5),
    'lo.json@readNatural'(XNStrm15, XStx10, Xlo_core_stream_s5, 0, XPl),
    ocall('-%3'(0, XPl, XX1190),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    XIn = XX1190.
'lo.json@readDecimal'(XStIn21, XStx11, Xlo_core_stream_s5, XIn) :- 'lo.json@readNatural'(XStIn21, XStx11, Xlo_core_stream_s5, 0, XIn).
'lo.json@exponent'(XStIn22, XStx12, Xlo_core_stream_s6, XSoFar, XFp) :- 'lo.json@Disj1'(XStIn22, XDjOut1, XNStrm17, XNStrm17, XNStrm16, Xlo_core_stream_s6, XNStrm16, XDjStrm1),
    'lo.json@readDecimal'(XDjOut1, XStx12, Xlo_core_stream_s6, XExp),
    '_int2flt'(XExp, XX1210),
    '_pwr'(10.0, XX1210, XX1211),
    ocall('*%3'(XSoFar, XX1211, XX1212),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    XFp = XX1212.
'lo.json@exponent'(XStIn23, XStIn23, Xlo_core_stream_s6, XFp, XFp).
'lo.json@readMoreNumber'(XStIn24, XStx15, Xlo_core_stream_s7, XFp, XDecimal) :- ocall('_hdtl%3'(XStIn24, 46, XNStrm18),Xlo_core_stream_s7,Xlo_core_stream_s7),
    'lo.json@Hed1'(XNStrm18, X_23, Xlo_core_stream_s7),
    'lo.json@fraction'(XNStrm18, XStx14, Xlo_core_stream_s7, 0.1, 0.0, XFr),
    '_int2flt'(XDecimal, XX1229),
    ocall('+%3'(XX1229, XFr, XX1231),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    'lo.json@exponent'(XStx14, XStx15, Xlo_core_stream_s7, XX1231, XFp).
'lo.json@readMoreNumber'(XStIn25, XStIn25, Xlo_core_stream_s7, XX1236, XIx) :- ocall('_coerce%2'(XIx, XX1236),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float').
'lo.json@readNumber'(XStIn26, XStx17, Xlo_core_stream_s8, XFlt) :- 'lo.json@readNatural'(XStIn26, XStx16, Xlo_core_stream_s8, 0, XFirst),
    'lo.json@readMoreNumber'(XStx16, XStx17, Xlo_core_stream_s8, XFlt, XFirst).
'lo.json@hexDigit'(XStIn27, XStx18, Xlo_core_stream_s9, XX) :- 'lo.json@digit'(XStIn27, XStx18, Xlo_core_stream_s9, XX).
'lo.json@hexDigit'(XStIn28, XNStrm19, Xlo_core_stream_s9, 10) :- ocall('_hdtl%3'(XStIn28, 97, XNStrm19),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn29, XNStrm20, Xlo_core_stream_s9, 11) :- ocall('_hdtl%3'(XStIn29, 98, XNStrm20),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn30, XNStrm21, Xlo_core_stream_s9, 12) :- ocall('_hdtl%3'(XStIn30, 99, XNStrm21),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn31, XNStrm22, Xlo_core_stream_s9, 13) :- ocall('_hdtl%3'(XStIn31, 100, XNStrm22),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn32, XNStrm23, Xlo_core_stream_s9, 14) :- ocall('_hdtl%3'(XStIn32, 101, XNStrm23),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn33, XNStrm24, Xlo_core_stream_s9, 15) :- ocall('_hdtl%3'(XStIn33, 102, XNStrm24),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn34, XNStrm25, Xlo_core_stream_s9, 10) :- ocall('_hdtl%3'(XStIn34, 65, XNStrm25),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn35, XNStrm26, Xlo_core_stream_s9, 11) :- ocall('_hdtl%3'(XStIn35, 66, XNStrm26),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn36, XNStrm27, Xlo_core_stream_s9, 12) :- ocall('_hdtl%3'(XStIn36, 67, XNStrm27),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn37, XNStrm28, Xlo_core_stream_s9, 13) :- ocall('_hdtl%3'(XStIn37, 68, XNStrm28),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn38, XNStrm29, Xlo_core_stream_s9, 14) :- ocall('_hdtl%3'(XStIn38, 69, XNStrm29),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@hexDigit'(XStIn39, XNStrm30, Xlo_core_stream_s9, 15) :- ocall('_hdtl%3'(XStIn39, 70, XNStrm30),Xlo_core_stream_s9,Xlo_core_stream_s9).
'lo.json@readHex'(XStIn40, XStx20, Xlo_core_stream_s10, XSoFar, XInt, XCnt) :- 'lo.core@>'('lo.core$comp$lo.core*integer', XCnt, 0),
    'lo.json@hexDigit'(XStIn40, XStx19, Xlo_core_stream_s10, XD),
    ocall('*%3'(XSoFar, 10, XX1308),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%3'(XX1308, XD, XX1311),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('-%3'(XCnt, 1, XX1315),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.json@readHex'(XStx19, XStx20, Xlo_core_stream_s10, XX1311, XInt, XX1315).
'lo.json@readHex'(XStIn41, XStIn41, Xlo_core_stream_s10, XSoFar, XSoFar, X_24).
'lo.json@parseStr'(XStIn42, XStIn42, Xlo_core_stream_s11, 'lo.core#[]') :- 'lo.json@Hed2'(XStIn42, XNStrm31, Xlo_core_stream_s11, XNStrm31, XHedStrm2).
'lo.json@parseStr'(XStIn43, XStx22, Xlo_core_stream_s11, 'lo.core#,..'(XH, Xl)) :- ocall('_hdtl%3'(XStIn43, 92, XNStrm32),Xlo_core_stream_s11,Xlo_core_stream_s11),
    ocall('_hdtl%3'(XNStrm32, 117, XNStrm33),Xlo_core_stream_s11,Xlo_core_stream_s11),
    'lo.json@readHex'(XNStrm33, XStx21, Xlo_core_stream_s11, 0, XH, 4),
    'lo.json@parseStr'(XStx21, XStx22, Xlo_core_stream_s11, Xl).
'lo.json@parseStr'(XStIn44, XStx23, Xlo_core_stream_s11, 'lo.core#,..'(8, Xl)) :- ocall('_hdtl%3'(XStIn44, 92, XNStrm34),Xlo_core_stream_s11,Xlo_core_stream_s11),
    ocall('_hdtl%3'(XNStrm34, 98, XNStrm35),Xlo_core_stream_s11,Xlo_core_stream_s11),
    'lo.json@parseStr'(XNStrm35, XStx23, Xlo_core_stream_s11, Xl).
'lo.json@parseStr'(XStIn45, XStx24, Xlo_core_stream_s11, 'lo.core#,..'(102, Xl)) :- ocall('_hdtl%3'(XStIn45, 92, XNStrm36),Xlo_core_stream_s11,Xlo_core_stream_s11),
    ocall('_hdtl%3'(XNStrm36, 102, XNStrm37),Xlo_core_stream_s11,Xlo_core_stream_s11),
    'lo.json@parseStr'(XNStrm37, XStx24, Xlo_core_stream_s11, Xl).
'lo.json@parseStr'(XStIn46, XStx25, Xlo_core_stream_s11, 'lo.core#,..'(10, Xl)) :- ocall('_hdtl%3'(XStIn46, 92, XNStrm38),Xlo_core_stream_s11,Xlo_core_stream_s11),
    ocall('_hdtl%3'(XNStrm38, 110, XNStrm39),Xlo_core_stream_s11,Xlo_core_stream_s11),
    'lo.json@parseStr'(XNStrm39, XStx25, Xlo_core_stream_s11, Xl).
'lo.json@parseStr'(XStIn47, XStx26, Xlo_core_stream_s11, 'lo.core#,..'(13, Xl)) :- ocall('_hdtl%3'(XStIn47, 92, XNStrm40),Xlo_core_stream_s11,Xlo_core_stream_s11),
    ocall('_hdtl%3'(XNStrm40, 114, XNStrm41),Xlo_core_stream_s11,Xlo_core_stream_s11),
    'lo.json@parseStr'(XNStrm41, XStx26, Xlo_core_stream_s11, Xl).
'lo.json@parseStr'(XStIn48, XStx27, Xlo_core_stream_s11, 'lo.core#,..'(9, Xl)) :- ocall('_hdtl%3'(XStIn48, 92, XNStrm42),Xlo_core_stream_s11,Xlo_core_stream_s11),
    ocall('_hdtl%3'(XNStrm42, 116, XNStrm43),Xlo_core_stream_s11,Xlo_core_stream_s11),
    'lo.json@parseStr'(XNStrm43, XStx27, Xlo_core_stream_s11, Xl).
'lo.json@parseStr'(XStIn49, XStx28, Xlo_core_stream_s11, 'lo.core#,..'(XX, Xl)) :- ocall('_hdtl%3'(XStIn49, 92, XNStrm44),Xlo_core_stream_s11,Xlo_core_stream_s11),
    ocall('_hdtl%3'(XNStrm44, XX, XNStrm45),Xlo_core_stream_s11,Xlo_core_stream_s11),
    'lo.json@parseStr'(XNStrm45, XStx28, Xlo_core_stream_s11, Xl).
'lo.json@parseStr'(XStIn50, XStx29, Xlo_core_stream_s11, 'lo.core#,..'(XC, Xl)) :- ocall('_hdtl%3'(XStIn50, XC, XNStrm46),Xlo_core_stream_s11,Xlo_core_stream_s11),
    'lo.json@parseStr'(XNStrm46, XStx29, Xlo_core_stream_s11, Xl).
'lo.json@parseField'(XStIn51, XNStrm48, Xlo_core_stream_s12, XX1420) :- ocall('_hdtl%3'(XStIn51, 34, XNStrm47),Xlo_core_stream_s12,Xlo_core_stream_s12),
    'lo.json@parseStr'(XNStrm47, XStx30, Xlo_core_stream_s12, XS),
    ocall('_hdtl%3'(XStx30, 34, XNStrm48),Xlo_core_stream_s12,Xlo_core_stream_s12),
    'implode'(XS, XX1420).
'lo.json@parseMoreColl'(XStIn52, XStIn52, Xlo_core_stream_s13, Xm, Xm) :- 'lo.json@Hed3'(XStIn52, XNStrm49, Xlo_core_stream_s13, XNStrm49, XHedStrm3).
'lo.json@parseMoreColl'(XStIn53, XStx36, Xlo_core_stream_s13, Xm, Xc) :- ocall('_hdtl%3'(XStIn53, 44, XNStrm50),Xlo_core_stream_s13,Xlo_core_stream_s13),
    'lo.json@skipBlanks'(XNStrm50, XStx31, Xlo_core_stream_s13),
    'lo.json@parseField'(XStx31, XStx32, Xlo_core_stream_s13, Xf),
    'lo.json@skipBlanks'(XStx32, XStx33, Xlo_core_stream_s13),
    ocall('_hdtl%3'(XStx33, 58, XNStrm51),Xlo_core_stream_s13,Xlo_core_stream_s13),
    'lo.json@skipBlanks'(XNStrm51, XStx34, Xlo_core_stream_s13),
    'lo.json@jP'(XStx34, XStx35, Xlo_core_stream_s13, Xv),
    ocall('_put%4'(Xm, Xf, Xv, XX1455),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.json@parseMoreColl'(XStx35, XStx36, Xlo_core_stream_s13, XX1455, Xc).
'lo.json@parseColl'(XStIn54, XStIn54, Xlo_core_stream_s14, Xm, Xm) :- 'lo.json@Hed4'(XStIn54, XNStrm52, Xlo_core_stream_s14, XNStrm52, XHedStrm4).
'lo.json@parseColl'(XStIn55, XStx41, Xlo_core_stream_s14, Xm, Xc) :- 'lo.json@parseField'(XStIn55, XStx37, Xlo_core_stream_s14, Xf),
    'lo.json@skipBlanks'(XStx37, XStx38, Xlo_core_stream_s14),
    ocall('_hdtl%3'(XStx38, 58, XNStrm53),Xlo_core_stream_s14,Xlo_core_stream_s14),
    'lo.json@skipBlanks'(XNStrm53, XStx39, Xlo_core_stream_s14),
    'lo.json@jP'(XStx39, XStx40, Xlo_core_stream_s14, Xv),
    ocall('_put%4'(Xm, Xf, Xv, XX1481),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.json@parseMoreColl'(XStx40, XStx41, Xlo_core_stream_s14, XX1481, Xc).
'lo.json@moreSeq'(XStIn56, XStIn56, Xlo_core_stream_s15, 'lo.core#[]') :- 'lo.json@Hed5'(XStIn56, XNStrm54, Xlo_core_stream_s15, XNStrm54, XHedStrm5).
'lo.json@moreSeq'(XStIn57, XStx45, Xlo_core_stream_s15, 'lo.core#,..'(Xe, Xl)) :- ocall('_hdtl%3'(XStIn57, 44, XNStrm55),Xlo_core_stream_s15,Xlo_core_stream_s15),
    'lo.json@skipBlanks'(XNStrm55, XStx42, Xlo_core_stream_s15),
    'lo.json@jP'(XStx42, XStx43, Xlo_core_stream_s15, Xe),
    'lo.json@skipBlanks'(XStx43, XStx44, Xlo_core_stream_s15),
    'lo.json@moreSeq'(XStx44, XStx45, Xlo_core_stream_s15, Xl).
'lo.json@parseSeq'(XStIn58, XStIn58, Xlo_core_stream_s16, 'lo.core#[]') :- 'lo.json@Hed6'(XStIn58, XNStrm56, Xlo_core_stream_s16, XNStrm56, XHedStrm6).
'lo.json@parseSeq'(XStIn59, XStx48, Xlo_core_stream_s16, 'lo.core#,..'(Xe, Xl)) :- 'lo.json@jP'(XStIn59, XStx46, Xlo_core_stream_s16, Xe),
    'lo.json@skipBlanks'(XStx46, XStx47, Xlo_core_stream_s16),
    'lo.json@moreSeq'(XStx47, XStx48, Xlo_core_stream_s16, Xl).
'lo.json@jP'(XStIn60, XNStrm60, Xlo_core_stream_s17, 'lo.json#jTrue') :- ocall('_hdtl%3'(XStIn60, 116, XNStrm57),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm57, 114, XNStrm58),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm58, 117, XNStrm59),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm59, 101, XNStrm60),Xlo_core_stream_s17,Xlo_core_stream_s17).
'lo.json@jP'(XStIn61, XNStrm65, Xlo_core_stream_s17, 'lo.json#jFalse') :- ocall('_hdtl%3'(XStIn61, 102, XNStrm61),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm61, 97, XNStrm62),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm62, 108, XNStrm63),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm63, 115, XNStrm64),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm64, 101, XNStrm65),Xlo_core_stream_s17,Xlo_core_stream_s17).
'lo.json@jP'(XStIn62, XNStrm69, Xlo_core_stream_s17, 'lo.json#jNull') :- ocall('_hdtl%3'(XStIn62, 110, XNStrm66),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm66, 117, XNStrm67),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm67, 108, XNStrm68),Xlo_core_stream_s17,Xlo_core_stream_s17),
    ocall('_hdtl%3'(XNStrm68, 108, XNStrm69),Xlo_core_stream_s17,Xlo_core_stream_s17).
'lo.json@jP'(XStIn63, XStx49, Xlo_core_stream_s17, 'lo.json#jNum'(XX1565)) :- ocall('_hdtl%3'(XStIn63, 45, XNStrm70),Xlo_core_stream_s17,Xlo_core_stream_s17),
    'lo.json@readNumber'(XNStrm70, XStx49, Xlo_core_stream_s17, XN),
    ocall('zero%1'(XXV6),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('-%3'(XXV6, XN, XX1565),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float').
'lo.json@jP'(XStIn64, XStx51, Xlo_core_stream_s17, 'lo.json#jNum'(XN)) :- 'lo.json@Hed7'(XStIn64, X_25, Xlo_core_stream_s17),
    'lo.json@readNumber'(XStIn64, XStx51, Xlo_core_stream_s17, XN).
'lo.json@jP'(XStIn65, XNStrm72, Xlo_core_stream_s17, 'lo.json#jTxt'(XX1582)) :- ocall('_hdtl%3'(XStIn65, 34, XNStrm71),Xlo_core_stream_s17,Xlo_core_stream_s17),
    'lo.json@parseStr'(XNStrm71, XStx52, Xlo_core_stream_s17, XS),
    ocall('_hdtl%3'(XStx52, 34, XNStrm72),Xlo_core_stream_s17,Xlo_core_stream_s17),
    'implode'(XS, XX1582).
'lo.json@jP'(XStIn66, XNStrm74, Xlo_core_stream_s17, 'lo.json#jSeq'(XL)) :- ocall('_hdtl%3'(XStIn66, 91, XNStrm73),Xlo_core_stream_s17,Xlo_core_stream_s17),
    'lo.json@skipBlanks'(XNStrm73, XStx53, Xlo_core_stream_s17),
    'lo.json@parseSeq'(XStx53, XStx54, Xlo_core_stream_s17, XL),
    ocall('_hdtl%3'(XStx54, 93, XNStrm74),Xlo_core_stream_s17,Xlo_core_stream_s17).
'lo.json@jP'(XStIn67, XNStrm76, Xlo_core_stream_s17, 'lo.json#jColl'(XM)) :- ocall('_hdtl%3'(XStIn67, 123, XNStrm75),Xlo_core_stream_s17,Xlo_core_stream_s17),
    'lo.json@skipBlanks'(XNStrm75, XStx55, Xlo_core_stream_s17),
    ocall('_empty%1'(XXV7),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.json@parseColl'(XStx55, XStx56, Xlo_core_stream_s17, XXV7, XM),
    ocall('_hdtl%3'(XStx56, 125, XNStrm76),Xlo_core_stream_s17,Xlo_core_stream_s17).
'lo.json@parseJson'(XStIn68, XStx58, Xlo_core_stream_s18, XJ) :- 'lo.json@skipBlanks'(XStIn68, XStx57, Xlo_core_stream_s18),
    'lo.json@jP'(XStx57, XStx58, Xlo_core_stream_s18, XJ).
'lo.coerce$coercion$lo.core*string$lo.json*json'('lo.coerce$coercion$lo.core*string$lo.json*json%1'('lo.coerce$coercion$lo.core*string$lo.json*json')) :- !.
'lo.coerce$coercion$lo.core*string$lo.json*json'('_coerce%2'(XV485, XV486), XLbl107, XThis107) :- !,
    'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XV485, XV486, XLbl107, XThis107).
'lo.coerce$coercion$lo.core*string$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'(XLbl108, XThis108)), XLbl108, XThis108).
'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XS, XJ, XLbV51, XThV51) :- 'explode'(XS, XX1625),
    'lo.json@parseJson'(XX1625, XStx59, 'lo.core$stream$lo.core*list', XJ),
    ocall('_eof%1'(XStx59),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !.
'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 62, 5, 45)).
'lo.json@entryCoerce'(Xlo_coerce_coercion_v_lo_json_json1, Xe, XX1634) :- !,
    ocall('_coerce%2'(Xe, XX1634),Xlo_coerce_coercion_v_lo_json_json1,Xlo_coerce_coercion_v_lo_json_json1).
'lo.json@entryCoerce'(_, _) :- raise_exception('error'("entryCoerce", 179, 3, 25)).
'lo.json@mapJson'(Xlo_coerce_coercion_v_lo_json_json2, XM, 'lo.json#jColl'(XX1641)) :- !,
    'lo.json@entryCoerce'(Xlo_coerce_coercion_v_lo_json_json2, XX1640),
    'lo.index@mapMap'(XM, XX1640, XX1641).
'lo.json@mapJson'(_, _) :- raise_exception('error'("mapJson", 176, 3, 42)).
'lo.coerce$coercion$lo.index*map$lo.json*json'('lo.coerce$coercion$lo.index*map$lo.json*json%1'('lo.coerce$coercion$lo.index*map$lo.json*json')) :- !.
'lo.coerce$coercion$lo.index*map$lo.json*json'('_coerce%2'(XV495, XV496), XLbl109, XThis109) :- !,
    'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XV495, XV496, XLbl109, XThis109).
'lo.coerce$coercion$lo.index*map$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'(XLbl110, XThis110)), XLbl110, XThis110).
'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XM, XX1648, XLbV52, XThV52) :- XLbV52 = 'lo.coerce$coercion$lo.index*map$lo.json*json'(Xlo_coerce_coercion_v_lo_json_json3),
    !,
    'lo.json@mapJson'(Xlo_coerce_coercion_v_lo_json_json3, XM, XX1648).
'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 172, 5, 24)).
'lo.json@cList'(Xlo_coerce_coercion_v_lo_json_json4, 'lo.core#[]', 'lo.core#[]') :- !.
'lo.json@cList'(Xlo_coerce_coercion_v_lo_json_json4, 'lo.core#,..'(Xe, Xl), 'lo.core#,..'(XX1657, XX1661)) :- !,
    ocall('_coerce%2'(Xe, XX1657),Xlo_coerce_coercion_v_lo_json_json4,Xlo_coerce_coercion_v_lo_json_json4),
    'lo.json@cList'(Xlo_coerce_coercion_v_lo_json_json4, Xl, XX1661).
'lo.json@cList'(_, _) :- raise_exception('error'("cList", 186, 3, 15)).
'lo.coerce$coercion$lo.core*list$lo.json*json'('lo.coerce$coercion$lo.core*list$lo.json*json%1'('lo.coerce$coercion$lo.core*list$lo.json*json')) :- !.
'lo.coerce$coercion$lo.core*list$lo.json*json'('_coerce%2'(XV503, XV504), XLbl111, XThis111) :- !,
    'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XV503, XV504, XLbl111, XThis111).
'lo.coerce$coercion$lo.core*list$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'(XLbl112, XThis112)), XLbl112, XThis112).
'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XL, 'lo.json#jSeq'(XX1668), XLbV53, XThV53) :- XLbV53 = 'lo.coerce$coercion$lo.core*list$lo.json*json'(Xlo_coerce_coercion_v_lo_json_json5),
    !,
    'lo.json@cList'(Xlo_coerce_coercion_v_lo_json_json5, XL, XX1668).
'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 182, 5, 28)).
'lo.json@jTrue'('lo.json#jTrue') :- !.
'lo.json@jFalse'('lo.json#jFalse') :- !.
'lo.json@jNull'('lo.json#jNull') :- !.
'lo.json^spaces'('_call%2'(XV384, XV385), 'lo.json^spaces', _) :- 'lo.json@spaces'(XV384, XV385).
'lo.json^break'('_call%2'(XV386, XV387), 'lo.json^break', _) :- 'lo.json@break'(XV386, XV387).
'lo.json^dispSeq'('_call%4'(XV388, XV389, XV390, XV391), 'lo.json^dispSeq', _) :- 'lo.json@dispSeq'(XV388, XV389, XV390, XV391).
'lo.json^dispColl'('_call%4'(XV392, XV393, XV394, XV395), 'lo.json^dispColl', _) :- 'lo.json@dispColl'(XV392, XV393, XV394, XV395).
'lo.json^dispJson'('_call%3'(XV396, XV397, XV398), 'lo.json^dispJson', _) :- 'lo.json@dispJson'(XV396, XV397, XV398).
'lo.core$display$lo.json*json^disp'('_call%2'(XV399, XV400), 'lo.core$display$lo.json*json^disp'(XLbV49, XThV49), _) :- 'lo.core$display$lo.json*json@disp'(XV399, XV400, XLbV49, XThV49).
'lo.core$display$lo.json*json^disp'('_call%2'(XV403, XV404), 'lo.core$display$lo.json*json^disp'(XLbV49, XThV49), _) :- 'lo.core$display$lo.json*json@disp'(XV403, XV404, XLbV49, XThV49).
'lo.json^equalJson'('_call%2'(XV405, XV406), 'lo.json^equalJson', _) :- 'lo.json@equalJson'(XV405, XV406).
'lo.json^hashJson'('_call%2'(XV407, XV408), 'lo.json^hashJson', _) :- 'lo.json@hashJson'(XV407, XV408).
'lo.core$equality$lo.json*json^=='('_call%2'(XV409, XV410), 'lo.core$equality$lo.json*json^=='(XLbV50, XThV50), _) :- 'lo.core$equality$lo.json*json@=='(XV409, XV410, XLbV50, XThV50).
'lo.core$equality$lo.json*json^=='('_call%2'(XV413, XV414), 'lo.core$equality$lo.json*json^=='(XLbV50, XThV50), _) :- 'lo.core$equality$lo.json*json@=='(XV413, XV414, XLbV50, XThV50).
'lo.core$equality$lo.json*json^hash'('_call%2'(XV415, XV416), 'lo.core$equality$lo.json*json^hash'(XLbV50, XThV50), _) :- 'lo.core$equality$lo.json*json@hash'(XV415, XV416, XLbV50, XThV50).
'lo.core$equality$lo.json*json^hash'('_call%2'(XV419, XV420), 'lo.core$equality$lo.json*json^hash'(XLbV50, XThV50), _) :- 'lo.core$equality$lo.json*json@hash'(XV419, XV420, XLbV50, XThV50).
'lo.json^skipBlanks'('_call%2'(XV421, XV422), 'lo.json^skipBlanks', _) :- 'lo.json@skipBlanks'(XV421, XV422).
'lo.json^digit'('_call%3'(XV423, XV424, XV425), 'lo.json^digit', _) :- 'lo.json@digit'(XV423, XV424, XV425).
'lo.json@Neg1'(XNegStrm1, X_21, Xlo_core_stream_s3) :- 'lo.json@digit'(XNegStrm1, XStx7, Xlo_core_stream_s3, X_21),
    !,
    fail.
'lo.json@Neg1'(XNegStrm1, X_21, Xlo_core_stream_s3).
'lo.json^readNatural'('_call%4'(XV426, XV427, XV428, XV429), 'lo.json^readNatural', _) :- 'lo.json@readNatural'(XV426, XV427, XV428, XV429).
'lo.json^fraction'('_call%5'(XV430, XV431, XV432, XV433, XV434), 'lo.json^fraction', _) :- 'lo.json@fraction'(XV430, XV431, XV432, XV433, XV434).
'lo.json^readDecimal'('_call%3'(XV435, XV436, XV437), 'lo.json^readDecimal', _) :- 'lo.json@readDecimal'(XV435, XV436, XV437).
'lo.json@Disj1'(XDjStrm1, XNStrm16, XNStrm17, XNStrm17, XNStrm16, Xlo_core_stream_s6, XNStrm16, XDjStrm1) :- ocall('_hdtl%3'(XDjStrm1, 101, XNStrm16),Xlo_core_stream_s6,Xlo_core_stream_s6).
'lo.json@Disj1'(XDjStrm1, XNStrm17, XNStrm17, XNStrm17, XNStrm16, Xlo_core_stream_s6, XNStrm16, XDjStrm1) :- ocall('_hdtl%3'(XDjStrm1, 69, XNStrm17),Xlo_core_stream_s6,Xlo_core_stream_s6).
'lo.json^exponent'('_call%4'(XV438, XV439, XV440, XV441), 'lo.json^exponent', _) :- 'lo.json@exponent'(XV438, XV439, XV440, XV441).
'lo.json@Hed1'(XHedStrm1, X_23, Xlo_core_stream_s7) :- 'lo.json@digit'(XHedStrm1, XStx13, Xlo_core_stream_s7, X_23).
'lo.json^readMoreNumber'('_call%4'(XV442, XV443, XV444, XV445), 'lo.json^readMoreNumber', _) :- 'lo.json@readMoreNumber'(XV442, XV443, XV444, XV445).
'lo.json^readNumber'('_call%3'(XV446, XV447, XV448), 'lo.json^readNumber', _) :- 'lo.json@readNumber'(XV446, XV447, XV448).
'lo.json^hexDigit'('_call%3'(XV449, XV450, XV451), 'lo.json^hexDigit', _) :- 'lo.json@hexDigit'(XV449, XV450, XV451).
'lo.json^readHex'('_call%5'(XV452, XV453, XV454, XV455, XV456), 'lo.json^readHex', _) :- 'lo.json@readHex'(XV452, XV453, XV454, XV455, XV456).
'lo.json@Hed2'(XHedStrm2, XNStrm31, Xlo_core_stream_s11, XNStrm31, XHedStrm2) :- ocall('_hdtl%3'(XHedStrm2, 34, XNStrm31),Xlo_core_stream_s11,Xlo_core_stream_s11).
'lo.json^parseStr'('_call%3'(XV457, XV458, XV459), 'lo.json^parseStr', _) :- 'lo.json@parseStr'(XV457, XV458, XV459).
'lo.json^parseField'('_call%3'(XV460, XV461, XV462), 'lo.json^parseField', _) :- 'lo.json@parseField'(XV460, XV461, XV462).
'lo.json@Hed3'(XHedStrm3, XNStrm49, Xlo_core_stream_s13, XNStrm49, XHedStrm3) :- ocall('_hdtl%3'(XHedStrm3, 125, XNStrm49),Xlo_core_stream_s13,Xlo_core_stream_s13).
'lo.json^parseMoreColl'('_call%4'(XV463, XV464, XV465, XV466), 'lo.json^parseMoreColl', _) :- 'lo.json@parseMoreColl'(XV463, XV464, XV465, XV466).
'lo.json@Hed4'(XHedStrm4, XNStrm52, Xlo_core_stream_s14, XNStrm52, XHedStrm4) :- ocall('_hdtl%3'(XHedStrm4, 125, XNStrm52),Xlo_core_stream_s14,Xlo_core_stream_s14).
'lo.json^parseColl'('_call%4'(XV467, XV468, XV469, XV470), 'lo.json^parseColl', _) :- 'lo.json@parseColl'(XV467, XV468, XV469, XV470).
'lo.json@Hed5'(XHedStrm5, XNStrm54, Xlo_core_stream_s15, XNStrm54, XHedStrm5) :- ocall('_hdtl%3'(XHedStrm5, 93, XNStrm54),Xlo_core_stream_s15,Xlo_core_stream_s15).
'lo.json^moreSeq'('_call%3'(XV471, XV472, XV473), 'lo.json^moreSeq', _) :- 'lo.json@moreSeq'(XV471, XV472, XV473).
'lo.json@Hed6'(XHedStrm6, XNStrm56, Xlo_core_stream_s16, XNStrm56, XHedStrm6) :- ocall('_hdtl%3'(XHedStrm6, 93, XNStrm56),Xlo_core_stream_s16,Xlo_core_stream_s16).
'lo.json^parseSeq'('_call%3'(XV474, XV475, XV476), 'lo.json^parseSeq', _) :- 'lo.json@parseSeq'(XV474, XV475, XV476).
'lo.json@Hed7'(XHedStrm7, X_25, Xlo_core_stream_s17) :- 'lo.json@digit'(XHedStrm7, XStx50, Xlo_core_stream_s17, X_25).
'lo.json^jP'('_call%3'(XV477, XV478, XV479), 'lo.json^jP', _) :- 'lo.json@jP'(XV477, XV478, XV479).
'lo.json^parseJson'('_call%3'(XV480, XV481, XV482), 'lo.json^parseJson', _) :- 'lo.json@parseJson'(XV480, XV481, XV482).
'lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'('_call%2'(XV483, XV484), 'lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'(XLbV51, XThV51), _) :- 'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XV483, XV484, XLbV51, XThV51).
'lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'('_call%2'(XV487, XV488), 'lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'(XLbV51, XThV51), _) :- 'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XV487, XV488, XLbV51, XThV51).
'lo.json^entryCoerce'('_call%2'(XV489, XV490), 'lo.json^entryCoerce', _) :- 'lo.json@entryCoerce'(XV489, XV490).
'lo.json^mapJson'('_call%2'(XV491, XV492), 'lo.json^mapJson', _) :- 'lo.json@mapJson'(XV491, XV492).
'lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'('_call%2'(XV493, XV494), 'lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'(XLbV52, XThV52), _) :- 'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XV493, XV494, XLbV52, XThV52).
'lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'('_call%2'(XV497, XV498), 'lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'(XLbV52, XThV52), _) :- 'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XV497, XV498, XLbV52, XThV52).
'lo.json^cList'('_call%2'(XV499, XV500), 'lo.json^cList', _) :- 'lo.json@cList'(XV499, XV500).
'lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'('_call%2'(XV501, XV502), 'lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'(XLbV53, XThV53), _) :- 'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XV501, XV502, XLbV53, XThV53).
'lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'('_call%2'(XV505, XV506), 'lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'(XLbV53, XThV53), _) :- 'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XV505, XV506, XLbV53, XThV53).
