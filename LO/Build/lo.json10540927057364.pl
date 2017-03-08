'#pkg'("n7o7'()7'n2o2'pkg's'lo.json's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I8'jTrue't'lo.json*json''jFalse't'lo.json*json''jNull't'lo.json*json''jTxt'CT1St'lo.json*json''jColl'CT1Uz2'lo.index*map'2St'lo.json*json't'lo.json*json''jSeq'CT1Lt'lo.json*json't'lo.json*json''jNum'CT1ft'lo.json*json''parseJson':k's'|GT1t'lo.json*json'k's'c'lo.core$stream'T1k's'T1i\"s\"I1'json'Yt'lo.json*json'I0\"n7o7'()7's'jTrue's'jFalse's'jNull's'jTxt's'jColl's'jSeq's'jNum'n0o0'()0'n5o5'()5'n2o2'()2's'lo.core$display$lo.json*json's\"c'lo.core$display'T1t'lo.json*json'T0\"n2o2'()2's'lo.core$equality$lo.json*json's\"c'lo.core$equality'T1t'lo.json*json'T0\"n2o2'()2's'lo.coerce$coercion$lo.core*string$lo.json*json's\"c'lo.coerce$coercion'T2St'lo.json*json'T0\"n2o2'()2's'lo.coerce$coercion$lo.index*map$lo.json*json's\":k'v'|c'lo.coerce$coercion'T2Uz2'lo.index*map'2Sk'v't'lo.json*json'T0c'lo.coerce$coercion'T2k'v't'lo.json*json'T0\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.json*json's\":k'v'|c'lo.coerce$coercion'T2Lk'v't'lo.json*json'T0c'lo.coerce$coercion'T2k'v't'lo.json*json'T0\"").
'lo.json@init'():- !.
'lo.json#jTrue'('jTrue%1'('lo.json@jTrue')):- !.
'lo.json#jFalse'('jFalse%1'('lo.json@jFalse')):- !.
'lo.json#jNull'('jNull%1'('lo.json@jNull')):- !.
'lo.json#jTxt'('jTxt%1'('lo.json@jTxt'())):- !.
'lo.json#jColl'('jColl%1'('lo.json@jColl'())):- !.
'lo.json#jSeq'('jSeq%1'('lo.json@jSeq'())):- !.
'lo.json#jNum'('jNum%1'('lo.json@jNum'())):- !.
'lo.json@spaces'(0, 'lo.core#[]'):- !.
'lo.json@spaces'(XX, 'lo.core#,..'('lo.core#ss'(" "), XXd8832)):- !,
    ocall('-%1'(XXV1910),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XX, 1, XXe1889),XXV1910,XXV1910),
    'lo.json@spaces'(XXe1889, XXd8832).
'lo.json@spaces'(_, _):- raise_exception('error'("lo.json@spaces", 35, 3, 15)).
'lo.json@break'(0, 'lo.core#ssSeq'('lo.core#[]')):- !.
'lo.json@break'(XX, 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("
"), XXd8836))):- !,
    'lo.json@spaces'(XX, XXd8836).
'lo.json@break'(_, _):- raise_exception('error'("lo.json@break", 31, 3, 21)).
'lo.json@dispColl'('lo.core#[]', X_5438, X_5439, 'lo.core#[]'):- !.
'lo.json@dispColl'('lo.core#,..'('()2'(Xf, Xe), Xl), XSp, Xs, 'lo.core#,..'('lo.core#ss'(Xs), 'lo.core#,..'(XXd8840, 'lo.core#,..'(XXe1890, 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXd8842, XXd8843)))))):- !,
    ocall('disp%1'(XXV1911),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo.json@break'(XSp, XXd8840),
    ocall('_call%2'(Xf, XXe1890),XXV1911,XXV1911),
    'lo.json@dispJson'(Xe, XSp, XXd8842),
    'lo.json@dispColl'(Xl, XSp, ",", XXd8843).
'lo.json@dispColl'(_, _, _, _):- raise_exception('error'("lo.json@dispColl", 23, 3, 22)).
'lo.json@dispSeq'('lo.core#[]', X_5446, X_5447, 'lo.core#[]'):- !.
'lo.json@dispSeq'('lo.core#,..'(Xe, Xl), XSp, Xs, 'lo.core#,..'('lo.core#ss'(Xs), 'lo.core#,..'(XXd8850, XXd8851))):- !,
    'lo.json@dispJson'(Xe, XSp, XXd8850),
    'lo.json@dispSeq'(Xl, XSp, ",", XXd8851).
'lo.json@dispSeq'(_, _, _, _):- raise_exception('error'("lo.json@dispSeq", 27, 3, 21)).
'lo.json@dispJson'('lo.json#jTrue', X_5451, 'lo.core#ss'("true")):- !.
'lo.json@dispJson'('lo.json#jFalse', X_5452, 'lo.core#ss'("false")):- !.
'lo.json@dispJson'('lo.json#jNull', X_5453, 'lo.core#ss'("null")):- !.
'lo.json@dispJson'('lo.json#jTxt'(XT), X_5454, XXe1891):- !,
    ocall('disp%1'(XXV1912),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XT, XXe1891),XXV1912,XXV1912).
'lo.json@dispJson'('lo.json#jNum'(XD), X_5455, XXe1892):- !,
    ocall('disp%1'(XXV1913),'lo.core$display$lo.core*float','lo.core$display$lo.core*float'),
    ocall('_call%2'(XD, XXe1892),XXV1913,XXV1913).
'lo.json@dispJson'('lo.json#jColl'(XM), XSp, 'lo.core#ssSeq'('lo.core#,..'('lo.core#sc'(123), 'lo.core#,..'('lo.core#ssSeq'(XXd8859), 'lo.core#,..'('lo.core#sc'(125), 'lo.core#[]'))))):- !,
    ocall('pairs%1'(XXV1914),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('+%1'(XXV1915),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XM, XXe1893),XXV1914,XXV1914),
    ocall('_call%3'(XSp, 2, XXe1894),XXV1915,XXV1915),
    'lo.json@dispColl'(XXe1893, XXe1894, "", XXd8859).
'lo.json@dispJson'('lo.json#jSeq'(XL), XSp, 'lo.core#ssSeq'('lo.core#,..'('lo.core#sc'(91), 'lo.core#,..'('lo.core#ssSeq'(XXd8867), 'lo.core#,..'('lo.core#sc'(93), 'lo.core#[]'))))):- !,
    'lo.json@dispSeq'(XL, XSp, "", XXd8867).
'lo.json@dispJson'(_, _, _):- raise_exception('error'("lo.json@dispJson", 14, 3, 31)).
'lo.core$display$lo.json*json'('lo.core$display$lo.json*json%1'('lo.core$display$lo.json*json')):- !.
'lo.core$display$lo.json*json'('disp%2'(XV18324, XV18325), XLbl3820, XThis3820):- !,
    'lo.core$display$lo.json*json@disp'(XV18324, XV18325, XLbl3820, XThis3820).
'lo.core$display$lo.json*json'('disp%1'('lo.core$display$lo.json*json^disp'(XLbl3821, XThis3821)), XLbl3821, XThis3821).
'lo.core$display$lo.json*json@disp'(Xj, XXd8874, XLbV1661, XThV1661):- !,
    'lo.json@dispJson'(Xj, 0, XXd8874).
'lo.core$display$lo.json*json@disp'(_, _):- raise_exception('error'("lo.core$display$lo.json*json@disp", 10, 5, 24)).
'lo.json@hashJson'('lo.json#jTrue', XXe1895):- !,
    ocall('hash%1'(XXV1916),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'("true", XXe1895),XXV1916,XXV1916).
'lo.json@hashJson'('lo.json#jFalse', XXe1896):- !,
    ocall('hash%1'(XXV1917),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'("false", XXe1896),XXV1917,XXV1917).
'lo.json@hashJson'('lo.json#jNull', XXe1897):- !,
    ocall('hash%1'(XXV1918),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'("null", XXe1897),XXV1918,XXV1918).
'lo.json@hashJson'('lo.json#jNum'(XD), XXe1898):- !,
    ocall('hash%1'(XXV1919),'lo.core$equality$lo.core*float','lo.core$equality$lo.core*float'),
    ocall('_call%2'(XD, XXe1898),XXV1919,XXV1919).
'lo.json@hashJson'('lo.json#jTxt'(XS), XXe1899):- !,
    ocall('hash%1'(XXV1920),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'(XS, XXe1899),XXV1920,XXV1920).
'lo.json@hashJson'('lo.json#jSeq'(XL), XXe1900):- !,
    ocall('hash%1'(XXV1921),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json'),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json')),
    ocall('_call%2'(XL, XXe1900),XXV1921,XXV1921).
'lo.json@hashJson'('lo.json#jColl'(XC), XXe1901):- !,
    ocall('hash%1'(XXV1922),'lo.core$equality$lo.index*map'('lo.core$equality$lo.json*json', 'lo.core$equality$lo.core*string'),'lo.core$equality$lo.index*map'('lo.core$equality$lo.json*json', 'lo.core$equality$lo.core*string')),
    ocall('_call%2'(XC, XXe1901),XXV1922,XXV1922).
'lo.json@hashJson'(_, _):- raise_exception('error'("lo.json@hashJson", 53, 3, 31)).
'lo.json@equalJson'('lo.json#jTrue', 'lo.json#jTrue').
'lo.json@equalJson'('lo.json#jFalse', 'lo.json#jFalse').
'lo.json@equalJson'('lo.json#jNull', 'lo.json#jNull').
'lo.json@equalJson'('lo.json#jTxt'(XS1), 'lo.json#jTxt'(XS2)):- ocall('==%2'(XS1, XS2),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.json@equalJson'('lo.json#jNum'(XD1), 'lo.json#jNum'(XD2)):- ocall('==%2'(XD1, XD2),'lo.core$equality$lo.core*float','lo.core$equality$lo.core*float').
'lo.json@equalJson'('lo.json#jColl'(XC1), 'lo.json#jColl'(XC2)):- ocall('==%2'(XC1, XC2),'lo.core$equality$lo.index*map'('lo.core$equality$lo.json*json', 'lo.core$equality$lo.core*string'),'lo.core$equality$lo.index*map'('lo.core$equality$lo.json*json', 'lo.core$equality$lo.core*string')).
'lo.json@equalJson'('lo.json#jSeq'(XL1), 'lo.json#jSeq'(XL2)):- ocall('==%2'(XL1, XL2),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json'),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json')).
'lo.core$equality$lo.json*json'('lo.core$equality$lo.json*json%1'('lo.core$equality$lo.json*json')):- !.
'lo.core$equality$lo.json*json'('==%2'(XV18334, XV18335), XLbl3822, XThis3822):- !,
    'lo.core$equality$lo.json*json@=='(XV18334, XV18335, XLbl3822, XThis3822).
'lo.core$equality$lo.json*json'('==%1'('lo.core$equality$lo.json*json^=='(XLbl3823, XThis3823)), XLbl3823, XThis3823).
'lo.core$equality$lo.json*json'('hash%2'(XV18338, XV18339), XLbl3824, XThis3824):- !,
    'lo.core$equality$lo.json*json@hash'(XV18338, XV18339, XLbl3824, XThis3824).
'lo.core$equality$lo.json*json'('hash%1'('lo.core$equality$lo.json*json^hash'(XLbl3825, XThis3825)), XLbl3825, XThis3825).
'lo.core$equality$lo.json*json@=='(XT1, XT2, XLbV1662, XThV1662):- 'lo.json@equalJson'(XT1, XT2).
'lo.core$equality$lo.json*json@hash'(XT, XXd8879, XLbV1662, XThV1662):- !,
    'lo.json@hashJson'(XT, XXd8879).
'lo.core$equality$lo.json*json@hash'(_, _):- raise_exception('error'("lo.core$equality$lo.json*json@hash", 40, 5, 22)).
'lo.json@skipBlanks'(XStIn1165, XStx1202, Xstream163):- ocall('_hdtl%3'(XStIn1165, 32, XNStrm933),Xstream163,Xstream163),
    'lo.json@skipBlanks'(XNStrm933, XStx1202, Xstream163).
'lo.json@skipBlanks'(XStIn1166, XStx1203, Xstream163):- ocall('_hdtl%3'(XStIn1166, 9, XNStrm934),Xstream163,Xstream163),
    'lo.json@skipBlanks'(XNStrm934, XStx1203, Xstream163).
'lo.json@skipBlanks'(XStIn1167, XStx1204, Xstream163):- ocall('_hdtl%3'(XStIn1167, 10, XNStrm935),Xstream163,Xstream163),
    'lo.json@skipBlanks'(XNStrm935, XStx1204, Xstream163).
'lo.json@skipBlanks'(XStIn1168, XStx1205, Xstream163):- ocall('_hdtl%3'(XStIn1168, 13, XNStrm936),Xstream163,Xstream163),
    'lo.json@skipBlanks'(XNStrm936, XStx1205, Xstream163).
'lo.json@skipBlanks'(XStIn1169, XStIn1169, Xstream163).
'lo.json@digit'(XStIn1170, XNStrm937, Xstream164, 0):- ocall('_hdtl%3'(XStIn1170, 48, XNStrm937),Xstream164,Xstream164).
'lo.json@digit'(XStIn1171, XNStrm938, Xstream164, 1):- ocall('_hdtl%3'(XStIn1171, 49, XNStrm938),Xstream164,Xstream164).
'lo.json@digit'(XStIn1172, XNStrm939, Xstream164, 2):- ocall('_hdtl%3'(XStIn1172, 50, XNStrm939),Xstream164,Xstream164).
'lo.json@digit'(XStIn1173, XNStrm940, Xstream164, 3):- ocall('_hdtl%3'(XStIn1173, 51, XNStrm940),Xstream164,Xstream164).
'lo.json@digit'(XStIn1174, XNStrm941, Xstream164, 4):- ocall('_hdtl%3'(XStIn1174, 52, XNStrm941),Xstream164,Xstream164).
'lo.json@digit'(XStIn1175, XNStrm942, Xstream164, 5):- ocall('_hdtl%3'(XStIn1175, 53, XNStrm942),Xstream164,Xstream164).
'lo.json@digit'(XStIn1176, XNStrm943, Xstream164, 6):- ocall('_hdtl%3'(XStIn1176, 54, XNStrm943),Xstream164,Xstream164).
'lo.json@digit'(XStIn1177, XNStrm944, Xstream164, 7):- ocall('_hdtl%3'(XStIn1177, 55, XNStrm944),Xstream164,Xstream164).
'lo.json@digit'(XStIn1178, XNStrm945, Xstream164, 8):- ocall('_hdtl%3'(XStIn1178, 56, XNStrm945),Xstream164,Xstream164).
'lo.json@digit'(XStIn1179, XNStrm946, Xstream164, 9):- ocall('_hdtl%3'(XStIn1179, 57, XNStrm946),Xstream164,Xstream164).
'lo.json@hexDigit'(XStIn1180, XStx1206, Xstream165, XX):- 'lo.json@digit'(XStIn1180, XStx1206, Xstream165, XX).
'lo.json@hexDigit'(XStIn1181, XNStrm947, Xstream165, 10):- ocall('_hdtl%3'(XStIn1181, 97, XNStrm947),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1182, XNStrm948, Xstream165, 11):- ocall('_hdtl%3'(XStIn1182, 98, XNStrm948),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1183, XNStrm949, Xstream165, 12):- ocall('_hdtl%3'(XStIn1183, 99, XNStrm949),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1184, XNStrm950, Xstream165, 13):- ocall('_hdtl%3'(XStIn1184, 100, XNStrm950),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1185, XNStrm951, Xstream165, 14):- ocall('_hdtl%3'(XStIn1185, 101, XNStrm951),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1186, XNStrm952, Xstream165, 15):- ocall('_hdtl%3'(XStIn1186, 102, XNStrm952),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1187, XNStrm953, Xstream165, 10):- ocall('_hdtl%3'(XStIn1187, 65, XNStrm953),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1188, XNStrm954, Xstream165, 11):- ocall('_hdtl%3'(XStIn1188, 66, XNStrm954),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1189, XNStrm955, Xstream165, 12):- ocall('_hdtl%3'(XStIn1189, 67, XNStrm955),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1190, XNStrm956, Xstream165, 13):- ocall('_hdtl%3'(XStIn1190, 68, XNStrm956),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1191, XNStrm957, Xstream165, 14):- ocall('_hdtl%3'(XStIn1191, 69, XNStrm957),Xstream165,Xstream165).
'lo.json@hexDigit'(XStIn1192, XNStrm958, Xstream165, 15):- ocall('_hdtl%3'(XStIn1192, 70, XNStrm958),Xstream165,Xstream165).
'lo.json@readHex'(XStIn1193, XStx1208, Xstream166, XSoFar, XInt, XCnt):- 'lo.core@>'('lo.core$comp$lo.core*integer', XCnt, 0),
    'lo.json@hexDigit'(XStIn1193, XStx1207, Xstream166, XD),
    ocall('*%1'(XXV1923),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%1'(XXV1924),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('-%1'(XXV1925),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XSoFar, 10, XXe1902),XXV1923,XXV1923),
    ocall('_call%3'(XXe1902, XD, XXe1903),XXV1924,XXV1924),
    ocall('_call%3'(XCnt, 1, XXe1904),XXV1925,XXV1925),
    'lo.json@readHex'(XStx1207, XStx1208, Xstream166, XXe1903, XInt, XXe1904).
'lo.json@readHex'(XStIn1194, XStIn1194, Xstream166, XSoFar, XSoFar, X_5462).
'lo.json@parseStr'(XStIn1195, XStIn1195, Xstream167, 'lo.core#[]'):- 'lo.json@Hed88'(XStIn1195, XNStrm959, Xstream167, XNStrm959, XHedStrm88).
'lo.json@parseStr'(XStIn1196, XStx1210, Xstream167, 'lo.core#,..'(XH, Xl)):- ocall('_hdtl%3'(XStIn1196, 92, XNStrm960),Xstream167,Xstream167),
    ocall('_hdtl%3'(XNStrm960, 117, XNStrm961),Xstream167,Xstream167),
    'lo.json@readHex'(XNStrm961, XStx1209, Xstream167, 0, XH, 4),
    'lo.json@parseStr'(XStx1209, XStx1210, Xstream167, Xl).
'lo.json@parseStr'(XStIn1197, XStx1211, Xstream167, 'lo.core#,..'(8, Xl)):- ocall('_hdtl%3'(XStIn1197, 92, XNStrm962),Xstream167,Xstream167),
    ocall('_hdtl%3'(XNStrm962, 98, XNStrm963),Xstream167,Xstream167),
    'lo.json@parseStr'(XNStrm963, XStx1211, Xstream167, Xl).
'lo.json@parseStr'(XStIn1198, XStx1212, Xstream167, 'lo.core#,..'(102, Xl)):- ocall('_hdtl%3'(XStIn1198, 92, XNStrm964),Xstream167,Xstream167),
    ocall('_hdtl%3'(XNStrm964, 102, XNStrm965),Xstream167,Xstream167),
    'lo.json@parseStr'(XNStrm965, XStx1212, Xstream167, Xl).
'lo.json@parseStr'(XStIn1199, XStx1213, Xstream167, 'lo.core#,..'(10, Xl)):- ocall('_hdtl%3'(XStIn1199, 92, XNStrm966),Xstream167,Xstream167),
    ocall('_hdtl%3'(XNStrm966, 110, XNStrm967),Xstream167,Xstream167),
    'lo.json@parseStr'(XNStrm967, XStx1213, Xstream167, Xl).
'lo.json@parseStr'(XStIn1200, XStx1214, Xstream167, 'lo.core#,..'(13, Xl)):- ocall('_hdtl%3'(XStIn1200, 92, XNStrm968),Xstream167,Xstream167),
    ocall('_hdtl%3'(XNStrm968, 114, XNStrm969),Xstream167,Xstream167),
    'lo.json@parseStr'(XNStrm969, XStx1214, Xstream167, Xl).
'lo.json@parseStr'(XStIn1201, XStx1215, Xstream167, 'lo.core#,..'(9, Xl)):- ocall('_hdtl%3'(XStIn1201, 92, XNStrm970),Xstream167,Xstream167),
    ocall('_hdtl%3'(XNStrm970, 116, XNStrm971),Xstream167,Xstream167),
    'lo.json@parseStr'(XNStrm971, XStx1215, Xstream167, Xl).
'lo.json@parseStr'(XStIn1202, XStx1216, Xstream167, 'lo.core#,..'(XX, Xl)):- ocall('_hdtl%3'(XStIn1202, 92, XNStrm972),Xstream167,Xstream167),
    ocall('_hdtl%3'(XNStrm972, XX, XNStrm973),Xstream167,Xstream167),
    'lo.json@parseStr'(XNStrm973, XStx1216, Xstream167, Xl).
'lo.json@parseStr'(XStIn1203, XStx1217, Xstream167, 'lo.core#,..'(XC, Xl)):- ocall('_hdtl%3'(XStIn1203, XC, XNStrm974),Xstream167,Xstream167),
    'lo.json@parseStr'(XNStrm974, XStx1217, Xstream167, Xl).
'lo.json@parseField'(XStIn1204, XNStrm976, Xstream168, XXa85):- ocall('_hdtl%3'(XStIn1204, 34, XNStrm975),Xstream168,Xstream168),
    'lo.json@parseStr'(XNStrm975, XStx1218, Xstream168, XS),
    ocall('_hdtl%3'(XStx1218, 34, XNStrm976),Xstream168,Xstream168),
    'implode'(XS, XXa85).
'lo.json@readNatural'(XStIn1205, XStx1220, Xstream169, XSoFar, XInt):- 'lo.json@digit'(XStIn1205, XStx1219, Xstream169, XD),
    ocall('*%1'(XXV1926),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%1'(XXV1927),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XSoFar, 10, XXe1905),XXV1926,XXV1926),
    ocall('_call%3'(XXe1905, XD, XXe1906),XXV1927,XXV1927),
    'lo.json@readNatural'(XStx1219, XStx1220, Xstream169, XXe1906, XInt).
'lo.json@readNatural'(XStIn1206, XStIn1206, Xstream169, XSoFar, XSoFar):- 'lo.json@Neg26'(XStIn1206, XStx1221, X_5471, Xstream169).
'lo.json@fraction'(XStIn1207, XStx1223, Xstream170, XScale, XSoFar, XResult):- 'lo.json@digit'(XStIn1207, XStx1222, Xstream170, XD),
    ocall('*%1'(XXV1928),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    ocall('*%1'(XXV1929),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    ocall('+%1'(XXV1930),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('_call%3'(XScale, 0.1, XXe1907),XXV1928,XXV1928),
    '_int2flt'(XD, XXc826),
    ocall('_call%3'(XXc826, XScale, XXe1908),XXV1929,XXV1929),
    ocall('_call%3'(XSoFar, XXe1908, XXe1909),XXV1930,XXV1930),
    'lo.json@fraction'(XStx1222, XStx1223, Xstream170, XXe1907, XXe1909, XResult).
'lo.json@fraction'(XStIn1208, XStIn1208, Xstream170, X_5472, XFract, XFract).
'lo.json@readDecimal'(XStIn1209, XStx1224, Xstream171, XIn):- ocall('_hdtl%3'(XStIn1209, 45, XNStrm977),Xstream171,Xstream171),
    'lo.json@readNatural'(XNStrm977, XStx1224, Xstream171, 0, XPl),
    ocall('-%1'(XXV1931),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(0, XPl, XXe1910),XXV1931,XXV1931),
    XIn = XXe1910.
'lo.json@readDecimal'(XStIn1210, XStx1225, Xstream171, XIn):- 'lo.json@readNatural'(XStIn1210, XStx1225, Xstream171, 0, XIn).
'lo.json@exponent'(XStIn1211, XStx1226, Xstream172, XSoFar, XFp):- 'lo.json@Disj82'(XStIn1211, XDjOut90, XNStrm979, XNStrm979, XNStrm978, Xstream172, XNStrm978, XDjStrm82),
    'lo.json@readDecimal'(XDjOut90, XStx1226, Xstream172, XExp),
    ocall('*%1'(XXV1932),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    '_int2flt'(XExp, XXc827),
    '_pwr'(10.0, XXc827, XXc828),
    ocall('_call%3'(XSoFar, XXc828, XXe1911),XXV1932,XXV1932),
    XFp = XXe1911.
'lo.json@exponent'(XStIn1212, XStIn1212, Xstream172, XFp, XFp).
'lo.json@readMoreNumber'(XStIn1213, XStx1229, Xstream173, XFp, XDecimal):- ocall('_hdtl%3'(XStIn1213, 46, XNStrm980),Xstream173,Xstream173),
    'lo.json@Hed89'(XNStrm980, XStx1227, X_5475, Xstream173),
    'lo.json@fraction'(XNStrm980, XStx1228, Xstream173, 0.1, 0.0, XFr),
    ocall('+%1'(XXV1933),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    '_int2flt'(XDecimal, XXc829),
    ocall('_call%3'(XXc829, XFr, XXe1912),XXV1933,XXV1933),
    'lo.json@exponent'(XStx1228, XStx1229, Xstream173, XXe1912, XFp).
'lo.json@readMoreNumber'(XStIn1214, XStIn1214, Xstream173, XXe1913, XIx):- ocall('_coerce%1'(XXV1934),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_call%2'(XIx, XXe1913),XXV1934,XXV1934).
'lo.json@readNumber'(XStIn1215, XStx1231, Xstream174, XFlt):- 'lo.json@readNatural'(XStIn1215, XStx1230, Xstream174, 0, XFirst),
    'lo.json@readMoreNumber'(XStx1230, XStx1231, Xstream174, XFlt, XFirst).
'lo.json@moreSeq'(XStIn1216, XStIn1216, Xstream175, 'lo.core#[]'):- 'lo.json@Hed90'(XStIn1216, XNStrm981, Xstream175, XNStrm981, XHedStrm90).
'lo.json@moreSeq'(XStIn1217, XStx1235, Xstream175, 'lo.core#,..'(Xe, Xl)):- ocall('_hdtl%3'(XStIn1217, 44, XNStrm982),Xstream175,Xstream175),
    'lo.json@skipBlanks'(XNStrm982, XStx1232, Xstream175),
    'lo.json@jP'(XStx1232, XStx1233, Xstream175, Xe),
    'lo.json@skipBlanks'(XStx1233, XStx1234, Xstream175),
    'lo.json@moreSeq'(XStx1234, XStx1235, Xstream175, Xl).
'lo.json@parseSeq'(XStIn1218, XStIn1218, Xstream176, 'lo.core#[]'):- 'lo.json@Hed91'(XStIn1218, XNStrm983, Xstream176, XNStrm983, XHedStrm91).
'lo.json@parseSeq'(XStIn1219, XStx1238, Xstream176, 'lo.core#,..'(Xe, Xl)):- 'lo.json@jP'(XStIn1219, XStx1236, Xstream176, Xe),
    'lo.json@skipBlanks'(XStx1236, XStx1237, Xstream176),
    'lo.json@moreSeq'(XStx1237, XStx1238, Xstream176, Xl).
'lo.json@parseMoreColl'(XStIn1220, XStIn1220, Xstream177, Xm, Xm):- 'lo.json@Hed92'(XStIn1220, XNStrm984, Xstream177, XNStrm984, XHedStrm92).
'lo.json@parseMoreColl'(XStIn1221, XStx1245, Xstream177, Xm, Xc):- ocall('_hdtl%3'(XStIn1221, 44, XNStrm985),Xstream177,Xstream177),
    'lo.json@skipBlanks'(XNStrm985, XStx1239, Xstream177),
    'lo.json@parseField'(XStx1239, XStx1240, Xstream177, Xf),
    'lo.json@skipBlanks'(XStx1240, XStx1241, Xstream177),
    ocall('_hdtl%3'(XStx1241, 58, XNStrm986),Xstream177,Xstream177),
    'lo.json@skipBlanks'(XNStrm986, XStx1242, Xstream177),
    'lo.json@jP'(XStx1242, XStx1243, Xstream177, Xv),
    'lo.json@skipBlanks'(XStx1243, XStx1244, Xstream177),
    ocall('_put%1'(XXV1935),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(Xm, Xf, Xv, XXe1914),XXV1935,XXV1935),
    'lo.json@parseMoreColl'(XStx1244, XStx1245, Xstream177, XXe1914, Xc).
'lo.json@parseColl'(XStIn1222, XStIn1222, Xstream178, Xm, Xm):- 'lo.json@Hed93'(XStIn1222, XNStrm987, Xstream178, XNStrm987, XHedStrm93).
'lo.json@parseColl'(XStIn1223, XStx1250, Xstream178, Xm, Xc):- 'lo.json@parseField'(XStIn1223, XStx1246, Xstream178, Xf),
    'lo.json@skipBlanks'(XStx1246, XStx1247, Xstream178),
    ocall('_hdtl%3'(XStx1247, 58, XNStrm988),Xstream178,Xstream178),
    'lo.json@skipBlanks'(XNStrm988, XStx1248, Xstream178),
    'lo.json@jP'(XStx1248, XStx1249, Xstream178, Xv),
    ocall('_put%1'(XXV1936),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(Xm, Xf, Xv, XXe1915),XXV1936,XXV1936),
    'lo.json@parseMoreColl'(XStx1249, XStx1250, Xstream178, XXe1915, Xc).
'lo.json@jP'(XStIn1224, XNStrm992, Xstream179, 'lo.json#jTrue'):- ocall('_hdtl%3'(XStIn1224, 116, XNStrm989),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm989, 114, XNStrm990),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm990, 117, XNStrm991),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm991, 101, XNStrm992),Xstream179,Xstream179).
'lo.json@jP'(XStIn1225, XNStrm997, Xstream179, 'lo.json#jFalse'):- ocall('_hdtl%3'(XStIn1225, 102, XNStrm993),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm993, 97, XNStrm994),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm994, 108, XNStrm995),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm995, 115, XNStrm996),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm996, 101, XNStrm997),Xstream179,Xstream179).
'lo.json@jP'(XStIn1226, XNStrm1001, Xstream179, 'lo.json#jNull'):- ocall('_hdtl%3'(XStIn1226, 110, XNStrm998),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm998, 117, XNStrm999),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm999, 108, XNStrm1000),Xstream179,Xstream179),
    ocall('_hdtl%3'(XNStrm1000, 108, XNStrm1001),Xstream179,Xstream179).
'lo.json@jP'(XStIn1227, XStx1251, Xstream179, 'lo.json#jNum'(XXe1916)):- ocall('_hdtl%3'(XStIn1227, 45, XNStrm1002),Xstream179,Xstream179),
    'lo.json@readNumber'(XNStrm1002, XStx1251, Xstream179, XN),
    ocall('-%1'(XXV1938),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('zero%1'(XXV1937),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('_call%3'(XXV1937, XN, XXe1916),XXV1938,XXV1938).
'lo.json@jP'(XStIn1228, XStx1253, Xstream179, 'lo.json#jNum'(XN)):- 'lo.json@Hed94'(XStIn1228, XStx1252, X_5478, Xstream179),
    'lo.json@readNumber'(XStIn1228, XStx1253, Xstream179, XN).
'lo.json@jP'(XStIn1229, XNStrm1004, Xstream179, 'lo.json#jTxt'(XXa86)):- ocall('_hdtl%3'(XStIn1229, 34, XNStrm1003),Xstream179,Xstream179),
    'lo.json@parseStr'(XNStrm1003, XStx1254, Xstream179, XS),
    ocall('_hdtl%3'(XStx1254, 34, XNStrm1004),Xstream179,Xstream179),
    'implode'(XS, XXa86).
'lo.json@jP'(XStIn1230, XNStrm1006, Xstream179, 'lo.json#jSeq'(XL)):- ocall('_hdtl%3'(XStIn1230, 91, XNStrm1005),Xstream179,Xstream179),
    'lo.json@skipBlanks'(XNStrm1005, XStx1255, Xstream179),
    'lo.json@parseSeq'(XStx1255, XStx1256, Xstream179, XL),
    ocall('_hdtl%3'(XStx1256, 93, XNStrm1006),Xstream179,Xstream179).
'lo.json@jP'(XStIn1231, XNStrm1008, Xstream179, 'lo.json#jColl'(XM)):- ocall('_hdtl%3'(XStIn1231, 123, XNStrm1007),Xstream179,Xstream179),
    'lo.json@skipBlanks'(XNStrm1007, XStx1257, Xstream179),
    ocall('_empty%1'(XXV1939),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.json@parseColl'(XStx1257, XStx1258, Xstream179, XXV1939, XM),
    ocall('_hdtl%3'(XStx1258, 125, XNStrm1008),Xstream179,Xstream179).
'lo.json@parseJson'(XStIn1232, XStx1260, Xstream180, XJ):- 'lo.json@skipBlanks'(XStIn1232, XStx1259, Xstream180),
    'lo.json@jP'(XStx1259, XStx1260, Xstream180, XJ).
'lo.coerce$coercion$lo.core*string$lo.json*json'('lo.coerce$coercion$lo.core*string$lo.json*json%1'('lo.coerce$coercion$lo.core*string$lo.json*json')):- !.
'lo.coerce$coercion$lo.core*string$lo.json*json'('_coerce%2'(XV18422, XV18423), XLbl3826, XThis3826):- !,
    'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XV18422, XV18423, XLbl3826, XThis3826).
'lo.coerce$coercion$lo.core*string$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'(XLbl3827, XThis3827)), XLbl3827, XThis3827).
'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XS, XJ, XLbV1663, XThV1663):- 'explode'(XS, XXc830),
    'lo.json@parseJson'(XXc830, XStx1261, 'lo.core$stream$lo.core*list', XJ),
    XStx1261 = X_5480,
    ocall('_eof%1'(X_5480),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_5479 = XStx1261,
    !.
'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*string$lo.json*json@_coerce", 62, 5, 45)).
'lo.json@entryCoerce'(Xcoercion70, Xe, XXe1917):- !,
    ocall('_coerce%1'(XXV1940),Xcoercion70,Xcoercion70),
    ocall('_call%2'(Xe, XXe1917),XXV1940,XXV1940).
'lo.json@entryCoerce'(_, _, _):- raise_exception('error'("lo.json@entryCoerce", 179, 3, 25)).
'lo.json@mapJson'(Xcoercion71, XM, 'lo.json#jColl'(XXd8884)):- !,
    'lo.json@entryCoerce'(Xcoercion71, XXd8883),
    'lo.index@mapMap'(XM, XXd8883, XXd8884).
'lo.json@mapJson'(_, _, _):- raise_exception('error'("lo.json@mapJson", 176, 3, 42)).
'lo.coerce$coercion$lo.index*map$lo.json*json'('lo.coerce$coercion$lo.index*map$lo.json*json%1'('lo.coerce$coercion$lo.index*map$lo.json*json')):- !.
'lo.coerce$coercion$lo.index*map$lo.json*json'('_coerce%2'(XV18432, XV18433), XLbl3828, XThis3828):- !,
    'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XV18432, XV18433, XLbl3828, XThis3828).
'lo.coerce$coercion$lo.index*map$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'(XLbl3829, XThis3829)), XLbl3829, XThis3829).
'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XM, XXd8886, XLbV1664, XThV1664):- XLbV1664 = 'lo.coerce$coercion$lo.index*map$lo.json*json'(Xcoercion72),
    !,
    'lo.json@mapJson'(Xcoercion72, XM, XXd8886).
'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.index*map$lo.json*json@_coerce", 172, 5, 24)).
'lo.json@cList'(Xcoercion73, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.json@cList'(Xcoercion73, 'lo.core#,..'(Xe, Xl), 'lo.core#,..'(XXe1918, XXd8887)):- !,
    ocall('_coerce%1'(XXV1941),Xcoercion73,Xcoercion73),
    ocall('_call%2'(Xe, XXe1918),XXV1941,XXV1941),
    'lo.json@cList'(Xcoercion73, Xl, XXd8887).
'lo.json@cList'(_, _, _):- raise_exception('error'("lo.json@cList", 186, 3, 15)).
'lo.coerce$coercion$lo.core*list$lo.json*json'('lo.coerce$coercion$lo.core*list$lo.json*json%1'('lo.coerce$coercion$lo.core*list$lo.json*json')):- !.
'lo.coerce$coercion$lo.core*list$lo.json*json'('_coerce%2'(XV18439, XV18440), XLbl3830, XThis3830):- !,
    'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XV18439, XV18440, XLbl3830, XThis3830).
'lo.coerce$coercion$lo.core*list$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'(XLbl3831, XThis3831)), XLbl3831, XThis3831).
'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XL, 'lo.json#jSeq'(XXd8889), XLbV1665, XThV1665):- XLbV1665 = 'lo.coerce$coercion$lo.core*list$lo.json*json'(Xcoercion74),
    !,
    'lo.json@cList'(Xcoercion74, XL, XXd8889).
'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.json*json@_coerce", 182, 5, 28)).
'lo.json^spaces'('_call%2'(XV18307, XV18308), 'lo.json^spaces', _):- 'lo.json@spaces'(XV18307, XV18308).
'lo.json^break'('_call%2'(XV18309, XV18310), 'lo.json^break', _):- 'lo.json@break'(XV18309, XV18310).
'lo.json^dispColl'('_call%4'(XV18311, XV18312, XV18313, XV18314), 'lo.json^dispColl', _):- 'lo.json@dispColl'(XV18311, XV18312, XV18313, XV18314).
'lo.json^dispSeq'('_call%4'(XV18315, XV18316, XV18317, XV18318), 'lo.json^dispSeq', _):- 'lo.json@dispSeq'(XV18315, XV18316, XV18317, XV18318).
'lo.json^dispJson'('_call%3'(XV18319, XV18320, XV18321), 'lo.json^dispJson', _):- 'lo.json@dispJson'(XV18319, XV18320, XV18321).
'lo.core$display$lo.json*json^disp'('_call%2'(XV18322, XV18323), 'lo.core$display$lo.json*json^disp'(XLbV1661, XThV1661), _):- 'lo.core$display$lo.json*json@disp'(XV18322, XV18323, XLbV1661, XThV1661).
'lo.json^hashJson'('_call%2'(XV18326, XV18327), 'lo.json^hashJson', _):- 'lo.json@hashJson'(XV18326, XV18327).
'lo.json^equalJson'('_call%2'(XV18328, XV18329), 'lo.json^equalJson', _):- 'lo.json@equalJson'(XV18328, XV18329).
'lo.core$equality$lo.json*json^=='('_call%4'(XV18330, XV18331, XV18332, XV18333), 'lo.core$equality$lo.json*json^=='(XLbV1662, XThV1662), _):- 'lo.core$equality$lo.json*json@=='(XV18330, XV18331, XV18332, XV18333, XLbV1662, XThV1662).
'lo.core$equality$lo.json*json^hash'('_call%2'(XV18336, XV18337), 'lo.core$equality$lo.json*json^hash'(XLbV1662, XThV1662), _):- 'lo.core$equality$lo.json*json@hash'(XV18336, XV18337, XLbV1662, XThV1662).
'lo.json^skipBlanks'('_call%3'(XV18340, XV18341, XV18342), 'lo.json^skipBlanks', _):- 'lo.json@skipBlanks'(XV18340, XV18341, XV18342).
'lo.json^digit'('_call%4'(XV18343, XV18344, XV18345, XV18346), 'lo.json^digit', _):- 'lo.json@digit'(XV18343, XV18344, XV18345, XV18346).
'lo.json^hexDigit'('_call%4'(XV18347, XV18348, XV18349, XV18350), 'lo.json^hexDigit', _):- 'lo.json@hexDigit'(XV18347, XV18348, XV18349, XV18350).
'lo.json^readHex'('_call%6'(XV18351, XV18352, XV18353, XV18354, XV18355, XV18356), 'lo.json^readHex', _):- 'lo.json@readHex'(XV18351, XV18352, XV18353, XV18354, XV18355, XV18356).
'lo.json@Hed88'(XHedStrm88, XNStrm959, Xstream167, XNStrm959, XHedStrm88):- ocall('_hdtl%3'(XHedStrm88, 34, XNStrm959),Xstream167,Xstream167).
'lo.json^parseStr'('_call%4'(XV18357, XV18358, XV18359, XV18360), 'lo.json^parseStr', _):- 'lo.json@parseStr'(XV18357, XV18358, XV18359, XV18360).
'lo.json^parseField'('_call%4'(XV18361, XV18362, XV18363, XV18364), 'lo.json^parseField', _):- 'lo.json@parseField'(XV18361, XV18362, XV18363, XV18364).
'lo.json@Neg26'(XNegStrm26, XStx1221, X_5471, Xstream169):- 'lo.json@digit'(XNegStrm26, XStx1221, Xstream169, X_5471),
    !,
    fail.
'lo.json@Neg26'(XNegStrm26, XStx1221, X_5471, Xstream169).
'lo.json^readNatural'('_call%5'(XV18365, XV18366, XV18367, XV18368, XV18369), 'lo.json^readNatural', _):- 'lo.json@readNatural'(XV18365, XV18366, XV18367, XV18368, XV18369).
'lo.json^fraction'('_call%6'(XV18370, XV18371, XV18372, XV18373, XV18374, XV18375), 'lo.json^fraction', _):- 'lo.json@fraction'(XV18370, XV18371, XV18372, XV18373, XV18374, XV18375).
'lo.json^readDecimal'('_call%4'(XV18376, XV18377, XV18378, XV18379), 'lo.json^readDecimal', _):- 'lo.json@readDecimal'(XV18376, XV18377, XV18378, XV18379).
'lo.json@Disj82'(XDjStrm82, XNStrm978, XNStrm979, XNStrm979, XNStrm978, Xstream172, XNStrm978, XDjStrm82):- ocall('_hdtl%3'(XDjStrm82, 101, XNStrm978),Xstream172,Xstream172).
'lo.json@Disj82'(XDjStrm82, XNStrm979, XNStrm979, XNStrm979, XNStrm978, Xstream172, XNStrm978, XDjStrm82):- ocall('_hdtl%3'(XDjStrm82, 69, XNStrm979),Xstream172,Xstream172).
'lo.json^exponent'('_call%5'(XV18380, XV18381, XV18382, XV18383, XV18384), 'lo.json^exponent', _):- 'lo.json@exponent'(XV18380, XV18381, XV18382, XV18383, XV18384).
'lo.json@Hed89'(XHedStrm89, XStx1227, X_5475, Xstream173):- 'lo.json@digit'(XHedStrm89, XStx1227, Xstream173, X_5475).
'lo.json^readMoreNumber'('_call%5'(XV18385, XV18386, XV18387, XV18388, XV18389), 'lo.json^readMoreNumber', _):- 'lo.json@readMoreNumber'(XV18385, XV18386, XV18387, XV18388, XV18389).
'lo.json^readNumber'('_call%4'(XV18390, XV18391, XV18392, XV18393), 'lo.json^readNumber', _):- 'lo.json@readNumber'(XV18390, XV18391, XV18392, XV18393).
'lo.json@Hed90'(XHedStrm90, XNStrm981, Xstream175, XNStrm981, XHedStrm90):- ocall('_hdtl%3'(XHedStrm90, 93, XNStrm981),Xstream175,Xstream175).
'lo.json^moreSeq'('_call%4'(XV18394, XV18395, XV18396, XV18397), 'lo.json^moreSeq', _):- 'lo.json@moreSeq'(XV18394, XV18395, XV18396, XV18397).
'lo.json@Hed91'(XHedStrm91, XNStrm983, Xstream176, XNStrm983, XHedStrm91):- ocall('_hdtl%3'(XHedStrm91, 93, XNStrm983),Xstream176,Xstream176).
'lo.json^parseSeq'('_call%4'(XV18398, XV18399, XV18400, XV18401), 'lo.json^parseSeq', _):- 'lo.json@parseSeq'(XV18398, XV18399, XV18400, XV18401).
'lo.json@Hed92'(XHedStrm92, XNStrm984, Xstream177, XNStrm984, XHedStrm92):- ocall('_hdtl%3'(XHedStrm92, 125, XNStrm984),Xstream177,Xstream177).
'lo.json^parseMoreColl'('_call%5'(XV18402, XV18403, XV18404, XV18405, XV18406), 'lo.json^parseMoreColl', _):- 'lo.json@parseMoreColl'(XV18402, XV18403, XV18404, XV18405, XV18406).
'lo.json@Hed93'(XHedStrm93, XNStrm987, Xstream178, XNStrm987, XHedStrm93):- ocall('_hdtl%3'(XHedStrm93, 125, XNStrm987),Xstream178,Xstream178).
'lo.json^parseColl'('_call%5'(XV18407, XV18408, XV18409, XV18410, XV18411), 'lo.json^parseColl', _):- 'lo.json@parseColl'(XV18407, XV18408, XV18409, XV18410, XV18411).
'lo.json@Hed94'(XHedStrm94, XStx1252, X_5478, Xstream179):- 'lo.json@digit'(XHedStrm94, XStx1252, Xstream179, X_5478).
'lo.json^jP'('_call%4'(XV18412, XV18413, XV18414, XV18415), 'lo.json^jP', _):- 'lo.json@jP'(XV18412, XV18413, XV18414, XV18415).
'lo.json^parseJson'('_call%4'(XV18416, XV18417, XV18418, XV18419), 'lo.json^parseJson', _):- 'lo.json@parseJson'(XV18416, XV18417, XV18418, XV18419).
'lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'('_call%2'(XV18420, XV18421), 'lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'(XLbV1663, XThV1663), _):- 'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XV18420, XV18421, XLbV1663, XThV1663).
'lo.json^entryCoerce'('_call%3'(XV18424, XV18425, XV18426), 'lo.json^entryCoerce', _):- 'lo.json@entryCoerce'(XV18424, XV18425, XV18426).
'lo.json^mapJson'('_call%3'(XV18427, XV18428, XV18429), 'lo.json^mapJson', _):- 'lo.json@mapJson'(XV18427, XV18428, XV18429).
'lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'('_call%2'(XV18430, XV18431), 'lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'(XLbV1664, XThV1664), _):- 'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XV18430, XV18431, XLbV1664, XThV1664).
'lo.json^cList'('_call%3'(XV18434, XV18435, XV18436), 'lo.json^cList', _):- 'lo.json@cList'(XV18434, XV18435, XV18436).
'lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'('_call%2'(XV18437, XV18438), 'lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'(XLbV1665, XThV1665), _):- 'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XV18437, XV18438, XLbV1665, XThV1665).
