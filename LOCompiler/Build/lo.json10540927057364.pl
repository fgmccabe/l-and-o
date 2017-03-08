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
'lo.json@spaces'(XX, 'lo.core#,..'('lo.core#ss'(" "), XXd35740)):- !,
    ocall('-%1'(XXV4702),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XX, 1, XXe4366),XXV4702,XXV4702),
    'lo.json@spaces'(XXe4366, XXd35740).
'lo.json@spaces'(_, _):- raise_exception('error'("lo.json@spaces", 35, 3, 15)).
'lo.json@break'(0, 'lo.core#ssSeq'('lo.core#[]')):- !.
'lo.json@break'(XX, 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("
"), XXd35744))):- !,
    'lo.json@spaces'(XX, XXd35744).
'lo.json@break'(_, _):- raise_exception('error'("lo.json@break", 31, 3, 21)).
'lo.json@dispColl'('lo.core#[]', X_30927, X_30928, 'lo.core#[]'):- !.
'lo.json@dispColl'('lo.core#,..'('()2'(Xf, Xe), Xl), XSp, Xs, 'lo.core#,..'('lo.core#ss'(Xs), 'lo.core#,..'(XXd35748, 'lo.core#,..'(XXe4367, 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXd35750, XXd35751)))))):- !,
    ocall('disp%1'(XXV4703),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo.json@break'(XSp, XXd35748),
    ocall('_call%2'(Xf, XXe4367),XXV4703,XXV4703),
    'lo.json@dispJson'(Xe, XSp, XXd35750),
    'lo.json@dispColl'(Xl, XSp, ",", XXd35751).
'lo.json@dispColl'(_, _, _, _):- raise_exception('error'("lo.json@dispColl", 23, 3, 22)).
'lo.json@dispSeq'('lo.core#[]', X_30935, X_30936, 'lo.core#[]'):- !.
'lo.json@dispSeq'('lo.core#,..'(Xe, Xl), XSp, Xs, 'lo.core#,..'('lo.core#ss'(Xs), 'lo.core#,..'(XXd35758, XXd35759))):- !,
    'lo.json@dispJson'(Xe, XSp, XXd35758),
    'lo.json@dispSeq'(Xl, XSp, ",", XXd35759).
'lo.json@dispSeq'(_, _, _, _):- raise_exception('error'("lo.json@dispSeq", 27, 3, 21)).
'lo.json@dispJson'('lo.json#jTrue', X_30940, 'lo.core#ss'("true")):- !.
'lo.json@dispJson'('lo.json#jFalse', X_30941, 'lo.core#ss'("false")):- !.
'lo.json@dispJson'('lo.json#jNull', X_30942, 'lo.core#ss'("null")):- !.
'lo.json@dispJson'('lo.json#jTxt'(XT), X_30943, XXe4368):- !,
    ocall('disp%1'(XXV4704),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XT, XXe4368),XXV4704,XXV4704).
'lo.json@dispJson'('lo.json#jNum'(XD), X_30944, XXe4369):- !,
    ocall('disp%1'(XXV4705),'lo.core$display$lo.core*float','lo.core$display$lo.core*float'),
    ocall('_call%2'(XD, XXe4369),XXV4705,XXV4705).
'lo.json@dispJson'('lo.json#jColl'(XM), XSp, 'lo.core#ssSeq'('lo.core#,..'('lo.core#sc'(123), 'lo.core#,..'('lo.core#ssSeq'(XXd35767), 'lo.core#,..'('lo.core#sc'(125), 'lo.core#[]'))))):- !,
    ocall('pairs%1'(XXV4706),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('+%1'(XXV4707),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XM, XXe4370),XXV4706,XXV4706),
    ocall('_call%3'(XSp, 2, XXe4371),XXV4707,XXV4707),
    'lo.json@dispColl'(XXe4370, XXe4371, "", XXd35767).
'lo.json@dispJson'('lo.json#jSeq'(XL), XSp, 'lo.core#ssSeq'('lo.core#,..'('lo.core#sc'(91), 'lo.core#,..'('lo.core#ssSeq'(XXd35775), 'lo.core#,..'('lo.core#sc'(93), 'lo.core#[]'))))):- !,
    'lo.json@dispSeq'(XL, XSp, "", XXd35775).
'lo.json@dispJson'(_, _, _):- raise_exception('error'("lo.json@dispJson", 14, 3, 31)).
'lo.core$display$lo.json*json'('lo.core$display$lo.json*json%1'('lo.core$display$lo.json*json')):- !.
'lo.core$display$lo.json*json'('disp%2'(XV28589, XV28590), XLbl2040, XThis2040):- !,
    'lo.core$display$lo.json*json@disp'(XV28589, XV28590, XLbl2040, XThis2040).
'lo.core$display$lo.json*json'('disp%1'('lo.core$display$lo.json*json^disp'(XLbl2041, XThis2041)), XLbl2041, XThis2041).
'lo.core$display$lo.json*json@disp'(Xj, XXd35782, XLbV2222, XThV2222):- !,
    'lo.json@dispJson'(Xj, 0, XXd35782).
'lo.core$display$lo.json*json@disp'(_, _):- raise_exception('error'("lo.core$display$lo.json*json@disp", 10, 5, 24)).
'lo.json@hashJson'('lo.json#jTrue', XXe4372):- !,
    ocall('hash%1'(XXV4708),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'("true", XXe4372),XXV4708,XXV4708).
'lo.json@hashJson'('lo.json#jFalse', XXe4373):- !,
    ocall('hash%1'(XXV4709),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'("false", XXe4373),XXV4709,XXV4709).
'lo.json@hashJson'('lo.json#jNull', XXe4374):- !,
    ocall('hash%1'(XXV4710),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'("null", XXe4374),XXV4710,XXV4710).
'lo.json@hashJson'('lo.json#jNum'(XD), XXe4375):- !,
    ocall('hash%1'(XXV4711),'lo.core$equality$lo.core*float','lo.core$equality$lo.core*float'),
    ocall('_call%2'(XD, XXe4375),XXV4711,XXV4711).
'lo.json@hashJson'('lo.json#jTxt'(XS), XXe4376):- !,
    ocall('hash%1'(XXV4712),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'(XS, XXe4376),XXV4712,XXV4712).
'lo.json@hashJson'('lo.json#jSeq'(XL), XXe4377):- !,
    ocall('hash%1'(XXV4713),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json'),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json')),
    ocall('_call%2'(XL, XXe4377),XXV4713,XXV4713).
'lo.json@hashJson'('lo.json#jColl'(XC), XXe4378):- !,
    ocall('hash%1'(XXV4714),'lo.core$equality$lo.index*map'('lo.core$equality$lo.json*json', 'lo.core$equality$lo.core*string'),'lo.core$equality$lo.index*map'('lo.core$equality$lo.json*json', 'lo.core$equality$lo.core*string')),
    ocall('_call%2'(XC, XXe4378),XXV4714,XXV4714).
'lo.json@hashJson'(_, _):- raise_exception('error'("lo.json@hashJson", 53, 3, 31)).
'lo.json@equalJson'('lo.json#jTrue', 'lo.json#jTrue').
'lo.json@equalJson'('lo.json#jFalse', 'lo.json#jFalse').
'lo.json@equalJson'('lo.json#jNull', 'lo.json#jNull').
'lo.json@equalJson'('lo.json#jTxt'(XS1), 'lo.json#jTxt'(XS2)):- ocall('==%2'(XS1, XS2),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.json@equalJson'('lo.json#jNum'(XD1), 'lo.json#jNum'(XD2)):- ocall('==%2'(XD1, XD2),'lo.core$equality$lo.core*float','lo.core$equality$lo.core*float').
'lo.json@equalJson'('lo.json#jColl'(XC1), 'lo.json#jColl'(XC2)):- ocall('==%2'(XC1, XC2),'lo.core$equality$lo.index*map'('lo.core$equality$lo.json*json', 'lo.core$equality$lo.core*string'),'lo.core$equality$lo.index*map'('lo.core$equality$lo.json*json', 'lo.core$equality$lo.core*string')).
'lo.json@equalJson'('lo.json#jSeq'(XL1), 'lo.json#jSeq'(XL2)):- ocall('==%2'(XL1, XL2),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json'),'lo.core$equality$lo.core*list'('lo.core$equality$lo.json*json')).
'lo.core$equality$lo.json*json'('lo.core$equality$lo.json*json%1'('lo.core$equality$lo.json*json')):- !.
'lo.core$equality$lo.json*json'('==%2'(XV28599, XV28600), XLbl2042, XThis2042):- !,
    'lo.core$equality$lo.json*json@=='(XV28599, XV28600, XLbl2042, XThis2042).
'lo.core$equality$lo.json*json'('==%1'('lo.core$equality$lo.json*json^=='(XLbl2043, XThis2043)), XLbl2043, XThis2043).
'lo.core$equality$lo.json*json'('hash%2'(XV28603, XV28604), XLbl2044, XThis2044):- !,
    'lo.core$equality$lo.json*json@hash'(XV28603, XV28604, XLbl2044, XThis2044).
'lo.core$equality$lo.json*json'('hash%1'('lo.core$equality$lo.json*json^hash'(XLbl2045, XThis2045)), XLbl2045, XThis2045).
'lo.core$equality$lo.json*json@=='(XT1, XT2, XLbV2223, XThV2223):- 'lo.json@equalJson'(XT1, XT2).
'lo.core$equality$lo.json*json@hash'(XT, XXd35787, XLbV2223, XThV2223):- !,
    'lo.json@hashJson'(XT, XXd35787).
'lo.core$equality$lo.json*json@hash'(_, _):- raise_exception('error'("lo.core$equality$lo.json*json@hash", 40, 5, 22)).
'lo.json@skipBlanks'(XStIn1670, XStx1580, Xstream73):- ocall('_hdtl%3'(XStIn1670, 32, XNStrm1464),Xstream73,Xstream73),
    'lo.json@skipBlanks'(XNStrm1464, XStx1580, Xstream73).
'lo.json@skipBlanks'(XStIn1671, XStx1581, Xstream73):- ocall('_hdtl%3'(XStIn1671, 9, XNStrm1465),Xstream73,Xstream73),
    'lo.json@skipBlanks'(XNStrm1465, XStx1581, Xstream73).
'lo.json@skipBlanks'(XStIn1672, XStx1582, Xstream73):- ocall('_hdtl%3'(XStIn1672, 10, XNStrm1466),Xstream73,Xstream73),
    'lo.json@skipBlanks'(XNStrm1466, XStx1582, Xstream73).
'lo.json@skipBlanks'(XStIn1673, XStx1583, Xstream73):- ocall('_hdtl%3'(XStIn1673, 13, XNStrm1467),Xstream73,Xstream73),
    'lo.json@skipBlanks'(XNStrm1467, XStx1583, Xstream73).
'lo.json@skipBlanks'(XStIn1674, XStIn1674, Xstream73).
'lo.json@digit'(XStIn1675, XNStrm1468, Xstream74, 0):- ocall('_hdtl%3'(XStIn1675, 48, XNStrm1468),Xstream74,Xstream74).
'lo.json@digit'(XStIn1676, XNStrm1469, Xstream74, 1):- ocall('_hdtl%3'(XStIn1676, 49, XNStrm1469),Xstream74,Xstream74).
'lo.json@digit'(XStIn1677, XNStrm1470, Xstream74, 2):- ocall('_hdtl%3'(XStIn1677, 50, XNStrm1470),Xstream74,Xstream74).
'lo.json@digit'(XStIn1678, XNStrm1471, Xstream74, 3):- ocall('_hdtl%3'(XStIn1678, 51, XNStrm1471),Xstream74,Xstream74).
'lo.json@digit'(XStIn1679, XNStrm1472, Xstream74, 4):- ocall('_hdtl%3'(XStIn1679, 52, XNStrm1472),Xstream74,Xstream74).
'lo.json@digit'(XStIn1680, XNStrm1473, Xstream74, 5):- ocall('_hdtl%3'(XStIn1680, 53, XNStrm1473),Xstream74,Xstream74).
'lo.json@digit'(XStIn1681, XNStrm1474, Xstream74, 6):- ocall('_hdtl%3'(XStIn1681, 54, XNStrm1474),Xstream74,Xstream74).
'lo.json@digit'(XStIn1682, XNStrm1475, Xstream74, 7):- ocall('_hdtl%3'(XStIn1682, 55, XNStrm1475),Xstream74,Xstream74).
'lo.json@digit'(XStIn1683, XNStrm1476, Xstream74, 8):- ocall('_hdtl%3'(XStIn1683, 56, XNStrm1476),Xstream74,Xstream74).
'lo.json@digit'(XStIn1684, XNStrm1477, Xstream74, 9):- ocall('_hdtl%3'(XStIn1684, 57, XNStrm1477),Xstream74,Xstream74).
'lo.json@hexDigit'(XStIn1685, XStx1584, Xstream75, XX):- 'lo.json@digit'(XStIn1685, XStx1584, Xstream75, XX).
'lo.json@hexDigit'(XStIn1686, XNStrm1478, Xstream75, 10):- ocall('_hdtl%3'(XStIn1686, 97, XNStrm1478),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1687, XNStrm1479, Xstream75, 11):- ocall('_hdtl%3'(XStIn1687, 98, XNStrm1479),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1688, XNStrm1480, Xstream75, 12):- ocall('_hdtl%3'(XStIn1688, 99, XNStrm1480),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1689, XNStrm1481, Xstream75, 13):- ocall('_hdtl%3'(XStIn1689, 100, XNStrm1481),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1690, XNStrm1482, Xstream75, 14):- ocall('_hdtl%3'(XStIn1690, 101, XNStrm1482),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1691, XNStrm1483, Xstream75, 15):- ocall('_hdtl%3'(XStIn1691, 102, XNStrm1483),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1692, XNStrm1484, Xstream75, 10):- ocall('_hdtl%3'(XStIn1692, 65, XNStrm1484),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1693, XNStrm1485, Xstream75, 11):- ocall('_hdtl%3'(XStIn1693, 66, XNStrm1485),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1694, XNStrm1486, Xstream75, 12):- ocall('_hdtl%3'(XStIn1694, 67, XNStrm1486),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1695, XNStrm1487, Xstream75, 13):- ocall('_hdtl%3'(XStIn1695, 68, XNStrm1487),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1696, XNStrm1488, Xstream75, 14):- ocall('_hdtl%3'(XStIn1696, 69, XNStrm1488),Xstream75,Xstream75).
'lo.json@hexDigit'(XStIn1697, XNStrm1489, Xstream75, 15):- ocall('_hdtl%3'(XStIn1697, 70, XNStrm1489),Xstream75,Xstream75).
'lo.json@readHex'(XStIn1698, XStx1586, Xstream76, XSoFar, XInt, XCnt):- 'lo.core@>'('lo.core$comp$lo.core*integer', XCnt, 0),
    'lo.json@hexDigit'(XStIn1698, XStx1585, Xstream76, XD),
    ocall('*%1'(XXV4715),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%1'(XXV4716),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('-%1'(XXV4717),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XSoFar, 10, XXe4379),XXV4715,XXV4715),
    ocall('_call%3'(XXe4379, XD, XXe4380),XXV4716,XXV4716),
    ocall('_call%3'(XCnt, 1, XXe4381),XXV4717,XXV4717),
    'lo.json@readHex'(XStx1585, XStx1586, Xstream76, XXe4380, XInt, XXe4381).
'lo.json@readHex'(XStIn1699, XStIn1699, Xstream76, XSoFar, XSoFar, X_30951).
'lo.json@parseStr'(XStIn1700, XStIn1700, Xstream77, 'lo.core#[]'):- 'lo.json@Hed97'(XStIn1700, XNStrm1490, Xstream77, XNStrm1490, XHedStrm97).
'lo.json@parseStr'(XStIn1701, XStx1588, Xstream77, 'lo.core#,..'(XH, Xl)):- ocall('_hdtl%3'(XStIn1701, 92, XNStrm1491),Xstream77,Xstream77),
    ocall('_hdtl%3'(XNStrm1491, 117, XNStrm1492),Xstream77,Xstream77),
    'lo.json@readHex'(XNStrm1492, XStx1587, Xstream77, 0, XH, 4),
    'lo.json@parseStr'(XStx1587, XStx1588, Xstream77, Xl).
'lo.json@parseStr'(XStIn1702, XStx1589, Xstream77, 'lo.core#,..'(8, Xl)):- ocall('_hdtl%3'(XStIn1702, 92, XNStrm1493),Xstream77,Xstream77),
    ocall('_hdtl%3'(XNStrm1493, 98, XNStrm1494),Xstream77,Xstream77),
    'lo.json@parseStr'(XNStrm1494, XStx1589, Xstream77, Xl).
'lo.json@parseStr'(XStIn1703, XStx1590, Xstream77, 'lo.core#,..'(102, Xl)):- ocall('_hdtl%3'(XStIn1703, 92, XNStrm1495),Xstream77,Xstream77),
    ocall('_hdtl%3'(XNStrm1495, 102, XNStrm1496),Xstream77,Xstream77),
    'lo.json@parseStr'(XNStrm1496, XStx1590, Xstream77, Xl).
'lo.json@parseStr'(XStIn1704, XStx1591, Xstream77, 'lo.core#,..'(10, Xl)):- ocall('_hdtl%3'(XStIn1704, 92, XNStrm1497),Xstream77,Xstream77),
    ocall('_hdtl%3'(XNStrm1497, 110, XNStrm1498),Xstream77,Xstream77),
    'lo.json@parseStr'(XNStrm1498, XStx1591, Xstream77, Xl).
'lo.json@parseStr'(XStIn1705, XStx1592, Xstream77, 'lo.core#,..'(13, Xl)):- ocall('_hdtl%3'(XStIn1705, 92, XNStrm1499),Xstream77,Xstream77),
    ocall('_hdtl%3'(XNStrm1499, 114, XNStrm1500),Xstream77,Xstream77),
    'lo.json@parseStr'(XNStrm1500, XStx1592, Xstream77, Xl).
'lo.json@parseStr'(XStIn1706, XStx1593, Xstream77, 'lo.core#,..'(9, Xl)):- ocall('_hdtl%3'(XStIn1706, 92, XNStrm1501),Xstream77,Xstream77),
    ocall('_hdtl%3'(XNStrm1501, 116, XNStrm1502),Xstream77,Xstream77),
    'lo.json@parseStr'(XNStrm1502, XStx1593, Xstream77, Xl).
'lo.json@parseStr'(XStIn1707, XStx1594, Xstream77, 'lo.core#,..'(XX, Xl)):- ocall('_hdtl%3'(XStIn1707, 92, XNStrm1503),Xstream77,Xstream77),
    ocall('_hdtl%3'(XNStrm1503, XX, XNStrm1504),Xstream77,Xstream77),
    'lo.json@parseStr'(XNStrm1504, XStx1594, Xstream77, Xl).
'lo.json@parseStr'(XStIn1708, XStx1595, Xstream77, 'lo.core#,..'(XC, Xl)):- ocall('_hdtl%3'(XStIn1708, XC, XNStrm1505),Xstream77,Xstream77),
    'lo.json@parseStr'(XNStrm1505, XStx1595, Xstream77, Xl).
'lo.json@parseField'(XStIn1709, XNStrm1507, Xstream78, XXa91):- ocall('_hdtl%3'(XStIn1709, 34, XNStrm1506),Xstream78,Xstream78),
    'lo.json@parseStr'(XNStrm1506, XStx1596, Xstream78, XS),
    ocall('_hdtl%3'(XStx1596, 34, XNStrm1507),Xstream78,Xstream78),
    'implode'(XS, XXa91).
'lo.json@readNatural'(XStIn1710, XStx1598, Xstream79, XSoFar, XInt):- 'lo.json@digit'(XStIn1710, XStx1597, Xstream79, XD),
    ocall('*%1'(XXV4718),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%1'(XXV4719),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XSoFar, 10, XXe4382),XXV4718,XXV4718),
    ocall('_call%3'(XXe4382, XD, XXe4383),XXV4719,XXV4719),
    'lo.json@readNatural'(XStx1597, XStx1598, Xstream79, XXe4383, XInt).
'lo.json@readNatural'(XStIn1711, XStIn1711, Xstream79, XSoFar, XSoFar):- 'lo.json@Neg50'(XStIn1711, XStx1599, X_30960, Xstream79).
'lo.json@fraction'(XStIn1712, XStx1601, Xstream80, XScale, XSoFar, XResult):- 'lo.json@digit'(XStIn1712, XStx1600, Xstream80, XD),
    ocall('*%1'(XXV4720),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    ocall('*%1'(XXV4721),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    ocall('+%1'(XXV4722),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('_call%3'(XScale, 0.1, XXe4384),XXV4720,XXV4720),
    '_int2flt'(XD, XXc498),
    ocall('_call%3'(XXc498, XScale, XXe4385),XXV4721,XXV4721),
    ocall('_call%3'(XSoFar, XXe4385, XXe4386),XXV4722,XXV4722),
    'lo.json@fraction'(XStx1600, XStx1601, Xstream80, XXe4384, XXe4386, XResult).
'lo.json@fraction'(XStIn1713, XStIn1713, Xstream80, X_30961, XFract, XFract).
'lo.json@readDecimal'(XStIn1714, XStx1602, Xstream81, XIn):- ocall('_hdtl%3'(XStIn1714, 45, XNStrm1508),Xstream81,Xstream81),
    'lo.json@readNatural'(XNStrm1508, XStx1602, Xstream81, 0, XPl),
    ocall('-%1'(XXV4723),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(0, XPl, XXe4387),XXV4723,XXV4723),
    XIn = XXe4387.
'lo.json@readDecimal'(XStIn1715, XStx1603, Xstream81, XIn):- 'lo.json@readNatural'(XStIn1715, XStx1603, Xstream81, 0, XIn).
'lo.json@exponent'(XStIn1716, XStx1604, Xstream82, XSoFar, XFp):- 'lo.json@Disj85'(XStIn1716, XDjOut117, XNStrm1510, XNStrm1510, XNStrm1509, Xstream82, XNStrm1509, XDjStrm85),
    'lo.json@readDecimal'(XDjOut117, XStx1604, Xstream82, XExp),
    ocall('*%1'(XXV4724),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    '_int2flt'(XExp, XXc499),
    '_pwr'(10.0, XXc499, XXc500),
    ocall('_call%3'(XSoFar, XXc500, XXe4388),XXV4724,XXV4724),
    XFp = XXe4388.
'lo.json@exponent'(XStIn1717, XStIn1717, Xstream82, XFp, XFp).
'lo.json@readMoreNumber'(XStIn1718, XStx1607, Xstream83, XFp, XDecimal):- ocall('_hdtl%3'(XStIn1718, 46, XNStrm1511),Xstream83,Xstream83),
    'lo.json@Hed98'(XNStrm1511, XStx1605, X_30964, Xstream83),
    'lo.json@fraction'(XNStrm1511, XStx1606, Xstream83, 0.1, 0.0, XFr),
    ocall('+%1'(XXV4725),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    '_int2flt'(XDecimal, XXc501),
    ocall('_call%3'(XXc501, XFr, XXe4389),XXV4725,XXV4725),
    'lo.json@exponent'(XStx1606, XStx1607, Xstream83, XXe4389, XFp).
'lo.json@readMoreNumber'(XStIn1719, XStIn1719, Xstream83, XXe4390, XIx):- ocall('_coerce%1'(XXV4726),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_call%2'(XIx, XXe4390),XXV4726,XXV4726).
'lo.json@readNumber'(XStIn1720, XStx1609, Xstream84, XFlt):- 'lo.json@readNatural'(XStIn1720, XStx1608, Xstream84, 0, XFirst),
    'lo.json@readMoreNumber'(XStx1608, XStx1609, Xstream84, XFlt, XFirst).
'lo.json@moreSeq'(XStIn1721, XStIn1721, Xstream85, 'lo.core#[]'):- 'lo.json@Hed99'(XStIn1721, XNStrm1512, Xstream85, XNStrm1512, XHedStrm99).
'lo.json@moreSeq'(XStIn1722, XStx1613, Xstream85, 'lo.core#,..'(Xe, Xl)):- ocall('_hdtl%3'(XStIn1722, 44, XNStrm1513),Xstream85,Xstream85),
    'lo.json@skipBlanks'(XNStrm1513, XStx1610, Xstream85),
    'lo.json@jP'(XStx1610, XStx1611, Xstream85, Xe),
    'lo.json@skipBlanks'(XStx1611, XStx1612, Xstream85),
    'lo.json@moreSeq'(XStx1612, XStx1613, Xstream85, Xl).
'lo.json@parseSeq'(XStIn1723, XStIn1723, Xstream86, 'lo.core#[]'):- 'lo.json@Hed100'(XStIn1723, XNStrm1514, Xstream86, XNStrm1514, XHedStrm100).
'lo.json@parseSeq'(XStIn1724, XStx1616, Xstream86, 'lo.core#,..'(Xe, Xl)):- 'lo.json@jP'(XStIn1724, XStx1614, Xstream86, Xe),
    'lo.json@skipBlanks'(XStx1614, XStx1615, Xstream86),
    'lo.json@moreSeq'(XStx1615, XStx1616, Xstream86, Xl).
'lo.json@parseMoreColl'(XStIn1725, XStIn1725, Xstream87, Xm, Xm):- 'lo.json@Hed101'(XStIn1725, XNStrm1515, Xstream87, XNStrm1515, XHedStrm101).
'lo.json@parseMoreColl'(XStIn1726, XStx1623, Xstream87, Xm, Xc):- ocall('_hdtl%3'(XStIn1726, 44, XNStrm1516),Xstream87,Xstream87),
    'lo.json@skipBlanks'(XNStrm1516, XStx1617, Xstream87),
    'lo.json@parseField'(XStx1617, XStx1618, Xstream87, Xf),
    'lo.json@skipBlanks'(XStx1618, XStx1619, Xstream87),
    ocall('_hdtl%3'(XStx1619, 58, XNStrm1517),Xstream87,Xstream87),
    'lo.json@skipBlanks'(XNStrm1517, XStx1620, Xstream87),
    'lo.json@jP'(XStx1620, XStx1621, Xstream87, Xv),
    'lo.json@skipBlanks'(XStx1621, XStx1622, Xstream87),
    ocall('_put%1'(XXV4727),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(Xm, Xf, Xv, XXe4391),XXV4727,XXV4727),
    'lo.json@parseMoreColl'(XStx1622, XStx1623, Xstream87, XXe4391, Xc).
'lo.json@parseColl'(XStIn1727, XStIn1727, Xstream88, Xm, Xm):- 'lo.json@Hed102'(XStIn1727, XNStrm1518, Xstream88, XNStrm1518, XHedStrm102).
'lo.json@parseColl'(XStIn1728, XStx1628, Xstream88, Xm, Xc):- 'lo.json@parseField'(XStIn1728, XStx1624, Xstream88, Xf),
    'lo.json@skipBlanks'(XStx1624, XStx1625, Xstream88),
    ocall('_hdtl%3'(XStx1625, 58, XNStrm1519),Xstream88,Xstream88),
    'lo.json@skipBlanks'(XNStrm1519, XStx1626, Xstream88),
    'lo.json@jP'(XStx1626, XStx1627, Xstream88, Xv),
    ocall('_put%1'(XXV4728),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(Xm, Xf, Xv, XXe4392),XXV4728,XXV4728),
    'lo.json@parseMoreColl'(XStx1627, XStx1628, Xstream88, XXe4392, Xc).
'lo.json@jP'(XStIn1729, XNStrm1523, Xstream89, 'lo.json#jTrue'):- ocall('_hdtl%3'(XStIn1729, 116, XNStrm1520),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1520, 114, XNStrm1521),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1521, 117, XNStrm1522),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1522, 101, XNStrm1523),Xstream89,Xstream89).
'lo.json@jP'(XStIn1730, XNStrm1528, Xstream89, 'lo.json#jFalse'):- ocall('_hdtl%3'(XStIn1730, 102, XNStrm1524),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1524, 97, XNStrm1525),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1525, 108, XNStrm1526),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1526, 115, XNStrm1527),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1527, 101, XNStrm1528),Xstream89,Xstream89).
'lo.json@jP'(XStIn1731, XNStrm1532, Xstream89, 'lo.json#jNull'):- ocall('_hdtl%3'(XStIn1731, 110, XNStrm1529),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1529, 117, XNStrm1530),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1530, 108, XNStrm1531),Xstream89,Xstream89),
    ocall('_hdtl%3'(XNStrm1531, 108, XNStrm1532),Xstream89,Xstream89).
'lo.json@jP'(XStIn1732, XStx1629, Xstream89, 'lo.json#jNum'(XXe4393)):- ocall('_hdtl%3'(XStIn1732, 45, XNStrm1533),Xstream89,Xstream89),
    'lo.json@readNumber'(XNStrm1533, XStx1629, Xstream89, XN),
    ocall('-%1'(XXV4730),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('zero%1'(XXV4729),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('_call%3'(XXV4729, XN, XXe4393),XXV4730,XXV4730).
'lo.json@jP'(XStIn1733, XStx1631, Xstream89, 'lo.json#jNum'(XN)):- 'lo.json@Hed103'(XStIn1733, XStx1630, X_30967, Xstream89),
    'lo.json@readNumber'(XStIn1733, XStx1631, Xstream89, XN).
'lo.json@jP'(XStIn1734, XNStrm1535, Xstream89, 'lo.json#jTxt'(XXa92)):- ocall('_hdtl%3'(XStIn1734, 34, XNStrm1534),Xstream89,Xstream89),
    'lo.json@parseStr'(XNStrm1534, XStx1632, Xstream89, XS),
    ocall('_hdtl%3'(XStx1632, 34, XNStrm1535),Xstream89,Xstream89),
    'implode'(XS, XXa92).
'lo.json@jP'(XStIn1735, XNStrm1537, Xstream89, 'lo.json#jSeq'(XL)):- ocall('_hdtl%3'(XStIn1735, 91, XNStrm1536),Xstream89,Xstream89),
    'lo.json@skipBlanks'(XNStrm1536, XStx1633, Xstream89),
    'lo.json@parseSeq'(XStx1633, XStx1634, Xstream89, XL),
    ocall('_hdtl%3'(XStx1634, 93, XNStrm1537),Xstream89,Xstream89).
'lo.json@jP'(XStIn1736, XNStrm1539, Xstream89, 'lo.json#jColl'(XM)):- ocall('_hdtl%3'(XStIn1736, 123, XNStrm1538),Xstream89,Xstream89),
    'lo.json@skipBlanks'(XNStrm1538, XStx1635, Xstream89),
    ocall('_empty%1'(XXV4731),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.json@parseColl'(XStx1635, XStx1636, Xstream89, XXV4731, XM),
    ocall('_hdtl%3'(XStx1636, 125, XNStrm1539),Xstream89,Xstream89).
'lo.json@parseJson'(XStIn1737, XStx1638, Xstream90, XJ):- 'lo.json@skipBlanks'(XStIn1737, XStx1637, Xstream90),
    'lo.json@jP'(XStx1637, XStx1638, Xstream90, XJ).
'lo.coerce$coercion$lo.core*string$lo.json*json'('lo.coerce$coercion$lo.core*string$lo.json*json%1'('lo.coerce$coercion$lo.core*string$lo.json*json')):- !.
'lo.coerce$coercion$lo.core*string$lo.json*json'('_coerce%2'(XV28687, XV28688), XLbl2046, XThis2046):- !,
    'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XV28687, XV28688, XLbl2046, XThis2046).
'lo.coerce$coercion$lo.core*string$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'(XLbl2047, XThis2047)), XLbl2047, XThis2047).
'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XS, XJ, XLbV2224, XThV2224):- 'explode'(XS, XXc502),
    'lo.json@parseJson'(XXc502, XStx1639, 'lo.core$stream$lo.core*list', XJ),
    XStx1639 = X_30969,
    ocall('_eof%1'(X_30969),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_30968 = XStx1639,
    !.
'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*string$lo.json*json@_coerce", 62, 5, 45)).
'lo.json@entryCoerce'(Xcoercion29, Xe, XXe4394):- !,
    ocall('_coerce%1'(XXV4732),Xcoercion29,Xcoercion29),
    ocall('_call%2'(Xe, XXe4394),XXV4732,XXV4732).
'lo.json@entryCoerce'(_, _, _):- raise_exception('error'("lo.json@entryCoerce", 179, 3, 25)).
'lo.json@mapJson'(Xcoercion30, XM, 'lo.json#jColl'(XXd35792)):- !,
    'lo.json@entryCoerce'(Xcoercion30, XXd35791),
    'lo.index@mapMap'(XM, XXd35791, XXd35792).
'lo.json@mapJson'(_, _, _):- raise_exception('error'("lo.json@mapJson", 176, 3, 42)).
'lo.coerce$coercion$lo.index*map$lo.json*json'('lo.coerce$coercion$lo.index*map$lo.json*json%1'('lo.coerce$coercion$lo.index*map$lo.json*json')):- !.
'lo.coerce$coercion$lo.index*map$lo.json*json'('_coerce%2'(XV28697, XV28698), XLbl2048, XThis2048):- !,
    'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XV28697, XV28698, XLbl2048, XThis2048).
'lo.coerce$coercion$lo.index*map$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'(XLbl2049, XThis2049)), XLbl2049, XThis2049).
'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XM, XXd35794, XLbV2225, XThV2225):- XLbV2225 = 'lo.coerce$coercion$lo.index*map$lo.json*json'(Xcoercion31),
    !,
    'lo.json@mapJson'(Xcoercion31, XM, XXd35794).
'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.index*map$lo.json*json@_coerce", 172, 5, 24)).
'lo.json@cList'(Xcoercion32, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.json@cList'(Xcoercion32, 'lo.core#,..'(Xe, Xl), 'lo.core#,..'(XXe4395, XXd35795)):- !,
    ocall('_coerce%1'(XXV4733),Xcoercion32,Xcoercion32),
    ocall('_call%2'(Xe, XXe4395),XXV4733,XXV4733),
    'lo.json@cList'(Xcoercion32, Xl, XXd35795).
'lo.json@cList'(_, _, _):- raise_exception('error'("lo.json@cList", 186, 3, 15)).
'lo.coerce$coercion$lo.core*list$lo.json*json'('lo.coerce$coercion$lo.core*list$lo.json*json%1'('lo.coerce$coercion$lo.core*list$lo.json*json')):- !.
'lo.coerce$coercion$lo.core*list$lo.json*json'('_coerce%2'(XV28704, XV28705), XLbl2050, XThis2050):- !,
    'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XV28704, XV28705, XLbl2050, XThis2050).
'lo.coerce$coercion$lo.core*list$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'(XLbl2051, XThis2051)), XLbl2051, XThis2051).
'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XL, 'lo.json#jSeq'(XXd35797), XLbV2226, XThV2226):- XLbV2226 = 'lo.coerce$coercion$lo.core*list$lo.json*json'(Xcoercion33),
    !,
    'lo.json@cList'(Xcoercion33, XL, XXd35797).
'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.json*json@_coerce", 182, 5, 28)).
'lo.json^spaces'('_call%2'(XV28572, XV28573), 'lo.json^spaces', _):- 'lo.json@spaces'(XV28572, XV28573).
'lo.json^break'('_call%2'(XV28574, XV28575), 'lo.json^break', _):- 'lo.json@break'(XV28574, XV28575).
'lo.json^dispColl'('_call%4'(XV28576, XV28577, XV28578, XV28579), 'lo.json^dispColl', _):- 'lo.json@dispColl'(XV28576, XV28577, XV28578, XV28579).
'lo.json^dispSeq'('_call%4'(XV28580, XV28581, XV28582, XV28583), 'lo.json^dispSeq', _):- 'lo.json@dispSeq'(XV28580, XV28581, XV28582, XV28583).
'lo.json^dispJson'('_call%3'(XV28584, XV28585, XV28586), 'lo.json^dispJson', _):- 'lo.json@dispJson'(XV28584, XV28585, XV28586).
'lo.core$display$lo.json*json^disp'('_call%2'(XV28587, XV28588), 'lo.core$display$lo.json*json^disp'(XLbV2222, XThV2222), _):- 'lo.core$display$lo.json*json@disp'(XV28587, XV28588, XLbV2222, XThV2222).
'lo.json^hashJson'('_call%2'(XV28591, XV28592), 'lo.json^hashJson', _):- 'lo.json@hashJson'(XV28591, XV28592).
'lo.json^equalJson'('_call%2'(XV28593, XV28594), 'lo.json^equalJson', _):- 'lo.json@equalJson'(XV28593, XV28594).
'lo.core$equality$lo.json*json^=='('_call%4'(XV28595, XV28596, XV28597, XV28598), 'lo.core$equality$lo.json*json^=='(XLbV2223, XThV2223), _):- 'lo.core$equality$lo.json*json@=='(XV28595, XV28596, XV28597, XV28598, XLbV2223, XThV2223).
'lo.core$equality$lo.json*json^hash'('_call%2'(XV28601, XV28602), 'lo.core$equality$lo.json*json^hash'(XLbV2223, XThV2223), _):- 'lo.core$equality$lo.json*json@hash'(XV28601, XV28602, XLbV2223, XThV2223).
'lo.json^skipBlanks'('_call%3'(XV28605, XV28606, XV28607), 'lo.json^skipBlanks', _):- 'lo.json@skipBlanks'(XV28605, XV28606, XV28607).
'lo.json^digit'('_call%4'(XV28608, XV28609, XV28610, XV28611), 'lo.json^digit', _):- 'lo.json@digit'(XV28608, XV28609, XV28610, XV28611).
'lo.json^hexDigit'('_call%4'(XV28612, XV28613, XV28614, XV28615), 'lo.json^hexDigit', _):- 'lo.json@hexDigit'(XV28612, XV28613, XV28614, XV28615).
'lo.json^readHex'('_call%6'(XV28616, XV28617, XV28618, XV28619, XV28620, XV28621), 'lo.json^readHex', _):- 'lo.json@readHex'(XV28616, XV28617, XV28618, XV28619, XV28620, XV28621).
'lo.json@Hed97'(XHedStrm97, XNStrm1490, Xstream77, XNStrm1490, XHedStrm97):- ocall('_hdtl%3'(XHedStrm97, 34, XNStrm1490),Xstream77,Xstream77).
'lo.json^parseStr'('_call%4'(XV28622, XV28623, XV28624, XV28625), 'lo.json^parseStr', _):- 'lo.json@parseStr'(XV28622, XV28623, XV28624, XV28625).
'lo.json^parseField'('_call%4'(XV28626, XV28627, XV28628, XV28629), 'lo.json^parseField', _):- 'lo.json@parseField'(XV28626, XV28627, XV28628, XV28629).
'lo.json@Neg50'(XNegStrm50, XStx1599, X_30960, Xstream79):- 'lo.json@digit'(XNegStrm50, XStx1599, Xstream79, X_30960),
    !,
    fail.
'lo.json@Neg50'(XNegStrm50, XStx1599, X_30960, Xstream79).
'lo.json^readNatural'('_call%5'(XV28630, XV28631, XV28632, XV28633, XV28634), 'lo.json^readNatural', _):- 'lo.json@readNatural'(XV28630, XV28631, XV28632, XV28633, XV28634).
'lo.json^fraction'('_call%6'(XV28635, XV28636, XV28637, XV28638, XV28639, XV28640), 'lo.json^fraction', _):- 'lo.json@fraction'(XV28635, XV28636, XV28637, XV28638, XV28639, XV28640).
'lo.json^readDecimal'('_call%4'(XV28641, XV28642, XV28643, XV28644), 'lo.json^readDecimal', _):- 'lo.json@readDecimal'(XV28641, XV28642, XV28643, XV28644).
'lo.json@Disj85'(XDjStrm85, XNStrm1509, XNStrm1510, XNStrm1510, XNStrm1509, Xstream82, XNStrm1509, XDjStrm85):- ocall('_hdtl%3'(XDjStrm85, 101, XNStrm1509),Xstream82,Xstream82).
'lo.json@Disj85'(XDjStrm85, XNStrm1510, XNStrm1510, XNStrm1510, XNStrm1509, Xstream82, XNStrm1509, XDjStrm85):- ocall('_hdtl%3'(XDjStrm85, 69, XNStrm1510),Xstream82,Xstream82).
'lo.json^exponent'('_call%5'(XV28645, XV28646, XV28647, XV28648, XV28649), 'lo.json^exponent', _):- 'lo.json@exponent'(XV28645, XV28646, XV28647, XV28648, XV28649).
'lo.json@Hed98'(XHedStrm98, XStx1605, X_30964, Xstream83):- 'lo.json@digit'(XHedStrm98, XStx1605, Xstream83, X_30964).
'lo.json^readMoreNumber'('_call%5'(XV28650, XV28651, XV28652, XV28653, XV28654), 'lo.json^readMoreNumber', _):- 'lo.json@readMoreNumber'(XV28650, XV28651, XV28652, XV28653, XV28654).
'lo.json^readNumber'('_call%4'(XV28655, XV28656, XV28657, XV28658), 'lo.json^readNumber', _):- 'lo.json@readNumber'(XV28655, XV28656, XV28657, XV28658).
'lo.json@Hed99'(XHedStrm99, XNStrm1512, Xstream85, XNStrm1512, XHedStrm99):- ocall('_hdtl%3'(XHedStrm99, 93, XNStrm1512),Xstream85,Xstream85).
'lo.json^moreSeq'('_call%4'(XV28659, XV28660, XV28661, XV28662), 'lo.json^moreSeq', _):- 'lo.json@moreSeq'(XV28659, XV28660, XV28661, XV28662).
'lo.json@Hed100'(XHedStrm100, XNStrm1514, Xstream86, XNStrm1514, XHedStrm100):- ocall('_hdtl%3'(XHedStrm100, 93, XNStrm1514),Xstream86,Xstream86).
'lo.json^parseSeq'('_call%4'(XV28663, XV28664, XV28665, XV28666), 'lo.json^parseSeq', _):- 'lo.json@parseSeq'(XV28663, XV28664, XV28665, XV28666).
'lo.json@Hed101'(XHedStrm101, XNStrm1515, Xstream87, XNStrm1515, XHedStrm101):- ocall('_hdtl%3'(XHedStrm101, 125, XNStrm1515),Xstream87,Xstream87).
'lo.json^parseMoreColl'('_call%5'(XV28667, XV28668, XV28669, XV28670, XV28671), 'lo.json^parseMoreColl', _):- 'lo.json@parseMoreColl'(XV28667, XV28668, XV28669, XV28670, XV28671).
'lo.json@Hed102'(XHedStrm102, XNStrm1518, Xstream88, XNStrm1518, XHedStrm102):- ocall('_hdtl%3'(XHedStrm102, 125, XNStrm1518),Xstream88,Xstream88).
'lo.json^parseColl'('_call%5'(XV28672, XV28673, XV28674, XV28675, XV28676), 'lo.json^parseColl', _):- 'lo.json@parseColl'(XV28672, XV28673, XV28674, XV28675, XV28676).
'lo.json@Hed103'(XHedStrm103, XStx1630, X_30967, Xstream89):- 'lo.json@digit'(XHedStrm103, XStx1630, Xstream89, X_30967).
'lo.json^jP'('_call%4'(XV28677, XV28678, XV28679, XV28680), 'lo.json^jP', _):- 'lo.json@jP'(XV28677, XV28678, XV28679, XV28680).
'lo.json^parseJson'('_call%4'(XV28681, XV28682, XV28683, XV28684), 'lo.json^parseJson', _):- 'lo.json@parseJson'(XV28681, XV28682, XV28683, XV28684).
'lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'('_call%2'(XV28685, XV28686), 'lo.coerce$coercion$lo.core*string$lo.json*json^_coerce'(XLbV2224, XThV2224), _):- 'lo.coerce$coercion$lo.core*string$lo.json*json@_coerce'(XV28685, XV28686, XLbV2224, XThV2224).
'lo.json^entryCoerce'('_call%3'(XV28689, XV28690, XV28691), 'lo.json^entryCoerce', _):- 'lo.json@entryCoerce'(XV28689, XV28690, XV28691).
'lo.json^mapJson'('_call%3'(XV28692, XV28693, XV28694), 'lo.json^mapJson', _):- 'lo.json@mapJson'(XV28692, XV28693, XV28694).
'lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'('_call%2'(XV28695, XV28696), 'lo.coerce$coercion$lo.index*map$lo.json*json^_coerce'(XLbV2225, XThV2225), _):- 'lo.coerce$coercion$lo.index*map$lo.json*json@_coerce'(XV28695, XV28696, XLbV2225, XThV2225).
'lo.json^cList'('_call%3'(XV28699, XV28700, XV28701), 'lo.json^cList', _):- 'lo.json@cList'(XV28699, XV28700, XV28701).
'lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'('_call%2'(XV28702, XV28703), 'lo.coerce$coercion$lo.core*list$lo.json*json^_coerce'(XLbV2226, XThV2226), _):- 'lo.coerce$coercion$lo.core*list$lo.json*json@_coerce'(XV28702, XV28703, XLbV2226, XThV2226).
