'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.misc's'0.0.1'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I7'pathSuffix'FT2SSS'subPath'FT3SSSS'starts_with'PT2SS'pr':k'x':k'y'CT2k'x'k'y'Uz2'lo.comp.misc*pr'2k'x'k'y''replace':k'x'FT3Lk'x'k'x'k'x'Lk'x''collect':k'x'PT4Lk'x'PT1k'x'Lk'x'Lk'x''nextPrime'FT1ii\"s\"I1'pr':k'x':k'y':k'x':k'y'YUz2'lo.comp.misc*pr'2k'x'k'y'I0\"n1o1'()1's'pr'n0o0'()0'n1o1'()1'n2o2'()2's'lo.core$display$lo.comp.misc*pr's\":k'x':k'y'||c'lo.core$display'T1Uz2'lo.comp.misc*pr'2k'x'k'y'T0c'lo.core$display'T1k'x'T0c'lo.core$display'T1k'y'T0\"").
'lo.comp.misc@init'():- !.
'lo.comp.misc@splitString'(XSrc, XMrk, XBefore, XAfter):- '_str_find'(XSrc, XMrk, 0, XPt),
    '_str_split'(XSrc, XPt, XBefore, XSecond),
    '_str_len'(XMrk, XXc398),
    '_str_split'(XSecond, XXc398, X_19757, XAfter).
'lo.comp.misc@pathSuffix'(XSrc, XMarker, XAfter):- 'lo.comp.misc@splitString'(XSrc, XMarker, XBefore, XAfter),
    !.
'lo.comp.misc@pathSuffix'(XSrc, X_19758, XSrc):- !.
'lo.comp.misc@pathSuffix'(_, _, _):- raise_exception('error'("lo.comp.misc@pathSuffix", 5, 3, 72)).
'lo.comp.misc@subPath'(XPath, XMarker, XSuffix, XXe2806):- 'lo.comp.misc@splitString'(XPath, XMarker, X_19759, X_19760),
    !,
    ocall('+%1'(XXV3011),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%1'(XXV3012),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('_call%3'(XPath, ".", XXe2805),XXV3011,XXV3011),
    ocall('_call%3'(XXe2805, XSuffix, XXe2806),XXV3012,XXV3012).
'lo.comp.misc@subPath'(XPath, XMarker, XSuffix, XXe2808):- !,
    ocall('+%1'(XXV3013),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%1'(XXV3014),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('_call%3'(XPath, XMarker, XXe2807),XXV3013,XXV3013),
    ocall('_call%3'(XXe2807, XSuffix, XXe2808),XXV3014,XXV3014).
'lo.comp.misc@subPath'(_, _, _, _):- raise_exception('error'("lo.comp.misc@subPath", 15, 3, 82)).
'lo.comp.misc@starts_with'(XS, XT):- ocall('size%1'(XXV3015),'lo.core$sizeable$lo.core*string','lo.core$sizeable$lo.core*string'),
    ocall('_call%2'(XT, XXe2809),XXV3015,XXV3015),
    '_sub_str'(XS, 0, XXe2809, XXc399),
    ocall('==%2'(XXc399, XT),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.comp.misc#pr'('pr%1'('lo.comp.misc@pr'())):- !.
'lo.comp.misc@dispDf'(Xdisplay98, Xdisplay99, 'lo.comp.misc#pr'(XX, XY), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("("), 'lo.core#,..'(XXe2810, 'lo.core#,..'('lo.core#ss'(","), 'lo.core#,..'(XXe2811, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]'))))))):- !,
    ocall('disp%1'(XXV3016),Xdisplay98,Xdisplay98),
    ocall('disp%1'(XXV3017),Xdisplay99,Xdisplay99),
    ocall('_call%2'(XX, XXe2810),XXV3016,XXV3016),
    ocall('_call%2'(XY, XXe2811),XXV3017,XXV3017).
'lo.comp.misc@dispDf'(_, _, _, _):- raise_exception('error'("lo.comp.misc@dispDf", 29, 3, 67)).
'lo.core$display$lo.comp.misc*pr'('lo.core$display$lo.comp.misc*pr%1'('lo.core$display$lo.comp.misc*pr')):- !.
'lo.core$display$lo.comp.misc*pr'('disp%2'(XV19592, XV19593), XLbl1722, XThis1722):- !,
    'lo.core$display$lo.comp.misc*pr@disp'(XV19592, XV19593, XLbl1722, XThis1722).
'lo.core$display$lo.comp.misc*pr'('disp%1'('lo.core$display$lo.comp.misc*pr^disp'(XLbl1723, XThis1723)), XLbl1723, XThis1723).
'lo.core$display$lo.comp.misc*pr@disp'(XD, XXd23232, XLbV1566, XThV1566):- XLbV1566 = 'lo.core$display$lo.comp.misc*pr'(Xdisplay100, Xdisplay101),
    !,
    'lo.comp.misc@dispDf'(Xdisplay101, Xdisplay100, XD, XXd23232).
'lo.core$display$lo.comp.misc*pr@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.misc*pr@disp", 25, 5, 20)).
'lo.comp.misc@prHash'(Xequality265, Xequality266, 'lo.comp.misc#pr'(XX, XY), XXe2815):- !,
    ocall('hash%1'(XXV3018),Xequality265,Xequality265),
    ocall('*%1'(XXV3019),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('hash%1'(XXV3020),Xequality266,Xequality266),
    ocall('+%1'(XXV3021),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XX, XXe2812),XXV3018,XXV3018),
    ocall('_call%3'(37, XXe2812, XXe2813),XXV3019,XXV3019),
    ocall('_call%2'(XY, XXe2814),XXV3020,XXV3020),
    ocall('_call%3'(XXe2813, XXe2814, XXe2815),XXV3021,XXV3021).
'lo.comp.misc@prHash'(_, _, _, _):- raise_exception('error'("lo.comp.misc@prHash", 41, 3, 39)).
'lo.comp.misc@prEq'(Xequality267, Xequality268, 'lo.comp.misc#pr'(XX1, XY1), 'lo.comp.misc#pr'(XX2, XY2)):- ocall('==%2'(XX1, XX2),Xequality267,Xequality267),
    ocall('==%2'(XY1, XY2),Xequality268,Xequality268).
'lo.core$equality$lo.comp.misc*pr'('lo.core$equality$lo.comp.misc*pr%1'('lo.core$equality$lo.comp.misc*pr')):- !.
'lo.core$equality$lo.comp.misc*pr'('==%2'(XV19606, XV19607), XLbl1724, XThis1724):- !,
    'lo.core$equality$lo.comp.misc*pr@=='(XV19606, XV19607, XLbl1724, XThis1724).
'lo.core$equality$lo.comp.misc*pr'('==%1'('lo.core$equality$lo.comp.misc*pr^=='(XLbl1725, XThis1725)), XLbl1725, XThis1725).
'lo.core$equality$lo.comp.misc*pr'('hash%2'(XV19610, XV19611), XLbl1726, XThis1726):- !,
    'lo.core$equality$lo.comp.misc*pr@hash'(XV19610, XV19611, XLbl1726, XThis1726).
'lo.core$equality$lo.comp.misc*pr'('hash%1'('lo.core$equality$lo.comp.misc*pr^hash'(XLbl1727, XThis1727)), XLbl1727, XThis1727).
'lo.core$equality$lo.comp.misc*pr@=='(XX, XY, XLbV1567, XThV1567):- XLbV1567 = 'lo.core$equality$lo.comp.misc*pr'(Xequality269, Xequality270),
    'lo.comp.misc@prEq'(Xequality270, Xequality269, XX, XY).
'lo.core$equality$lo.comp.misc*pr@hash'(XD, XXd23233, XLbV1567, XThV1567):- XLbV1567 = 'lo.core$equality$lo.comp.misc*pr'(Xequality269, Xequality270),
    !,
    'lo.comp.misc@prHash'(Xequality270, Xequality269, XD, XXd23233).
'lo.core$equality$lo.comp.misc*pr@hash'(_, _):- raise_exception('error'("lo.core$equality$lo.comp.misc*pr@hash", 34, 5, 20)).
'lo.comp.misc@replace'('lo.core#[]', X_19768, Xr, 'lo.core#,..'(Xr, 'lo.core#[]')):- !.
'lo.comp.misc@replace'('lo.core#,..'(Xe, Xl), Xe, Xr, 'lo.core#,..'(Xr, Xl)):- !.
'lo.comp.misc@replace'('lo.core#,..'(Xe, Xl), Xt, Xr, 'lo.core#,..'(Xe, XXd23236)):- 'lo.comp.misc@neg186'(Xt, Xe),
    !,
    'lo.comp.misc@replace'(Xl, Xt, Xr, XXd23236).
'lo.comp.misc@replace'(_, _, _, _):- raise_exception('error'("lo.comp.misc@replace", 44, 3, 22)).
'lo.comp.misc@collect'('lo.core#[]', X_19774, 'lo.core#[]', 'lo.core#[]').
'lo.comp.misc@collect'('lo.core#,..'(Xe, Xl), Xp, 'lo.core#,..'(Xe, Xm), Xo):- ocall('_call%1'(Xe),Xp,Xp),
    'lo.comp.misc@collect'(Xl, Xp, Xm, Xo).
'lo.comp.misc@collect'('lo.core#,..'(Xe, Xl), Xp, Xm, 'lo.core#,..'(Xe, Xo)):- 'lo.comp.misc@neg187'(Xp, Xe),
    'lo.comp.misc@collect'(Xl, Xp, Xm, Xo).
'lo.comp.misc@filter'(XIx, XIx, XL):- 'lo.comp.misc@forallA9'(XXe2816, XXV3022, XIx, XL, XE).
'lo.comp.misc@filter'(XIx, XNx, XL):- ocall('+%1'(XXV3023),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 2, XXe2817),XXV3023,XXV3023),
    'lo.comp.misc@filter'(XXe2817, XNx, XL).
'lo.comp.misc@sieve'(XCurr, XMax, XSoFar, XSoFar):- 'lo.core@>'('lo.core$comp$lo.core*integer', XCurr, XMax),
    !.
'lo.comp.misc@sieve'(XCurr, XMax, XSoFar, XXd23239):- ocall('+%1'(XXV3024),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XCurr, 2, XXe2818),XXV3024,XXV3024),
    'lo.comp.misc@filter'(XXe2818, XNxt, XSoFar),
    !,
    'lo.comp.misc@sieve'(XNxt, XMax, 'lo.core#,..'(XNxt, XSoFar), XXd23239).
'lo.comp.misc@sieve'(_, _, _, _):- raise_exception('error'("lo.comp.misc@sieve", 58, 3, 42)).
'lo.comp.misc@nextPrime'(XFrom, XXd23242):- !,
    'lo.comp.misc@sieve'(3, XFrom, 'lo.core#,..'(3, 'lo.core#[]'), XXd23241),
    'lo.list@head'(XXd23241, XXd23242).
'lo.comp.misc@nextPrime'(_, _):- raise_exception('error'("lo.comp.misc@nextPrime", 55, 3, 42)).
'lo.comp.misc^splitString'('_call%4'(XV19573, XV19574, XV19575, XV19576), 'lo.comp.misc^splitString', _):- 'lo.comp.misc@splitString'(XV19573, XV19574, XV19575, XV19576).
'lo.comp.misc^pathSuffix'('_call%3'(XV19577, XV19578, XV19579), 'lo.comp.misc^pathSuffix', _):- 'lo.comp.misc@pathSuffix'(XV19577, XV19578, XV19579).
'lo.comp.misc^subPath'('_call%4'(XV19580, XV19581, XV19582, XV19583), 'lo.comp.misc^subPath', _):- 'lo.comp.misc@subPath'(XV19580, XV19581, XV19582, XV19583).
'lo.comp.misc^starts_with'('_call%2'(XV19584, XV19585), 'lo.comp.misc^starts_with', _):- 'lo.comp.misc@starts_with'(XV19584, XV19585).
'lo.comp.misc^dispDf'('_call%4'(XV19586, XV19587, XV19588, XV19589), 'lo.comp.misc^dispDf', _):- 'lo.comp.misc@dispDf'(XV19586, XV19587, XV19588, XV19589).
'lo.core$display$lo.comp.misc*pr^disp'('_call%2'(XV19590, XV19591), 'lo.core$display$lo.comp.misc*pr^disp'(XLbV1566, XThV1566), _):- 'lo.core$display$lo.comp.misc*pr@disp'(XV19590, XV19591, XLbV1566, XThV1566).
'lo.comp.misc^prHash'('_call%4'(XV19594, XV19595, XV19596, XV19597), 'lo.comp.misc^prHash', _):- 'lo.comp.misc@prHash'(XV19594, XV19595, XV19596, XV19597).
'lo.comp.misc^prEq'('_call%4'(XV19598, XV19599, XV19600, XV19601), 'lo.comp.misc^prEq', _):- 'lo.comp.misc@prEq'(XV19598, XV19599, XV19600, XV19601).
'lo.core$equality$lo.comp.misc*pr^=='('_call%4'(XV19602, XV19603, XV19604, XV19605), 'lo.core$equality$lo.comp.misc*pr^=='(XLbV1567, XThV1567), _):- 'lo.core$equality$lo.comp.misc*pr@=='(XV19602, XV19603, XV19604, XV19605, XLbV1567, XThV1567).
'lo.core$equality$lo.comp.misc*pr^hash'('_call%2'(XV19608, XV19609), 'lo.core$equality$lo.comp.misc*pr^hash'(XLbV1567, XThV1567), _):- 'lo.core$equality$lo.comp.misc*pr@hash'(XV19608, XV19609, XLbV1567, XThV1567).
'lo.comp.misc@neg186'(Xt, Xe):- Xe = Xt,
    !,
    fail.
'lo.comp.misc@neg186'(Xt, Xe).
'lo.comp.misc^replace'('_call%4'(XV19612, XV19613, XV19614, XV19615), 'lo.comp.misc^replace', _):- 'lo.comp.misc@replace'(XV19612, XV19613, XV19614, XV19615).
'lo.comp.misc@neg187'(Xp, Xe):- ocall('_call%1'(Xe),Xp,Xp),
    !,
    fail.
'lo.comp.misc@neg187'(Xp, Xe).
'lo.comp.misc^collect'('_call%4'(XV19616, XV19617, XV19618, XV19619), 'lo.comp.misc^collect', _):- 'lo.comp.misc@collect'(XV19616, XV19617, XV19618, XV19619).
'lo.comp.misc@neg188'(XXe2816, XXV3022, XIx, XE):- ocall('%%1'(XXV3022),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('_call%3'(XE, XIx, XXe2816),XXV3022,XXV3022),
    XXe2816 = 0,
    !,
    fail.
'lo.comp.misc@neg188'(XXe2816, XXV3022, XIx, XE).
'lo.comp.misc@forallA9'(XXe2816, XXV3022, XIx, XL, XE):- ocall('in%2'(XE, XL),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.misc@forallB9'(XXe2816, XXV3022, XIx, XL, XE),
    !,
    fail.
'lo.comp.misc@forallA9'(XXe2816, XXV3022, XIx, XL, XE).
'lo.comp.misc@forallB9'(XXe2816, XXV3022, XIx, XL, XE):- 'lo.comp.misc@neg188'(XXe2816, XXV3022, XIx, XE),
    !,
    fail.
'lo.comp.misc@forallB9'(XXe2816, XXV3022, XIx, XL, XE).
'lo.comp.misc^filter'('_call%3'(XV19620, XV19621, XV19622), 'lo.comp.misc^filter', _):- 'lo.comp.misc@filter'(XV19620, XV19621, XV19622).
'lo.comp.misc^sieve'('_call%4'(XV19623, XV19624, XV19625, XV19626), 'lo.comp.misc^sieve', _):- 'lo.comp.misc@sieve'(XV19623, XV19624, XV19625, XV19626).
'lo.comp.misc^nextPrime'('_call%2'(XV19627, XV19628), 'lo.comp.misc^nextPrime', _):- 'lo.comp.misc@nextPrime'(XV19627, XV19628).
