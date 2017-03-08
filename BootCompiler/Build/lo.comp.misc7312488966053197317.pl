'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.misc'e'*'n10o10'()10'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I7'pathSuffix'FT2SSS'subPath'FT3SSSS'starts_with'PT2SS'pr':k'x':k'y'CT2k'x'k'y'Uz2'lo.comp.misc*pr'2k'x'k'y''replace':k'x'FT3Lk'x'k'x'k'x'Lk'x''collect':k'x'PT4Lk'x'PT1k'x'Lk'x'Lk'x''nextPrime'FT1ii\"s\"I1'pr':k'x':k'y'YUz2'lo.comp.misc*pr'2k'x'k'y'I0\"n0o0'()0'n0o0'()0'n1o1'()1'n2o2'()2's'lo.core$display$lo.comp.misc*pr's\":k'x':k'y'||c'lo.core$display'T1Uz2'lo.comp.misc*pr'2k'x'k'y'T0c'lo.core$display'T1k'y'T0c'lo.core$display'T1k'x'T0\"").
'lo.comp.misc@init'() :- !.
'lo.comp.misc@splitString'(XSrc, XMrk, XBefore, XAfter) :- '_str_find'(XSrc, XMrk, 0, XPt),
    '_str_split'(XSrc, XPt, XBefore, XSecond),
    '_str_len'(XMrk, XX1911),
    '_str_split'(XSecond, XX1911, X_37, XAfter).
'lo.comp.misc@pathSuffix'(XSrc, XMarker, XAfter) :- 'lo.comp.misc@splitString'(XSrc, XMarker, XBefore, XAfter),
    !.
'lo.comp.misc@pathSuffix'(XSrc, X_38, XSrc) :- !.
'lo.comp.misc@pathSuffix'(_, _, _) :- raise_exception('error'("pathSuffix", 5, 3, 72)).
'lo.comp.misc@subPath'(XPath, XMarker, XSuffix, XX1935) :- 'lo.comp.misc@splitString'(XPath, XMarker, X_39, X_40),
    !,
    ocall('+%3'(XPath, ".", XX1932),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%3'(XX1932, XSuffix, XX1935),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string').
'lo.comp.misc@subPath'(XPath, XMarker, XSuffix, XX1945) :- !,
    ocall('+%3'(XPath, XMarker, XX1942),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%3'(XX1942, XSuffix, XX1945),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string').
'lo.comp.misc@subPath'(_, _, _, _) :- raise_exception('error'("subPath", 15, 3, 82)).
'lo.comp.misc@starts_with'(XS, XT) :- ocall('size%2'(XT, XX1951),'lo.core$sizeable$lo.core*string','lo.core$sizeable$lo.core*string'),
    '_sub_str'(XS, 0, XX1951, XX1953),
    ocall('==%2'(XX1953, XT),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.comp.misc#pr'('pr%1'('lo.comp.misc@pr'())) :- !.
'lo.comp.misc@dispDf'(Xlo_core_display_x1, Xlo_core_display_y1, 'lo.comp.misc#pr'(XX, XY), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("("), 'lo.core#,..'(XX1966, 'lo.core#,..'('lo.core#ss'(","), 'lo.core#,..'(XX1970, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]'))))))) :- !,
    ocall('disp%2'(XX, XX1966),Xlo_core_display_x1,Xlo_core_display_x1),
    ocall('disp%2'(XY, XX1970),Xlo_core_display_y1,Xlo_core_display_y1).
'lo.comp.misc@dispDf'(_, _) :- raise_exception('error'("dispDf", 29, 3, 67)).
'lo.core$display$lo.comp.misc*pr'('lo.core$display$lo.comp.misc*pr%1'('lo.core$display$lo.comp.misc*pr')) :- !.
'lo.core$display$lo.comp.misc*pr'('disp%2'(XV584, XV585), XLbl125, XThis125) :- !,
    'lo.core$display$lo.comp.misc*pr@disp'(XV584, XV585, XLbl125, XThis125).
'lo.core$display$lo.comp.misc*pr'('disp%1'('lo.core$display$lo.comp.misc*pr^disp'(XLbl126, XThis126)), XLbl126, XThis126).
'lo.core$display$lo.comp.misc*pr@disp'(XD, XX1987, XLbV60, XThV60) :- XLbV60 = 'lo.core$display$lo.comp.misc*pr'(Xlo_core_display_x2, Xlo_core_display_y2),
    !,
    'lo.comp.misc@dispDf'(Xlo_core_display_x2, Xlo_core_display_y2, XD, XX1987).
'lo.core$display$lo.comp.misc*pr@disp'(_, _, _, _) :- raise_exception('error'("disp", 25, 5, 20)).
'lo.comp.misc@prEq'(Xlo_core_equality_x1, Xlo_core_equality_y1, 'lo.comp.misc#pr'(XX1, XY1), 'lo.comp.misc#pr'(XX2, XY2)) :- ocall('==%2'(XX1, XX2),Xlo_core_equality_x1,Xlo_core_equality_x1),
    ocall('==%2'(XY1, XY2),Xlo_core_equality_y1,Xlo_core_equality_y1).
'lo.comp.misc@prHash'(Xlo_core_equality_x2, Xlo_core_equality_y2, 'lo.comp.misc#pr'(XX, XY), XX2015) :- !,
    ocall('hash%2'(XX, XX2008),Xlo_core_equality_x2,Xlo_core_equality_x2),
    ocall('*%3'(37, XX2008, XX2010),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('hash%2'(XY, XX2013),Xlo_core_equality_y2,Xlo_core_equality_y2),
    ocall('+%3'(XX2010, XX2013, XX2015),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.misc@prHash'(_, _) :- raise_exception('error'("prHash", 41, 3, 39)).
'lo.core$equality$lo.comp.misc*pr'('lo.core$equality$lo.comp.misc*pr%1'('lo.core$equality$lo.comp.misc*pr')) :- !.
'lo.core$equality$lo.comp.misc*pr'('==%2'(XV594, XV595), XLbl127, XThis127) :- !,
    'lo.core$equality$lo.comp.misc*pr@=='(XV594, XV595, XLbl127, XThis127).
'lo.core$equality$lo.comp.misc*pr'('==%1'('lo.core$equality$lo.comp.misc*pr^=='(XLbl128, XThis128)), XLbl128, XThis128).
'lo.core$equality$lo.comp.misc*pr'('hash%2'(XV600, XV601), XLbl129, XThis129) :- !,
    'lo.core$equality$lo.comp.misc*pr@hash'(XV600, XV601, XLbl129, XThis129).
'lo.core$equality$lo.comp.misc*pr'('hash%1'('lo.core$equality$lo.comp.misc*pr^hash'(XLbl130, XThis130)), XLbl130, XThis130).
'lo.core$equality$lo.comp.misc*pr@=='(XX, XY, XLbV61, XThV61) :- XLbV61 = 'lo.core$equality$lo.comp.misc*pr'(Xlo_core_equality_x3, Xlo_core_equality_y3),
    'lo.comp.misc@prEq'(Xlo_core_equality_x3, Xlo_core_equality_y3, XX, XY).
'lo.core$equality$lo.comp.misc*pr@hash'(XD, XX2030, XLbV61, XThV61) :- XLbV61 = 'lo.core$equality$lo.comp.misc*pr'(Xlo_core_equality_x3, Xlo_core_equality_y3),
    !,
    'lo.comp.misc@prHash'(Xlo_core_equality_x3, Xlo_core_equality_y3, XD, XX2030).
'lo.core$equality$lo.comp.misc*pr@hash'(_, _, _, _) :- raise_exception('error'("hash", 34, 5, 20)).
'lo.comp.misc@replace'('lo.core#[]', X_43, Xr, 'lo.core#,..'(Xr, 'lo.core#[]')) :- !.
'lo.comp.misc@replace'('lo.core#,..'(Xe, Xl), Xe, Xr, 'lo.core#,..'(Xr, Xl)) :- !.
'lo.comp.misc@replace'('lo.core#,..'(Xe, Xl), Xt, Xr, 'lo.core#,..'(Xe, XX2056)) :- 'lo.comp.misc@neg1'(Xt, Xe),
    !,
    'lo.comp.misc@replace'(Xl, Xt, Xr, XX2056).
'lo.comp.misc@replace'(_, _, _, _) :- raise_exception('error'("replace", 44, 3, 22)).
'lo.comp.misc@collect'('lo.core#[]', X_44, 'lo.core#[]', 'lo.core#[]').
'lo.comp.misc@collect'('lo.core#,..'(Xe, Xl), Xp, 'lo.core#,..'(Xe, Xm), Xo) :- ocall('_call%1'(Xe),Xp,Xp),
    'lo.comp.misc@collect'(Xl, Xp, Xm, Xo).
'lo.comp.misc@collect'('lo.core#,..'(Xe, Xl), Xp, Xm, 'lo.core#,..'(Xe, Xo)) :- 'lo.comp.misc@neg2'(Xp, Xe),
    'lo.comp.misc@collect'(Xl, Xp, Xm, Xo).
'lo.comp.misc@filter'(XIx, XIx, XL) :- 'lo.comp.misc@forallA1'(XIx, XX2098, XL, XE).
'lo.comp.misc@filter'(XIx, XNx, XL) :- ocall('+%3'(XIx, 2, XX2104),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.misc@filter'(XX2104, XNx, XL).
'lo.comp.misc@sieve'(XCurr, XMax, XSoFar, XSoFar) :- 'lo.core@>'('lo.core$comp$lo.core*integer', XCurr, XMax),
    !.
'lo.comp.misc@sieve'(XCurr, XMax, XSoFar, XX2128) :- ocall('+%3'(XCurr, 2, XX2119),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.misc@filter'(XX2119, XNxt, XSoFar),
    !,
    'lo.comp.misc@sieve'(XNxt, XMax, 'lo.core#,..'(XNxt, XSoFar), XX2128).
'lo.comp.misc@sieve'(_, _, _, _) :- raise_exception('error'("sieve", 58, 3, 42)).
'lo.comp.misc@nextPrime'(XFrom, XX2134) :- !,
    'lo.comp.misc@sieve'(3, XFrom, 'lo.core#,..'(3, 'lo.core#[]'), XX2133),
    'lo.list@head'(XX2133, XX2134).
'lo.comp.misc@nextPrime'(_, _) :- raise_exception('error'("nextPrime", 55, 3, 42)).
'lo.comp.misc^splitString'('_call%4'(XV567, XV568, XV569, XV570), 'lo.comp.misc^splitString', _) :- 'lo.comp.misc@splitString'(XV567, XV568, XV569, XV570).
'lo.comp.misc^pathSuffix'('_call%3'(XV571, XV572, XV573), 'lo.comp.misc^pathSuffix', _) :- 'lo.comp.misc@pathSuffix'(XV571, XV572, XV573).
'lo.comp.misc^subPath'('_call%4'(XV574, XV575, XV576, XV577), 'lo.comp.misc^subPath', _) :- 'lo.comp.misc@subPath'(XV574, XV575, XV576, XV577).
'lo.comp.misc^starts_with'('_call%2'(XV578, XV579), 'lo.comp.misc^starts_with', _) :- 'lo.comp.misc@starts_with'(XV578, XV579).
'lo.comp.misc^dispDf'('_call%2'(XV580, XV581), 'lo.comp.misc^dispDf', _) :- 'lo.comp.misc@dispDf'(XV580, XV581).
'lo.core$display$lo.comp.misc*pr^disp'('_call%2'(XV582, XV583), 'lo.core$display$lo.comp.misc*pr^disp'(XLbV60, XThV60), _) :- 'lo.core$display$lo.comp.misc*pr@disp'(XV582, XV583, XLbV60, XThV60).
'lo.core$display$lo.comp.misc*pr^disp'('_call%2'(XV586, XV587), 'lo.core$display$lo.comp.misc*pr^disp'(XLbV60, XThV60), _) :- 'lo.core$display$lo.comp.misc*pr@disp'(XV586, XV587, XLbV60, XThV60).
'lo.comp.misc^prEq'('_call%2'(XV588, XV589), 'lo.comp.misc^prEq', _) :- 'lo.comp.misc@prEq'(XV588, XV589).
'lo.comp.misc^prHash'('_call%2'(XV590, XV591), 'lo.comp.misc^prHash', _) :- 'lo.comp.misc@prHash'(XV590, XV591).
'lo.core$equality$lo.comp.misc*pr^=='('_call%2'(XV592, XV593), 'lo.core$equality$lo.comp.misc*pr^=='(XLbV61, XThV61), _) :- 'lo.core$equality$lo.comp.misc*pr@=='(XV592, XV593, XLbV61, XThV61).
'lo.core$equality$lo.comp.misc*pr^=='('_call%2'(XV596, XV597), 'lo.core$equality$lo.comp.misc*pr^=='(XLbV61, XThV61), _) :- 'lo.core$equality$lo.comp.misc*pr@=='(XV596, XV597, XLbV61, XThV61).
'lo.core$equality$lo.comp.misc*pr^hash'('_call%2'(XV598, XV599), 'lo.core$equality$lo.comp.misc*pr^hash'(XLbV61, XThV61), _) :- 'lo.core$equality$lo.comp.misc*pr@hash'(XV598, XV599, XLbV61, XThV61).
'lo.core$equality$lo.comp.misc*pr^hash'('_call%2'(XV602, XV603), 'lo.core$equality$lo.comp.misc*pr^hash'(XLbV61, XThV61), _) :- 'lo.core$equality$lo.comp.misc*pr@hash'(XV602, XV603, XLbV61, XThV61).
'lo.comp.misc@neg1'(Xt, Xe) :- Xe = Xt,
    !,
    fail.
'lo.comp.misc@neg1'(Xt, Xe).
'lo.comp.misc^replace'('_call%4'(XV604, XV605, XV606, XV607), 'lo.comp.misc^replace', _) :- 'lo.comp.misc@replace'(XV604, XV605, XV606, XV607).
'lo.comp.misc@neg2'(Xp, Xe) :- ocall('_call%1'(Xe),Xp,Xp),
    !,
    fail.
'lo.comp.misc@neg2'(Xp, Xe).
'lo.comp.misc^collect'('_call%4'(XV608, XV609, XV610, XV611), 'lo.comp.misc^collect', _) :- 'lo.comp.misc@collect'(XV608, XV609, XV610, XV611).
'lo.comp.misc@neg3'(XX2098, XIx, XE) :- ocall('%%3'(XE, XIx, XX2098),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    XX2098 = 0,
    !,
    fail.
'lo.comp.misc@neg3'(XX2098, XIx, XE).
'lo.comp.misc@forallA1'(XIx, XX2098, XL, XE) :- ocall('in%2'(XE, XL),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.misc@forallB1'(XIx, XX2098, XL, XE),
    !,
    fail.
'lo.comp.misc@forallA1'(XIx, XX2098, XL, XE).
'lo.comp.misc@forallB1'(XIx, XX2098, XL, XE) :- 'lo.comp.misc@neg3'(XX2098, XIx, XE),
    !,
    fail.
'lo.comp.misc@forallB1'(XIx, XX2098, XL, XE).
'lo.comp.misc^filter'('_call%3'(XV612, XV613, XV614), 'lo.comp.misc^filter', _) :- 'lo.comp.misc@filter'(XV612, XV613, XV614).
'lo.comp.misc^sieve'('_call%4'(XV615, XV616, XV617, XV618), 'lo.comp.misc^sieve', _) :- 'lo.comp.misc@sieve'(XV615, XV616, XV617, XV618).
'lo.comp.misc^nextPrime'('_call%2'(XV619, XV620), 'lo.comp.misc^nextPrime', _) :- 'lo.comp.misc@nextPrime'(XV619, XV620).
