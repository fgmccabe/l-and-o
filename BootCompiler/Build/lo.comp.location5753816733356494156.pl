'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.location'e'*'n12o12'()12'n2o2'import'e'private'n2o2'pkg's'lo.json'e'*'n2o2'import'e'private'n2o2'pkg's'lo.array'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I7'std't'lo.comp.location*location''loc'CT5iiiiSt'lo.comp.location*location''lineOf'FT1t'lo.comp.location*location'i'columnOf'FT1t'lo.comp.location*location'i'widthOf'FT1t'lo.comp.location*location'i'locationContext'FT2t'lo.comp.location*location'St'lo.core*ss''collectSrc'FT3LiiUz1'lo.array*array'1SUz1'lo.array*array'1S\"s\"I2'location'Yt'lo.comp.location*location'I1'merge'FT1t'lo.comp.location*location't'lo.comp.location*location''hasLoc'Yt'lo.comp.location*hasLoc'I1'loc't'lo.comp.location*location'\"n1o1'()1's'std'n0o0'()0'n3o3'()3'n2o2'()2's'lo.core$display$lo.comp.location*location's\"c'lo.core$display'T1t'lo.comp.location*location'T0\"n2o2'()2's'lo.core$sizeable$lo.comp.location*location's\"c'lo.core$sizeable'T1t'lo.comp.location*location'T0\"n2o2'()2's'lo.coerce$coercion$lo.comp.location*location$lo.json*json's\"c'lo.coerce$coercion'T2t'lo.comp.location*location't'lo.json*json'T0\"").
'lo.comp.location@init'() :- !.
'lo.comp.location#std'('merge%2'(XV509, XV510), XLbl113, XThis113) :- !,
    'lo.comp.location#std@merge'(XV509, XV510, XLbl113, XThis113).
'lo.comp.location#std'('merge%1'('lo.comp.location#std^merge'(XLbl114, XThis114)), XLbl114, XThis114).
'lo.comp.location#std@merge'(XLc, XLc, XLbV54, XThV54) :- !.
'lo.comp.location#std@merge'(_, _, _, _) :- raise_exception('error'("merge", 13, 5, 15)).
'lo.comp.location#loc'('loc%1'('lo.comp.location@loc'())) :- !.
'lo.comp.location#loc'('merge%2'(XV515, XV516), XLbl115, XThis115) :- !,
    'lo.comp.location#loc@merge'(XV515, XV516, XLbl115, XThis115).
'lo.comp.location#loc'('merge%1'('lo.comp.location#loc^merge'(XLbl116, XThis116)), XLbl116, XThis116).
'lo.comp.location#loc@merge'('lo.comp.location#loc'(X_26, XOO, XC, XL, X_27), 'lo.comp.location#loc'(XLine, XOff, XCol, XX1692, XPth), XLbV55, XThV55) :- XLbV55 = 'lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth),
    !,
    ocall('-%3'(XOO, XOff, XX1689),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('+%3'(XX1689, XL, XX1692),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.location#loc@merge'(_, _, _, _) :- raise_exception('error'("merge", 18, 5, 56)).
'lo.comp.location@locDisp'('lo.comp.location#std', 'lo.core#ss'("<standard>")) :- !.
'lo.comp.location@locDisp'('lo.comp.location#loc'(XLn, XOff, XCol, XLen, XPth), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XPth), 'lo.core#,..'('lo.core#ss'("@"), 'lo.core#,..'(XX1708, 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XX1712, 'lo.core#,..'('lo.core#ss'("("), 'lo.core#,..'(XX1716, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]')))))))))) :- !,
    ocall('disp%2'(XLn, XX1708),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('disp%2'(XCol, XX1712),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('disp%2'(XLen, XX1716),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer').
'lo.comp.location@locDisp'(_, _) :- raise_exception('error'("locDisp", 27, 3, 32)).
'lo.core$display$lo.comp.location*location'('lo.core$display$lo.comp.location*location%1'('lo.core$display$lo.comp.location*location')) :- !.
'lo.core$display$lo.comp.location*location'('disp%2'(XV523, XV524), XLbl117, XThis117) :- !,
    'lo.core$display$lo.comp.location*location@disp'(XV523, XV524, XLbl117, XThis117).
'lo.core$display$lo.comp.location*location'('disp%1'('lo.core$display$lo.comp.location*location^disp'(XLbl118, XThis118)), XLbl118, XThis118).
'lo.core$display$lo.comp.location*location@disp'(XL, XX1731, XLbV56, XThV56) :- !,
    'lo.comp.location@locDisp'(XL, XX1731).
'lo.core$display$lo.comp.location*location@disp'(_, _, _, _) :- raise_exception('error'("disp", 23, 5, 21)).
'lo.comp.location@lineOf'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), XLine) :- !.
'lo.comp.location@lineOf'(_, _) :- raise_exception('error'("lineOf", 32, 3, 41)).
'lo.comp.location@columnOf'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), XCol) :- !.
'lo.comp.location@columnOf'(_, _) :- raise_exception('error'("columnOf", 35, 3, 42)).
'lo.comp.location@widthOf'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), XLen) :- !.
'lo.comp.location@widthOf'(_, _) :- raise_exception('error'("widthOf", 38, 3, 41)).
'lo.core$sizeable$lo.comp.location*location'('lo.core$sizeable$lo.comp.location*location%1'('lo.core$sizeable$lo.comp.location*location')) :- !.
'lo.core$sizeable$lo.comp.location*location'('size%2'(XV535, XV536), XLbl119, XThis119) :- !,
    'lo.core$sizeable$lo.comp.location*location@size'(XV535, XV536, XLbl119, XThis119).
'lo.core$sizeable$lo.comp.location*location'('size%1'('lo.core$sizeable$lo.comp.location*location^size'(XLbl120, XThis120)), XLbl120, XThis120).
'lo.core$sizeable$lo.comp.location*location'('isEmpty%1'(XV540), XLbl121, XThis121) :- !,
    'lo.core$sizeable$lo.comp.location*location@isEmpty'(XV540, XLbl121, XThis121).
'lo.core$sizeable$lo.comp.location*location'('isEmpty%1'('lo.core$sizeable$lo.comp.location*location^isEmpty'(XLbl122, XThis122)), XLbl122, XThis122).
'lo.core$sizeable$lo.comp.location*location@size'('lo.comp.location#loc'(X_28, X_29, X_30, XLen, X_31), XLen, XLbV57, XThV57) :- !.
'lo.core$sizeable$lo.comp.location*location@size'('lo.comp.location#std', 0, XLbV57, XThV57) :- !.
'lo.core$sizeable$lo.comp.location*location@size'(_, _, _, _) :- raise_exception('error'("size", 41, 5, 29)).
'lo.core$sizeable$lo.comp.location*location@isEmpty'('lo.comp.location#std', XLbV57, XThV57).
'lo.comp.location@loc2json'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), 'lo.json#jColl'(XX1800)) :- !,
    ocall('_empty%1'(XXV8),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%2'(XLen, XX1771),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_put%4'(XXV8, "length", 'lo.json#jNum'(XX1771), XX1774),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%2'(XOff, XX1778),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_put%4'(XX1774, "start", 'lo.json#jNum'(XX1778), XX1781),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%2'(XCol, XX1785),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_put%4'(XX1781, "column", 'lo.json#jNum'(XX1785), XX1788),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%2'(XLine, XX1792),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_put%4'(XX1788, "line", 'lo.json#jNum'(XX1792), XX1795),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XX1795, "path", 'lo.json#jTxt'(XPth), XX1800),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.location@loc2json'('lo.comp.location#std', 'lo.json#jNull') :- !.
'lo.comp.location@loc2json'(_, _) :- raise_exception('error'("loc2json", 52, 3, 223)).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json'('lo.coerce$coercion$lo.comp.location*location$lo.json*json%1'('lo.coerce$coercion$lo.comp.location*location$lo.json*json')) :- !.
'lo.coerce$coercion$lo.comp.location*location$lo.json*json'('_coerce%2'(XV546, XV547), XLbl123, XThis123) :- !,
    'lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce'(XV546, XV547, XLbl123, XThis123).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.comp.location*location$lo.json*json^_coerce'(XLbl124, XThis124)), XLbl124, XThis124).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce'(Xlc, XX1808, XLbV58, XThV58) :- !,
    'lo.comp.location@loc2json'(Xlc, XX1808).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 48, 5, 27)).
'lo.comp.location@showChars'(0, X_32, XTl, XTl) :- !.
'lo.comp.location@showChars'(XN, XC, XTl, 'lo.core#,..'(XC, XX1821)) :- !,
    ocall('-%3'(XN, 1, XX1817),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.location@showChars'(XX1817, XC, XTl, XX1821).
'lo.comp.location@showChars'(_, _, _, _) :- raise_exception('error'("showChars", 70, 3, 23)).
'lo.comp.location@showArrow'(XCol, XLen, XX1830) :- !,
    'lo.comp.location@showChars'(XLen, 94, 'lo.core#,..'(10, 'lo.core#[]'), XX1829),
    'lo.comp.location@showChars'(XCol, 32, XX1829, XX1830).
'lo.comp.location@showArrow'(_, _, _) :- raise_exception('error'("showArrow", 66, 3, 70)).
'lo.comp.location@locationContext'('lo.comp.location#loc'(X_33, X_34, XCol, XLen, X_35), XLine, 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XLine), 'lo.core#,..'('lo.core#ss'(XX1853), 'lo.core#[]')))) :- !,
    ocall('-%3'(XCol, 1, XX1841),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('size%2'(XLine, XX1846),'lo.core$sizeable$lo.core*string','lo.core$sizeable$lo.core*string'),
    ocall('-%3'(XX1846, XCol, XX1849),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.core@min'('lo.core$comp$lo.core*integer', XLen, XX1849, XX1851),
    'lo.comp.location@showArrow'(XX1841, XX1851, XX1852),
    'implode'(XX1852, XX1853).
'lo.comp.location@locationContext'(_, _, _) :- raise_exception('error'("locationContext", 63, 3, 115)).
'lo.comp.location@findLine'('lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.comp.location@findLine'('lo.core#,..'(10, XL), 'lo.core#,..'(10, 'lo.core#[]'), XL).
'lo.comp.location@findLine'('lo.core#,..'(XCh, XL), 'lo.core#,..'(XCh, XLn), XRest) :- 'lo.comp.location@findLine'(XL, XLn, XRest).
'lo.comp.location@collectSrc'('lo.core#[]', X_36, XA, XA) :- !.
'lo.comp.location@collectSrc'(XCodes, XIx, XA, XX1897) :- 'lo.comp.location@findLine'(XCodes, XLn, XRest),
    !,
    ocall('+%3'(XIx, 1, XX1889),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'implode'(XLn, XX1894),
    ocall('_put%4'(XA, XIx, XX1894, XX1895),'lo.collection$map$lo.array*array','lo.collection$map$lo.array*array'),
    'lo.comp.location@collectSrc'(XRest, XX1889, XX1895, XX1897).
'lo.comp.location@collectSrc'(_, _, _, _) :- raise_exception('error'("collectSrc", 75, 3, 23)).
'lo.comp.location#std^merge'('_call%2'(XV507, XV508), 'lo.comp.location#std^merge'(XLbV54, XThV54), _) :- 'lo.comp.location#std@merge'(XV507, XV508, XLbV54, XThV54).
'lo.comp.location#std^merge'('_call%2'(XV511, XV512), 'lo.comp.location#std^merge'(XLbV54, XThV54), _) :- 'lo.comp.location#std@merge'(XV511, XV512, XLbV54, XThV54).
'lo.comp.location@std'('lo.comp.location#std') :- !.
'lo.comp.location#loc^merge'('_call%2'(XV513, XV514), 'lo.comp.location#loc^merge'(XLbV55, XThV55), _) :- 'lo.comp.location#loc@merge'(XV513, XV514, XLbV55, XThV55).
'lo.comp.location#loc^merge'('_call%2'(XV517, XV518), 'lo.comp.location#loc^merge'(XLbV55, XThV55), _) :- 'lo.comp.location#loc@merge'(XV517, XV518, XLbV55, XThV55).
'lo.comp.location^locDisp'('_call%2'(XV519, XV520), 'lo.comp.location^locDisp', _) :- 'lo.comp.location@locDisp'(XV519, XV520).
'lo.core$display$lo.comp.location*location^disp'('_call%2'(XV521, XV522), 'lo.core$display$lo.comp.location*location^disp'(XLbV56, XThV56), _) :- 'lo.core$display$lo.comp.location*location@disp'(XV521, XV522, XLbV56, XThV56).
'lo.core$display$lo.comp.location*location^disp'('_call%2'(XV525, XV526), 'lo.core$display$lo.comp.location*location^disp'(XLbV56, XThV56), _) :- 'lo.core$display$lo.comp.location*location@disp'(XV525, XV526, XLbV56, XThV56).
'lo.comp.location^lineOf'('_call%2'(XV527, XV528), 'lo.comp.location^lineOf', _) :- 'lo.comp.location@lineOf'(XV527, XV528).
'lo.comp.location^columnOf'('_call%2'(XV529, XV530), 'lo.comp.location^columnOf', _) :- 'lo.comp.location@columnOf'(XV529, XV530).
'lo.comp.location^widthOf'('_call%2'(XV531, XV532), 'lo.comp.location^widthOf', _) :- 'lo.comp.location@widthOf'(XV531, XV532).
'lo.core$sizeable$lo.comp.location*location^size'('_call%2'(XV533, XV534), 'lo.core$sizeable$lo.comp.location*location^size'(XLbV57, XThV57), _) :- 'lo.core$sizeable$lo.comp.location*location@size'(XV533, XV534, XLbV57, XThV57).
'lo.core$sizeable$lo.comp.location*location^size'('_call%2'(XV537, XV538), 'lo.core$sizeable$lo.comp.location*location^size'(XLbV57, XThV57), _) :- 'lo.core$sizeable$lo.comp.location*location@size'(XV537, XV538, XLbV57, XThV57).
'lo.core$sizeable$lo.comp.location*location^isEmpty'('_call%1'(XV539), 'lo.core$sizeable$lo.comp.location*location^isEmpty'(XLbV57, XThV57), _) :- 'lo.core$sizeable$lo.comp.location*location@isEmpty'(XV539, XLbV57, XThV57).
'lo.core$sizeable$lo.comp.location*location^isEmpty'('_call%1'(XV541), 'lo.core$sizeable$lo.comp.location*location^isEmpty'(XLbV57, XThV57), _) :- 'lo.core$sizeable$lo.comp.location*location@isEmpty'(XV541, XLbV57, XThV57).
'lo.comp.location^loc2json'('_call%2'(XV542, XV543), 'lo.comp.location^loc2json', _) :- 'lo.comp.location@loc2json'(XV542, XV543).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json^_coerce'('_call%2'(XV544, XV545), 'lo.coerce$coercion$lo.comp.location*location$lo.json*json^_coerce'(XLbV58, XThV58), _) :- 'lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce'(XV544, XV545, XLbV58, XThV58).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json^_coerce'('_call%2'(XV548, XV549), 'lo.coerce$coercion$lo.comp.location*location$lo.json*json^_coerce'(XLbV58, XThV58), _) :- 'lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce'(XV548, XV549, XLbV58, XThV58).
'lo.comp.location^showChars'('_call%4'(XV550, XV551, XV552, XV553), 'lo.comp.location^showChars', _) :- 'lo.comp.location@showChars'(XV550, XV551, XV552, XV553).
'lo.comp.location^showArrow'('_call%3'(XV554, XV555, XV556), 'lo.comp.location^showArrow', _) :- 'lo.comp.location@showArrow'(XV554, XV555, XV556).
'lo.comp.location^locationContext'('_call%3'(XV557, XV558, XV559), 'lo.comp.location^locationContext', _) :- 'lo.comp.location@locationContext'(XV557, XV558, XV559).
'lo.comp.location^findLine'('_call%3'(XV560, XV561, XV562), 'lo.comp.location^findLine', _) :- 'lo.comp.location@findLine'(XV560, XV561, XV562).
'lo.comp.location^collectSrc'('_call%4'(XV563, XV564, XV565, XV566), 'lo.comp.location^collectSrc', _) :- 'lo.comp.location@collectSrc'(XV563, XV564, XV565, XV566).
