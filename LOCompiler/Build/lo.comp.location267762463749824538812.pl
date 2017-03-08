'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.location's'0.0.1'n3o3'()3'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.array'e'*'n2o2'import'e'private'n2o2'pkg's'lo.json'e'*'s\"I7'std't'lo.comp.location*location''loc'CT5iiiiSt'lo.comp.location*location''lineOf'FT1t'lo.comp.location*location'i'columnOf'FT1t'lo.comp.location*location'i'widthOf'FT1t'lo.comp.location*location'i'locationContext'FT2t'lo.comp.location*location'St'lo.core*ss''collectSrc'FT3LiiUz1'lo.array*array'1SUz1'lo.array*array'1S\"s\"I2'location'Yt'lo.comp.location*location'I1'merge'FT1t'lo.comp.location*location't'lo.comp.location*location''hasLoc'Yt'lo.comp.location*hasLoc'I1'loc't'lo.comp.location*location'\"n2o2'()2's'std's'loc'n0o0'()0'n3o3'()3'n2o2'()2's'lo.core$display$lo.comp.location*location's\"c'lo.core$display'T1t'lo.comp.location*location'T0\"n2o2'()2's'lo.core$sizeable$lo.comp.location*location's\"c'lo.core$sizeable'T1t'lo.comp.location*location'T0\"n2o2'()2's'lo.coerce$coercion$lo.comp.location*location$lo.json*json's\"c'lo.coerce$coercion'T2t'lo.comp.location*location't'lo.json*json'T0\"").
'lo.comp.location@init'():- !.
'lo.comp.location#std'('std%1'('lo.comp.location@std')):- !.
'lo.comp.location#std'('merge%2'(XV28709, XV28710), XLbl2052, XThis2052):- !,
    'lo.comp.location#std@merge'(XV28709, XV28710, XLbl2052, XThis2052).
'lo.comp.location#std'('merge%1'('lo.comp.location#std^merge'(XLbl2053, XThis2053)), XLbl2053, XThis2053).
'lo.comp.location#std@merge'(XLc, XLc, XLbV2227, XThV2227):- !.
'lo.comp.location#std@merge'(_, _):- raise_exception('error'("lo.comp.location#std@merge", 13, 5, 15)).
'lo.comp.location#loc'('loc%1'('lo.comp.location@loc'())):- !.
'lo.comp.location#loc'('merge%2'(XV28713, XV28714), XLbl2054, XThis2054):- !,
    'lo.comp.location#loc@merge'(XV28713, XV28714, XLbl2054, XThis2054).
'lo.comp.location#loc'('merge%1'('lo.comp.location#loc^merge'(XLbl2055, XThis2055)), XLbl2055, XThis2055).
'lo.comp.location#loc@merge'('lo.comp.location#loc'(X_30972, XOO, XC, XL, X_30973), 'lo.comp.location#loc'(XLine, XOff, XCol, XXe4397, XPth), XLbV2228, XThV2228):- XLbV2228 = 'lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth),
    !,
    ocall('-%1'(XXV4734),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('+%1'(XXV4735),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XOO, XOff, XXe4396),XXV4734,XXV4734),
    ocall('_call%3'(XXe4396, XL, XXe4397),XXV4735,XXV4735).
'lo.comp.location#loc@merge'(_, _):- raise_exception('error'("lo.comp.location#loc@merge", 18, 5, 56)).
'lo.comp.location@locDisp'('lo.comp.location#std', 'lo.core#ss'("<standard>")):- !.
'lo.comp.location@locDisp'('lo.comp.location#loc'(XLn, XOff, XCol, XLen, XPth), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XPth), 'lo.core#,..'('lo.core#ss'("@"), 'lo.core#,..'(XXe4398, 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXe4399, 'lo.core#,..'('lo.core#ss'("("), 'lo.core#,..'(XXe4400, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]')))))))))):- !,
    ocall('disp%1'(XXV4736),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('disp%1'(XXV4737),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('disp%1'(XXV4738),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('_call%2'(XLn, XXe4398),XXV4736,XXV4736),
    ocall('_call%2'(XCol, XXe4399),XXV4737,XXV4737),
    ocall('_call%2'(XLen, XXe4400),XXV4738,XXV4738).
'lo.comp.location@locDisp'(_, _):- raise_exception('error'("lo.comp.location@locDisp", 27, 3, 32)).
'lo.core$display$lo.comp.location*location'('lo.core$display$lo.comp.location*location%1'('lo.core$display$lo.comp.location*location')):- !.
'lo.core$display$lo.comp.location*location'('disp%2'(XV28719, XV28720), XLbl2056, XThis2056):- !,
    'lo.core$display$lo.comp.location*location@disp'(XV28719, XV28720, XLbl2056, XThis2056).
'lo.core$display$lo.comp.location*location'('disp%1'('lo.core$display$lo.comp.location*location^disp'(XLbl2057, XThis2057)), XLbl2057, XThis2057).
'lo.core$display$lo.comp.location*location@disp'(XL, XXd35815, XLbV2229, XThV2229):- !,
    'lo.comp.location@locDisp'(XL, XXd35815).
'lo.core$display$lo.comp.location*location@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.location*location@disp", 23, 5, 21)).
'lo.comp.location@lineOf'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), XLine):- !.
'lo.comp.location@lineOf'(_, _):- raise_exception('error'("lo.comp.location@lineOf", 32, 3, 41)).
'lo.comp.location@columnOf'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), XCol):- !.
'lo.comp.location@columnOf'(_, _):- raise_exception('error'("lo.comp.location@columnOf", 35, 3, 42)).
'lo.comp.location@widthOf'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), XLen):- !.
'lo.comp.location@widthOf'(_, _):- raise_exception('error'("lo.comp.location@widthOf", 38, 3, 41)).
'lo.core$sizeable$lo.comp.location*location'('lo.core$sizeable$lo.comp.location*location%1'('lo.core$sizeable$lo.comp.location*location')):- !.
'lo.core$sizeable$lo.comp.location*location'('size%2'(XV28729, XV28730), XLbl2058, XThis2058):- !,
    'lo.core$sizeable$lo.comp.location*location@size'(XV28729, XV28730, XLbl2058, XThis2058).
'lo.core$sizeable$lo.comp.location*location'('size%1'('lo.core$sizeable$lo.comp.location*location^size'(XLbl2059, XThis2059)), XLbl2059, XThis2059).
'lo.core$sizeable$lo.comp.location*location'('isEmpty%1'(XV28734), XLbl2060, XThis2060):- !,
    'lo.core$sizeable$lo.comp.location*location@isEmpty'(XV28734, XLbl2060, XThis2060).
'lo.core$sizeable$lo.comp.location*location'('isEmpty%1'('lo.core$sizeable$lo.comp.location*location^isEmpty'(XLbl2061, XThis2061)), XLbl2061, XThis2061).
'lo.core$sizeable$lo.comp.location*location@size'('lo.comp.location#loc'(X_30982, X_30983, X_30984, XLen, X_30985), XLen, XLbV2230, XThV2230):- !.
'lo.core$sizeable$lo.comp.location*location@size'('lo.comp.location#std', 0, XLbV2230, XThV2230):- !.
'lo.core$sizeable$lo.comp.location*location@size'(_, _):- raise_exception('error'("lo.core$sizeable$lo.comp.location*location@size", 41, 5, 29)).
'lo.core$sizeable$lo.comp.location*location@isEmpty'('lo.comp.location#std', XLbV2230, XThV2230).
'lo.comp.location@loc2json'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), 'lo.json#jColl'(XXe4409)):- !,
    ocall('_coerce%1'(XXV4740),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_put%1'(XXV4741),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%1'(XXV4742),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_put%1'(XXV4743),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%1'(XXV4744),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_put%1'(XXV4745),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%1'(XXV4746),'lo.coerce$coercion$lo.core*integer$lo.core*float','lo.coerce$coercion$lo.core*integer$lo.core*float'),
    ocall('_put%1'(XXV4747),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%1'(XXV4748),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV4739),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%2'(XLen, XXe4401),XXV4740,XXV4740),
    ocall('_call%4'(XXV4739, "length", 'lo.json#jNum'(XXe4401), XXe4402),XXV4741,XXV4741),
    ocall('_call%2'(XOff, XXe4403),XXV4742,XXV4742),
    ocall('_call%4'(XXe4402, "start", 'lo.json#jNum'(XXe4403), XXe4404),XXV4743,XXV4743),
    ocall('_call%2'(XCol, XXe4405),XXV4744,XXV4744),
    ocall('_call%4'(XXe4404, "column", 'lo.json#jNum'(XXe4405), XXe4406),XXV4745,XXV4745),
    ocall('_call%2'(XLine, XXe4407),XXV4746,XXV4746),
    ocall('_call%4'(XXe4406, "line", 'lo.json#jNum'(XXe4407), XXe4408),XXV4747,XXV4747),
    ocall('_call%4'(XXe4408, "path", 'lo.json#jTxt'(XPth), XXe4409),XXV4748,XXV4748).
'lo.comp.location@loc2json'('lo.comp.location#std', 'lo.json#jNull'):- !.
'lo.comp.location@loc2json'(_, _):- raise_exception('error'("lo.comp.location@loc2json", 52, 3, 223)).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json'('lo.coerce$coercion$lo.comp.location*location$lo.json*json%1'('lo.coerce$coercion$lo.comp.location*location$lo.json*json')):- !.
'lo.coerce$coercion$lo.comp.location*location$lo.json*json'('_coerce%2'(XV28739, XV28740), XLbl2062, XThis2062):- !,
    'lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce'(XV28739, XV28740, XLbl2062, XThis2062).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.comp.location*location$lo.json*json^_coerce'(XLbl2063, XThis2063)), XLbl2063, XThis2063).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce'(Xlc, XXd35828, XLbV2231, XThV2231):- !,
    'lo.comp.location@loc2json'(Xlc, XXd35828).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce", 48, 5, 27)).
'lo.comp.location@showChars'(0, X_30986, XTl, XTl):- !.
'lo.comp.location@showChars'(XN, XC, XTl, 'lo.core#,..'(XC, XXd35829)):- !,
    ocall('-%1'(XXV4749),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XN, 1, XXe4410),XXV4749,XXV4749),
    'lo.comp.location@showChars'(XXe4410, XC, XTl, XXd35829).
'lo.comp.location@showChars'(_, _, _, _):- raise_exception('error'("lo.comp.location@showChars", 70, 3, 23)).
'lo.comp.location@showArrow'(XCol, XLen, XXd35833):- !,
    'lo.comp.location@showChars'(XLen, 94, 'lo.core#,..'(10, 'lo.core#[]'), XXd35832),
    'lo.comp.location@showChars'(XCol, 32, XXd35832, XXd35833).
'lo.comp.location@showArrow'(_, _, _):- raise_exception('error'("lo.comp.location@showArrow", 66, 3, 70)).
'lo.comp.location@locationContext'('lo.comp.location#loc'(X_30989, X_30990, XCol, XLen, X_30991), XLine, 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XLine), 'lo.core#,..'('lo.core#ss'(XXc503), 'lo.core#[]')))):- !,
    ocall('-%1'(XXV4750),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('size%1'(XXV4751),'lo.core$sizeable$lo.core*string','lo.core$sizeable$lo.core*string'),
    ocall('-%1'(XXV4752),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XCol, 1, XXe4411),XXV4750,XXV4750),
    ocall('_call%2'(XLine, XXe4412),XXV4751,XXV4751),
    ocall('_call%3'(XXe4412, XCol, XXe4413),XXV4752,XXV4752),
    'lo.core@min'('lo.core$comp$lo.core*integer', XLen, XXe4413, XXd35835),
    'lo.comp.location@showArrow'(XXe4411, XXd35835, XXd35836),
    'implode'(XXd35836, XXc503).
'lo.comp.location@locationContext'(_, _, _):- raise_exception('error'("lo.comp.location@locationContext", 63, 3, 115)).
'lo.comp.location@findLine'('lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.comp.location@findLine'('lo.core#,..'(10, XL), 'lo.core#,..'(10, 'lo.core#[]'), XL).
'lo.comp.location@findLine'('lo.core#,..'(XCh, XL), 'lo.core#,..'(XCh, XLn), XRest):- 'lo.comp.location@findLine'(XL, XLn, XRest).
'lo.comp.location@collectSrc'('lo.core#[]', X_30998, XA, XA):- !.
'lo.comp.location@collectSrc'(XCodes, XIx, XA, XXd35841):- 'lo.comp.location@findLine'(XCodes, XLn, XRest),
    !,
    ocall('+%1'(XXV4753),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_put%1'(XXV4754),'lo.collection$map$lo.array*array','lo.collection$map$lo.array*array'),
    ocall('_call%3'(XIx, 1, XXe4414),XXV4753,XXV4753),
    'implode'(XLn, XXc504),
    ocall('_call%4'(XA, XIx, XXc504, XXe4415),XXV4754,XXV4754),
    'lo.comp.location@collectSrc'(XRest, XXe4414, XXe4415, XXd35841).
'lo.comp.location@collectSrc'(_, _, _, _):- raise_exception('error'("lo.comp.location@collectSrc", 75, 3, 23)).
'lo.comp.location#std^merge'('_call%2'(XV28707, XV28708), 'lo.comp.location#std^merge'(XLbV2227, XThV2227), _):- 'lo.comp.location#std@merge'(XV28707, XV28708, XLbV2227, XThV2227).
'lo.comp.location#loc^merge'('_call%2'(XV28711, XV28712), 'lo.comp.location#loc^merge'(XLbV2228, XThV2228), _):- 'lo.comp.location#loc@merge'(XV28711, XV28712, XLbV2228, XThV2228).
'lo.comp.location^locDisp'('_call%2'(XV28715, XV28716), 'lo.comp.location^locDisp', _):- 'lo.comp.location@locDisp'(XV28715, XV28716).
'lo.core$display$lo.comp.location*location^disp'('_call%2'(XV28717, XV28718), 'lo.core$display$lo.comp.location*location^disp'(XLbV2229, XThV2229), _):- 'lo.core$display$lo.comp.location*location@disp'(XV28717, XV28718, XLbV2229, XThV2229).
'lo.comp.location^lineOf'('_call%2'(XV28721, XV28722), 'lo.comp.location^lineOf', _):- 'lo.comp.location@lineOf'(XV28721, XV28722).
'lo.comp.location^columnOf'('_call%2'(XV28723, XV28724), 'lo.comp.location^columnOf', _):- 'lo.comp.location@columnOf'(XV28723, XV28724).
'lo.comp.location^widthOf'('_call%2'(XV28725, XV28726), 'lo.comp.location^widthOf', _):- 'lo.comp.location@widthOf'(XV28725, XV28726).
'lo.core$sizeable$lo.comp.location*location^size'('_call%2'(XV28727, XV28728), 'lo.core$sizeable$lo.comp.location*location^size'(XLbV2230, XThV2230), _):- 'lo.core$sizeable$lo.comp.location*location@size'(XV28727, XV28728, XLbV2230, XThV2230).
'lo.core$sizeable$lo.comp.location*location^isEmpty'('_call%3'(XV28731, XV28732, XV28733), 'lo.core$sizeable$lo.comp.location*location^isEmpty'(XLbV2230, XThV2230), _):- 'lo.core$sizeable$lo.comp.location*location@isEmpty'(XV28731, XV28732, XV28733, XLbV2230, XThV2230).
'lo.comp.location^loc2json'('_call%2'(XV28735, XV28736), 'lo.comp.location^loc2json', _):- 'lo.comp.location@loc2json'(XV28735, XV28736).
'lo.coerce$coercion$lo.comp.location*location$lo.json*json^_coerce'('_call%2'(XV28737, XV28738), 'lo.coerce$coercion$lo.comp.location*location$lo.json*json^_coerce'(XLbV2231, XThV2231), _):- 'lo.coerce$coercion$lo.comp.location*location$lo.json*json@_coerce'(XV28737, XV28738, XLbV2231, XThV2231).
'lo.comp.location^showChars'('_call%4'(XV28741, XV28742, XV28743, XV28744), 'lo.comp.location^showChars', _):- 'lo.comp.location@showChars'(XV28741, XV28742, XV28743, XV28744).
'lo.comp.location^showArrow'('_call%3'(XV28745, XV28746, XV28747), 'lo.comp.location^showArrow', _):- 'lo.comp.location@showArrow'(XV28745, XV28746, XV28747).
'lo.comp.location^locationContext'('_call%3'(XV28748, XV28749, XV28750), 'lo.comp.location^locationContext', _):- 'lo.comp.location@locationContext'(XV28748, XV28749, XV28750).
'lo.comp.location^findLine'('_call%3'(XV28751, XV28752, XV28753), 'lo.comp.location^findLine', _):- 'lo.comp.location@findLine'(XV28751, XV28752, XV28753).
'lo.comp.location^collectSrc'('_call%4'(XV28754, XV28755, XV28756, XV28757), 'lo.comp.location^collectSrc', _):- 'lo.comp.location@collectSrc'(XV28754, XV28755, XV28756, XV28757).
