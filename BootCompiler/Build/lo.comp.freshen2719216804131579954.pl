'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.freshen'e'*'n12o12'()12'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'freezeType'PT3t'lo.comp.types*tipe'Uz2'lo.index*map'2St'lo.comp.types*tipe't'lo.comp.types*tipe'\"s'I0'n0o0'()0'n1o1'()1'n4o4'()4's'freshen's'lo.comp.freshen$freshen's\":k's'c'lo.comp.freshen$freshen'T1k's'T0\"s\":k's'I2'skolemize'FT2k's'Uz2'lo.index*map'2St'lo.comp.types*tipe'T2Uz2'lo.index*map'2St'lo.comp.types*tipe'k's''freshen'FT2k's'Uz2'lo.index*map'2St'lo.comp.types*tipe'T2Uz2'lo.index*map'2St'lo.comp.types*tipe'k's'\"n2o2'()2'n2o2'()2's'lo.comp.freshen$freshen$lo.comp.types*tipe's\"c'lo.comp.freshen$freshen'T1t'lo.comp.types*tipe'T0\"n2o2'()2's'lo.comp.freshen$freshen$lo.comp.types*constraint's\"c'lo.comp.freshen$freshen'T1t'lo.comp.types*constraint'T0\"").
'lo.comp.freshen@init'() :- !.
'lo.comp.freshen@frshnConstraint'('lo.comp.types#conTract'(XNm, XArgs, XDeps), XB, 'lo.comp.types#conTract'(XNm, XX23591, XX23594)) :- !,
    'lo.comp.freshen@frshnTypes'(XArgs, XB, XX23591),
    'lo.comp.freshen@frshnTypes'(XDeps, XB, XX23594).
'lo.comp.freshen@frshnConstraint'('lo.comp.types#implementsFace'(XTp, XEls), XB, 'lo.comp.types#implementsFace'(XX23602, XX23605)) :- !,
    'lo.comp.freshen@frshnType'(XTp, XB, XX23602),
    'lo.comp.freshen@frshnFields'(XEls, XB, XX23605).
'lo.comp.freshen@frshnConstraint'('lo.comp.types#conCon'(XCon, XExt), XB, 'lo.comp.types#conCon'(XX23613, XX23616)) :- !,
    'lo.comp.freshen@frshnConstraint'(XCon, XB, XX23613),
    'lo.comp.freshen@frshnConstraint'(XExt, XB, XX23616).
'lo.comp.freshen@frshnConstraint'('lo.comp.types#univCon'('lo.comp.types#kVar'(XNm), XC), XB, XX23629) :- !,
    ocall('_remove%3'(XB, XNm, XX23626),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@frshnConstraint'(XC, XX23626, XX23629).
'lo.comp.freshen@frshnConstraint'('lo.comp.types#univCon'('lo.comp.types#kFun'(XNm, X_1571), XC), XB, XX23642) :- !,
    ocall('_remove%3'(XB, XNm, XX23639),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@frshnConstraint'(XC, XX23639, XX23642).
'lo.comp.freshen@frshnConstraint'(_, _, _) :- raise_exception('error'("frshnConstraint", 85, 3, 95)).
'lo.comp.freshen@frshnFields'('lo.core#[]', X_1572, 'lo.core#[]') :- !.
'lo.comp.freshen@frshnFields'('lo.core#,..'((XF, XE), XR), XB, 'lo.core#,..'((XF, XX23654), XX23657)) :- !,
    'lo.comp.freshen@frshnType'(XE, XB, XX23654),
    'lo.comp.freshen@frshnFields'(XR, XB, XX23657).
'lo.comp.freshen@frshnFields'(_, _, _) :- raise_exception('error'("frshnFields", 71, 3, 23)).
'lo.comp.freshen@frshnTypes'('lo.core#[]', X_1573, 'lo.core#[]') :- !.
'lo.comp.freshen@frshnTypes'('lo.core#,..'(XE, XR), XB, 'lo.core#,..'(XX23668, XX23671)) :- !,
    'lo.comp.freshen@frshnType'(XE, XB, XX23668),
    'lo.comp.freshen@frshnTypes'(XR, XB, XX23671).
'lo.comp.freshen@frshnTypes'(_, _, _) :- raise_exception('error'("frshnTypes", 66, 3, 22)).
'lo.comp.freshen@frshn'('lo.comp.types#anonType', X_1574, 'lo.comp.types#anonType') :- !.
'lo.comp.freshen@frshn'('lo.comp.types#voidType', X_1575, 'lo.comp.types#voidType') :- !.
'lo.comp.freshen@frshn'('lo.comp.types#thisType', XB, XTp) :- ocall('present%3'(XB, "this", XTp),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !.
'lo.comp.freshen@frshn'('lo.comp.types#thisType', X_1576, 'lo.comp.types#thisType') :- !.
'lo.comp.freshen@frshn'('lo.comp.types#kVar'(XNm), XB, XTp) :- ocall('present%3'(XB, XNm, XTp),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !.
'lo.comp.freshen@frshn'('lo.comp.types#kVar'(XNm), X_1577, 'lo.comp.types#kVar'(XNm)) :- !.
'lo.comp.freshen@frshn'('lo.comp.types#kFun'(XNm, X_1578), XB, XTp) :- ocall('present%3'(XB, XNm, XTp),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !.
'lo.comp.freshen@frshn'('lo.comp.types#kFun'(XNm, XAr), X_1579, 'lo.comp.types#kFun'(XNm, XAr)) :- !.
'lo.comp.freshen@frshn'('lo.comp.types#tipe'(XNm), X_1580, 'lo.comp.types#tipe'(XNm)) :- !.
'lo.comp.freshen@frshn'('lo.comp.types#tpFun'(XNm, XAr), X_1581, 'lo.comp.types#tpFun'(XNm, XAr)) :- !.
'lo.comp.freshen@frshn'('lo.comp.types#typeExp'(XOp, XA), XB, 'lo.comp.types#typeExp'(XX23738, XX23741)) :- !,
    'lo.comp.freshen@frshnType'(XOp, XB, XX23738),
    'lo.comp.freshen@frshnTypes'(XA, XB, XX23741).
'lo.comp.freshen@frshn'('lo.comp.types#funType'(XA, XR), XB, 'lo.comp.types#funType'(XX23749, XX23752)) :- !,
    'lo.comp.freshen@frshnType'(XA, XB, XX23749),
    'lo.comp.freshen@frshnType'(XR, XB, XX23752).
'lo.comp.freshen@frshn'('lo.comp.types#predType'(XA), XB, 'lo.comp.types#predType'(XX23759)) :- !,
    'lo.comp.freshen@frshnType'(XA, XB, XX23759).
'lo.comp.freshen@frshn'('lo.comp.types#grammarType'(XA, XR), XB, 'lo.comp.types#grammarType'(XX23767, XX23770)) :- !,
    'lo.comp.freshen@frshnType'(XA, XB, XX23767),
    'lo.comp.freshen@frshnType'(XR, XB, XX23770).
'lo.comp.freshen@frshn'('lo.comp.types#classType'(XA, XR), XB, 'lo.comp.types#classType'(XX23778, XX23781)) :- !,
    'lo.comp.freshen@frshnType'(XA, XB, XX23778),
    'lo.comp.freshen@frshnType'(XR, XB, XX23781).
'lo.comp.freshen@frshn'('lo.comp.types#tupleType'(XA), XB, 'lo.comp.types#tupleType'(XX23788)) :- !,
    'lo.comp.freshen@frshnTypes'(XA, XB, XX23788).
'lo.comp.freshen@frshn'('lo.comp.types#faceType'(XF), XB, 'lo.comp.types#faceType'(XX23795)) :- !,
    'lo.comp.freshen@frshnFields'(XF, XB, XX23795).
'lo.comp.freshen@frshn'('lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XT), XB, 'lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XX23810)) :- !,
    ocall('_remove%3'(XB, XNm, XX23807),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@frshnType'(XT, XX23807, XX23810).
'lo.comp.freshen@frshn'('lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XT), XB, 'lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XX23827)) :- !,
    ocall('_remove%3'(XB, XNm, XX23824),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@frshnType'(XdT, XX23824, XX23827).
'lo.comp.freshen@frshn'('lo.comp.types#constrained'(XT, XC), XB, 'lo.comp.types#constrained'(XX23835, XX23838)) :- !,
    'lo.comp.freshen@frshnType'(XT, XB, XX23835),
    'lo.comp.freshen@frshnConstraint'(XC, XB, XX23838).
'lo.comp.freshen@frshn'('lo.comp.types#typeRule'(XS, XT), XB, 'lo.comp.types#typeRule'(XX23846, XX23849)) :- !,
    'lo.comp.freshen@frshnType'(XS, XB, XX23846),
    'lo.comp.freshen@frshnType'(XT, XB, XX23849).
'lo.comp.freshen@frshn'(XTp, X_1582, XTp) :- !.
'lo.comp.freshen@frshn'(_, _, _) :- raise_exception('error'("frshn", 41, 3, 29)).
'lo.comp.freshen@frshnType'(XTp, XM, XX23859) :- !,
    'lo.comp.types@deRef'(XTp, XX23857),
    'lo.comp.freshen@frshn'(XX23857, XM, XX23859).
'lo.comp.freshen@frshnType'(_, _, _) :- raise_exception('error'("frshnType", 38, 3, 37)).
'lo.comp.freshen@freshenType'('lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XT), XB, XX23873) :- !,
    'lo.comp.types@newVar'(XNm, XX23869),
    ocall('_put%4'(XB, XNm, XX23869, XX23870),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@freshenType'(XT, XX23870, XX23873).
'lo.comp.freshen@freshenType'('lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XT), XB, XX23889) :- !,
    'lo.comp.types@newTFun'(XNm, XAr, XX23885),
    ocall('_put%4'(XB, XNm, XX23885, XX23886),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@freshenType'(XT, XX23886, XX23889).
'lo.comp.freshen@freshenType'(XT, XB, (XB, XX23895)) :- !,
    'lo.comp.freshen@frshnType'(XT, XB, XX23895).
'lo.comp.freshen@freshenType'(_, _, _) :- raise_exception('error'("freshenType", 17, 3, 71)).
'lo.comp.freshen@skolem'(XNm, 'lo.comp.types#kVar'(XX23898)) :- !,
    '_str_gen'(XNm, XX23898).
'lo.comp.freshen@skolem'(_, _) :- raise_exception('error'("skolem", 27, 3, 32)).
'lo.comp.freshen@skolemFun'(XNm, XAr, 'lo.comp.types#kFun'(XX23903, XAr)) :- !,
    '_str_gen'(XNm, XX23903).
'lo.comp.freshen@skolemFun'(_, _, _) :- raise_exception('error'("skolemFun", 30, 3, 41)).
'lo.comp.freshen@skolemizeType'('lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XT), XB, XX23919) :- !,
    'lo.comp.freshen@skolem'(XNm, XX23915),
    ocall('_put%4'(XB, XNm, XX23915, XX23916),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@skolemizeType'(XT, XX23916, XX23919).
'lo.comp.freshen@skolemizeType'('lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XT), XB, XX23935) :- !,
    'lo.comp.freshen@skolemFun'(XNm, XAr, XX23931),
    ocall('_put%4'(XB, XNm, XX23931, XX23932),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@skolemizeType'(XT, XX23932, XX23935).
'lo.comp.freshen@skolemizeType'(XT, XB, (XB, XX23941)) :- !,
    'lo.comp.freshen@frshnType'(XT, XB, XX23941).
'lo.comp.freshen@skolemizeType'(_, _, _) :- raise_exception('error'("skolemizeType", 22, 3, 75)).
'lo.comp.freshen$freshen$lo.comp.types*tipe'('lo.comp.freshen$freshen$lo.comp.types*tipe%1'('lo.comp.freshen$freshen$lo.comp.types*tipe')) :- !.
'lo.comp.freshen$freshen$lo.comp.types*tipe'('freshen%3'(XV3430, XV3431, XV3432), XLbl289, XThis289) :- !,
    'lo.comp.freshen$freshen$lo.comp.types*tipe@freshen'(XV3430, XV3431, XV3432, XLbl289, XThis289).
'lo.comp.freshen$freshen$lo.comp.types*tipe'('freshen%1'('lo.comp.freshen$freshen$lo.comp.types*tipe^freshen'(XLbl290, XThis290)), XLbl290, XThis290).
'lo.comp.freshen$freshen$lo.comp.types*tipe'('skolemize%3'(XV3439, XV3440, XV3441), XLbl291, XThis291) :- !,
    'lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize'(XV3439, XV3440, XV3441, XLbl291, XThis291).
'lo.comp.freshen$freshen$lo.comp.types*tipe'('skolemize%1'('lo.comp.freshen$freshen$lo.comp.types*tipe^skolemize'(XLbl292, XThis292)), XLbl292, XThis292).
'lo.comp.freshen$freshen$lo.comp.types*tipe@freshen'(XT, XM, XX23947, XLbV304, XThV304) :- !,
    'lo.comp.types@deRef'(XT, XX23945),
    'lo.comp.freshen@freshenType'(XX23945, XM, XX23947).
'lo.comp.freshen$freshen$lo.comp.types*tipe@freshen'(_, _, _, _, _) :- raise_exception('error'("freshen", 12, 5, 39)).
'lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize'(XT, XM, XX23953, XLbV304, XThV304) :- !,
    'lo.comp.types@deRef'(XT, XX23951),
    'lo.comp.freshen@skolemizeType'(XX23951, XM, XX23953).
'lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize'(_, _, _, _, _) :- raise_exception('error'("skolemize", 13, 5, 43)).
'lo.comp.freshen@freshenConstraint'('lo.comp.types#univCon'('lo.comp.types#kVar'(XNm), XC), XB, XX23967) :- !,
    'lo.comp.types@newVar'(XNm, XX23963),
    ocall('_put%4'(XB, XNm, XX23963, XX23964),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@freshenConstraint'(XC, XX23964, XX23967).
'lo.comp.freshen@freshenConstraint'('lo.comp.types#univCon'('lo.comp.types#kFun'(XNm, XAr), XC), XB, XX23983) :- !,
    'lo.comp.types@newTFun'(XNm, XAr, XX23979),
    ocall('_put%4'(XB, XNm, XX23979, XX23980),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@freshenConstraint'(XC, XX23980, XX23983).
'lo.comp.freshen@freshenConstraint'(XC, XB, (XB, XX23989)) :- !,
    'lo.comp.freshen@frshnConstraint'(XC, XB, XX23989).
'lo.comp.freshen@freshenConstraint'(_, _, _) :- raise_exception('error'("freshenConstraint", 75, 3, 82)).
'lo.comp.freshen@skolemizeConstraint'('lo.comp.types#univCon'('lo.comp.types#kVar'(XNm), XT), XB, XX24003) :- !,
    'lo.comp.freshen@skolem'(XNm, XX23999),
    ocall('_put%4'(XB, XNm, XX23999, XX24000),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@skolemizeConstraint'(XT, XX24000, XX24003).
'lo.comp.freshen@skolemizeConstraint'('lo.comp.types#univCon'('lo.comp.types#kFun'(XNm, XAr), XT), XB, XX24018) :- !,
    'lo.comp.freshen@skolem'(XNm, XX24014),
    ocall('_put%4'(XB, XNm, XX24014, XX24015),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@skolemizeConstraint'(XT, XX24015, XX24018).
'lo.comp.freshen@skolemizeConstraint'(XT, XB, (XB, XX24024)) :- !,
    'lo.comp.freshen@frshnConstraint'(XT, XB, XX24024).
'lo.comp.freshen@skolemizeConstraint'(_, _, _) :- raise_exception('error'("skolemizeConstraint", 80, 3, 86)).
'lo.comp.freshen$freshen$lo.comp.types*constraint'('lo.comp.freshen$freshen$lo.comp.types*constraint%1'('lo.comp.freshen$freshen$lo.comp.types*constraint')) :- !.
'lo.comp.freshen$freshen$lo.comp.types*constraint'('freshen%3'(XV3454, XV3455, XV3456), XLbl293, XThis293) :- !,
    'lo.comp.freshen$freshen$lo.comp.types*constraint@freshen'(XV3454, XV3455, XV3456, XLbl293, XThis293).
'lo.comp.freshen$freshen$lo.comp.types*constraint'('freshen%1'('lo.comp.freshen$freshen$lo.comp.types*constraint^freshen'(XLbl294, XThis294)), XLbl294, XThis294).
'lo.comp.freshen$freshen$lo.comp.types*constraint'('skolemize%3'(XV3463, XV3464, XV3465), XLbl295, XThis295) :- !,
    'lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize'(XV3463, XV3464, XV3465, XLbl295, XThis295).
'lo.comp.freshen$freshen$lo.comp.types*constraint'('skolemize%1'('lo.comp.freshen$freshen$lo.comp.types*constraint^skolemize'(XLbl296, XThis296)), XLbl296, XThis296).
'lo.comp.freshen$freshen$lo.comp.types*constraint@freshen'(XC, XB, XX24029, XLbV305, XThV305) :- !,
    'lo.comp.freshen@freshenConstraint'(XC, XB, XX24029).
'lo.comp.freshen$freshen$lo.comp.types*constraint@freshen'(_, _, _, _, _) :- raise_exception('error'("freshen", 33, 5, 38)).
'lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize'(XC, XB, XX24034, XLbV305, XThV305) :- !,
    'lo.comp.freshen@skolemizeConstraint'(XC, XB, XX24034).
'lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize'(_, _, _, _, _) :- raise_exception('error'("skolemize", 34, 5, 42)).
'lo.comp.freshen@freezeConstraint'('lo.comp.types#conTract'(XNm, XA, XD), XB, 'lo.comp.types#conTract'(XNm, XFA, XFD)) :- 'lo.comp.freshen@freezeTypes'(XA, XB, XFA),
    'lo.comp.freshen@freezeTypes'(XD, XB, XFD).
'lo.comp.freshen@freezeConstraint'('lo.comp.types#implementsFace'(XT, XF), XB, 'lo.comp.types#implementsFace'(XFT, XFF)) :- 'lo.comp.freshen@freeze'(XT, XB, XFT),
    'lo.comp.freshen@freezeFields'(XF, XB, XFF).
'lo.comp.freshen@freezeFields'('lo.core#[]', X_1583, 'lo.core#[]').
'lo.comp.freshen@freezeFields'('lo.core#,..'((XNm, XA), XL), XB, 'lo.core#,..'((XNm, XFA), XFL)) :- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freezeFields'(XL, XB, XFL).
'lo.comp.freshen@freezeTypes'('lo.core#[]', X_1584, 'lo.core#[]').
'lo.comp.freshen@freezeTypes'('lo.core#,..'(XA, XL), XB, 'lo.core#,..'(XFA, XFL)) :- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freezeTypes'(XL, XB, XFL).
'lo.comp.freshen@frze'('lo.comp.types#voidType', X_1585, 'lo.comp.types#voidType').
'lo.comp.freshen@frze'('lo.comp.types#anonType', X_1586, 'lo.comp.types#anonType').
'lo.comp.freshen@frze'('lo.comp.types#thisType', X_1587, 'lo.comp.types#thisType').
'lo.comp.freshen@frze'('lo.comp.types#kVar'(XTV), X_1588, 'lo.comp.types#kVar'(XTV)).
'lo.comp.freshen@frze'('lo.comp.types#kFun'(XF, XA), X_1589, 'lo.comp.types#kFun'(XF, XA)).
'lo.comp.freshen@frze'(XV, XB, 'lo.comp.types#kVar'(XTV)) :- 'lo.comp.types@isUnbound'(XV),
    ocall('present%3'(XB, XTV, XVV),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@deRef'(XVV, XX24130),
    'lo.comp.types@isIdenticalVar'(XV, XX24130).
'lo.comp.freshen@frze'(XV, X_1590, XV) :- 'lo.comp.types@isUnbound'(XV).
'lo.comp.freshen@frze'('lo.comp.types#tipe'(XNm), X_1591, 'lo.comp.types#tipe'(XNm)).
'lo.comp.freshen@frze'('lo.comp.types#tpFun'(XF, XA), X_1592, 'lo.comp.types#tpFun'(XF, XA)).
'lo.comp.freshen@frze'('lo.comp.types#funType'(XA, XR), XB, 'lo.comp.types#funType'(XFA, XFR)) :- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freeze'(XR, XB, XFR).
'lo.comp.freshen@frze'('lo.comp.types#grammarType'(XA, XR), XB, 'lo.comp.types#grammarType'(XFA, XFR)) :- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freeze'(XR, XB, XFR).
'lo.comp.freshen@frze'('lo.comp.types#classType'(XA, XR), XB, 'lo.comp.types#classType'(XFA, XFR)) :- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freeze'(XR, XB, XFR).
'lo.comp.freshen@frze'('lo.comp.types#predType'(XA), XB, 'lo.comp.types#predType'(XFA)) :- 'lo.comp.freshen@freeze'(XA, XB, XFA).
'lo.comp.freshen@frze'('lo.comp.types#tupleType'(XL), XB, 'lo.comp.types#tupleType'(XFL)) :- 'lo.comp.freshen@freezeTypes'(XL, XB, XFL).
'lo.comp.freshen@frze'('lo.comp.types#typeExp'(XO, XA), XB, 'lo.comp.types#typeExp'(XFO, XFA)) :- 'lo.comp.freshen@frze'(XO, XB, XFO),
    'lo.comp.freshen@freezeTypes'(XA, XB, XFA).
'lo.comp.freshen@frze'('lo.comp.types#univType'('lo.comp.types#kVar'(XV), XTp), XB, 'lo.comp.types#univType'('lo.comp.types#kVar'(XV), XFTp)) :- ocall('_remove%3'(XB, XV, XX24227),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@freeze'(XTp, XX24227, XFTp).
'lo.comp.freshen@frze'('lo.comp.types#faceType'(XL), XB, 'lo.comp.types#faceType'(XFL)) :- 'lo.comp.freshen@freezeFields'(XL, XB, XFL).
'lo.comp.freshen@frze'('lo.comp.types#typeRule'(XA, XR), XB, 'lo.comp.types#typeRule'(XFA, XFR)) :- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freeze'(XR, XB, XFR).
'lo.comp.freshen@frze'('lo.comp.types#constrained'(XTp, XCon), XB, 'lo.comp.types#constrained'(XFTp, XFCon)) :- 'lo.comp.freshen@freeze'(XTp, XB, XFTp),
    'lo.comp.freshen@freezeConstraint'(XCon, XB, XFCon).
'lo.comp.freshen@freeze'(XT, XB, XFrzn) :- 'lo.comp.freshen@one26'(XFrzn, XB, XX24269, XT).
'lo.comp.freshen@reQuant'('lo.core#[]', X_1593, XT, XT).
'lo.comp.freshen@reQuant'('lo.core#,..'((X_1594, XTp), XR), XBB, XFT, XFZT) :- 'lo.comp.types@deRef'(XTp, XX24285),
    XV = XX24285,
    'lo.comp.types@isUnbound'(XV),
    'lo.comp.freshen@reQuant'(XR, XBB, XFT, XFZT).
'lo.comp.freshen@reQuant'('lo.core#,..'(X_1595, XB), XBB, XT, XFT) :- 'lo.comp.freshen@reQuant'(XB, XBB, XT, XFT).
'lo.comp.freshen@freezeType'(XTp, XB, XFrZ) :- 'lo.comp.freshen@freeze'(XTp, XB, XFT),
    ocall('pairs%2'(XB, XX24308),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@reQuant'(XX24308, XB, XFT, XFrZ).
'lo.comp.freshen@freezeConstraints'(XC, X_1596, XT, XT) :- 'lo.comp.freshen@one27'(XC).
'lo.comp.freshen@freezeConstraints'('lo.core#,..'(XC, X_1597), X_1598, XT, XT) :- 'lo.comp.freshen@one28'(XC).
'lo.comp.freshen@freezeConstraints'('lo.core#,..'(XC, XR), XBB, XT, XFT) :- 'lo.comp.freshen@freezeConstraint'(XC, XBB, XFC),
    'lo.comp.freshen@freezeConstraints'(XR, XBB, 'lo.comp.types#constrained'(XT, XFC), XFT).
'lo.comp.freshen^frshnConstraint'('_call%3'(XV3401, XV3402, XV3403), 'lo.comp.freshen^frshnConstraint', _) :- 'lo.comp.freshen@frshnConstraint'(XV3401, XV3402, XV3403).
'lo.comp.freshen^frshnFields'('_call%3'(XV3404, XV3405, XV3406), 'lo.comp.freshen^frshnFields', _) :- 'lo.comp.freshen@frshnFields'(XV3404, XV3405, XV3406).
'lo.comp.freshen^frshnTypes'('_call%3'(XV3407, XV3408, XV3409), 'lo.comp.freshen^frshnTypes', _) :- 'lo.comp.freshen@frshnTypes'(XV3407, XV3408, XV3409).
'lo.comp.freshen^frshn'('_call%3'(XV3410, XV3411, XV3412), 'lo.comp.freshen^frshn', _) :- 'lo.comp.freshen@frshn'(XV3410, XV3411, XV3412).
'lo.comp.freshen^frshnType'('_call%3'(XV3413, XV3414, XV3415), 'lo.comp.freshen^frshnType', _) :- 'lo.comp.freshen@frshnType'(XV3413, XV3414, XV3415).
'lo.comp.freshen^freshenType'('_call%3'(XV3416, XV3417, XV3418), 'lo.comp.freshen^freshenType', _) :- 'lo.comp.freshen@freshenType'(XV3416, XV3417, XV3418).
'lo.comp.freshen^skolem'('_call%2'(XV3419, XV3420), 'lo.comp.freshen^skolem', _) :- 'lo.comp.freshen@skolem'(XV3419, XV3420).
'lo.comp.freshen^skolemFun'('_call%3'(XV3421, XV3422, XV3423), 'lo.comp.freshen^skolemFun', _) :- 'lo.comp.freshen@skolemFun'(XV3421, XV3422, XV3423).
'lo.comp.freshen^skolemizeType'('_call%3'(XV3424, XV3425, XV3426), 'lo.comp.freshen^skolemizeType', _) :- 'lo.comp.freshen@skolemizeType'(XV3424, XV3425, XV3426).
'lo.comp.freshen$freshen$lo.comp.types*tipe^freshen'('_call%3'(XV3427, XV3428, XV3429), 'lo.comp.freshen$freshen$lo.comp.types*tipe^freshen'(XLbV304, XThV304), _) :- 'lo.comp.freshen$freshen$lo.comp.types*tipe@freshen'(XV3427, XV3428, XV3429, XLbV304, XThV304).
'lo.comp.freshen$freshen$lo.comp.types*tipe^freshen'('_call%3'(XV3433, XV3434, XV3435), 'lo.comp.freshen$freshen$lo.comp.types*tipe^freshen'(XLbV304, XThV304), _) :- 'lo.comp.freshen$freshen$lo.comp.types*tipe@freshen'(XV3433, XV3434, XV3435, XLbV304, XThV304).
'lo.comp.freshen$freshen$lo.comp.types*tipe^skolemize'('_call%3'(XV3436, XV3437, XV3438), 'lo.comp.freshen$freshen$lo.comp.types*tipe^skolemize'(XLbV304, XThV304), _) :- 'lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize'(XV3436, XV3437, XV3438, XLbV304, XThV304).
'lo.comp.freshen$freshen$lo.comp.types*tipe^skolemize'('_call%3'(XV3442, XV3443, XV3444), 'lo.comp.freshen$freshen$lo.comp.types*tipe^skolemize'(XLbV304, XThV304), _) :- 'lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize'(XV3442, XV3443, XV3444, XLbV304, XThV304).
'lo.comp.freshen^freshenConstraint'('_call%3'(XV3445, XV3446, XV3447), 'lo.comp.freshen^freshenConstraint', _) :- 'lo.comp.freshen@freshenConstraint'(XV3445, XV3446, XV3447).
'lo.comp.freshen^skolemizeConstraint'('_call%3'(XV3448, XV3449, XV3450), 'lo.comp.freshen^skolemizeConstraint', _) :- 'lo.comp.freshen@skolemizeConstraint'(XV3448, XV3449, XV3450).
'lo.comp.freshen$freshen$lo.comp.types*constraint^freshen'('_call%3'(XV3451, XV3452, XV3453), 'lo.comp.freshen$freshen$lo.comp.types*constraint^freshen'(XLbV305, XThV305), _) :- 'lo.comp.freshen$freshen$lo.comp.types*constraint@freshen'(XV3451, XV3452, XV3453, XLbV305, XThV305).
'lo.comp.freshen$freshen$lo.comp.types*constraint^freshen'('_call%3'(XV3457, XV3458, XV3459), 'lo.comp.freshen$freshen$lo.comp.types*constraint^freshen'(XLbV305, XThV305), _) :- 'lo.comp.freshen$freshen$lo.comp.types*constraint@freshen'(XV3457, XV3458, XV3459, XLbV305, XThV305).
'lo.comp.freshen$freshen$lo.comp.types*constraint^skolemize'('_call%3'(XV3460, XV3461, XV3462), 'lo.comp.freshen$freshen$lo.comp.types*constraint^skolemize'(XLbV305, XThV305), _) :- 'lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize'(XV3460, XV3461, XV3462, XLbV305, XThV305).
'lo.comp.freshen$freshen$lo.comp.types*constraint^skolemize'('_call%3'(XV3466, XV3467, XV3468), 'lo.comp.freshen$freshen$lo.comp.types*constraint^skolemize'(XLbV305, XThV305), _) :- 'lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize'(XV3466, XV3467, XV3468, XLbV305, XThV305).
'lo.comp.freshen^freezeConstraint'('_call%3'(XV3469, XV3470, XV3471), 'lo.comp.freshen^freezeConstraint', _) :- 'lo.comp.freshen@freezeConstraint'(XV3469, XV3470, XV3471).
'lo.comp.freshen^freezeFields'('_call%3'(XV3472, XV3473, XV3474), 'lo.comp.freshen^freezeFields', _) :- 'lo.comp.freshen@freezeFields'(XV3472, XV3473, XV3474).
'lo.comp.freshen^freezeTypes'('_call%3'(XV3475, XV3476, XV3477), 'lo.comp.freshen^freezeTypes', _) :- 'lo.comp.freshen@freezeTypes'(XV3475, XV3476, XV3477).
'lo.comp.freshen^frze'('_call%3'(XV3478, XV3479, XV3480), 'lo.comp.freshen^frze', _) :- 'lo.comp.freshen@frze'(XV3478, XV3479, XV3480).
'lo.comp.freshen@one26'(XFrzn, XB, XX24269, XT) :- 'lo.comp.types@deRef'(XT, XX24269),
    'lo.comp.freshen@frze'(XX24269, XB, XFrzn),
    !.
'lo.comp.freshen^freeze'('_call%3'(XV3481, XV3482, XV3483), 'lo.comp.freshen^freeze', _) :- 'lo.comp.freshen@freeze'(XV3481, XV3482, XV3483).
'lo.comp.freshen^reQuant'('_call%4'(XV3484, XV3485, XV3486, XV3487), 'lo.comp.freshen^reQuant', _) :- 'lo.comp.freshen@reQuant'(XV3484, XV3485, XV3486, XV3487).
'lo.comp.freshen^freezeType'('_call%3'(XV3488, XV3489, XV3490), 'lo.comp.freshen^freezeType', _) :- 'lo.comp.freshen@freezeType'(XV3488, XV3489, XV3490).
'lo.comp.freshen@one27'(XC) :- 'var'(XC),
    !.
'lo.comp.freshen@one28'(XC) :- 'var'(XC),
    !.
'lo.comp.freshen^freezeConstraints'('_call%4'(XV3491, XV3492, XV3493, XV3494), 'lo.comp.freshen^freezeConstraints', _) :- 'lo.comp.freshen@freezeConstraints'(XV3491, XV3492, XV3493, XV3494).
