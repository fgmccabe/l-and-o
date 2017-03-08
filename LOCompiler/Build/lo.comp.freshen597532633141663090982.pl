'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.freshen's'0.0.1'n3o3'()3'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'s\"I1'freezeType'PT3t'lo.comp.types*tipe'Uz2'lo.index*map'2St'lo.comp.types*tipe't'lo.comp.types*tipe'\"s'I0'n0o0'()0'n1o1'()1'n4o4'()4's'freshen's'lo.comp.freshen$freshen's\":k's'c'lo.comp.freshen$freshen'T1k's'T0\"s\":k's'I2'freshen'FT2k's'Uz2'lo.index*map'2St'lo.comp.types*tipe'T2Uz2'lo.index*map'2St'lo.comp.types*tipe'k's''skolemize'FT2k's'Uz2'lo.index*map'2St'lo.comp.types*tipe'T2Uz2'lo.index*map'2St'lo.comp.types*tipe'k's'\"n2o2'()2'n2o2'()2's'lo.comp.freshen$freshen$lo.comp.types*tipe's\"c'lo.comp.freshen$freshen'T1t'lo.comp.types*tipe'T0\"n2o2'()2's'lo.comp.freshen$freshen$lo.comp.types*constraint's\"c'lo.comp.freshen$freshen'T1t'lo.comp.types*constraint'T0\"").
'lo.comp.freshen@init'():- !.
'lo.comp.freshen@frshnTypes'('lo.core#[]', X_21921, 'lo.core#[]'):- !.
'lo.comp.freshen@frshnTypes'('lo.core#,..'(XE, XR), XB, 'lo.core#,..'(XXd27042, XXd27043)):- !,
    'lo.comp.freshen@frshnType'(XE, XB, XXd27042),
    'lo.comp.freshen@frshnTypes'(XR, XB, XXd27043).
'lo.comp.freshen@frshnTypes'(_, _, _):- raise_exception('error'("lo.comp.freshen@frshnTypes", 66, 3, 22)).
'lo.comp.freshen@frshnFields'('lo.core#[]', X_21924, 'lo.core#[]'):- !.
'lo.comp.freshen@frshnFields'('lo.core#,..'('()2'(XF, XE), XR), XB, 'lo.core#,..'('()2'(XF, XXd27045), XXd27046)):- !,
    'lo.comp.freshen@frshnType'(XE, XB, XXd27045),
    'lo.comp.freshen@frshnFields'(XR, XB, XXd27046).
'lo.comp.freshen@frshnFields'(_, _, _):- raise_exception('error'("lo.comp.freshen@frshnFields", 71, 3, 23)).
'lo.comp.freshen@frshnConstraint'('lo.comp.types#conTract'(XNm, XArgs, XDeps), XB, 'lo.comp.types#conTract'(XNm, XXd27048, XXd27049)):- !,
    'lo.comp.freshen@frshnTypes'(XArgs, XB, XXd27048),
    'lo.comp.freshen@frshnTypes'(XDeps, XB, XXd27049).
'lo.comp.freshen@frshnConstraint'('lo.comp.types#implementsFace'(XTp, XEls), XB, 'lo.comp.types#implementsFace'(XXd27051, XXd27052)):- !,
    'lo.comp.freshen@frshnType'(XTp, XB, XXd27051),
    'lo.comp.freshen@frshnFields'(XEls, XB, XXd27052).
'lo.comp.freshen@frshnConstraint'('lo.comp.types#conCon'(XCon, XExt), XB, 'lo.comp.types#conCon'(XXd27054, XXd27055)):- !,
    'lo.comp.freshen@frshnConstraint'(XCon, XB, XXd27054),
    'lo.comp.freshen@frshnConstraint'(XExt, XB, XXd27055).
'lo.comp.freshen@frshnConstraint'('lo.comp.types#univCon'('lo.comp.types#kVar'(XNm), XC), XB, XXd27058):- !,
    ocall('_remove%1'(XXV3396),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XB, XNm, XXe3180),XXV3396,XXV3396),
    'lo.comp.freshen@frshnConstraint'(XC, XXe3180, XXd27058).
'lo.comp.freshen@frshnConstraint'('lo.comp.types#univCon'('lo.comp.types#kFun'(XNm, X_21927), XC), XB, XXd27060):- !,
    ocall('_remove%1'(XXV3397),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XB, XNm, XXe3181),XXV3397,XXV3397),
    'lo.comp.freshen@frshnConstraint'(XC, XXe3181, XXd27060).
'lo.comp.freshen@frshnConstraint'(_, _, _):- raise_exception('error'("lo.comp.freshen@frshnConstraint", 85, 3, 95)).
'lo.comp.freshen@frshn'('lo.comp.types#anonType', X_21928, 'lo.comp.types#anonType'):- !.
'lo.comp.freshen@frshn'('lo.comp.types#voidType', X_21929, 'lo.comp.types#voidType'):- !.
'lo.comp.freshen@frshn'('lo.comp.types#thisType', XB, XTp):- ocall('present%3'(XB, "this", XTp),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !.
'lo.comp.freshen@frshn'('lo.comp.types#thisType', X_21930, 'lo.comp.types#thisType'):- !.
'lo.comp.freshen@frshn'('lo.comp.types#kVar'(XNm), XB, XTp):- ocall('present%3'(XB, XNm, XTp),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !.
'lo.comp.freshen@frshn'('lo.comp.types#kVar'(XNm), X_21931, 'lo.comp.types#kVar'(XNm)):- !.
'lo.comp.freshen@frshn'('lo.comp.types#kFun'(XNm, X_21932), XB, XTp):- ocall('present%3'(XB, XNm, XTp),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !.
'lo.comp.freshen@frshn'('lo.comp.types#kFun'(XNm, XAr), X_21933, 'lo.comp.types#kFun'(XNm, XAr)):- !.
'lo.comp.freshen@frshn'('lo.comp.types#tipe'(XNm), X_21934, 'lo.comp.types#tipe'(XNm)):- !.
'lo.comp.freshen@frshn'('lo.comp.types#tpFun'(XNm, XAr), X_21935, 'lo.comp.types#tpFun'(XNm, XAr)):- !.
'lo.comp.freshen@frshn'('lo.comp.types#typeExp'(XOp, XA), XB, 'lo.comp.types#typeExp'(XXd27068, XXd27069)):- !,
    'lo.comp.freshen@frshnType'(XOp, XB, XXd27068),
    'lo.comp.freshen@frshnTypes'(XA, XB, XXd27069).
'lo.comp.freshen@frshn'('lo.comp.types#funType'(XA, XR), XB, 'lo.comp.types#funType'(XXd27071, XXd27072)):- !,
    'lo.comp.freshen@frshnType'(XA, XB, XXd27071),
    'lo.comp.freshen@frshnType'(XR, XB, XXd27072).
'lo.comp.freshen@frshn'('lo.comp.types#predType'(XA), XB, 'lo.comp.types#predType'(XXd27074)):- !,
    'lo.comp.freshen@frshnType'(XA, XB, XXd27074).
'lo.comp.freshen@frshn'('lo.comp.types#grammarType'(XA, XR), XB, 'lo.comp.types#grammarType'(XXd27076, XXd27077)):- !,
    'lo.comp.freshen@frshnType'(XA, XB, XXd27076),
    'lo.comp.freshen@frshnType'(XR, XB, XXd27077).
'lo.comp.freshen@frshn'('lo.comp.types#classType'(XA, XR), XB, 'lo.comp.types#classType'(XXd27079, XXd27080)):- !,
    'lo.comp.freshen@frshnType'(XA, XB, XXd27079),
    'lo.comp.freshen@frshnType'(XR, XB, XXd27080).
'lo.comp.freshen@frshn'('lo.comp.types#tupleType'(XA), XB, 'lo.comp.types#tupleType'(XXd27082)):- !,
    'lo.comp.freshen@frshnTypes'(XA, XB, XXd27082).
'lo.comp.freshen@frshn'('lo.comp.types#faceType'(XF), XB, 'lo.comp.types#faceType'(XXd27084)):- !,
    'lo.comp.freshen@frshnFields'(XF, XB, XXd27084).
'lo.comp.freshen@frshn'('lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XT), XB, 'lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XXd27088)):- !,
    ocall('_remove%1'(XXV3398),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XB, XNm, XXe3182),XXV3398,XXV3398),
    'lo.comp.freshen@frshnType'(XT, XXe3182, XXd27088).
'lo.comp.freshen@frshn'('lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XT), XB, 'lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XXd27092)):- !,
    ocall('_remove%1'(XXV3399),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XB, XNm, XXe3183),XXV3399,XXV3399),
    'lo.comp.freshen@frshnType'(XdT, XXe3183, XXd27092).
'lo.comp.freshen@frshn'('lo.comp.types#constrained'(XT, XC), XB, 'lo.comp.types#constrained'(XXd27094, XXd27095)):- !,
    'lo.comp.freshen@frshnType'(XT, XB, XXd27094),
    'lo.comp.freshen@frshnConstraint'(XC, XB, XXd27095).
'lo.comp.freshen@frshn'('lo.comp.types#typeRule'(XS, XT), XB, 'lo.comp.types#typeRule'(XXd27097, XXd27098)):- !,
    'lo.comp.freshen@frshnType'(XS, XB, XXd27097),
    'lo.comp.freshen@frshnType'(XT, XB, XXd27098).
'lo.comp.freshen@frshn'(XTp, X_21936, XTp):- !.
'lo.comp.freshen@frshn'(_, _, _):- raise_exception('error'("lo.comp.freshen@frshn", 41, 3, 29)).
'lo.comp.freshen@frshnType'(XTp, XM, XXd27101):- !,
    'lo.comp.types@deRef'(XTp, XXd27100),
    'lo.comp.freshen@frshn'(XXd27100, XM, XXd27101).
'lo.comp.freshen@frshnType'(_, _, _):- raise_exception('error'("lo.comp.freshen@frshnType", 38, 3, 37)).
'lo.comp.freshen@skolemFun'(XNm, XAr, 'lo.comp.types#kFun'(XXc417, XAr)):- !,
    '_str_gen'(XNm, XXc417).
'lo.comp.freshen@skolemFun'(_, _, _):- raise_exception('error'("lo.comp.freshen@skolemFun", 30, 3, 41)).
'lo.comp.freshen@skolem'(XNm, 'lo.comp.types#kVar'(XXc418)):- !,
    '_str_gen'(XNm, XXc418).
'lo.comp.freshen@skolem'(_, _):- raise_exception('error'("lo.comp.freshen@skolem", 27, 3, 32)).
'lo.comp.freshen@skolemizeType'('lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XT), XB, XXd27106):- !,
    ocall('_put%1'(XXV3400),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@skolem'(XNm, XXd27104),
    ocall('_call%4'(XB, XNm, XXd27104, XXe3184),XXV3400,XXV3400),
    'lo.comp.freshen@skolemizeType'(XT, XXe3184, XXd27106).
'lo.comp.freshen@skolemizeType'('lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XT), XB, XXd27109):- !,
    ocall('_put%1'(XXV3401),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@skolemFun'(XNm, XAr, XXd27107),
    ocall('_call%4'(XB, XNm, XXd27107, XXe3185),XXV3401,XXV3401),
    'lo.comp.freshen@skolemizeType'(XT, XXe3185, XXd27109).
'lo.comp.freshen@skolemizeType'(XT, XB, '()2'(XB, XXd27110)):- !,
    'lo.comp.freshen@frshnType'(XT, XB, XXd27110).
'lo.comp.freshen@skolemizeType'(_, _, _):- raise_exception('error'("lo.comp.freshen@skolemizeType", 22, 3, 75)).
'lo.comp.freshen@freshenType'('lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XT), XB, XXd27113):- !,
    ocall('_put%1'(XXV3402),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@newVar'(XNm, XXd27111),
    ocall('_call%4'(XB, XNm, XXd27111, XXe3186),XXV3402,XXV3402),
    'lo.comp.freshen@freshenType'(XT, XXe3186, XXd27113).
'lo.comp.freshen@freshenType'('lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XT), XB, XXd27116):- !,
    ocall('_put%1'(XXV3403),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@newTFun'(XNm, XAr, XXd27114),
    ocall('_call%4'(XB, XNm, XXd27114, XXe3187),XXV3403,XXV3403),
    'lo.comp.freshen@freshenType'(XT, XXe3187, XXd27116).
'lo.comp.freshen@freshenType'(XT, XB, '()2'(XB, XXd27117)):- !,
    'lo.comp.freshen@frshnType'(XT, XB, XXd27117).
'lo.comp.freshen@freshenType'(_, _, _):- raise_exception('error'("lo.comp.freshen@freshenType", 17, 3, 71)).
'lo.comp.freshen$freshen$lo.comp.types*tipe'('lo.comp.freshen$freshen$lo.comp.types*tipe%1'('lo.comp.freshen$freshen$lo.comp.types*tipe')):- !.
'lo.comp.freshen$freshen$lo.comp.types*tipe'('freshen%3'(XV20559, XV20560, XV20561), XLbl1841, XThis1841):- !,
    'lo.comp.freshen$freshen$lo.comp.types*tipe@freshen'(XV20559, XV20560, XV20561, XLbl1841, XThis1841).
'lo.comp.freshen$freshen$lo.comp.types*tipe'('freshen%1'('lo.comp.freshen$freshen$lo.comp.types*tipe^freshen'(XLbl1842, XThis1842)), XLbl1842, XThis1842).
'lo.comp.freshen$freshen$lo.comp.types*tipe'('skolemize%3'(XV20565, XV20566, XV20567), XLbl1843, XThis1843):- !,
    'lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize'(XV20565, XV20566, XV20567, XLbl1843, XThis1843).
'lo.comp.freshen$freshen$lo.comp.types*tipe'('skolemize%1'('lo.comp.freshen$freshen$lo.comp.types*tipe^skolemize'(XLbl1844, XThis1844)), XLbl1844, XThis1844).
'lo.comp.freshen$freshen$lo.comp.types*tipe@freshen'(XT, XM, XXd27119, XLbV1831, XThV1831):- !,
    'lo.comp.types@deRef'(XT, XXd27118),
    'lo.comp.freshen@freshenType'(XXd27118, XM, XXd27119).
'lo.comp.freshen$freshen$lo.comp.types*tipe@freshen'(_, _, _):- raise_exception('error'("lo.comp.freshen$freshen$lo.comp.types*tipe@freshen", 12, 5, 39)).
'lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize'(XT, XM, XXd27121, XLbV1831, XThV1831):- !,
    'lo.comp.types@deRef'(XT, XXd27120),
    'lo.comp.freshen@skolemizeType'(XXd27120, XM, XXd27121).
'lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize'(_, _, _):- raise_exception('error'("lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize", 13, 5, 43)).
'lo.comp.freshen@skolemizeConstraint'('lo.comp.types#univCon'('lo.comp.types#kVar'(XNm), XT), XB, XXd27124):- !,
    ocall('_put%1'(XXV3404),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@skolem'(XNm, XXd27122),
    ocall('_call%4'(XB, XNm, XXd27122, XXe3188),XXV3404,XXV3404),
    'lo.comp.freshen@skolemizeConstraint'(XT, XXe3188, XXd27124).
'lo.comp.freshen@skolemizeConstraint'('lo.comp.types#univCon'('lo.comp.types#kFun'(XNm, XAr), XT), XB, XXd27127):- !,
    ocall('_put%1'(XXV3405),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.freshen@skolem'(XNm, XXd27125),
    ocall('_call%4'(XB, XNm, XXd27125, XXe3189),XXV3405,XXV3405),
    'lo.comp.freshen@skolemizeConstraint'(XT, XXe3189, XXd27127).
'lo.comp.freshen@skolemizeConstraint'(XT, XB, '()2'(XB, XXd27128)):- !,
    'lo.comp.freshen@frshnConstraint'(XT, XB, XXd27128).
'lo.comp.freshen@skolemizeConstraint'(_, _, _):- raise_exception('error'("lo.comp.freshen@skolemizeConstraint", 80, 3, 86)).
'lo.comp.freshen@freshenConstraint'('lo.comp.types#univCon'('lo.comp.types#kVar'(XNm), XC), XB, XXd27131):- !,
    ocall('_put%1'(XXV3406),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@newVar'(XNm, XXd27129),
    ocall('_call%4'(XB, XNm, XXd27129, XXe3190),XXV3406,XXV3406),
    'lo.comp.freshen@freshenConstraint'(XC, XXe3190, XXd27131).
'lo.comp.freshen@freshenConstraint'('lo.comp.types#univCon'('lo.comp.types#kFun'(XNm, XAr), XC), XB, XXd27134):- !,
    ocall('_put%1'(XXV3407),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@newTFun'(XNm, XAr, XXd27132),
    ocall('_call%4'(XB, XNm, XXd27132, XXe3191),XXV3407,XXV3407),
    'lo.comp.freshen@freshenConstraint'(XC, XXe3191, XXd27134).
'lo.comp.freshen@freshenConstraint'(XC, XB, '()2'(XB, XXd27135)):- !,
    'lo.comp.freshen@frshnConstraint'(XC, XB, XXd27135).
'lo.comp.freshen@freshenConstraint'(_, _, _):- raise_exception('error'("lo.comp.freshen@freshenConstraint", 75, 3, 82)).
'lo.comp.freshen$freshen$lo.comp.types*constraint'('lo.comp.freshen$freshen$lo.comp.types*constraint%1'('lo.comp.freshen$freshen$lo.comp.types*constraint')):- !.
'lo.comp.freshen$freshen$lo.comp.types*constraint'('freshen%3'(XV20577, XV20578, XV20579), XLbl1845, XThis1845):- !,
    'lo.comp.freshen$freshen$lo.comp.types*constraint@freshen'(XV20577, XV20578, XV20579, XLbl1845, XThis1845).
'lo.comp.freshen$freshen$lo.comp.types*constraint'('freshen%1'('lo.comp.freshen$freshen$lo.comp.types*constraint^freshen'(XLbl1846, XThis1846)), XLbl1846, XThis1846).
'lo.comp.freshen$freshen$lo.comp.types*constraint'('skolemize%3'(XV20583, XV20584, XV20585), XLbl1847, XThis1847):- !,
    'lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize'(XV20583, XV20584, XV20585, XLbl1847, XThis1847).
'lo.comp.freshen$freshen$lo.comp.types*constraint'('skolemize%1'('lo.comp.freshen$freshen$lo.comp.types*constraint^skolemize'(XLbl1848, XThis1848)), XLbl1848, XThis1848).
'lo.comp.freshen$freshen$lo.comp.types*constraint@freshen'(XC, XB, XXd27136, XLbV1832, XThV1832):- !,
    'lo.comp.freshen@freshenConstraint'(XC, XB, XXd27136).
'lo.comp.freshen$freshen$lo.comp.types*constraint@freshen'(_, _, _):- raise_exception('error'("lo.comp.freshen$freshen$lo.comp.types*constraint@freshen", 33, 5, 38)).
'lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize'(XC, XB, XXd27137, XLbV1832, XThV1832):- !,
    'lo.comp.freshen@skolemizeConstraint'(XC, XB, XXd27137).
'lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize'(_, _, _):- raise_exception('error'("lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize", 34, 5, 42)).
'lo.comp.freshen@freezeTypes'('lo.core#[]', X_21937, 'lo.core#[]').
'lo.comp.freshen@freezeTypes'('lo.core#,..'(XA, XL), XB, 'lo.core#,..'(XFA, XFL)):- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freezeTypes'(XL, XB, XFL).
'lo.comp.freshen@freezeFields'('lo.core#[]', X_21940, 'lo.core#[]').
'lo.comp.freshen@freezeFields'('lo.core#,..'('()2'(XNm, XA), XL), XB, 'lo.core#,..'('()2'(XNm, XFA), XFL)):- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freezeFields'(XL, XB, XFL).
'lo.comp.freshen@freezeConstraint'('lo.comp.types#conTract'(XNm, XA, XD), XB, 'lo.comp.types#conTract'(XNm, XFA, XFD)):- 'lo.comp.freshen@freezeTypes'(XA, XB, XFA),
    'lo.comp.freshen@freezeTypes'(XD, XB, XFD).
'lo.comp.freshen@freezeConstraint'('lo.comp.types#implementsFace'(XT, XF), XB, 'lo.comp.types#implementsFace'(XFT, XFF)):- 'lo.comp.freshen@freeze'(XT, XB, XFT),
    'lo.comp.freshen@freezeFields'(XF, XB, XFF).
'lo.comp.freshen@frze'('lo.comp.types#voidType', X_21943, 'lo.comp.types#voidType').
'lo.comp.freshen@frze'('lo.comp.types#anonType', X_21944, 'lo.comp.types#anonType').
'lo.comp.freshen@frze'('lo.comp.types#thisType', X_21945, 'lo.comp.types#thisType').
'lo.comp.freshen@frze'('lo.comp.types#kVar'(XTV), X_21946, 'lo.comp.types#kVar'(XTV)).
'lo.comp.freshen@frze'('lo.comp.types#kFun'(XF, XA), X_21947, 'lo.comp.types#kFun'(XF, XA)).
'lo.comp.freshen@frze'(XV, XB, 'lo.comp.types#kVar'(XTV)):- 'lo.comp.types@isUnbound'(XV),
    ocall('present%3'(XB, XTV, XVV),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@deRef'(XVV, XXd27139),
    'lo.comp.types@isIdenticalVar'(XV, XXd27139).
'lo.comp.freshen@frze'(XV, X_21948, XV):- 'lo.comp.types@isUnbound'(XV).
'lo.comp.freshen@frze'('lo.comp.types#tipe'(XNm), X_21949, 'lo.comp.types#tipe'(XNm)).
'lo.comp.freshen@frze'('lo.comp.types#tpFun'(XF, XA), X_21950, 'lo.comp.types#tpFun'(XF, XA)).
'lo.comp.freshen@frze'('lo.comp.types#funType'(XA, XR), XB, 'lo.comp.types#funType'(XFA, XFR)):- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freeze'(XR, XB, XFR).
'lo.comp.freshen@frze'('lo.comp.types#grammarType'(XA, XR), XB, 'lo.comp.types#grammarType'(XFA, XFR)):- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freeze'(XR, XB, XFR).
'lo.comp.freshen@frze'('lo.comp.types#classType'(XA, XR), XB, 'lo.comp.types#classType'(XFA, XFR)):- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freeze'(XR, XB, XFR).
'lo.comp.freshen@frze'('lo.comp.types#predType'(XA), XB, 'lo.comp.types#predType'(XFA)):- 'lo.comp.freshen@freeze'(XA, XB, XFA).
'lo.comp.freshen@frze'('lo.comp.types#tupleType'(XL), XB, 'lo.comp.types#tupleType'(XFL)):- 'lo.comp.freshen@freezeTypes'(XL, XB, XFL).
'lo.comp.freshen@frze'('lo.comp.types#typeExp'(XO, XA), XB, 'lo.comp.types#typeExp'(XFO, XFA)):- 'lo.comp.freshen@frze'(XO, XB, XFO),
    'lo.comp.freshen@freezeTypes'(XA, XB, XFA).
'lo.comp.freshen@frze'('lo.comp.types#univType'('lo.comp.types#kVar'(XV), XTp), XB, 'lo.comp.types#univType'('lo.comp.types#kVar'(XV), XFTp)):- ocall('_remove%1'(XXV3408),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XB, XV, XXe3192),XXV3408,XXV3408),
    'lo.comp.freshen@freeze'(XTp, XXe3192, XFTp).
'lo.comp.freshen@frze'('lo.comp.types#faceType'(XL), XB, 'lo.comp.types#faceType'(XFL)):- 'lo.comp.freshen@freezeFields'(XL, XB, XFL).
'lo.comp.freshen@frze'('lo.comp.types#typeRule'(XA, XR), XB, 'lo.comp.types#typeRule'(XFA, XFR)):- 'lo.comp.freshen@freeze'(XA, XB, XFA),
    'lo.comp.freshen@freeze'(XR, XB, XFR).
'lo.comp.freshen@frze'('lo.comp.types#constrained'(XTp, XCon), XB, 'lo.comp.types#constrained'(XFTp, XFCon)):- 'lo.comp.freshen@freeze'(XTp, XB, XFTp),
    'lo.comp.freshen@freezeConstraint'(XCon, XB, XFCon).
'lo.comp.freshen@freeze'(XT, XB, XFrzn):- 'lo.comp.freshen@one174'(XFrzn, XB, XXd27141, XT).
'lo.comp.freshen@reQuant'('lo.core#[]', X_21951, XT, XT).
'lo.comp.freshen@reQuant'('lo.core#,..'('()2'(X_21953, XTp), XR), XBB, XFT, XFZT):- 'lo.comp.types@deRef'(XTp, XXd27142),
    XV = XXd27142,
    'lo.comp.types@isUnbound'(XV),
    'lo.comp.freshen@reQuant'(XR, XBB, XFT, XFZT).
'lo.comp.freshen@reQuant'('lo.core#,..'(X_21955, XB), XBB, XT, XFT):- 'lo.comp.freshen@reQuant'(XB, XBB, XT, XFT).
'lo.comp.freshen@freezeType'(XTp, XB, XFrZ):- 'lo.comp.freshen@freeze'(XTp, XB, XFT),
    ocall('pairs%1'(XXV3409),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%2'(XB, XXe3193),XXV3409,XXV3409),
    'lo.comp.freshen@reQuant'(XXe3193, XB, XFT, XFrZ).
'lo.comp.freshen@freezeConstraints'(XC, X_21956, XT, XT):- 'lo.comp.freshen@one175'(XC).
'lo.comp.freshen@freezeConstraints'('lo.core#,..'(XC, X_21958), X_21959, XT, XT):- 'lo.comp.freshen@one176'(XC).
'lo.comp.freshen@freezeConstraints'('lo.core#,..'(XC, XR), XBB, XT, XFT):- 'lo.comp.freshen@freezeConstraint'(XC, XBB, XFC),
    'lo.comp.freshen@freezeConstraints'(XR, XBB, 'lo.comp.types#constrained'(XT, XFC), XFT).
'lo.comp.freshen^frshnTypes'('_call%3'(XV20530, XV20531, XV20532), 'lo.comp.freshen^frshnTypes', _):- 'lo.comp.freshen@frshnTypes'(XV20530, XV20531, XV20532).
'lo.comp.freshen^frshnFields'('_call%3'(XV20533, XV20534, XV20535), 'lo.comp.freshen^frshnFields', _):- 'lo.comp.freshen@frshnFields'(XV20533, XV20534, XV20535).
'lo.comp.freshen^frshnConstraint'('_call%3'(XV20536, XV20537, XV20538), 'lo.comp.freshen^frshnConstraint', _):- 'lo.comp.freshen@frshnConstraint'(XV20536, XV20537, XV20538).
'lo.comp.freshen^frshn'('_call%3'(XV20539, XV20540, XV20541), 'lo.comp.freshen^frshn', _):- 'lo.comp.freshen@frshn'(XV20539, XV20540, XV20541).
'lo.comp.freshen^frshnType'('_call%3'(XV20542, XV20543, XV20544), 'lo.comp.freshen^frshnType', _):- 'lo.comp.freshen@frshnType'(XV20542, XV20543, XV20544).
'lo.comp.freshen^skolemFun'('_call%3'(XV20545, XV20546, XV20547), 'lo.comp.freshen^skolemFun', _):- 'lo.comp.freshen@skolemFun'(XV20545, XV20546, XV20547).
'lo.comp.freshen^skolem'('_call%2'(XV20548, XV20549), 'lo.comp.freshen^skolem', _):- 'lo.comp.freshen@skolem'(XV20548, XV20549).
'lo.comp.freshen^skolemizeType'('_call%3'(XV20550, XV20551, XV20552), 'lo.comp.freshen^skolemizeType', _):- 'lo.comp.freshen@skolemizeType'(XV20550, XV20551, XV20552).
'lo.comp.freshen^freshenType'('_call%3'(XV20553, XV20554, XV20555), 'lo.comp.freshen^freshenType', _):- 'lo.comp.freshen@freshenType'(XV20553, XV20554, XV20555).
'lo.comp.freshen$freshen$lo.comp.types*tipe^freshen'('_call%3'(XV20556, XV20557, XV20558), 'lo.comp.freshen$freshen$lo.comp.types*tipe^freshen'(XLbV1831, XThV1831), _):- 'lo.comp.freshen$freshen$lo.comp.types*tipe@freshen'(XV20556, XV20557, XV20558, XLbV1831, XThV1831).
'lo.comp.freshen$freshen$lo.comp.types*tipe^skolemize'('_call%3'(XV20562, XV20563, XV20564), 'lo.comp.freshen$freshen$lo.comp.types*tipe^skolemize'(XLbV1831, XThV1831), _):- 'lo.comp.freshen$freshen$lo.comp.types*tipe@skolemize'(XV20562, XV20563, XV20564, XLbV1831, XThV1831).
'lo.comp.freshen^skolemizeConstraint'('_call%3'(XV20568, XV20569, XV20570), 'lo.comp.freshen^skolemizeConstraint', _):- 'lo.comp.freshen@skolemizeConstraint'(XV20568, XV20569, XV20570).
'lo.comp.freshen^freshenConstraint'('_call%3'(XV20571, XV20572, XV20573), 'lo.comp.freshen^freshenConstraint', _):- 'lo.comp.freshen@freshenConstraint'(XV20571, XV20572, XV20573).
'lo.comp.freshen$freshen$lo.comp.types*constraint^freshen'('_call%3'(XV20574, XV20575, XV20576), 'lo.comp.freshen$freshen$lo.comp.types*constraint^freshen'(XLbV1832, XThV1832), _):- 'lo.comp.freshen$freshen$lo.comp.types*constraint@freshen'(XV20574, XV20575, XV20576, XLbV1832, XThV1832).
'lo.comp.freshen$freshen$lo.comp.types*constraint^skolemize'('_call%3'(XV20580, XV20581, XV20582), 'lo.comp.freshen$freshen$lo.comp.types*constraint^skolemize'(XLbV1832, XThV1832), _):- 'lo.comp.freshen$freshen$lo.comp.types*constraint@skolemize'(XV20580, XV20581, XV20582, XLbV1832, XThV1832).
'lo.comp.freshen^freezeTypes'('_call%3'(XV20586, XV20587, XV20588), 'lo.comp.freshen^freezeTypes', _):- 'lo.comp.freshen@freezeTypes'(XV20586, XV20587, XV20588).
'lo.comp.freshen^freezeFields'('_call%3'(XV20589, XV20590, XV20591), 'lo.comp.freshen^freezeFields', _):- 'lo.comp.freshen@freezeFields'(XV20589, XV20590, XV20591).
'lo.comp.freshen^freezeConstraint'('_call%3'(XV20592, XV20593, XV20594), 'lo.comp.freshen^freezeConstraint', _):- 'lo.comp.freshen@freezeConstraint'(XV20592, XV20593, XV20594).
'lo.comp.freshen^frze'('_call%3'(XV20595, XV20596, XV20597), 'lo.comp.freshen^frze', _):- 'lo.comp.freshen@frze'(XV20595, XV20596, XV20597).
'lo.comp.freshen@one174'(XFrzn, XB, XXd27141, XT):- 'lo.comp.types@deRef'(XT, XXd27141),
    'lo.comp.freshen@frze'(XXd27141, XB, XFrzn),
    !.
'lo.comp.freshen^freeze'('_call%3'(XV20598, XV20599, XV20600), 'lo.comp.freshen^freeze', _):- 'lo.comp.freshen@freeze'(XV20598, XV20599, XV20600).
'lo.comp.freshen^reQuant'('_call%4'(XV20601, XV20602, XV20603, XV20604), 'lo.comp.freshen^reQuant', _):- 'lo.comp.freshen@reQuant'(XV20601, XV20602, XV20603, XV20604).
'lo.comp.freshen^freezeType'('_call%3'(XV20605, XV20606, XV20607), 'lo.comp.freshen^freezeType', _):- 'lo.comp.freshen@freezeType'(XV20605, XV20606, XV20607).
'lo.comp.freshen@one175'(XC):- 'var'(XC),
    !.
'lo.comp.freshen@one176'(XC):- 'var'(XC),
    !.
'lo.comp.freshen^freezeConstraints'('_call%4'(XV20608, XV20609, XV20610, XV20611), 'lo.comp.freshen^freezeConstraints', _):- 'lo.comp.freshen@freezeConstraints'(XV20608, XV20609, XV20610, XV20611).
