'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.unify'e'*'n15o15'()15'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.freshen'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dict'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I3'sameType'PT3t'lo.comp.types*tipe't'lo.comp.types*tipe'Lt'lo.comp.dict*env''sameContract'PT3t'lo.comp.types*constraint't'lo.comp.types*constraint'Lt'lo.comp.dict*env''faceOfType'PT3t'lo.comp.types*tipe'Lt'lo.comp.dict*env't'lo.comp.types*tipe'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.unify@init'() :- !.
'lo.comp.unify@varBinding'(XT1, XT2, X_1599) :- 'lo.comp.types@isIdenticalVar'(XT1, XT2).
'lo.comp.unify@varBinding'(XV1, XV2, XEnv) :- 'lo.comp.unify@neg29'(XT2, XT1),
    'lo.comp.types@bind'(XV1, XV2).
'lo.comp.unify@checkBinding'(XV, XTp, X_1600) :- 'lo.comp.types@bind'(XV, XTp).
'lo.comp.unify@smFields'('lo.core#[]', X_1601, X_1602).
'lo.comp.unify@smFields'('lo.core#,..'((XF, XE1), XL1), XL2, XEnv) :- ocall('in%2'((XF, XE2), XL2),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.unify@sameType'(XE1, XE2, XEnv),
    'lo.comp.unify@smFields'(XL1, XL2, XEnv).
'lo.comp.unify@smList'('lo.core#[]', 'lo.core#[]', X_1603).
'lo.comp.unify@smList'('lo.core#,..'(XE1, XL1), 'lo.core#,..'(XE2, XL2), XEnv) :- 'lo.comp.unify@sameType'(XE1, XE2, XEnv),
    'lo.comp.unify@smList'(XL1, XL2, XEnv).
'lo.comp.unify@sm'(X_1604, 'lo.comp.types#anonType', X_1605).
'lo.comp.unify@sm'('lo.comp.types#anonType', X_1606, X_1607).
'lo.comp.unify@sm'('lo.comp.types#voidType', 'lo.comp.types#voidType', X_1608).
'lo.comp.unify@sm'('lo.comp.types#thisType', 'lo.comp.types#thisType', X_1609).
'lo.comp.unify@sm'('lo.comp.types#thisType', XT2, XEnv) :- 'lo.comp.dict@isVar'("this", XEnv, 'lo.comp.dict#vr'(X_1610, XT)),
    'lo.comp.unify@sameType'(XT, XT2, XEnv).
'lo.comp.unify@sm'(XT1, 'lo.comp.types#thisType', XEnv) :- 'lo.comp.dict@isVar'("this", XEnv, 'lo.comp.dict#vr'(X_1611, XT)),
    'lo.comp.unify@sameType'(XT, XT2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#kVar'(XNm), 'lo.comp.types#kVar'(XNm), X_1612).
'lo.comp.unify@sm'('lo.comp.types#kFun'(XNm, XAr), 'lo.comp.types#kFun'(XNm, XAr), X_1613).
'lo.comp.unify@sm'(XV1, XV2, XEnv) :- 'lo.comp.types@isUnbound'(XV1),
    'lo.comp.types@isUnbound'(XV2),
    'lo.comp.unify@varBinding'(XV1, XV2, XEnv).
'lo.comp.unify@sm'(XV1, XT2, XEnv) :- 'lo.comp.types@isUnbound'(XV1),
    'lo.comp.unify@checkBinding'(XV1, XT2, XEnv).
'lo.comp.unify@sm'(XT1, XV2, XEnv) :- 'lo.comp.types@isUnbound'(XV2),
    'lo.comp.unify@checkBinding'(XV2, XT1, XEnv).
'lo.comp.unify@sm'('lo.comp.types#tipe'(XNm), 'lo.comp.types#tipe'(XNm), X_1614).
'lo.comp.unify@sm'('lo.comp.types#tpFun'(XNm, XAr), 'lo.comp.types#tpFun'(XNm, XAr), X_1615).
'lo.comp.unify@sm'('lo.comp.types#typeExp'(XO1, XA1), 'lo.comp.types#typeExp'(XO2, XA2), XEnv) :- 'lo.comp.types@deRef'(XO1, XX24479),
    'lo.comp.types@deRef'(XO2, XX24481),
    'lo.comp.unify@sm'(XX24479, XX24481, XEnv),
    'lo.comp.unify@smList'(XA1, XA2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#funType'(XA1, XR1), 'lo.comp.types#funType'(XA2, XR2), XEnv) :- 'lo.comp.unify@sameType'(XA1, XA2, XEnv),
    'lo.comp.unify@sameType'(XR1, XR2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#grammarType'(XA1, XR1), 'lo.comp.types#grammarType'(XA2, XR2), XEnv) :- 'lo.comp.unify@sameType'(XA1, XA2, XEnv),
    'lo.comp.unify@sameType'(XR1, XR2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#predType'(XA1), 'lo.comp.types#predType'(XA2), XEnv) :- 'lo.comp.unify@sameType'(XA1, XA2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#tupleType'(XA1), 'lo.comp.types#tupleType'(XA2), XEnv) :- 'lo.comp.unify@smList'(XA1, XA2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#classType'(XA1, XC1), 'lo.comp.types#classType'(XA2, XC2), XEnv) :- 'lo.comp.unify@sameType'(XA1, XA2, XEnv),
    'lo.comp.unify@sameType'(XC1, XC2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#faceType'(XF1), 'lo.comp.types#faceType'(XF2), XEnv) :- 'lo.list@length'(XF1, XX24547),
    'lo.list@length'(XF2, XX24549),
    XX24547 = XX24549,
    'lo.comp.unify@smFields'(XF1, XF2, XEnv).
'lo.comp.unify@sameType'(XT1, XT2, XEnv) :- 'lo.comp.unify@one29'(XEnv, XX24559, XT2, XX24557, XT1).
'lo.comp.unify@surfaceBound'('lo.core#[]').
'lo.comp.unify@surfaceBound'('lo.core#,..'(XE, XM)) :- 'lo.comp.unify@neg30'(XX24566, XE),
    'lo.comp.unify@surfaceBound'(XM).
'lo.comp.unify@sameContract'('lo.comp.types#conTract'(XNm, XA1, XD1), 'lo.comp.types#conTract'(XNm, XA2, XD2), XEnv) :- 'lo.comp.unify@smList'(XA1, XA2, XEnv),
    'lo.comp.unify@smList'(XD1, XD2, XEnv).
'lo.comp.unify@checkForImpl'(XCon, XEnv) :- 'lo.comp.dict@isImplemented'(XCon, XEnv, 'lo.comp.types#implEntry'(X_1616, XITp)),
    ocall('_empty%1'(XXV43),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('freshen%3'(XITp, XXV43, XX24595),'lo.comp.freshen$freshen$lo.comp.types*constraint','lo.comp.freshen$freshen$lo.comp.types*constraint'),
    (X_1617, XOCon) = XX24595,
    'lo.comp.unify@sameContract'(XOCon, XCon, XEnv).
'lo.comp.unify@checkFace'('lo.core#[]', X_1618, X_1619).
'lo.comp.unify@checkFace'('lo.core#,..'((XNm, XElTp), XR), XTpFace, XEnv) :- ocall('in%2'((XNm, XXTp), XTpFace),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.unify@sameType'(XElTp, XXTp, XEnv),
    'lo.comp.unify@checkFace'(XR, XTpFace, XEnv).
'lo.comp.unify@mergeFields'('lo.core#[]', X_1620, XSoFar, XSoFar) :- !.
'lo.comp.unify@mergeFields'('lo.core#,..'((XNm, XF), XEls), XEnv, XSoFar, XX24639) :- ocall('in%2'((XNm, XV), XSoFar),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.unify@sameType'(XF, XV, XEnv),
    !,
    'lo.comp.unify@mergeFields'(XEls, XEnv, XSoFar, XX24639).
'lo.comp.unify@mergeFields'('lo.core#,..'((XNm, XF), XEls), XEnv, XSoFar, XX24652) :- !,
    'lo.comp.unify@mergeFields'(XEls, XEnv, 'lo.core#,..'((XNm, XF), XSoFar), XX24652).
'lo.comp.unify@mergeFields'(_, _, _, _) :- raise_exception('error'("mergeFields", 103, 3, 32)).
'lo.comp.unify@mergeCons'('lo.core#[]', X_1621, X_1622, XSo, XSo) :- !.
'lo.comp.unify@mergeCons'('lo.core#,..'('lo.comp.types#implementsFace'(XV, XF), XC), XEnv, XTp, XSo, XX24675) :- 'lo.comp.types@isIdenticalType'(XV, XTp),
    !,
    'lo.comp.unify@mergeFields'(XF, XEnv, XSo, XX24674),
    'lo.comp.unify@mergeCons'(XC, XEnv, XTp, XX24674, XX24675).
'lo.comp.unify@mergeCons'('lo.core#,..'(X_1623, XC), XEnv, XTp, XSo, XX24686) :- !,
    'lo.comp.unify@mergeCons'(XC, XEnv, XTp, XSo, XX24686).
'lo.comp.unify@mergeCons'(_, _, _, _, _) :- raise_exception('error'("mergeCons", 98, 3, 26)).
'lo.comp.unify@allImpCons'('lo.core#[]', X_1624, X_1625, XSo, XSo) :- !.
'lo.comp.unify@allImpCons'('lo.core#,..'('lo.comp.dict#scope'(X_1626, X_1627, XCons, X_1628, X_1629), XE), XEnv, XTp, XSo, XX24711) :- !,
    'lo.comp.unify@mergeCons'(XCons, XEnv, XTp, XSo, XX24710),
    'lo.comp.unify@allImpCons'(XE, XEnv, XTp, XX24710, XX24711).
'lo.comp.unify@allImpCons'(_, _, _, _, _) :- raise_exception('error'("allImpCons", 94, 3, 27)).
'lo.comp.unify@faceOfType'('lo.comp.types#faceType'(XF), X_1630, 'lo.comp.types#faceType'(XF)).
'lo.comp.unify@faceOfType'('lo.comp.types#tipe'(XNm), XEnv, XF) :- 'lo.comp.dict@typeInDict'(XNm, XEnv, XFR),
    'lo.comp.types@moveQuants'(XFR, X_1631, XFQR),
    'lo.comp.types@moveConstraints'(XFQR, X_1632, 'lo.comp.types#typeRule'(X_1633, XF)).
'lo.comp.unify@faceOfType'('lo.comp.types#typeExp'(XOp, XArgs), XEnv, XFace) :- 'lo.comp.types@deRef'(XOp, XX24738),
    XX24738 = 'lo.comp.types#tpFun'(XNm, XAr),
    'lo.list@length'(XArgs, XX24744),
    XAr = XX24744,
    'lo.comp.dict@typeInDict'(XNm, XEnv, XFR),
    'lo.comp.types@moveQuants'(XFR, X_1634, XFQR),
    'lo.comp.types@moveConstraints'(XFQR, X_1635, 'lo.comp.types#typeRule'('lo.comp.types#typeExp'(X_1636, XAT), XF)),
    ocall('_empty%1'(XXV44),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@bindAT'(XAT, XArgs, XXV44, XX24765),
    ocall('freshen%3'(XF, XX24765, XX24766),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    (X_1637, XFace) = XX24766.
'lo.comp.unify@faceOfType'(XT, XEnv, 'lo.comp.types#faceType'(XX24774)) :- 'lo.comp.types@isTypeVar'(XT),
    'lo.comp.unify@allImpCons'(XEnv, XEnv, XT, 'lo.core#[]', XX24774).
'lo.comp.unify@neg29'(XT2, XT1) :- 'lo.comp.types@isIdenticalVar'(XT1, XT2),
    !,
    fail.
'lo.comp.unify@neg29'(XT2, XT1).
'lo.comp.unify^varBinding'('_call%3'(XV3495, XV3496, XV3497), 'lo.comp.unify^varBinding', _) :- 'lo.comp.unify@varBinding'(XV3495, XV3496, XV3497).
'lo.comp.unify^checkBinding'('_call%3'(XV3498, XV3499, XV3500), 'lo.comp.unify^checkBinding', _) :- 'lo.comp.unify@checkBinding'(XV3498, XV3499, XV3500).
'lo.comp.unify^smFields'('_call%3'(XV3501, XV3502, XV3503), 'lo.comp.unify^smFields', _) :- 'lo.comp.unify@smFields'(XV3501, XV3502, XV3503).
'lo.comp.unify^smList'('_call%3'(XV3504, XV3505, XV3506), 'lo.comp.unify^smList', _) :- 'lo.comp.unify@smList'(XV3504, XV3505, XV3506).
'lo.comp.unify^sm'('_call%3'(XV3507, XV3508, XV3509), 'lo.comp.unify^sm', _) :- 'lo.comp.unify@sm'(XV3507, XV3508, XV3509).
'lo.comp.unify@one29'(XEnv, XX24559, XT2, XX24557, XT1) :- 'lo.comp.types@deRef'(XT1, XX24557),
    'lo.comp.types@deRef'(XT2, XX24559),
    'lo.comp.unify@sm'(XX24557, XX24559, XEnv),
    !.
'lo.comp.unify^sameType'('_call%3'(XV3510, XV3511, XV3512), 'lo.comp.unify^sameType', _) :- 'lo.comp.unify@sameType'(XV3510, XV3511, XV3512).
'lo.comp.unify@neg30'(XX24566, XE) :- 'lo.comp.types@deRef'(XE, XX24566),
    'lo.comp.types@isUnbound'(XX24566),
    !,
    fail.
'lo.comp.unify@neg30'(XX24566, XE).
'lo.comp.unify^surfaceBound'('_call%1'(XV3513), 'lo.comp.unify^surfaceBound', _) :- 'lo.comp.unify@surfaceBound'(XV3513).
'lo.comp.unify^sameContract'('_call%3'(XV3514, XV3515, XV3516), 'lo.comp.unify^sameContract', _) :- 'lo.comp.unify@sameContract'(XV3514, XV3515, XV3516).
'lo.comp.unify^checkForImpl'('_call%2'(XV3517, XV3518), 'lo.comp.unify^checkForImpl', _) :- 'lo.comp.unify@checkForImpl'(XV3517, XV3518).
'lo.comp.unify^checkFace'('_call%3'(XV3519, XV3520, XV3521), 'lo.comp.unify^checkFace', _) :- 'lo.comp.unify@checkFace'(XV3519, XV3520, XV3521).
'lo.comp.unify^mergeFields'('_call%4'(XV3522, XV3523, XV3524, XV3525), 'lo.comp.unify^mergeFields', _) :- 'lo.comp.unify@mergeFields'(XV3522, XV3523, XV3524, XV3525).
'lo.comp.unify^mergeCons'('_call%5'(XV3526, XV3527, XV3528, XV3529, XV3530), 'lo.comp.unify^mergeCons', _) :- 'lo.comp.unify@mergeCons'(XV3526, XV3527, XV3528, XV3529, XV3530).
'lo.comp.unify^allImpCons'('_call%5'(XV3531, XV3532, XV3533, XV3534, XV3535), 'lo.comp.unify^allImpCons', _) :- 'lo.comp.unify@allImpCons'(XV3531, XV3532, XV3533, XV3534, XV3535).
'lo.comp.unify^faceOfType'('_call%3'(XV3536, XV3537, XV3538), 'lo.comp.unify^faceOfType', _) :- 'lo.comp.unify@faceOfType'(XV3536, XV3537, XV3538).
