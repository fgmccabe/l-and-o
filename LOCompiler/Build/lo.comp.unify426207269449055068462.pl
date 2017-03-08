'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.unify's'0.0.1'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dict'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.freshen'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'s\"I3'sameType'PT3t'lo.comp.types*tipe't'lo.comp.types*tipe'Lt'lo.comp.dict*env''sameContract'PT3t'lo.comp.types*constraint't'lo.comp.types*constraint'Lt'lo.comp.dict*env''faceOfType'PT3t'lo.comp.types*tipe'Lt'lo.comp.dict*env't'lo.comp.types*tipe'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.unify@init'():- !.
'lo.comp.unify@checkBinding'(XV, XTp, X_35012):- 'lo.comp.types@bind'(XV, XTp).
'lo.comp.unify@varBinding'(XT1, XT2, X_35013):- 'lo.comp.types@isIdenticalVar'(XT1, XT2).
'lo.comp.unify@varBinding'(XV1, XV2, XEnv):- 'lo.comp.unify@neg331'(XT2, XT1),
    'lo.comp.types@bind'(XV1, XV2).
'lo.comp.unify@smList'('lo.core#[]', 'lo.core#[]', X_35014).
'lo.comp.unify@smList'('lo.core#,..'(XE1, XL1), 'lo.core#,..'(XE2, XL2), XEnv):- 'lo.comp.unify@sameType'(XE1, XE2, XEnv),
    'lo.comp.unify@smList'(XL1, XL2, XEnv).
'lo.comp.unify@smFields'('lo.core#[]', X_35017, X_35018).
'lo.comp.unify@smFields'('lo.core#,..'('()2'(XF, XE1), XL1), XL2, XEnv):- ocall('in%2'('()2'(XF, XE2), XL2),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.unify@sameType'(XE1, XE2, XEnv),
    'lo.comp.unify@smFields'(XL1, XL2, XEnv).
'lo.comp.unify@sm'(X_35020, 'lo.comp.types#anonType', X_35021).
'lo.comp.unify@sm'('lo.comp.types#anonType', X_35022, X_35023).
'lo.comp.unify@sm'('lo.comp.types#voidType', 'lo.comp.types#voidType', X_35024).
'lo.comp.unify@sm'('lo.comp.types#thisType', 'lo.comp.types#thisType', X_35025).
'lo.comp.unify@sm'('lo.comp.types#thisType', XT2, XEnv):- 'lo.comp.dict@isVar'("this", XEnv, 'lo.comp.dict#vr'(X_35026, XT)),
    'lo.comp.unify@sameType'(XT, XT2, XEnv).
'lo.comp.unify@sm'(XT1, 'lo.comp.types#thisType', XEnv):- 'lo.comp.dict@isVar'("this", XEnv, 'lo.comp.dict#vr'(X_35027, XT)),
    'lo.comp.unify@sameType'(XT, XT2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#kVar'(XNm), 'lo.comp.types#kVar'(XNm), X_35028).
'lo.comp.unify@sm'('lo.comp.types#kFun'(XNm, XAr), 'lo.comp.types#kFun'(XNm, XAr), X_35029).
'lo.comp.unify@sm'(XV1, XV2, XEnv):- 'lo.comp.types@isUnbound'(XV1),
    'lo.comp.types@isUnbound'(XV2),
    'lo.comp.unify@varBinding'(XV1, XV2, XEnv).
'lo.comp.unify@sm'(XV1, XT2, XEnv):- 'lo.comp.types@isUnbound'(XV1),
    'lo.comp.unify@checkBinding'(XV1, XT2, XEnv).
'lo.comp.unify@sm'(XT1, XV2, XEnv):- 'lo.comp.types@isUnbound'(XV2),
    'lo.comp.unify@checkBinding'(XV2, XT1, XEnv).
'lo.comp.unify@sm'('lo.comp.types#tipe'(XNm), 'lo.comp.types#tipe'(XNm), X_35030).
'lo.comp.unify@sm'('lo.comp.types#tpFun'(XNm, XAr), 'lo.comp.types#tpFun'(XNm, XAr), X_35031).
'lo.comp.unify@sm'('lo.comp.types#typeExp'(XO1, XA1), 'lo.comp.types#typeExp'(XO2, XA2), XEnv):- 'lo.comp.types@deRef'(XO1, XXd39844),
    'lo.comp.types@deRef'(XO2, XXd39845),
    'lo.comp.unify@sm'(XXd39844, XXd39845, XEnv),
    'lo.comp.unify@smList'(XA1, XA2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#funType'(XA1, XR1), 'lo.comp.types#funType'(XA2, XR2), XEnv):- 'lo.comp.unify@sameType'(XA1, XA2, XEnv),
    'lo.comp.unify@sameType'(XR1, XR2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#grammarType'(XA1, XR1), 'lo.comp.types#grammarType'(XA2, XR2), XEnv):- 'lo.comp.unify@sameType'(XA1, XA2, XEnv),
    'lo.comp.unify@sameType'(XR1, XR2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#predType'(XA1), 'lo.comp.types#predType'(XA2), XEnv):- 'lo.comp.unify@sameType'(XA1, XA2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#tupleType'(XA1), 'lo.comp.types#tupleType'(XA2), XEnv):- 'lo.comp.unify@smList'(XA1, XA2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#classType'(XA1, XC1), 'lo.comp.types#classType'(XA2, XC2), XEnv):- 'lo.comp.unify@sameType'(XA1, XA2, XEnv),
    'lo.comp.unify@sameType'(XC1, XC2, XEnv).
'lo.comp.unify@sm'('lo.comp.types#faceType'(XF1), 'lo.comp.types#faceType'(XF2), XEnv):- 'lo.list@length'(XF1, XXd39846),
    'lo.list@length'(XF2, XXd39847),
    XXd39846 = XXd39847,
    'lo.comp.unify@smFields'(XF1, XF2, XEnv).
'lo.comp.unify@sameType'(XT1, XT2, XEnv):- 'lo.comp.unify@one296'(XEnv, XXd39849, XT2, XXd39848, XT1).
'lo.comp.unify@surfaceBound'('lo.core#[]').
'lo.comp.unify@surfaceBound'('lo.core#,..'(XE, XM)):- 'lo.comp.unify@neg332'(XXd39850, XE),
    'lo.comp.unify@surfaceBound'(XM).
'lo.comp.unify@sameContract'('lo.comp.types#conTract'(XNm, XA1, XD1), 'lo.comp.types#conTract'(XNm, XA2, XD2), XEnv):- 'lo.comp.unify@smList'(XA1, XA2, XEnv),
    'lo.comp.unify@smList'(XD1, XD2, XEnv).
'lo.comp.unify@checkForImpl'(XCon, XEnv):- 'lo.comp.dict@isImplemented'(XCon, XEnv, 'lo.comp.types#implEntry'(X_35033, XITp)),
    ocall('freshen%1'(XXV5443),'lo.comp.freshen$freshen$lo.comp.types*constraint','lo.comp.freshen$freshen$lo.comp.types*constraint'),
    ocall('_empty%1'(XXV5442),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XITp, XXV5442, XXe5076),XXV5443,XXV5443),
    '()2'(X_35034, XOCon) = XXe5076,
    'lo.comp.unify@sameContract'(XOCon, XCon, XEnv).
'lo.comp.unify@checkFace'('lo.core#[]', X_35035, X_35036).
'lo.comp.unify@checkFace'('lo.core#,..'('()2'(XNm, XElTp), XR), XTpFace, XEnv):- ocall('in%2'('()2'(XNm, XXTp), XTpFace),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.unify@sameType'(XElTp, XXTp, XEnv),
    'lo.comp.unify@checkFace'(XR, XTpFace, XEnv).
'lo.comp.unify@mergeFields'('lo.core#[]', X_35038, XSoFar, XSoFar):- !.
'lo.comp.unify@mergeFields'('lo.core#,..'('()2'(XNm, XF), XEls), XEnv, XSoFar, XXd39853):- ocall('in%2'('()2'(XNm, XV), XSoFar),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.unify@sameType'(XF, XV, XEnv),
    !,
    'lo.comp.unify@mergeFields'(XEls, XEnv, XSoFar, XXd39853).
'lo.comp.unify@mergeFields'('lo.core#,..'('()2'(XNm, XF), XEls), XEnv, XSoFar, XXd39855):- !,
    'lo.comp.unify@mergeFields'(XEls, XEnv, 'lo.core#,..'('()2'(XNm, XF), XSoFar), XXd39855).
'lo.comp.unify@mergeFields'(_, _, _, _):- raise_exception('error'("lo.comp.unify@mergeFields", 103, 3, 32)).
'lo.comp.unify@mergeCons'('lo.core#[]', X_35042, X_35043, XSo, XSo):- !.
'lo.comp.unify@mergeCons'('lo.core#,..'('lo.comp.types#implementsFace'(XV, XF), XC), XEnv, XTp, XSo, XXd39857):- 'lo.comp.types@isIdenticalType'(XV, XTp),
    !,
    'lo.comp.unify@mergeFields'(XF, XEnv, XSo, XXd39856),
    'lo.comp.unify@mergeCons'(XC, XEnv, XTp, XXd39856, XXd39857).
'lo.comp.unify@mergeCons'('lo.core#,..'(X_35046, XC), XEnv, XTp, XSo, XXd39858):- !,
    'lo.comp.unify@mergeCons'(XC, XEnv, XTp, XSo, XXd39858).
'lo.comp.unify@mergeCons'(_, _, _, _, _):- raise_exception('error'("lo.comp.unify@mergeCons", 98, 3, 26)).
'lo.comp.unify@allImpCons'('lo.core#[]', X_35047, X_35048, XSo, XSo):- !.
'lo.comp.unify@allImpCons'('lo.core#,..'('lo.comp.dict#scope'(X_35050, X_35051, XCons, X_35052, X_35053), XE), XEnv, XTp, XSo, XXd39860):- !,
    'lo.comp.unify@mergeCons'(XCons, XEnv, XTp, XSo, XXd39859),
    'lo.comp.unify@allImpCons'(XE, XEnv, XTp, XXd39859, XXd39860).
'lo.comp.unify@allImpCons'(_, _, _, _, _):- raise_exception('error'("lo.comp.unify@allImpCons", 94, 3, 27)).
'lo.comp.unify@faceOfType'('lo.comp.types#faceType'(XF), X_35054, 'lo.comp.types#faceType'(XF)).
'lo.comp.unify@faceOfType'('lo.comp.types#tipe'(XNm), XEnv, XF):- 'lo.comp.dict@typeInDict'(XNm, XEnv, XFR),
    'lo.comp.types@moveQuants'(XFR, X_35055, XFQR),
    'lo.comp.types@moveConstraints'(XFQR, X_35056, 'lo.comp.types#typeRule'(X_35057, XF)).
'lo.comp.unify@faceOfType'('lo.comp.types#typeExp'(XOp, XArgs), XEnv, XFace):- 'lo.comp.types@deRef'(XOp, XXd39862),
    XXd39862 = 'lo.comp.types#tpFun'(XNm, XAr),
    'lo.list@length'(XArgs, XXd39864),
    XAr = XXd39864,
    'lo.comp.dict@typeInDict'(XNm, XEnv, XFR),
    'lo.comp.types@moveQuants'(XFR, X_35058, XFQR),
    'lo.comp.types@moveConstraints'(XFQR, X_35059, 'lo.comp.types#typeRule'('lo.comp.types#typeExp'(X_35060, XAT), XF)),
    ocall('freshen%1'(XXV5445),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    ocall('_empty%1'(XXV5444),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@bindAT'(XAT, XArgs, XXV5444, XXd39868),
    ocall('_call%3'(XF, XXd39868, XXe5077),XXV5445,XXV5445),
    '()2'(X_35061, XFace) = XXe5077.
'lo.comp.unify@faceOfType'(XT, XEnv, 'lo.comp.types#faceType'(XXb20134)):- 'lo.comp.types@isTypeVar'(XT),
    'lo.comp.unify@allImpCons'(XEnv, XEnv, XT, 'lo.core#[]', XXb20134).
'lo.comp.unify^checkBinding'('_call%3'(XV31773, XV31774, XV31775), 'lo.comp.unify^checkBinding', _):- 'lo.comp.unify@checkBinding'(XV31773, XV31774, XV31775).
'lo.comp.unify@neg331'(XT2, XT1):- 'lo.comp.types@isIdenticalVar'(XT1, XT2),
    !,
    fail.
'lo.comp.unify@neg331'(XT2, XT1).
'lo.comp.unify^varBinding'('_call%3'(XV31776, XV31777, XV31778), 'lo.comp.unify^varBinding', _):- 'lo.comp.unify@varBinding'(XV31776, XV31777, XV31778).
'lo.comp.unify^smList'('_call%3'(XV31779, XV31780, XV31781), 'lo.comp.unify^smList', _):- 'lo.comp.unify@smList'(XV31779, XV31780, XV31781).
'lo.comp.unify^smFields'('_call%3'(XV31782, XV31783, XV31784), 'lo.comp.unify^smFields', _):- 'lo.comp.unify@smFields'(XV31782, XV31783, XV31784).
'lo.comp.unify^sm'('_call%3'(XV31785, XV31786, XV31787), 'lo.comp.unify^sm', _):- 'lo.comp.unify@sm'(XV31785, XV31786, XV31787).
'lo.comp.unify@one296'(XEnv, XXd39849, XT2, XXd39848, XT1):- 'lo.comp.types@deRef'(XT1, XXd39848),
    'lo.comp.types@deRef'(XT2, XXd39849),
    'lo.comp.unify@sm'(XXd39848, XXd39849, XEnv),
    !.
'lo.comp.unify^sameType'('_call%3'(XV31788, XV31789, XV31790), 'lo.comp.unify^sameType', _):- 'lo.comp.unify@sameType'(XV31788, XV31789, XV31790).
'lo.comp.unify@neg332'(XXd39850, XE):- 'lo.comp.types@deRef'(XE, XXd39850),
    'lo.comp.types@isUnbound'(XXd39850),
    !,
    fail.
'lo.comp.unify@neg332'(XXd39850, XE).
'lo.comp.unify^surfaceBound'('_call%1'(XV31791), 'lo.comp.unify^surfaceBound', _):- 'lo.comp.unify@surfaceBound'(XV31791).
'lo.comp.unify^sameContract'('_call%3'(XV31792, XV31793, XV31794), 'lo.comp.unify^sameContract', _):- 'lo.comp.unify@sameContract'(XV31792, XV31793, XV31794).
'lo.comp.unify^checkForImpl'('_call%2'(XV31795, XV31796), 'lo.comp.unify^checkForImpl', _):- 'lo.comp.unify@checkForImpl'(XV31795, XV31796).
'lo.comp.unify^checkFace'('_call%3'(XV31797, XV31798, XV31799), 'lo.comp.unify^checkFace', _):- 'lo.comp.unify@checkFace'(XV31797, XV31798, XV31799).
'lo.comp.unify^mergeFields'('_call%4'(XV31800, XV31801, XV31802, XV31803), 'lo.comp.unify^mergeFields', _):- 'lo.comp.unify@mergeFields'(XV31800, XV31801, XV31802, XV31803).
'lo.comp.unify^mergeCons'('_call%5'(XV31804, XV31805, XV31806, XV31807, XV31808), 'lo.comp.unify^mergeCons', _):- 'lo.comp.unify@mergeCons'(XV31804, XV31805, XV31806, XV31807, XV31808).
'lo.comp.unify^allImpCons'('_call%5'(XV31809, XV31810, XV31811, XV31812, XV31813), 'lo.comp.unify^allImpCons', _):- 'lo.comp.unify@allImpCons'(XV31809, XV31810, XV31811, XV31812, XV31813).
'lo.comp.unify^faceOfType'('_call%3'(XV31814, XV31815, XV31816), 'lo.comp.unify^faceOfType', _):- 'lo.comp.unify@faceOfType'(XV31814, XV31815, XV31816).
