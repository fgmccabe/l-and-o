'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.parseType's'0.0.1'n11o11'()11'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dict'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.unify'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.freshen'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'s\"I10'parseTypeBound'PT6t'lo.comp.ast*ast'Uz2'lo.index*map'2St'lo.comp.types*tipe'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.dict*env't'lo.comp.types*tipe't'lo.comp.types*tipe''parseConstraint'PT7t'lo.comp.ast*ast'Lt'lo.comp.dict*env'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.types*constraint'Lt'lo.comp.types*constraint't'lo.comp.errors*report't'lo.comp.errors*report''parseType'PT5t'lo.comp.ast*ast'Lt'lo.comp.dict*env't'lo.comp.types*tipe't'lo.comp.errors*report't'lo.comp.errors*report''parseTypeRule'PT7t'lo.comp.ast*ast'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.dict*env't'lo.comp.types*tipe'St'lo.comp.errors*report't'lo.comp.errors*report''parseConBound'PT6t'lo.comp.ast*ast'Uz2'lo.index*map'2St'lo.comp.types*tipe'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.dict*env't'lo.comp.types*constraint't'lo.comp.types*constraint''wrapConConstraints'PT3Lt'lo.comp.types*constraint't'lo.comp.types*constraint't'lo.comp.types*constraint''parseContract'PT6t'lo.comp.ast*ast'Lt'lo.comp.dict*env'St'lo.comp.types*conEntry't'lo.comp.errors*report't'lo.comp.errors*report''parseContractConstraint'PT6t'lo.comp.ast*ast'Lt'lo.comp.dict*env'St'lo.comp.types*constraint't'lo.comp.errors*report't'lo.comp.errors*report''parseTypeCore'PT3t'lo.comp.ast*ast't'lo.comp.types*tipe'S'rewriteConstraints'PT4Lt'lo.comp.types*constraint'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.types*constraint'Lt'lo.comp.types*constraint'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.parseType@init'():- !.
'lo.comp.parseType@validTypeOp'('lo.comp.types#tpFun'(X_35488, XAr), XAr).
'lo.comp.parseType@validTypeOp'('lo.comp.types#kFun'(X_35489, XAr), XAr).
'lo.comp.parseType@applyTypeExp'(XOp, XArgs, X_35490, 'lo.comp.types#typeExp'(XOp, XArgs), XRp, XRp):- 'lo.comp.parseType@one300'(XXd40331, XArgs, XOp).
'lo.comp.parseType@applyTypeExp'(XT, XArgs, XLc, 'lo.comp.types#voidType', XRp, XRpx):- ocall('disp%1'(XXV5468),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%1'(XXV5469),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.types*tipe'),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.types*tipe')),
    ocall('_call%2'(XT, XXe5094),XXV5468,XXV5468),
    ocall('_call%2'(XArgs, XXe5095),XXV5469,XXV5469),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot apply type exp "), 'lo.core#,..'(XXe5094, 'lo.core#,..'('lo.core#ss'(" to "), 'lo.core#,..'(XXe5095, 'lo.core#[]'))))), XXd40340),
    'lo.comp.errors@reportError'(XXd40340, XLc, XRp, XRpx).
'lo.comp.parseType@prsTypeName'(X_35495, "_", X_35496, X_35497, 'lo.comp.types#anonType', XRp, XRp).
'lo.comp.parseType@prsTypeName'(X_35498, "void", X_35499, X_35500, 'lo.comp.types#voidType', XRp, XRp).
'lo.comp.parseType@prsTypeName'(X_35501, "this", X_35502, X_35503, 'lo.comp.types#thisType', XRp, XRp).
'lo.comp.parseType@prsTypeName'(X_35504, XId, X_35505, XB, XTp, XRp, XRp):- ocall('present%3'(XB, XId, XTp),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.parseType@prsTypeName'(X_35506, XId, XEnv, X_35507, XTp, XRp, XRp):- 'lo.comp.dict@isType'(XId, XEnv, X_35508, XTp).
'lo.comp.parseType@prsTypeName'(XLc, XId, X_35509, X_35510, 'lo.comp.types#anonType', XRp, XRpx):- ocall('disp%1'(XXV5470),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XId, XXe5096),XXV5470,XXV5470),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type "), 'lo.core#,..'(XXe5096, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]')))), XXd40348),
    'lo.comp.errors@reportError'(XXd40348, XLc, XRp, XRpx).
'lo.comp.parseType@parseContractName'(X_35514, XId, XEnv, XCon, XRp, XRp):- 'lo.comp.dict@isContract'(XId, XEnv, 'lo.comp.types#conEntry'(X_35515, X_35516, XSpec, X_35517)),
    ocall('freshen%1'(XXV5472),'lo.comp.freshen$freshen$lo.comp.types*constraint','lo.comp.freshen$freshen$lo.comp.types*constraint'),
    ocall('_empty%1'(XXV5471),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XSpec, XXV5471, XXe5097),XXV5472,XXV5472),
    '()2'(X_35518, XCon) = XXe5097.
'lo.comp.parseType@parseContractName'(XLc, XId, X_35519, 'lo.comp.types#conTract'(XId, 'lo.core#[]', 'lo.core#[]'), XRp, XRpx):- ocall('disp%1'(XXV5473),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XId, XXe5098),XXV5473,XXV5473),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("contract "), 'lo.core#,..'(XXe5098, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]')))), XXd40357),
    'lo.comp.errors@reportError'(XXd40357, XLc, XRp, XRpx).
'lo.comp.parseType@addConstraint'(XCon, XC0, XC0):- 'lo.comp.parseType@one301'(XC0, XCon).
'lo.comp.parseType@addConstraint'(XCon, XC0, 'lo.core#,..'(XCon, XC0)):- 'lo.comp.parseType@neg337'(XC0, XCon).
'lo.comp.parseType@parseTypeBound'(XP, XBV, XBound, XEnv, XQT, XInner):- 'lo.comp.abstract@isBinary'(XP, ",", X_35524, XL, XR),
    'lo.comp.parseType@parseTypeBound'(XL, XBV, XB0, XEnv, XQT, XQ0),
    'lo.comp.parseType@parseTypeBound'(XR, XB0, XBound, XEnv, XQ0, XInner).
'lo.comp.parseType@parseTypeBound'(XV, XB, XB1, XEnv, 'lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XInner), XInner):- 'lo.comp.abstract@isBinary'(XV, "/", X_35525, XL, XR),
    'lo.comp.abstract@isInteger'(XR, X_35526, XAr),
    'lo.comp.abstract@isIden'(XL, X_35527, XNm),
    ocall('_put%1'(XXV5474),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XB, XNm, 'lo.comp.types#kFun'(XNm, XAr), XXe5099),XXV5474,XXV5474),
    XB1 = XXe5099.
'lo.comp.parseType@parseTypeBound'(XV, XB, XB1, XEnv, 'lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XInner), XInner):- 'lo.comp.abstract@isIden'(XV, X_35528, XNm),
    ocall('_put%1'(XXV5475),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XB, XNm, 'lo.comp.types#kVar'(XNm), XXe5100),XXV5475,XXV5475),
    XB1 = XXe5100.
'lo.comp.parseType@parseContractArgs'('lo.core#,..'(XA, 'lo.core#[]'), XEnv, XB, XC0, XCx, XArgs, XDeps, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XA, "->>", X_35530, XL, XR),
    'lo.comp.abstract@deComma'(XL, XXd40362),
    'lo.comp.parseType@prsTypes'(XXd40362, XEnv, XB, XC0, XC1, XArgs, XRp, XRp0),
    'lo.comp.abstract@deComma'(XR, XXd40363),
    'lo.comp.parseType@prsTypes'(XXd40363, XEnv, XB, XC1, XCx, XDeps, XRp0, XRpx).
'lo.comp.parseType@parseContractArgs'(XA, XEnv, XB, XC0, XCx, XArgs, 'lo.core#[]', XRp, XRpx):- 'lo.comp.parseType@prsTypes'(XA, XEnv, XB, XC0, XCx, XArgs, XRp, XRpx).
'lo.comp.parseType@parseConstraint'(XT, XEnv, XB, XC0, XCx, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XT, ",", X_35531, XL, XR),
    'lo.comp.parseType@parseConstraint'(XR, XEnv, XB, XC0, XC1, XRp, XRp0),
    'lo.comp.parseType@parseConstraint'(XL, XEnv, XB, XC1, XCx, XRp0, XRpx).
'lo.comp.parseType@parseConstraint'(XT, XEnv, XB, XC0, XCx, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XT, "<~", X_35532, XL, XR),
    'lo.comp.parseType@prsType'(XL, XEnv, XB, XC0, XC1, XTV, XRp, XRp0),
    'lo.comp.abstract@isBraceTuple'(XR, X_35533, XE),
    'lo.comp.parseType@prsTypeFields'(XE, XEnv, XB, XC1, XC2, XAT, XRp0, XRpx),
    'lo.comp.parseType@addConstraint'('lo.comp.types#implementsFace'(XTV, XAT), XC2, XCx).
'lo.comp.parseType@parseConstraint'(XSq, XEnv, XB, XC0, XCx, XRp, XRpx):- 'lo.comp.abstract@isSquareTerm'(XSq, XLc, XOp, XArgs),
    'lo.comp.abstract@isIden'(XOp, XLc0, XNm),
    'lo.comp.parseType@parseContractName'(XLc0, XNm, XEnv, 'lo.comp.types#conTract'(XConNm, X_ATs, X_Dps), XRp, XRp0),
    'lo.comp.parseType@parseContractArgs'(XArgs, XEnv, XB, XC0, XC1, XArgTps, XDeps, XRp0, XRpx),
    'lo.comp.parseType@addConstraint'('lo.comp.types#conTract'(XConNm, XArgTps, XDeps), XC1, XCx).
'lo.comp.parseType@parseConstraint'(XT, X_35534, XB, XC, XC, XRp, XRpx):- ocall('disp%1'(XXV5476),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XT, XXe5101),XXV5476,XXV5476),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("invalid type constraint "), 'lo.core#,..'(XXe5101, 'lo.core#[]'))), XXd40371),
    ocall('loc%1'(XXV5477),XT,XT),
    'lo.comp.errors@reportError'(XXd40371, XXV5477, XRp, XRpx).
'lo.comp.parseType@prsTypes'('lo.core#[]', X_35537, X_35538, XC, XC, 'lo.core#[]', XRp, XRp).
'lo.comp.parseType@prsTypes'('lo.core#,..'(XT, XR), XEnv, XQ, XC, XCx, 'lo.core#,..'(XPT, XM), XRp, XRpx):- 'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XC0, XPT, XRp, XRp0),
    'lo.comp.parseType@prsTypes'(XR, XEnv, XQ, XC0, XCx, XM, XRp0, XRpx).
'lo.comp.parseType@prsArgType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#tupleType'(XAT), XRp, XRpx):- 'lo.comp.abstract@isRoundTuple'(XT, X_35541, XA),
    'lo.comp.parseType@prsTypes'(XA, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsArgType'(XT, XEnv, XQ, XC, XCx, XAT, XRp, XRpx):- 'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsTypeFields'('lo.core#[]', X_35542, X_35543, XCx, XCx, 'lo.core#[]', XRp, XRp).
'lo.comp.parseType@prsTypeFields'('lo.core#,..'(XA, XAT), XEnv, XQ, XC, XCx, 'lo.core#,..'('()2'(XFld, XFldTp), XArgTypes), XRp, XRp):- 'lo.comp.abstract@isBinary'(XA, ":", X_35546, XL, XR),
    'lo.comp.abstract@isIden'(XL, X_35547, XFld),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, XC, XC0, XFldTp, XRp, XRp0),
    'lo.comp.parseType@prsTypeFields'(XAT, XEnv, XQ, XC0, XCx, XArgTypes, XRp0, XRpx).
'lo.comp.parseType@prsTypeFields'('lo.core#,..'(XA, XAT), XEnv, XQ, XC, XCx, XArgTypes, XRp, XRpx):- ocall('disp%1'(XXV5478),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XA, XXe5102),XXV5478,XXV5478),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe5102, 'lo.core#,..'('lo.core#ss'(" is not a valid field designator"), 'lo.core#[]'))), XXd40376),
    ocall('loc%1'(XXV5479),XA,XA),
    'lo.comp.errors@reportError'(XXd40376, XXV5479, XRp, XRp0),
    'lo.comp.parseType@prsTypeFields'(XAT, XEnv, XQ, XC, XCx, XArgTypes, XRp0, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XCx, XCx, XPT, XRp, XRpx):- 'lo.comp.abstract@isQuantified'(XT, XV, XBT),
    'lo.comp.parseType@parseTypeBound'(XV, XQ, XB0, XEnv, XPT, XInner),
    'lo.comp.parseType@prsType'(XBT, XEnv, XB0, 'lo.core#[]', XC0, XBTp, XRp, XRpx),
    'lo.comp.types@moveConstraints'(XInner, XC0, XBTp).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XC, XType, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XT, "|:", X_35551, XL, XR),
    'lo.comp.parseType@parseConstraint'(XL, XEnv, XQ, 'lo.core#[]', XC0, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, 'lo.core#[]', X_35552, XInner, XRp0, XRpx),
    'lo.comp.types@moveConstraints'(XType, XC0, XInner).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XC, XTp, XRp, XRpx):- 'lo.comp.abstract@isIden'(XT, XLc, XNm),
    'lo.comp.parseType@prsTypeName'(XLc, XNm, XEnv, XQ, XTp, XRp, XRpx).
'lo.comp.parseType@prsType'(XSq, XEnv, XQ, XC, XCx, XType, XRp, XRpx):- 'lo.comp.abstract@isSquareTerm'(XSq, XLc, XOp, XArgs),
    'lo.comp.abstract@isIden'(XOp, X_35553, XNm),
    'lo.comp.parseType@prsTypeName'(XLc, XNm, XEnv, XQ, XTpCon, XRp, XRp0),
    'lo.comp.parseType@prsTypes'(XArgs, XEnv, XQ, XC, XCx, XArgTps, XRp0, XRp1),
    'lo.comp.types@deRef'(XTpCon, XXd40377),
    'lo.comp.parseType@applyTypeExp'(XXd40377, XArgTps, XLc, XType, XRp1, XRpx).
'lo.comp.parseType@prsType'(XF, XEnv, XQ, XC, XCx, 'lo.comp.types#funType'(XAT, XRT), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XF, "=>", X_35554, XL, XR),
    'lo.comp.parseType@prsArgType'(XL, XEnv, XQ, XC, XC1, XAT, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, XC1, XCx, XRT, XRp0, XRpx).
'lo.comp.parseType@prsType'(XF, XEnv, XQ, XC, XCx, 'lo.comp.types#grammarType'(XAT, XRT), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XF, "-->", X_35555, XL, XR),
    'lo.comp.parseType@prsArgType'(XL, XEnv, XQ, XC, XC1, XAT, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, XC1, XCx, XRT, XRp0, XRpx).
'lo.comp.parseType@prsType'(XF, XEnv, XQ, XC, XCx, 'lo.comp.types#classType'(XAT, XRT), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XF, "<=>", X_35556, XL, XR),
    'lo.comp.parseType@prsArgType'(XL, XEnv, XQ, XC, XC1, XAT, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, XC1, XCx, XRT, XRp0, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#predType'(XAT), XRp, XRpx):- 'lo.comp.abstract@isBraceTerm'(XT, X_35557, XL, 'lo.core#[]'),
    'lo.comp.parseType@prsArgType'(XL, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#tupleType'(XAT), XRp, XRpx):- 'lo.comp.abstract@isRoundTuple'(XT, X_35558, 'lo.core#,..'(XA, 'lo.core#[]')),
    'lo.comp.abstract@isRoundTuple'(XA, X_35560, XInner),
    'lo.comp.parseType@prsTypes'(XInner, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, XAT, XRp, XRpx):- 'lo.comp.abstract@isRoundTuple'(XT, X_35561, 'lo.core#,..'(XA, 'lo.core#[]')),
    'lo.comp.parseType@prsType'(XA, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#tupleType'(XAT), XRp, XRpx):- 'lo.comp.abstract@isRoundTuple'(XT, X_35563, XA),
    'lo.comp.parseType@prsTypes'(XA, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#faceType'(XAT), XRp, XRpx):- 'lo.comp.abstract@isBraceTuple'(XT, X_35564, XL),
    'lo.comp.parseType@prsTypeFields'(XL, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, X_35565, X_35566, XCx, XCx, 'lo.comp.types#anonType', XRp, XRpx):- ocall('disp%1'(XXV5480),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XT, XXe5103),XXV5480,XXV5480),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot understand type "), 'lo.core#,..'(XXe5103, 'lo.core#[]'))), XXd40384),
    ocall('loc%1'(XXV5481),XT,XT),
    'lo.comp.errors@reportError'(XXd40384, XXV5481, XRp, XRpx).
'lo.comp.parseType@parseType'(XT, XEnv, XType, XRp, XRpx):- ocall('_empty%1'(XXV5482),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@prsType'(XT, XEnv, XXV5482, 'lo.core#[]', XCons, XTp, XRp, XRpx),
    'lo.comp.types@moveConstraints'(XType, XCons, XTp).
'lo.comp.parseType@parseHeadArgs'('lo.core#[]', X_35569, 'lo.core#[]', XRp, XRp).
'lo.comp.parseType@parseHeadArgs'('lo.core#,..'(XH, XL), XB, 'lo.core#,..'(XV, XArgs), XRp, XRpx):- 'lo.comp.abstract@isIden'(XH, XLc, XNm),
    'lo.comp.parseType@cond418'(XRp0, XLc, XXd40393, XXd40392, XXd40391, XXd40390, XXd40389, XXd40388, XXe5104, XXV5483, XH, XXd40387, XRpx, XRp, XXd40386, XV, XNm, XB),
    'lo.comp.parseType@parseHeadArgs'(XL, XB, XArgs, XRp0, XRpx).
'lo.comp.parseType@parseTypeHead'(XN, X_35575, 'lo.comp.types#tipe'(XXb20343), XPath, XRp, XRp):- 'lo.comp.abstract@isIden'(XN, X_35576, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#tpe', XXb20342),
    'lo.comp.misc@subPath'(XPath, XXb20342, XNm, XXb20343).
'lo.comp.parseType@parseTypeHead'(XN, XB, 'lo.comp.types#typeExp'('lo.comp.types#tpFun'(XXb20346, XXb20347), XArgs), XPath, XRp, XRpx):- 'lo.comp.abstract@isSquareTerm'(XN, X_35577, XNm, XA),
    'lo.comp.abstract@isIden'(XNm, X_35578, XTpNm),
    'lo.comp.parseType@parseHeadArgs'(XA, XB, XArgs, XRp, XRpx),
    'lo.comp.abstract@marker'('lo.comp.abstract#tpe', XXb20345),
    'lo.comp.misc@subPath'(XPath, XXb20345, XTpNm, XXb20346),
    'lo.list@length'(XArgs, XXb20347).
'lo.comp.parseType@wrapConstraints'('lo.core#[]', XTp, XTp).
'lo.comp.parseType@wrapConstraints'('lo.core#,..'(XCon, XC), XTp, XWTp):- 'lo.comp.parseType@wrapConstraints'(XC, 'lo.comp.types#constrained'(XTp, XCon), XWTp).
'lo.comp.parseType@reQuant'('lo.core#[]', XTp, XTp).
'lo.comp.parseType@reQuant'('lo.core#,..'('()2'(XNm, X_35581), XM), XTp, XQTp):- 'lo.comp.parseType@reQuant'(XM, 'lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XTp), XQTp).
'lo.comp.parseType@parseTypeRule'(XSt, X_35582, XEnv, XRule, XPath, XRp, XRpx):- 'lo.comp.abstract@isQuantified'(XSt, XV, XBody),
    ocall('_empty%1'(XXV5484),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@parseTypeBound'(XV, XXV5484, XB, XEnv, XRule, XRl),
    'lo.comp.parseType@parseTypeRule'(XBody, XB, XEnv, XInner, XPath, XRp, XRpx),
    ocall('pairs%1'(XXV5485),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%2'(XB, XXe5105),XXV5485,XXV5485),
    'lo.comp.parseType@reQuant'(XXe5105, XInner, XRl).
'lo.comp.parseType@parseTypeRule'(XSt, XB, XEnv, XRule, XPath, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XSt, "|:", X_35583, XL, XR),
    'lo.comp.parseType@parseConstraint'(XL, XEnv, XB, 'lo.core#[]', XCx, XRp, XRp0),
    'lo.comp.parseType@parseTypeRule'(XR, XB, XEnv, XInner, XPath, XRp0, XRpx),
    'lo.comp.parseType@wrapConstraints'(XCx, XInner, XRule).
'lo.comp.parseType@parseTypeRule'(XSt, XB, XEnv, 'lo.comp.types#typeRule'(XLhs, XRhs), XPath, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XSt, "<~", X_35584, XL, XR),
    'lo.comp.parseType@parseTypeHead'(XL, XB, XLhs, XPath, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XB, XC0, XCx, XTp, XRp0, XRpx),
    'lo.comp.types@deRef'(XTp, XXd40399),
    'lo.comp.unify@faceOfType'(XXd40399, XEnv, XRhs).
'lo.comp.parseType@parseContractHead'(XT, XQ, XC, XCx, XEnv, 'lo.comp.types#conTract'(XConNm, XArgTps, XDeps), XNm, XConNm, XPath, XRp, XRpx):- 'lo.comp.abstract@isSquareTerm'(XT, XLc, XOp, XA),
    'lo.comp.abstract@isIden'(XOp, X_35585, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#con', XXd40400),
    'lo.comp.misc@subPath'(XPath, XXd40400, XNm, XXd40401),
    XConNm = XXd40401,
    'lo.comp.parseType@parseContractArgs'(XA, XEnv, XQ, XC, XCx, XArgTps, XDeps, XRp, XRpx).
'lo.comp.parseType@parseConBound'(XP, XBV, XBound, XEnv, XQT, XInner):- 'lo.comp.abstract@isBinary'(XP, ",", X_35586, XL, XR),
    'lo.comp.parseType@parseConBound'(XL, XBV, XB0, XEnv, XQT, XQ0),
    'lo.comp.parseType@parseConBound'(XR, XB0, XBound, XEnv, XQ0, XInner).
'lo.comp.parseType@parseConBound'(XV, XB, XXe5106, XEnv, 'lo.comp.types#univCon'('lo.comp.types#kFun'(XNm, XAr), XInner), XInner):- 'lo.comp.abstract@isBinary'(XV, "/", X_35587, XL, XR),
    'lo.comp.abstract@isInteger'(XR, X_35588, XAr),
    'lo.comp.abstract@isIden'(XL, X_35589, XNm),
    ocall('_put%1'(XXV5486),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XB, XNm, 'lo.comp.types#kFun'(XNm, XAr), XXe5106),XXV5486,XXV5486).
'lo.comp.parseType@parseConBound'(XV, XB, XXe5107, XEnv, 'lo.comp.types#univCon'('lo.comp.types#kVar'(XNm), XInner), XInner):- 'lo.comp.abstract@isIden'(XV, X_35590, XNm),
    ocall('_put%1'(XXV5487),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XB, XNm, 'lo.comp.types#kVar'(XNm), XXe5107),XXV5487,XXV5487).
'lo.comp.parseType@parseContractSpec'(XT, XR, XQ, XQx, XC0, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx):- 'lo.comp.abstract@isQuantified'(XT, XV, XB),
    'lo.comp.parseType@parseConBound'(XV, XQ, XQ0, XEnv, X_35591, X_35592),
    'lo.comp.parseType@parseContractSpec'(XB, XR, XQ0, XQx, XC0, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx).
'lo.comp.parseType@parseContractSpec'(XT, XF, XQ, XQx, XC0, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XT, "|:", X_35593, XL, XR),
    ocall('disp%1'(XXV5488),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XT, XXe5108),XXV5488,XXV5488),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("constrained contract "), 'lo.core#,..'(XXe5108, 'lo.core#,..'('lo.core#ss'(" not supported"), 'lo.core#[]')))), XXd40412),
    'lo.comp.errors@reportError'(XXd40412, XLc, XRp, XRp0),
    'lo.comp.parseType@parseContractSpec'(XR, XF, XQ, XQx, XC1, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp0, XRpx).
'lo.comp.parseType@parseContractSpec'(XT, XF, XQ, XQ, XC, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XT, "<~", XLc, XL, XF),
    'lo.comp.parseType@parseContractHead'(XL, XQ, XC, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx).
'lo.comp.parseType@wrapConConstraints'('lo.core#[]', XC, XC).
'lo.comp.parseType@wrapConConstraints'('lo.core#,..'(XCon, XL), XC, XWrCon):- 'lo.comp.parseType@wrapConConstraints'(XL, 'lo.comp.types#conCon'(XC, XCon), XWrCon).
'lo.comp.parseType@reConQuant'('lo.core#[]', XTp, XTp).
'lo.comp.parseType@reConQuant'('lo.core#,..'('()2'(XNm, XV), XM), XTp, XQTp):- 'lo.comp.parseType@reConQuant'(XM, 'lo.comp.types#univCon'(XV, XTp), XQTp).
'lo.comp.parseType@parseContract'(XT, XEnv, XPath, 'lo.comp.types#conEntry'(XNm, XConNm, XSpec, XFace), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XT, "contract", X_35599, XTI),
    ocall('_empty%1'(XXV5489),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@parseContractSpec'(XTI, XF, XXV5489, XQ, 'lo.core#[]', XC0, XEnv, XSpc, XNm, XConNm, XPath, XRp, XRp0),
    'lo.comp.abstract@isBraceTuple'(XF, X_35600, XTLs),
    'lo.comp.parseType@prsTypeFields'(XTLs, XEnv, XQ, XC0, X_35601, XFc, XRp0, XRpx),
    ocall('pairs%1'(XXV5490),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%2'(XQ, XXe5109),XXV5490,XXV5490),
    XPQ = XXe5109,
    'lo.comp.parseType@reQuant'(XPQ, 'lo.comp.types#faceType'(XFc), XFace),
    'lo.comp.parseType@reConQuant'(XPQ, XSpc, XSpec),
    'lo.comp.parseType@wrapConConstraints'(XC0, XSpc, XSpcC),
    'lo.comp.parseType@reConQuant'(XPQ, XSpcC, XFullSpec).
'lo.comp.parseType@mergeConstraints'('lo.core#[]', XC, XC).
'lo.comp.parseType@mergeConstraints'('lo.core#,..'(XC0, XL), XC, XCx):- 'lo.comp.parseType@addConstraint'(XC0, XC, XC1),
    'lo.comp.parseType@mergeConstraints'(XL, XC1, XCx).
'lo.comp.parseType@parseContractCon'(XTp, XEnv, XB, XC, XC, XN, XPT, XRp, XRpx):- 'lo.comp.abstract@isQuantified'(XTp, XV, XBT),
    'lo.comp.parseType@parseConBound'(XV, XB, XB0, XEnv, XPT, XInner),
    'lo.comp.parseType@parseContractCon'(XBT, XEnv, XB0, 'lo.core#[]', XC0, XN, XBTp, XRp, XRpx),
    'lo.comp.parseType@wrapConConstraints'(XC0, XBTp, XInner).
'lo.comp.parseType@parseContractCon'(XTp, XEnv, XB, XC0, XCx, XN, XCn, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTp, "|:", X_35603, XL, XR),
    'lo.comp.parseType@parseConstraint'(XL, XEnv, XB, XC0, XC1, XRp, XRp0),
    'lo.comp.parseType@parseContractCon'(XR, XEnv, XB, XC1, XCx, XN, XCn, XRp0, XRpx).
'lo.comp.parseType@parseContractCon'(XSq, XEnv, XQ, XC0, XCx, XN, 'lo.comp.types#conTract'(XOp, XArgTps, XDeps), XRp, XRpx):- 'lo.comp.abstract@isSquareTerm'(XSq, XLc, XNm, XArgs),
    'lo.comp.abstract@isIden'(XNm, X_35604, XN),
    'lo.comp.parseType@parseContractArgs'(XArgs, XEnv, XQ, XC0, XCx, XArgTps, XDeps, XRp, XRp0),
    'lo.comp.parseType@cond419'(XXd40429, XXd40428, XXd40427, XXd40426, XXd40425, XXd40424, XXe5110, XXV5491, XXd40423, XXd40422, XDeps, XXd40421, XXd40420, XArgTps, XXd40419, XRpx, XRp0, XXd40418, XDps, XATs, XOp, XEnv, XN, XLc).
'lo.comp.parseType@parseContractConstraint'(XT, XEnv, XN, XCon, XRp, XRpx):- 'lo.comp.parseType@one302'(XRpx, XRp, XTp, XN, XCons, XXV5492, XXd40430, XEnv, XT),
    'lo.comp.parseType@wrapConConstraints'(XCons, XTp, XCon).
'lo.comp.parseType@parseTypeCore'(XSt, XType, XPath):- 'lo.comp.abstract@isUnary'(XSt, "type", X_35608, XL),
    'lo.comp.parseType@parseTypeCore'(XL, XType, XPath).
'lo.comp.parseType@parseTypeCore'(XSt, XType, XPath):- 'lo.comp.abstract@isQuantified'(XSt, X_35609, XBody),
    'lo.comp.parseType@parseTypeCore'(XBody, XType, XPath).
'lo.comp.parseType@parseTypeCore'(XSt, XType, XPath):- 'lo.comp.abstract@isBinary'(XSt, "|:", X_35610, X_35611, XR),
    'lo.comp.parseType@parseTypeCore'(XR, XType, XPath).
'lo.comp.parseType@parseTypeCore'(XSt, XType, XPath):- 'lo.comp.abstract@isBinary'(XSt, "<~", X_35612, XL, X_35613),
    'lo.comp.parseType@parseTypeCore'(XL, XType, XPath).
'lo.comp.parseType@parseTypeCore'(XN, 'lo.comp.types#tipe'(XXb20364), XPath):- 'lo.comp.abstract@isIden'(XN, X_35614, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#tpe', XXb20363),
    'lo.comp.misc@subPath'(XPath, XXb20363, XNm, XXb20364).
'lo.comp.parseType@parseTypeCore'(XT, 'lo.comp.types#tpFun'(XXb20367, XXb20368), XPath):- 'lo.comp.abstract@isSquareTerm'(XT, X_35615, XN, XA),
    'lo.comp.abstract@isIden'(XN, X_35616, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#tpe', XXb20366),
    'lo.comp.misc@subPath'(XPath, XXb20366, XNm, XXb20367),
    'lo.list@length'(XA, XXb20368).
'lo.comp.parseType@rewriteConstraints'('lo.core#[]', X_35617, XCx, XCx).
'lo.comp.parseType@rewriteConstraints'('lo.core#,..'(XCon, XCons), XQ, XC0, XCx):- ocall('freshen%1'(XXV5493),'lo.comp.freshen$freshen$lo.comp.types*constraint','lo.comp.freshen$freshen$lo.comp.types*constraint'),
    ocall('_call%3'(XCon, XQ, XXe5111),XXV5493,XXV5493),
    '()2'(X_35619, XFCon) = XXe5111,
    'lo.comp.parseType@addConstraint'(XFCon, XC0, XC1),
    'lo.comp.parseType@rewriteConstraints'(XCons, XQ, XC1, XCx).
'lo.comp.parseType^validTypeOp'('_call%2'(XV32247, XV32248), 'lo.comp.parseType^validTypeOp', _):- 'lo.comp.parseType@validTypeOp'(XV32247, XV32248).
'lo.comp.parseType@one300'(XXd40331, XArgs, XOp):- 'lo.list@length'(XArgs, XXd40331),
    'lo.comp.parseType@validTypeOp'(XOp, XXd40331),
    !.
'lo.comp.parseType^applyTypeExp'('_call%6'(XV32249, XV32250, XV32251, XV32252, XV32253, XV32254), 'lo.comp.parseType^applyTypeExp', _):- 'lo.comp.parseType@applyTypeExp'(XV32249, XV32250, XV32251, XV32252, XV32253, XV32254).
'lo.comp.parseType^prsTypeName'('_call%7'(XV32255, XV32256, XV32257, XV32258, XV32259, XV32260, XV32261), 'lo.comp.parseType^prsTypeName', _):- 'lo.comp.parseType@prsTypeName'(XV32255, XV32256, XV32257, XV32258, XV32259, XV32260, XV32261).
'lo.comp.parseType^parseContractName'('_call%6'(XV32262, XV32263, XV32264, XV32265, XV32266, XV32267), 'lo.comp.parseType^parseContractName', _):- 'lo.comp.parseType@parseContractName'(XV32262, XV32263, XV32264, XV32265, XV32266, XV32267).
'lo.comp.parseType@one301'(XC0, XCon):- ocall('in%2'(XCon, XC0),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.parseType@neg337'(XC0, XCon):- ocall('in%2'(XCon, XC0),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.parseType@neg337'(XC0, XCon).
'lo.comp.parseType^addConstraint'('_call%3'(XV32268, XV32269, XV32270), 'lo.comp.parseType^addConstraint', _):- 'lo.comp.parseType@addConstraint'(XV32268, XV32269, XV32270).
'lo.comp.parseType^parseTypeBound'('_call%6'(XV32271, XV32272, XV32273, XV32274, XV32275, XV32276), 'lo.comp.parseType^parseTypeBound', _):- 'lo.comp.parseType@parseTypeBound'(XV32271, XV32272, XV32273, XV32274, XV32275, XV32276).
'lo.comp.parseType^parseContractArgs'('_call%9'(XV32277, XV32278, XV32279, XV32280, XV32281, XV32282, XV32283, XV32284, XV32285), 'lo.comp.parseType^parseContractArgs', _):- 'lo.comp.parseType@parseContractArgs'(XV32277, XV32278, XV32279, XV32280, XV32281, XV32282, XV32283, XV32284, XV32285).
'lo.comp.parseType^parseConstraint'('_call%7'(XV32286, XV32287, XV32288, XV32289, XV32290, XV32291, XV32292), 'lo.comp.parseType^parseConstraint', _):- 'lo.comp.parseType@parseConstraint'(XV32286, XV32287, XV32288, XV32289, XV32290, XV32291, XV32292).
'lo.comp.parseType^prsTypes'('_call%8'(XV32293, XV32294, XV32295, XV32296, XV32297, XV32298, XV32299, XV32300), 'lo.comp.parseType^prsTypes', _):- 'lo.comp.parseType@prsTypes'(XV32293, XV32294, XV32295, XV32296, XV32297, XV32298, XV32299, XV32300).
'lo.comp.parseType^prsArgType'('_call%8'(XV32301, XV32302, XV32303, XV32304, XV32305, XV32306, XV32307, XV32308), 'lo.comp.parseType^prsArgType', _):- 'lo.comp.parseType@prsArgType'(XV32301, XV32302, XV32303, XV32304, XV32305, XV32306, XV32307, XV32308).
'lo.comp.parseType^prsTypeFields'('_call%8'(XV32309, XV32310, XV32311, XV32312, XV32313, XV32314, XV32315, XV32316), 'lo.comp.parseType^prsTypeFields', _):- 'lo.comp.parseType@prsTypeFields'(XV32309, XV32310, XV32311, XV32312, XV32313, XV32314, XV32315, XV32316).
'lo.comp.parseType^prsType'('_call%8'(XV32317, XV32318, XV32319, XV32320, XV32321, XV32322, XV32323, XV32324), 'lo.comp.parseType^prsType', _):- 'lo.comp.parseType@prsType'(XV32317, XV32318, XV32319, XV32320, XV32321, XV32322, XV32323, XV32324).
'lo.comp.parseType^parseType'('_call%5'(XV32325, XV32326, XV32327, XV32328, XV32329), 'lo.comp.parseType^parseType', _):- 'lo.comp.parseType@parseType'(XV32325, XV32326, XV32327, XV32328, XV32329).
'lo.comp.parseType@cond418'(XRp0, XLc, XXd40393, XXd40392, XXd40391, XXd40390, XXd40389, XXd40388, XXe5104, XXV5483, XH, XXd40387, XRpx, XRp, XXd40386, XV, XNm, XB):- ocall('present%3'(XB, XNm, XV),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !,
    XRp = XRpx.
'lo.comp.parseType@cond418'(XRp0, XLc, XXd40393, XXd40392, XXd40391, XXd40390, XXd40389, XXd40388, XXe5104, XXV5483, XH, XXd40387, XRpx, XRp, XXd40386, XV, XNm, XB):- ocall('disp%1'(XXV5483),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XH, XXe5104),XXV5483,XXV5483),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type argument "), 'lo.core#,..'(XXe5104, 'lo.core#,..'('lo.core#ss'(" not quantified "), 'lo.core#[]')))), XXd40393),
    'lo.comp.errors@reportError'(XXd40393, XLc, XRp, XRp0).
'lo.comp.parseType^parseHeadArgs'('_call%5'(XV32330, XV32331, XV32332, XV32333, XV32334), 'lo.comp.parseType^parseHeadArgs', _):- 'lo.comp.parseType@parseHeadArgs'(XV32330, XV32331, XV32332, XV32333, XV32334).
'lo.comp.parseType^parseTypeHead'('_call%6'(XV32335, XV32336, XV32337, XV32338, XV32339, XV32340), 'lo.comp.parseType^parseTypeHead', _):- 'lo.comp.parseType@parseTypeHead'(XV32335, XV32336, XV32337, XV32338, XV32339, XV32340).
'lo.comp.parseType^wrapConstraints'('_call%3'(XV32341, XV32342, XV32343), 'lo.comp.parseType^wrapConstraints', _):- 'lo.comp.parseType@wrapConstraints'(XV32341, XV32342, XV32343).
'lo.comp.parseType^reQuant'('_call%3'(XV32344, XV32345, XV32346), 'lo.comp.parseType^reQuant', _):- 'lo.comp.parseType@reQuant'(XV32344, XV32345, XV32346).
'lo.comp.parseType^parseTypeRule'('_call%7'(XV32347, XV32348, XV32349, XV32350, XV32351, XV32352, XV32353), 'lo.comp.parseType^parseTypeRule', _):- 'lo.comp.parseType@parseTypeRule'(XV32347, XV32348, XV32349, XV32350, XV32351, XV32352, XV32353).
'lo.comp.parseType^parseContractHead'('_call%11'(XV32354, XV32355, XV32356, XV32357, XV32358, XV32359, XV32360, XV32361, XV32362, XV32363, XV32364), 'lo.comp.parseType^parseContractHead', _):- 'lo.comp.parseType@parseContractHead'(XV32354, XV32355, XV32356, XV32357, XV32358, XV32359, XV32360, XV32361, XV32362, XV32363, XV32364).
'lo.comp.parseType^parseConBound'('_call%6'(XV32365, XV32366, XV32367, XV32368, XV32369, XV32370), 'lo.comp.parseType^parseConBound', _):- 'lo.comp.parseType@parseConBound'(XV32365, XV32366, XV32367, XV32368, XV32369, XV32370).
'lo.comp.parseType^parseContractSpec'('_call%13'(XV32371, XV32372, XV32373, XV32374, XV32375, XV32376, XV32377, XV32378, XV32379, XV32380, XV32381, XV32382, XV32383), 'lo.comp.parseType^parseContractSpec', _):- 'lo.comp.parseType@parseContractSpec'(XV32371, XV32372, XV32373, XV32374, XV32375, XV32376, XV32377, XV32378, XV32379, XV32380, XV32381, XV32382, XV32383).
'lo.comp.parseType^wrapConConstraints'('_call%3'(XV32384, XV32385, XV32386), 'lo.comp.parseType^wrapConConstraints', _):- 'lo.comp.parseType@wrapConConstraints'(XV32384, XV32385, XV32386).
'lo.comp.parseType^reConQuant'('_call%3'(XV32387, XV32388, XV32389), 'lo.comp.parseType^reConQuant', _):- 'lo.comp.parseType@reConQuant'(XV32387, XV32388, XV32389).
'lo.comp.parseType^parseContract'('_call%6'(XV32390, XV32391, XV32392, XV32393, XV32394, XV32395), 'lo.comp.parseType^parseContract', _):- 'lo.comp.parseType@parseContract'(XV32390, XV32391, XV32392, XV32393, XV32394, XV32395).
'lo.comp.parseType^mergeConstraints'('_call%3'(XV32396, XV32397, XV32398), 'lo.comp.parseType^mergeConstraints', _):- 'lo.comp.parseType@mergeConstraints'(XV32396, XV32397, XV32398).
'lo.comp.parseType@cond419'(XXd40429, XXd40428, XXd40427, XXd40426, XXd40425, XXd40424, XXe5110, XXV5491, XXd40423, XXd40422, XDeps, XXd40421, XXd40420, XArgTps, XXd40419, XRpx, XRp0, XXd40418, XDps, XATs, XOp, XEnv, XN, XLc):- 'lo.comp.parseType@parseContractName'(XLc, XN, XEnv, 'lo.comp.types#conTract'(XOp, XATs, XDps), XRp0, XRpx),
    !,
    'lo.comp.unify@sameType'('lo.comp.types#tupleType'(XATs), 'lo.comp.types#tupleType'(XArgTps), XEnv),
    'lo.comp.unify@sameType'('lo.comp.types#tupleType'(XDps), 'lo.comp.types#tupleType'(XDeps), XEnv).
'lo.comp.parseType@cond419'(XXd40429, XXd40428, XXd40427, XXd40426, XXd40425, XXd40424, XXe5110, XXV5491, XXd40423, XXd40422, XDeps, XXd40421, XXd40420, XArgTps, XXd40419, XRpx, XRp0, XXd40418, XDps, XATs, XOp, XEnv, XN, XLc):- ocall('disp%1'(XXV5491),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XN, XXe5110),XXV5491,XXV5491),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("contract "), 'lo.core#,..'(XXe5110, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]')))), XXd40429),
    'lo.comp.errors@reportError'(XXd40429, XLc, XRp0, XRpx),
    XOp = XN.
'lo.comp.parseType^parseContractCon'('_call%9'(XV32399, XV32400, XV32401, XV32402, XV32403, XV32404, XV32405, XV32406, XV32407), 'lo.comp.parseType^parseContractCon', _):- 'lo.comp.parseType@parseContractCon'(XV32399, XV32400, XV32401, XV32402, XV32403, XV32404, XV32405, XV32406, XV32407).
'lo.comp.parseType@one302'(XRpx, XRp, XTp, XN, XCons, XXV5492, XXd40430, XEnv, XT):- ocall('_empty%1'(XXV5492),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@parseContractCon'(XT, XEnv, XXV5492, 'lo.core#[]', XCons, XN, XTp, XRp, XRpx),
    !.
'lo.comp.parseType^parseContractConstraint'('_call%6'(XV32408, XV32409, XV32410, XV32411, XV32412, XV32413), 'lo.comp.parseType^parseContractConstraint', _):- 'lo.comp.parseType@parseContractConstraint'(XV32408, XV32409, XV32410, XV32411, XV32412, XV32413).
'lo.comp.parseType^parseTypeCore'('_call%3'(XV32414, XV32415, XV32416), 'lo.comp.parseType^parseTypeCore', _):- 'lo.comp.parseType@parseTypeCore'(XV32414, XV32415, XV32416).
'lo.comp.parseType^rewriteConstraints'('_call%4'(XV32417, XV32418, XV32419, XV32420), 'lo.comp.parseType^rewriteConstraints', _):- 'lo.comp.parseType@rewriteConstraints'(XV32417, XV32418, XV32419, XV32420).
