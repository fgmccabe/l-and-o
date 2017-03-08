'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.parseType'e'*'n21o21'()21'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.freshen'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.unify'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dict'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I10'parseTypeBound'PT6t'lo.comp.ast*ast'Uz2'lo.index*map'2St'lo.comp.types*tipe'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.dict*env't'lo.comp.types*tipe't'lo.comp.types*tipe''parseConstraint'PT7t'lo.comp.ast*ast'Lt'lo.comp.dict*env'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.types*constraint'Lt'lo.comp.types*constraint't'lo.comp.errors*report't'lo.comp.errors*report''parseType'PT5t'lo.comp.ast*ast'Lt'lo.comp.dict*env't'lo.comp.types*tipe't'lo.comp.errors*report't'lo.comp.errors*report''parseTypeRule'PT7t'lo.comp.ast*ast'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.dict*env't'lo.comp.types*tipe'St'lo.comp.errors*report't'lo.comp.errors*report''parseConBound'PT6t'lo.comp.ast*ast'Uz2'lo.index*map'2St'lo.comp.types*tipe'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.dict*env't'lo.comp.types*constraint't'lo.comp.types*constraint''wrapConConstraints'PT3Lt'lo.comp.types*constraint't'lo.comp.types*constraint't'lo.comp.types*constraint''parseContract'PT6t'lo.comp.ast*ast'Lt'lo.comp.dict*env'St'lo.comp.types*conEntry't'lo.comp.errors*report't'lo.comp.errors*report''parseContractConstraint'PT6t'lo.comp.ast*ast'Lt'lo.comp.dict*env'St'lo.comp.types*constraint't'lo.comp.errors*report't'lo.comp.errors*report''parseTypeCore'PT3t'lo.comp.ast*ast't'lo.comp.types*tipe'S'rewriteConstraints'PT4Lt'lo.comp.types*constraint'Uz2'lo.index*map'2St'lo.comp.types*tipe'Lt'lo.comp.types*constraint'Lt'lo.comp.types*constraint'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.parseType@init'() :- !.
'lo.comp.parseType@parseTypeBound'(XP, XBV, XBound, XEnv, XQT, XInner) :- 'lo.comp.abstract@isBinary'(XP, ",", X_1941, XL, XR),
    'lo.comp.parseType@parseTypeBound'(XL, XBV, XB0, XEnv, XQT, XQ0),
    'lo.comp.parseType@parseTypeBound'(XR, XB0, XBound, XEnv, XQ0, XInner).
'lo.comp.parseType@parseTypeBound'(XV, XB, XB1, XEnv, 'lo.comp.types#univType'('lo.comp.types#kFun'(XNm, XAr), XInner), XInner) :- 'lo.comp.abstract@isBinary'(XV, "/", X_1942, XL, XR),
    'lo.comp.abstract@isInteger'(XR, X_1943, XAr),
    'lo.comp.abstract@isIden'(XL, X_1944, XNm),
    ocall('_put%4'(XB, XNm, 'lo.comp.types#kFun'(XNm, XAr), XX29301),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    XB1 = XX29301.
'lo.comp.parseType@parseTypeBound'(XV, XB, XB1, XEnv, 'lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XInner), XInner) :- 'lo.comp.abstract@isIden'(XV, X_1945, XNm),
    ocall('_put%4'(XB, XNm, 'lo.comp.types#kVar'(XNm), XX29321),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    XB1 = XX29321.
'lo.comp.parseType@addConstraint'(XCon, XC0, XC0) :- 'lo.comp.parseType@one33'(XC0, XCon).
'lo.comp.parseType@addConstraint'(XCon, XC0, 'lo.core#,..'(XCon, XC0)) :- 'lo.comp.parseType@neg35'(XC0, XCon).
'lo.comp.parseType@parseContractName'(X_1946, XId, XEnv, XCon, XRp, XRp) :- 'lo.comp.dict@isContract'(XId, XEnv, 'lo.comp.types#conEntry'(X_1947, X_1948, XSpec, X_1949)),
    ocall('_empty%1'(XXV51),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('freshen%3'(XSpec, XXV51, XX29356),'lo.comp.freshen$freshen$lo.comp.types*constraint','lo.comp.freshen$freshen$lo.comp.types*constraint'),
    (X_1950, XCon) = XX29356.
'lo.comp.parseType@parseContractName'(XLc, XId, X_1951, 'lo.comp.types#conTract'(XId, 'lo.core#[]', 'lo.core#[]'), XRp, XRpx) :- ocall('disp%2'(XId, XX29369),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("contract "), 'lo.core#,..'(XX29369, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]')))), XX29377),
    'lo.comp.errors@reportError'(XX29377, XLc, XRp, XRpx).
'lo.comp.parseType@prsTypeName'(X_1952, "_", X_1953, X_1954, 'lo.comp.types#anonType', XRp, XRp).
'lo.comp.parseType@prsTypeName'(X_1955, "void", X_1956, X_1957, 'lo.comp.types#voidType', XRp, XRp).
'lo.comp.parseType@prsTypeName'(X_1958, "this", X_1959, X_1960, 'lo.comp.types#thisType', XRp, XRp).
'lo.comp.parseType@prsTypeName'(X_1961, XId, X_1962, XB, XTp, XRp, XRp) :- ocall('present%3'(XB, XId, XTp),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.parseType@prsTypeName'(X_1963, XId, XEnv, X_1964, XTp, XRp, XRp) :- 'lo.comp.dict@isType'(XId, XEnv, X_1965, XTp).
'lo.comp.parseType@prsTypeName'(XLc, XId, X_1966, X_1967, 'lo.comp.types#anonType', XRp, XRpx) :- ocall('disp%2'(XId, XX29431),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type "), 'lo.core#,..'(XX29431, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]')))), XX29439),
    'lo.comp.errors@reportError'(XX29439, XLc, XRp, XRpx).
'lo.comp.parseType@validTypeOp'('lo.comp.types#tpFun'(X_1968, XAr), XAr).
'lo.comp.parseType@validTypeOp'('lo.comp.types#kFun'(X_1969, XAr), XAr).
'lo.comp.parseType@applyTypeExp'(XOp, XArgs, X_1970, 'lo.comp.types#typeExp'(XOp, XArgs), XRp, XRp) :- 'lo.comp.parseType@one34'(XX29461, XArgs, XOp).
'lo.comp.parseType@applyTypeExp'(XT, XArgs, XLc, 'lo.comp.types#voidType', XRp, XRpx) :- ocall('disp%2'(XT, XX29470),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%2'(XArgs, XX29474),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.types*tipe'),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.types*tipe')),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot apply type exp "), 'lo.core#,..'(XX29470, 'lo.core#,..'('lo.core#ss'(" to "), 'lo.core#,..'(XX29474, 'lo.core#[]'))))), XX29483),
    'lo.comp.errors@reportError'(XX29483, XLc, XRp, XRpx).
'lo.comp.parseType@prsArgType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#tupleType'(XAT), XRp, XRpx) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1971, XA),
    'lo.comp.parseType@prsTypes'(XA, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsArgType'(XT, XEnv, XQ, XC, XCx, XAT, XRp, XRpx) :- 'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsTypes'('lo.core#[]', X_1972, X_1973, XC, XC, 'lo.core#[]', XRp, XRp).
'lo.comp.parseType@prsTypes'('lo.core#,..'(XT, XR), XEnv, XQ, XC, XCx, 'lo.core#,..'(XPT, XM), XRp, XRpx) :- 'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XC0, XPT, XRp, XRp0),
    'lo.comp.parseType@prsTypes'(XR, XEnv, XQ, XC0, XCx, XM, XRp0, XRpx).
'lo.comp.parseType@parseContractArgs'('lo.core#,..'(XA, 'lo.core#[]'), XEnv, XB, XC0, XCx, XArgs, XDeps, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XA, "->>", X_1974, XL, XR),
    'lo.comp.abstract@deComma'(XL, XX29575),
    'lo.comp.parseType@prsTypes'(XX29575, XEnv, XB, XC0, XC1, XArgs, XRp, XRp0),
    'lo.comp.abstract@deComma'(XR, XX29584),
    'lo.comp.parseType@prsTypes'(XX29584, XEnv, XB, XC1, XCx, XDeps, XRp0, XRpx).
'lo.comp.parseType@parseContractArgs'(XA, XEnv, XB, XC0, XCx, XArgs, 'lo.core#[]', XRp, XRpx) :- 'lo.comp.parseType@prsTypes'(XA, XEnv, XB, XC0, XCx, XArgs, XRp, XRpx).
'lo.comp.parseType@prsTypeFields'('lo.core#[]', X_1975, X_1976, XCx, XCx, 'lo.core#[]', XRp, XRp).
'lo.comp.parseType@prsTypeFields'('lo.core#,..'(XA, XAT), XEnv, XQ, XC, XCx, 'lo.core#,..'((XFld, XFldTp), XArgTypes), XRp, XRp) :- 'lo.comp.abstract@isBinary'(XA, ":", X_1977, XL, XR),
    'lo.comp.abstract@isIden'(XL, X_1978, XFld),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, XC, XC0, XFldTp, XRp, XRp0),
    'lo.comp.parseType@prsTypeFields'(XAT, XEnv, XQ, XC0, XCx, XArgTypes, XRp0, XRpx).
'lo.comp.parseType@prsTypeFields'('lo.core#,..'(XA, XAT), XEnv, XQ, XC, XCx, XArgTypes, XRp, XRpx) :- ocall('disp%2'(XA, XX29664),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XX29664, 'lo.core#,..'('lo.core#ss'(" is not a valid field designator"), 'lo.core#[]'))), XX29671),
    ocall('loc%1'(XXV52),XA,XA),
    'lo.comp.errors@reportError'(XX29671, XXV52, XRp, XRp0),
    'lo.comp.parseType@prsTypeFields'(XAT, XEnv, XQ, XC, XCx, XArgTypes, XRp0, XRpx).
'lo.comp.parseType@parseConstraint'(XT, XEnv, XB, XC0, XCx, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XT, ",", X_1979, XL, XR),
    'lo.comp.parseType@parseConstraint'(XR, XEnv, XB, XC0, XC1, XRp, XRp0),
    'lo.comp.parseType@parseConstraint'(XL, XEnv, XB, XC1, XCx, XRp0, XRpx).
'lo.comp.parseType@parseConstraint'(XT, XEnv, XB, XC0, XCx, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XT, "<~", X_1980, XL, XR),
    'lo.comp.parseType@prsType'(XL, XEnv, XB, XC0, XC1, XTV, XRp, XRp0),
    'lo.comp.abstract@isBraceTuple'(XR, X_1981, XE),
    'lo.comp.parseType@prsTypeFields'(XE, XEnv, XB, XC1, XC2, XAT, XRp0, XRpx),
    'lo.comp.parseType@addConstraint'('lo.comp.types#implementsFace'(XTV, XAT), XC2, XCx).
'lo.comp.parseType@parseConstraint'(XSq, XEnv, XB, XC0, XCx, XRp, XRpx) :- 'lo.comp.abstract@isSquareTerm'(XSq, XLc, XOp, XArgs),
    'lo.comp.abstract@isIden'(XOp, XLc0, XNm),
    'lo.comp.parseType@parseContractName'(XLc0, XNm, XEnv, 'lo.comp.types#conTract'(XConNm, X_ATs, X_Dps), XRp, XRp0),
    'lo.comp.parseType@parseContractArgs'(XArgs, XEnv, XB, XC0, XC1, XArgTps, XDeps, XRp0, XRpx),
    'lo.comp.parseType@addConstraint'('lo.comp.types#conTract'(XConNm, XArgTps, XDeps), XC1, XCx).
'lo.comp.parseType@parseConstraint'(XT, X_1982, XB, XC, XC, XRp, XRpx) :- ocall('disp%2'(XT, XX29790),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("invalid type constraint "), 'lo.core#,..'(XX29790, 'lo.core#[]'))), XX29796),
    ocall('loc%1'(XXV53),XT,XT),
    'lo.comp.errors@reportError'(XX29796, XXV53, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XCx, XCx, XPT, XRp, XRpx) :- 'lo.comp.abstract@isQuantified'(XT, XV, XBT),
    'lo.comp.parseType@parseTypeBound'(XV, XQ, XB0, XEnv, XPT, XInner),
    'lo.comp.parseType@prsType'(XBT, XEnv, XB0, 'lo.core#[]', XC0, XBTp, XRp, XRpx),
    'lo.comp.types@moveConstraints'(XInner, XC0, XBTp).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XC, XType, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XT, "|:", X_1983, XL, XR),
    'lo.comp.parseType@parseConstraint'(XL, XEnv, XQ, 'lo.core#[]', XC0, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, 'lo.core#[]', X_1984, XInner, XRp0, XRpx),
    'lo.comp.types@moveConstraints'(XType, XC0, XInner).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XC, XTp, XRp, XRpx) :- 'lo.comp.abstract@isIden'(XT, XLc, XNm),
    'lo.comp.parseType@prsTypeName'(XLc, XNm, XEnv, XQ, XTp, XRp, XRpx).
'lo.comp.parseType@prsType'(XSq, XEnv, XQ, XC, XCx, XType, XRp, XRpx) :- 'lo.comp.abstract@isSquareTerm'(XSq, XLc, XOp, XArgs),
    'lo.comp.abstract@isIden'(XOp, X_1985, XNm),
    'lo.comp.parseType@prsTypeName'(XLc, XNm, XEnv, XQ, XTpCon, XRp, XRp0),
    'lo.comp.parseType@prsTypes'(XArgs, XEnv, XQ, XC, XCx, XArgTps, XRp0, XRp1),
    'lo.comp.types@deRef'(XTpCon, XX29907),
    'lo.comp.parseType@applyTypeExp'(XX29907, XArgTps, XLc, XType, XRp1, XRpx).
'lo.comp.parseType@prsType'(XF, XEnv, XQ, XC, XCx, 'lo.comp.types#funType'(XAT, XRT), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XF, "=>", X_1986, XL, XR),
    'lo.comp.parseType@prsArgType'(XL, XEnv, XQ, XC, XC1, XAT, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, XC1, XCx, XRT, XRp0, XRpx).
'lo.comp.parseType@prsType'(XF, XEnv, XQ, XC, XCx, 'lo.comp.types#grammarType'(XAT, XRT), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XF, "-->", X_1987, XL, XR),
    'lo.comp.parseType@prsArgType'(XL, XEnv, XQ, XC, XC1, XAT, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, XC1, XCx, XRT, XRp0, XRpx).
'lo.comp.parseType@prsType'(XF, XEnv, XQ, XC, XCx, 'lo.comp.types#classType'(XAT, XRT), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XF, "<=>", X_1988, XL, XR),
    'lo.comp.parseType@prsArgType'(XL, XEnv, XQ, XC, XC1, XAT, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XQ, XC1, XCx, XRT, XRp0, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#predType'(XAT), XRp, XRpx) :- 'lo.comp.abstract@isBraceTerm'(XT, X_1989, XL, 'lo.core#[]'),
    'lo.comp.parseType@prsArgType'(XL, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#tupleType'(XAT), XRp, XRpx) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1990, 'lo.core#,..'(XA, 'lo.core#[]')),
    'lo.comp.abstract@isRoundTuple'(XA, X_1991, XInner),
    'lo.comp.parseType@prsTypes'(XInner, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, XAT, XRp, XRpx) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1992, 'lo.core#,..'(XA, 'lo.core#[]')),
    'lo.comp.parseType@prsType'(XA, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#tupleType'(XAT), XRp, XRpx) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1993, XA),
    'lo.comp.parseType@prsTypes'(XA, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, XEnv, XQ, XC, XCx, 'lo.comp.types#faceType'(XAT), XRp, XRpx) :- 'lo.comp.abstract@isBraceTuple'(XT, X_1994, XL),
    'lo.comp.parseType@prsTypeFields'(XL, XEnv, XQ, XC, XCx, XAT, XRp, XRpx).
'lo.comp.parseType@prsType'(XT, X_1995, X_1996, XCx, XCx, 'lo.comp.types#anonType', XRp, XRpx) :- ocall('disp%2'(XT, XX30120),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot understand type "), 'lo.core#,..'(XX30120, 'lo.core#[]'))), XX30126),
    ocall('loc%1'(XXV54),XT,XT),
    'lo.comp.errors@reportError'(XX30126, XXV54, XRp, XRpx).
'lo.comp.parseType@parseType'(XT, XEnv, XType, XRp, XRpx) :- ocall('_empty%1'(XXV55),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@prsType'(XT, XEnv, XXV55, 'lo.core#[]', XCons, XTp, XRp, XRpx),
    'lo.comp.types@moveConstraints'(XType, XCons, XTp).
'lo.comp.parseType@reQuant'('lo.core#[]', XTp, XTp).
'lo.comp.parseType@reQuant'('lo.core#,..'((XNm, X_1997), XM), XTp, XQTp) :- 'lo.comp.parseType@reQuant'(XM, 'lo.comp.types#univType'('lo.comp.types#kVar'(XNm), XTp), XQTp).
'lo.comp.parseType@wrapConstraints'('lo.core#[]', XTp, XTp).
'lo.comp.parseType@wrapConstraints'('lo.core#,..'(XCon, XC), XTp, XWTp) :- 'lo.comp.parseType@wrapConstraints'(XC, 'lo.comp.types#constrained'(XTp, XCon), XWTp).
'lo.comp.parseType@parseHeadArgs'('lo.core#[]', X_1998, 'lo.core#[]', XRp, XRp).
'lo.comp.parseType@parseHeadArgs'('lo.core#,..'(XH, XL), XB, 'lo.core#,..'(XV, XArgs), XRp, XRpx) :- 'lo.comp.abstract@isIden'(XH, XLc, XNm),
    'lo.comp.parseType@cond20'(XRp0, XLc, XX30209, XX30201, XH, XRpx, XRp, XV, XNm, XB),
    'lo.comp.parseType@parseHeadArgs'(XL, XB, XArgs, XRp0, XRpx).
'lo.comp.parseType@parseTypeHead'(XN, X_1999, 'lo.comp.types#tipe'(XX30224), XPath, XRp, XRp) :- 'lo.comp.abstract@isIden'(XN, X_2000, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#tpe', XX30222),
    'lo.comp.misc@subPath'(XPath, XX30222, XNm, XX30224).
'lo.comp.parseType@parseTypeHead'(XN, XB, 'lo.comp.types#typeExp'('lo.comp.types#tpFun'(XX30238, XX30240), XArgs), XPath, XRp, XRpx) :- 'lo.comp.abstract@isSquareTerm'(XN, X_2001, XNm, XA),
    'lo.comp.abstract@isIden'(XNm, X_2002, XTpNm),
    'lo.comp.parseType@parseHeadArgs'(XA, XB, XArgs, XRp, XRpx),
    'lo.comp.abstract@marker'('lo.comp.abstract#tpe', XX30236),
    'lo.comp.misc@subPath'(XPath, XX30236, XTpNm, XX30238),
    'lo.list@length'(XArgs, XX30240).
'lo.comp.parseType@parseTypeRule'(XSt, X_2003, XEnv, XRule, XPath, XRp, XRpx) :- 'lo.comp.abstract@isQuantified'(XSt, XV, XBody),
    ocall('_empty%1'(XXV56),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@parseTypeBound'(XV, XXV56, XB, XEnv, XRule, XRl),
    'lo.comp.parseType@parseTypeRule'(XBody, XB, XEnv, XInner, XPath, XRp, XRpx),
    ocall('pairs%2'(XB, XX30284),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@reQuant'(XX30284, XInner, XRl).
'lo.comp.parseType@parseTypeRule'(XSt, XB, XEnv, XRule, XPath, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XSt, "|:", X_2004, XL, XR),
    'lo.comp.parseType@parseConstraint'(XL, XEnv, XB, 'lo.core#[]', XCx, XRp, XRp0),
    'lo.comp.parseType@parseTypeRule'(XR, XB, XEnv, XInner, XPath, XRp0, XRpx),
    'lo.comp.parseType@wrapConstraints'(XCx, XInner, XRule).
'lo.comp.parseType@parseTypeRule'(XSt, XB, XEnv, 'lo.comp.types#typeRule'(XLhs, XRhs), XPath, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XSt, "<~", X_2005, XL, XR),
    'lo.comp.parseType@parseTypeHead'(XL, XB, XLhs, XPath, XRp, XRp0),
    'lo.comp.parseType@prsType'(XR, XEnv, XB, XC0, XCx, XTp, XRp0, XRpx),
    'lo.comp.types@deRef'(XTp, XX30345),
    'lo.comp.unify@faceOfType'(XX30345, XEnv, XRhs).
'lo.comp.parseType@parseConBound'(XP, XBV, XBound, XEnv, XQT, XInner) :- 'lo.comp.abstract@isBinary'(XP, ",", X_2006, XL, XR),
    'lo.comp.parseType@parseConBound'(XL, XBV, XB0, XEnv, XQT, XQ0),
    'lo.comp.parseType@parseConBound'(XR, XB0, XBound, XEnv, XQ0, XInner).
'lo.comp.parseType@parseConBound'(XV, XB, XX30377, XEnv, 'lo.comp.types#univCon'('lo.comp.types#kFun'(XNm, XAr), XInner), XInner) :- 'lo.comp.abstract@isBinary'(XV, "/", X_2007, XL, XR),
    'lo.comp.abstract@isInteger'(XR, X_2008, XAr),
    'lo.comp.abstract@isIden'(XL, X_2009, XNm),
    ocall('_put%4'(XB, XNm, 'lo.comp.types#kFun'(XNm, XAr), XX30377),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.parseType@parseConBound'(XV, XB, XX30403, XEnv, 'lo.comp.types#univCon'('lo.comp.types#kVar'(XNm), XInner), XInner) :- 'lo.comp.abstract@isIden'(XV, X_2010, XNm),
    ocall('_put%4'(XB, XNm, 'lo.comp.types#kVar'(XNm), XX30403),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.parseType@parseContractHead'(XT, XQ, XC, XCx, XEnv, 'lo.comp.types#conTract'(XConNm, XArgTps, XDeps), XNm, XConNm, XPath, XRp, XRpx) :- 'lo.comp.abstract@isSquareTerm'(XT, XLc, XOp, XA),
    'lo.comp.abstract@isIden'(XOp, X_2011, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#con', XX30439),
    'lo.comp.misc@subPath'(XPath, XX30439, XNm, XX30441),
    XConNm = XX30441,
    'lo.comp.parseType@parseContractArgs'(XA, XEnv, XQ, XC, XCx, XArgTps, XDeps, XRp, XRpx).
'lo.comp.parseType@parseContractSpec'(XT, XR, XQ, XQx, XC0, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx) :- 'lo.comp.abstract@isQuantified'(XT, XV, XB),
    'lo.comp.parseType@parseConBound'(XV, XQ, XQ0, XEnv, X_2012, X_2013),
    'lo.comp.parseType@parseContractSpec'(XB, XR, XQ0, XQx, XC0, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx).
'lo.comp.parseType@parseContractSpec'(XT, XF, XQ, XQx, XC0, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XT, "|:", X_2014, XL, XR),
    ocall('disp%2'(XT, XX30505),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("constrained contract "), 'lo.core#,..'(XX30505, 'lo.core#,..'('lo.core#ss'(" not supported"), 'lo.core#[]')))), XX30513),
    'lo.comp.errors@reportError'(XX30513, XLc, XRp, XRp0),
    'lo.comp.parseType@parseContractSpec'(XR, XF, XQ, XQx, XC1, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp0, XRpx).
'lo.comp.parseType@parseContractSpec'(XT, XF, XQ, XQ, XC, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XT, "<~", XLc, XL, XF),
    'lo.comp.parseType@parseContractHead'(XL, XQ, XC, XCx, XEnv, XSpec, XNm, XConNm, XPath, XRp, XRpx).
'lo.comp.parseType@reConQuant'('lo.core#[]', XTp, XTp).
'lo.comp.parseType@reConQuant'('lo.core#,..'((XNm, XV), XM), XTp, XQTp) :- 'lo.comp.parseType@reConQuant'(XM, 'lo.comp.types#univCon'(XV, XTp), XQTp).
'lo.comp.parseType@wrapConConstraints'('lo.core#[]', XC, XC).
'lo.comp.parseType@wrapConConstraints'('lo.core#,..'(XCon, XL), XC, XWrCon) :- 'lo.comp.parseType@wrapConConstraints'(XL, 'lo.comp.types#conCon'(XC, XCon), XWrCon).
'lo.comp.parseType@parseContract'(XT, XEnv, XPath, 'lo.comp.types#conEntry'(XNm, XConNm, XSpec, XFace), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XT, "contract", X_2015, XTI),
    ocall('_empty%1'(XXV57),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@parseContractSpec'(XTI, XF, XXV57, XQ, 'lo.core#[]', XC0, XEnv, XSpc, XNm, XConNm, XPath, XRp, XRp0),
    'lo.comp.abstract@isBraceTuple'(XF, X_2016, XTLs),
    'lo.comp.parseType@prsTypeFields'(XTLs, XEnv, XQ, XC0, X_2017, XFc, XRp0, XRpx),
    ocall('pairs%2'(XQ, XX30625),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    XPQ = XX30625,
    'lo.comp.parseType@reQuant'(XPQ, 'lo.comp.types#faceType'(XFc), XFace),
    'lo.comp.parseType@reConQuant'(XPQ, XSpc, XSpec),
    'lo.comp.parseType@wrapConConstraints'(XC0, XSpc, XSpcC),
    'lo.comp.parseType@reConQuant'(XPQ, XSpcC, XFullSpec).
'lo.comp.parseType@mergeConstraints'('lo.core#[]', XC, XC).
'lo.comp.parseType@mergeConstraints'('lo.core#,..'(XC0, XL), XC, XCx) :- 'lo.comp.parseType@addConstraint'(XC0, XC, XC1),
    'lo.comp.parseType@mergeConstraints'(XL, XC1, XCx).
'lo.comp.parseType@parseContractCon'(XTp, XEnv, XB, XC, XC, XN, XPT, XRp, XRpx) :- 'lo.comp.abstract@isQuantified'(XTp, XV, XBT),
    'lo.comp.parseType@parseConBound'(XV, XB, XB0, XEnv, XPT, XInner),
    'lo.comp.parseType@parseContractCon'(XBT, XEnv, XB0, 'lo.core#[]', XC0, XN, XBTp, XRp, XRpx),
    'lo.comp.parseType@wrapConConstraints'(XC0, XBTp, XInner).
'lo.comp.parseType@parseContractCon'(XTp, XEnv, XB, XC0, XCx, XN, XCn, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTp, "|:", X_2018, XL, XR),
    'lo.comp.parseType@parseConstraint'(XL, XEnv, XB, XC0, XC1, XRp, XRp0),
    'lo.comp.parseType@parseContractCon'(XR, XEnv, XB, XC1, XCx, XN, XCn, XRp0, XRpx).
'lo.comp.parseType@parseContractCon'(XSq, XEnv, XQ, XC0, XCx, XN, 'lo.comp.types#conTract'(XOp, XArgTps, XDeps), XRp, XRpx) :- 'lo.comp.abstract@isSquareTerm'(XSq, XLc, XNm, XArgs),
    'lo.comp.abstract@isIden'(XNm, X_2019, XN),
    'lo.comp.parseType@parseContractArgs'(XArgs, XEnv, XQ, XC0, XCx, XArgTps, XDeps, XRp, XRp0),
    'lo.comp.parseType@cond21'(XX30771, XX30763, XDeps, XArgTps, XRpx, XRp0, XDps, XATs, XOp, XEnv, XN, XLc).
'lo.comp.parseType@parseContractConstraint'(XT, XEnv, XN, XCon, XRp, XRpx) :- 'lo.comp.parseType@one35'(XRpx, XRp, XTp, XN, XCons, XXV58, XEnv, XT),
    'lo.comp.parseType@wrapConConstraints'(XCons, XTp, XCon).
'lo.comp.parseType@parseTypeCore'(XSt, XType, XPath) :- 'lo.comp.abstract@isUnary'(XSt, "type", X_2020, XL),
    'lo.comp.parseType@parseTypeCore'(XL, XType, XPath).
'lo.comp.parseType@parseTypeCore'(XSt, XType, XPath) :- 'lo.comp.abstract@isQuantified'(XSt, X_2021, XBody),
    'lo.comp.parseType@parseTypeCore'(XBody, XType, XPath).
'lo.comp.parseType@parseTypeCore'(XSt, XType, XPath) :- 'lo.comp.abstract@isBinary'(XSt, "|:", X_2022, X_2023, XR),
    'lo.comp.parseType@parseTypeCore'(XR, XType, XPath).
'lo.comp.parseType@parseTypeCore'(XSt, XType, XPath) :- 'lo.comp.abstract@isBinary'(XSt, "<~", X_2024, XL, X_2025),
    'lo.comp.parseType@parseTypeCore'(XL, XType, XPath).
'lo.comp.parseType@parseTypeCore'(XN, 'lo.comp.types#tipe'(XX30839), XPath) :- 'lo.comp.abstract@isIden'(XN, X_2026, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#tpe', XX30837),
    'lo.comp.misc@subPath'(XPath, XX30837, XNm, XX30839).
'lo.comp.parseType@parseTypeCore'(XT, 'lo.comp.types#tpFun'(XX30850, XX30852), XPath) :- 'lo.comp.abstract@isSquareTerm'(XT, X_2027, XN, XA),
    'lo.comp.abstract@isIden'(XN, X_2028, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#tpe', XX30848),
    'lo.comp.misc@subPath'(XPath, XX30848, XNm, XX30850),
    'lo.list@length'(XA, XX30852).
'lo.comp.parseType@rewriteConstraints'('lo.core#[]', X_2029, XCx, XCx).
'lo.comp.parseType@rewriteConstraints'('lo.core#,..'(XCon, XCons), XQ, XC0, XCx) :- ocall('freshen%3'(XCon, XQ, XX30876),'lo.comp.freshen$freshen$lo.comp.types*constraint','lo.comp.freshen$freshen$lo.comp.types*constraint'),
    (X_2030, XFCon) = XX30876,
    'lo.comp.parseType@addConstraint'(XFCon, XC0, XC1),
    'lo.comp.parseType@rewriteConstraints'(XCons, XQ, XC1, XCx).
'lo.comp.parseType^parseTypeBound'('_call%6'(XV3969, XV3970, XV3971, XV3972, XV3973, XV3974), 'lo.comp.parseType^parseTypeBound', _) :- 'lo.comp.parseType@parseTypeBound'(XV3969, XV3970, XV3971, XV3972, XV3973, XV3974).
'lo.comp.parseType@one33'(XC0, XCon) :- ocall('in%2'(XCon, XC0),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.parseType@neg35'(XC0, XCon) :- ocall('in%2'(XCon, XC0),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.parseType@neg35'(XC0, XCon).
'lo.comp.parseType^addConstraint'('_call%3'(XV3975, XV3976, XV3977), 'lo.comp.parseType^addConstraint', _) :- 'lo.comp.parseType@addConstraint'(XV3975, XV3976, XV3977).
'lo.comp.parseType^parseContractName'('_call%6'(XV3978, XV3979, XV3980, XV3981, XV3982, XV3983), 'lo.comp.parseType^parseContractName', _) :- 'lo.comp.parseType@parseContractName'(XV3978, XV3979, XV3980, XV3981, XV3982, XV3983).
'lo.comp.parseType^prsTypeName'('_call%7'(XV3984, XV3985, XV3986, XV3987, XV3988, XV3989, XV3990), 'lo.comp.parseType^prsTypeName', _) :- 'lo.comp.parseType@prsTypeName'(XV3984, XV3985, XV3986, XV3987, XV3988, XV3989, XV3990).
'lo.comp.parseType^validTypeOp'('_call%2'(XV3991, XV3992), 'lo.comp.parseType^validTypeOp', _) :- 'lo.comp.parseType@validTypeOp'(XV3991, XV3992).
'lo.comp.parseType@one34'(XX29461, XArgs, XOp) :- 'lo.list@length'(XArgs, XX29461),
    'lo.comp.parseType@validTypeOp'(XOp, XX29461),
    !.
'lo.comp.parseType^applyTypeExp'('_call%6'(XV3993, XV3994, XV3995, XV3996, XV3997, XV3998), 'lo.comp.parseType^applyTypeExp', _) :- 'lo.comp.parseType@applyTypeExp'(XV3993, XV3994, XV3995, XV3996, XV3997, XV3998).
'lo.comp.parseType^prsArgType'('_call%8'(XV3999, XV4000, XV4001, XV4002, XV4003, XV4004, XV4005, XV4006), 'lo.comp.parseType^prsArgType', _) :- 'lo.comp.parseType@prsArgType'(XV3999, XV4000, XV4001, XV4002, XV4003, XV4004, XV4005, XV4006).
'lo.comp.parseType^prsTypes'('_call%8'(XV4007, XV4008, XV4009, XV4010, XV4011, XV4012, XV4013, XV4014), 'lo.comp.parseType^prsTypes', _) :- 'lo.comp.parseType@prsTypes'(XV4007, XV4008, XV4009, XV4010, XV4011, XV4012, XV4013, XV4014).
'lo.comp.parseType^parseContractArgs'('_call%9'(XV4015, XV4016, XV4017, XV4018, XV4019, XV4020, XV4021, XV4022, XV4023), 'lo.comp.parseType^parseContractArgs', _) :- 'lo.comp.parseType@parseContractArgs'(XV4015, XV4016, XV4017, XV4018, XV4019, XV4020, XV4021, XV4022, XV4023).
'lo.comp.parseType^prsTypeFields'('_call%8'(XV4024, XV4025, XV4026, XV4027, XV4028, XV4029, XV4030, XV4031), 'lo.comp.parseType^prsTypeFields', _) :- 'lo.comp.parseType@prsTypeFields'(XV4024, XV4025, XV4026, XV4027, XV4028, XV4029, XV4030, XV4031).
'lo.comp.parseType^parseConstraint'('_call%7'(XV4032, XV4033, XV4034, XV4035, XV4036, XV4037, XV4038), 'lo.comp.parseType^parseConstraint', _) :- 'lo.comp.parseType@parseConstraint'(XV4032, XV4033, XV4034, XV4035, XV4036, XV4037, XV4038).
'lo.comp.parseType^prsType'('_call%8'(XV4039, XV4040, XV4041, XV4042, XV4043, XV4044, XV4045, XV4046), 'lo.comp.parseType^prsType', _) :- 'lo.comp.parseType@prsType'(XV4039, XV4040, XV4041, XV4042, XV4043, XV4044, XV4045, XV4046).
'lo.comp.parseType^parseType'('_call%5'(XV4047, XV4048, XV4049, XV4050, XV4051), 'lo.comp.parseType^parseType', _) :- 'lo.comp.parseType@parseType'(XV4047, XV4048, XV4049, XV4050, XV4051).
'lo.comp.parseType^reQuant'('_call%3'(XV4052, XV4053, XV4054), 'lo.comp.parseType^reQuant', _) :- 'lo.comp.parseType@reQuant'(XV4052, XV4053, XV4054).
'lo.comp.parseType^wrapConstraints'('_call%3'(XV4055, XV4056, XV4057), 'lo.comp.parseType^wrapConstraints', _) :- 'lo.comp.parseType@wrapConstraints'(XV4055, XV4056, XV4057).
'lo.comp.parseType@cond20'(XRp0, XLc, XX30209, XX30201, XH, XRpx, XRp, XV, XNm, XB) :- ocall('present%3'(XB, XNm, XV),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !,
    XRp = XRpx.
'lo.comp.parseType@cond20'(XRp0, XLc, XX30209, XX30201, XH, XRpx, XRp, XV, XNm, XB) :- ocall('disp%2'(XH, XX30201),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type argument "), 'lo.core#,..'(XX30201, 'lo.core#,..'('lo.core#ss'(" not quantified "), 'lo.core#[]')))), XX30209),
    'lo.comp.errors@reportError'(XX30209, XLc, XRp, XRp0).
'lo.comp.parseType^parseHeadArgs'('_call%5'(XV4058, XV4059, XV4060, XV4061, XV4062), 'lo.comp.parseType^parseHeadArgs', _) :- 'lo.comp.parseType@parseHeadArgs'(XV4058, XV4059, XV4060, XV4061, XV4062).
'lo.comp.parseType^parseTypeHead'('_call%6'(XV4063, XV4064, XV4065, XV4066, XV4067, XV4068), 'lo.comp.parseType^parseTypeHead', _) :- 'lo.comp.parseType@parseTypeHead'(XV4063, XV4064, XV4065, XV4066, XV4067, XV4068).
'lo.comp.parseType^parseTypeRule'('_call%7'(XV4069, XV4070, XV4071, XV4072, XV4073, XV4074, XV4075), 'lo.comp.parseType^parseTypeRule', _) :- 'lo.comp.parseType@parseTypeRule'(XV4069, XV4070, XV4071, XV4072, XV4073, XV4074, XV4075).
'lo.comp.parseType^parseConBound'('_call%6'(XV4076, XV4077, XV4078, XV4079, XV4080, XV4081), 'lo.comp.parseType^parseConBound', _) :- 'lo.comp.parseType@parseConBound'(XV4076, XV4077, XV4078, XV4079, XV4080, XV4081).
'lo.comp.parseType^parseContractHead'('_call%11'(XV4082, XV4083, XV4084, XV4085, XV4086, XV4087, XV4088, XV4089, XV4090, XV4091, XV4092), 'lo.comp.parseType^parseContractHead', _) :- 'lo.comp.parseType@parseContractHead'(XV4082, XV4083, XV4084, XV4085, XV4086, XV4087, XV4088, XV4089, XV4090, XV4091, XV4092).
'lo.comp.parseType^parseContractSpec'('_call%13'(XV4093, XV4094, XV4095, XV4096, XV4097, XV4098, XV4099, XV4100, XV4101, XV4102, XV4103, XV4104, XV4105), 'lo.comp.parseType^parseContractSpec', _) :- 'lo.comp.parseType@parseContractSpec'(XV4093, XV4094, XV4095, XV4096, XV4097, XV4098, XV4099, XV4100, XV4101, XV4102, XV4103, XV4104, XV4105).
'lo.comp.parseType^reConQuant'('_call%3'(XV4106, XV4107, XV4108), 'lo.comp.parseType^reConQuant', _) :- 'lo.comp.parseType@reConQuant'(XV4106, XV4107, XV4108).
'lo.comp.parseType^wrapConConstraints'('_call%3'(XV4109, XV4110, XV4111), 'lo.comp.parseType^wrapConConstraints', _) :- 'lo.comp.parseType@wrapConConstraints'(XV4109, XV4110, XV4111).
'lo.comp.parseType^parseContract'('_call%6'(XV4112, XV4113, XV4114, XV4115, XV4116, XV4117), 'lo.comp.parseType^parseContract', _) :- 'lo.comp.parseType@parseContract'(XV4112, XV4113, XV4114, XV4115, XV4116, XV4117).
'lo.comp.parseType^mergeConstraints'('_call%3'(XV4118, XV4119, XV4120), 'lo.comp.parseType^mergeConstraints', _) :- 'lo.comp.parseType@mergeConstraints'(XV4118, XV4119, XV4120).
'lo.comp.parseType@cond21'(XX30771, XX30763, XDeps, XArgTps, XRpx, XRp0, XDps, XATs, XOp, XEnv, XN, XLc) :- 'lo.comp.parseType@parseContractName'(XLc, XN, XEnv, 'lo.comp.types#conTract'(XOp, XATs, XDps), XRp0, XRpx),
    !,
    'lo.comp.unify@sameType'('lo.comp.types#tupleType'(XATs), 'lo.comp.types#tupleType'(XArgTps), XEnv),
    'lo.comp.unify@sameType'('lo.comp.types#tupleType'(XDps), 'lo.comp.types#tupleType'(XDeps), XEnv).
'lo.comp.parseType@cond21'(XX30771, XX30763, XDeps, XArgTps, XRpx, XRp0, XDps, XATs, XOp, XEnv, XN, XLc) :- ocall('disp%2'(XN, XX30763),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("contract "), 'lo.core#,..'(XX30763, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]')))), XX30771),
    'lo.comp.errors@reportError'(XX30771, XLc, XRp0, XRpx),
    XOp = XN.
'lo.comp.parseType^parseContractCon'('_call%9'(XV4121, XV4122, XV4123, XV4124, XV4125, XV4126, XV4127, XV4128, XV4129), 'lo.comp.parseType^parseContractCon', _) :- 'lo.comp.parseType@parseContractCon'(XV4121, XV4122, XV4123, XV4124, XV4125, XV4126, XV4127, XV4128, XV4129).
'lo.comp.parseType@one35'(XRpx, XRp, XTp, XN, XCons, XXV58, XEnv, XT) :- ocall('_empty%1'(XXV58),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@parseContractCon'(XT, XEnv, XXV58, 'lo.core#[]', XCons, XN, XTp, XRp, XRpx),
    !.
'lo.comp.parseType^parseContractConstraint'('_call%6'(XV4130, XV4131, XV4132, XV4133, XV4134, XV4135), 'lo.comp.parseType^parseContractConstraint', _) :- 'lo.comp.parseType@parseContractConstraint'(XV4130, XV4131, XV4132, XV4133, XV4134, XV4135).
'lo.comp.parseType^parseTypeCore'('_call%3'(XV4136, XV4137, XV4138), 'lo.comp.parseType^parseTypeCore', _) :- 'lo.comp.parseType@parseTypeCore'(XV4136, XV4137, XV4138).
'lo.comp.parseType^rewriteConstraints'('_call%4'(XV4139, XV4140, XV4141, XV4142), 'lo.comp.parseType^rewriteConstraints', _) :- 'lo.comp.parseType@rewriteConstraints'(XV4139, XV4140, XV4141, XV4142).
