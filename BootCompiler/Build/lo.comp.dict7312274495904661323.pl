'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.dict'e'*'n16o16'()16'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.escapes'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'s\"I20'vr'CT2t'lo.comp.canon*canonTerm't'lo.comp.types*tipe't'lo.comp.dict*vr''scope'CT5Uz2'lo.index*map'2St'lo.comp.dict*typeDef'Uz2'lo.index*map'2St'lo.comp.dict*vr'Lt'lo.comp.types*constraint'Uz2'lo.index*map'2St'lo.comp.types*conEntry'Uz2'lo.index*map'2SUz2'lo.index*map'2St'lo.comp.types*implEntry't'lo.comp.dict*env''tpDef'CT3t'lo.comp.location*location't'lo.comp.types*tipe't'lo.comp.types*tipe't'lo.comp.dict*typeDef''typeInDict'PT3SLt'lo.comp.dict*env't'lo.comp.types*tipe''isType'PT4SLt'lo.comp.dict*env't'lo.comp.location*location't'lo.comp.types*tipe''declareType'FT3St'lo.comp.dict*typeDef'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''declareTypeExists'FT4St'lo.comp.location*location't'lo.comp.types*tipe'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''typeOfVar'FT2SLt'lo.comp.dict*env'Uz1'lo.core*option'1t'lo.comp.dict*vr''isVar'PT3SLt'lo.comp.dict*env't'lo.comp.dict*vr''declareVar'FT3St'lo.comp.dict*vr'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''topVars'FT1Lt'lo.comp.dict*env'Uz2'lo.index*map'2St'lo.comp.dict*vr''pushScope'FT1Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''declareConstraint'FT2t'lo.comp.types*constraint'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''definedConstraint'PT2t'lo.comp.types*constraint'Lt'lo.comp.dict*env''declareContract'FT3St'lo.comp.types*conEntry'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''getContract'PT3SLt'lo.comp.dict*env't'lo.comp.types*conEntry''isContract'PT3SLt'lo.comp.dict*env't'lo.comp.types*conEntry''declareImplementation'FT4SSt'lo.comp.types*implEntry'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''isImplemented'PT3t'lo.comp.types*constraint'Lt'lo.comp.dict*env't'lo.comp.types*implEntry''stdDict'Lt'lo.comp.dict*env'\"s\"I3'vr'Yt'lo.comp.dict*vr'I0'env'Yt'lo.comp.dict*env'I0'typeDef'Yt'lo.comp.dict*typeDef'I0\"n0o0'()0'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.comp.dict*env's\"c'lo.core$display'T1t'lo.comp.dict*env'T0\"n2o2'()2's'lo.core$display$lo.comp.dict*vr's\"c'lo.core$display'T1t'lo.comp.dict*vr'T0\"").
'lo.comp.dict@init'() :- !.
'lo.comp.dict#vr'('vr%1'('lo.comp.dict@vr'())) :- !.
'lo.comp.dict#scope'('scope%1'('lo.comp.dict@scope'())) :- !.
'lo.comp.dict#tpDef'('tpDef%1'('lo.comp.dict@tpDef'())) :- !.
'lo.comp.dict@showEnv'('lo.comp.dict#scope'(XTps, XVars, XConstraints, XContracts, XImplementations), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("scope types: "), 'lo.core#,..'(XX8811, 'lo.core#,..'('lo.core#ss'("
vars: "), 'lo.core#,..'(XX8817, 'lo.core#[]')))))) :- !,
    ocall('disp%2'(XTps, XX8811),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.comp.dict*typeDef'),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.comp.dict*typeDef')),
    ocall('disp%2'(XVars, XX8817),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.comp.dict*vr'),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.comp.dict*vr')).
'lo.comp.dict@showEnv'(_, _) :- raise_exception('error'("showEnv", 23, 3, 130)).
'lo.core$display$lo.comp.dict*env'('lo.core$display$lo.comp.dict*env%1'('lo.core$display$lo.comp.dict*env')) :- !.
'lo.core$display$lo.comp.dict*env'('disp%2'(XV1408, XV1409), XLbl248, XThis248) :- !,
    'lo.core$display$lo.comp.dict*env@disp'(XV1408, XV1409, XLbl248, XThis248).
'lo.core$display$lo.comp.dict*env'('disp%1'('lo.core$display$lo.comp.dict*env^disp'(XLbl249, XThis249)), XLbl249, XThis249).
'lo.core$display$lo.comp.dict*env@disp'(XE, XX8829, XLbV227, XThV227) :- !,
    'lo.comp.dict@showEnv'(XE, XX8829).
'lo.core$display$lo.comp.dict*env@disp'(_, _, _, _) :- raise_exception('error'("disp", 19, 5, 21)).
'lo.comp.dict@showTpDef'('lo.comp.dict#tpDef'(XLc, XTp, XFace), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type @"), 'lo.core#,..'(XX8836, 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XX8840, 'lo.core#,..'('lo.core#ss'("
Type: "), 'lo.core#,..'(XX8844, 'lo.core#,..'('lo.core#ss'(" face "), 'lo.core#,..'(XX8848, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))))))) :- !,
    ocall('disp%2'(XLc, XX8836),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location'),
    ocall('disp%2'(XTp, XX8840),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%2'(XTp, XX8844),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%2'(XFace, XX8848),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe').
'lo.comp.dict@showTpDef'(_, _) :- raise_exception('error'("showTpDef", 30, 3, 136)).
'lo.core$display$lo.comp.dict*typeDef'('lo.core$display$lo.comp.dict*typeDef%1'('lo.core$display$lo.comp.dict*typeDef')) :- !.
'lo.core$display$lo.comp.dict*typeDef'('disp%2'(XV1416, XV1417), XLbl250, XThis250) :- !,
    'lo.core$display$lo.comp.dict*typeDef@disp'(XV1416, XV1417, XLbl250, XThis250).
'lo.core$display$lo.comp.dict*typeDef'('disp%1'('lo.core$display$lo.comp.dict*typeDef^disp'(XLbl251, XThis251)), XLbl251, XThis251).
'lo.core$display$lo.comp.dict*typeDef@disp'(XD, XX8864, XLbV228, XThV228) :- !,
    'lo.comp.dict@showTpDef'(XD, XX8864).
'lo.core$display$lo.comp.dict*typeDef@disp'(_, _, _, _) :- raise_exception('error'("disp", 26, 5, 23)).
'lo.comp.dict@showVr'('lo.comp.dict#vr'(XT, XTp), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XX8870, 'lo.core#,..'('lo.core#ss'(" |= "), 'lo.core#,..'(XX8874, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))) :- !,
    ocall('disp%2'(XT, XX8870),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    ocall('disp%2'(XTp, XX8874),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe').
'lo.comp.dict@showVr'(_, _) :- raise_exception('error'("showVr", 37, 3, 73)).
'lo.core$display$lo.comp.dict*vr'('lo.core$display$lo.comp.dict*vr%1'('lo.core$display$lo.comp.dict*vr')) :- !.
'lo.core$display$lo.comp.dict*vr'('disp%2'(XV1424, XV1425), XLbl252, XThis252) :- !,
    'lo.core$display$lo.comp.dict*vr@disp'(XV1424, XV1425, XLbl252, XThis252).
'lo.core$display$lo.comp.dict*vr'('disp%1'('lo.core$display$lo.comp.dict*vr^disp'(XLbl253, XThis253)), XLbl253, XThis253).
'lo.core$display$lo.comp.dict*vr@disp'(XD, XX8886, XLbV229, XThV229) :- !,
    'lo.comp.dict@showVr'(XD, XX8886).
'lo.core$display$lo.comp.dict*vr@disp'(_, _, _, _) :- raise_exception('error'("disp", 33, 5, 20)).
'lo.comp.dict@typeInD'(XKey, 'lo.core#,..'('lo.comp.dict#scope'(XDefs, X_621, X_622, X_623, X_624), X_625), XDef) :- ocall('present%3'(XDefs, XKey, XDef),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@typeInD'(XKey, 'lo.core#,..'(X_626, XEnv), XDef) :- 'lo.comp.dict@typeInD'(XKey, XEnv, XDef).
'lo.comp.dict@localTypeName'(XNm, XX8912) :- !,
    'lo.comp.misc@pathSuffix'(XNm, "*", XX8912).
'lo.comp.dict@localTypeName'(_, _) :- raise_exception('error'("localTypeName", 65, 3, 39)).
'lo.comp.dict@typeInDict'(XNm, XEnv, XTp) :- 'lo.comp.dict@one3'(XTp, X_628, X_627, XEnv, XX8917, XNm).
'lo.comp.dict@isType'(XNm, XEnv, XLc, XTp) :- 'lo.comp.dict@one4'(X_629, XTp, XLc, XEnv, XX8928, XNm).
'lo.comp.dict@tpInDict'(XNm, XEnv, 'lo.core#some'(XD)) :- 'lo.comp.dict@localTypeName'(XNm, XX8937),
    'lo.comp.dict@typeInD'(XX8937, XEnv, XD),
    !.
'lo.comp.dict@tpInDict'(X_630, X_631, 'lo.core#none') :- !.
'lo.comp.dict@tpInDict'(_, _, _) :- raise_exception('error'("tpInDict", 48, 3, 63)).
'lo.comp.dict@declareType'(XNm, XTp, 'lo.core#,..'('lo.comp.dict#scope'(XTypes, XNames, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XX8958, XNames, XCons, XContracts, XImps), XE)) :- !,
    ocall('_put%4'(XTypes, XNm, XTp, XX8958),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@declareType'(_, _, _, _) :- raise_exception('error'("declareType", 56, 3, 125)).
'lo.comp.dict@declareTypeExists'(XNm, XLc, XTp, XEnv, XX8978) :- !,
    'lo.comp.dict@declareType'(XNm, 'lo.comp.dict#tpDef'(XLc, XTp, 'lo.comp.types#voidType'), XEnv, XX8978).
'lo.comp.dict@declareTypeExists'(_, _, _, _, _) :- raise_exception('error'("declareTypeExists", 53, 3, 76)).
'lo.comp.dict@standardType'(XNm, XTp, 'lo.core#,..'('lo.comp.dict#scope'(XTypes, XNames, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XX8998, XNames, XCons, XContracts, XImps), XE)) :- !,
    ocall('_put%4'(XTypes, XNm, 'lo.comp.dict#tpDef'('lo.comp.location#std', XTp, 'lo.comp.types#typeRule'(XTp, 'lo.comp.types#faceType'('lo.core#[]'))), XX8998),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@standardType'(_, _, _, _) :- raise_exception('error'("standardType", 60, 3, 164)).
'lo.comp.dict@varInD'(XNm, 'lo.core#,..'('lo.comp.dict#scope'(X_632, XVars, X_633, X_634, X_635), X_636), XD) :- ocall('present%3'(XVars, XNm, XD),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@varInD'(XNm, 'lo.core#,..'(X_637, XE), XD) :- 'lo.comp.dict@varInD'(XNm, XE, XD).
'lo.comp.dict@typeOfVar'(XNm, XEnv, 'lo.core#some'(XD)) :- 'lo.comp.dict@varInD'(XNm, XEnv, XD),
    !.
'lo.comp.dict@typeOfVar'(X_638, X_639, 'lo.core#none') :- !.
'lo.comp.dict@typeOfVar'(_, _, _) :- raise_exception('error'("typeOfVar", 74, 3, 50)).
'lo.comp.dict@isVar'(XNm, X_640, 'lo.comp.dict#vr'('lo.comp.canon#v'('lo.comp.location#std', XNm), XX9047)) :- 'lo.comp.escapes@isEscape'(XNm),
    'lo.comp.escapes@escapeType'(XNm, XX9047).
'lo.comp.dict@isVar'(XNm, XEnv, XDef) :- 'lo.comp.dict@one5'(XDef, XEnv, XNm).
'lo.comp.dict@declareVar'(XNm, XV, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XX9070, XCons, XContracts, XImps), XE)) :- !,
    ocall('_put%4'(XNms, XNm, XV, XX9070),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@declareVar'(_, _, _, _) :- raise_exception('error'("declareVar", 89, 3, 108)).
'lo.comp.dict@topVars'('lo.core#,..'('lo.comp.dict#scope'(X_641, XVars, X_642, X_643, X_644), X_645), XVars) :- !.
'lo.comp.dict@topVars'(_, _) :- raise_exception('error'("topVars", 93, 3, 42)).
'lo.comp.dict@pushScope'(XE, 'lo.core#,..'('lo.comp.dict#scope'(XXV19, XXV20, 'lo.core#[]', XXV21, XXV22), XE)) :- !,
    ocall('_empty%1'(XXV19),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV20),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV21),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV22),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@pushScope'(_, _) :- raise_exception('error'("pushScope", 96, 3, 43)).
'lo.comp.dict@declareConstraint'(XC, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, 'lo.core#,..'(XC, XCons), XContracts, XImps), XE)) :- !.
'lo.comp.dict@declareConstraint'(_, _, _) :- raise_exception('error'("declareConstraint", 99, 3, 111)).
'lo.comp.dict@definedConstraint'(XCx, 'lo.core#,..'('lo.comp.dict#scope'(X_646, X_647, XCons, X_648, X_649), X_650)) :- ocall('in%2'(XCx, XCons),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.dict@definedConstraint'(XCx, 'lo.core#,..'(X_651, XE)) :- 'lo.comp.dict@definedConstraint'(XCx, XE).
'lo.comp.dict@declareContract'(XNm, XCon, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XX9154, XImps), XE)) :- !,
    ocall('_put%4'(XContracts, XNm, XCon, XX9154),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@declareContract'(_, _, _, _) :- raise_exception('error'("declareContract", 109, 3, 117)).
'lo.comp.dict@conInD'(XNm, 'lo.core#,..'('lo.comp.dict#scope'(X_652, X_653, X_654, XCons, X_655), X_656), XDec) :- ocall('present%3'(XCons, XNm, XDec),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@conInD'(XNm, 'lo.core#,..'(X_657, XEnv), XCon) :- 'lo.comp.dict@conInD'(XNm, XEnv, XCon).
'lo.comp.dict@getContract'(XNm, XEnv, XDef) :- 'lo.comp.dict@conInD'(XNm, XEnv, XDef).
'lo.comp.dict@isContract'(XNm, XEnv, XDef) :- 'lo.comp.dict@one6'(XDef, XEnv, XNm).
'lo.comp.dict@declareImplementation'(XNm, XImplNm, XCon, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XX9224), XE)) :- ocall('present%3'(XImps, XNm, XI),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !,
    ocall('_put%4'(XI, XImplNm, XCon, XX9221),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XImps, XNm, XX9221, XX9224),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@declareImplementation'(XNm, XImplNm, XCon, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XX9254), XE)) :- !,
    ocall('_empty%1'(XXV23),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XXV23, XImplNm, XCon, XX9251),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XImps, XNm, XX9251, XX9254),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@declareImplementation'(_, _, _, _, _) :- raise_exception('error'("declareImplementation", 125, 3, 164)).
'lo.comp.dict@impInD'(XNm, 'lo.core#,..'('lo.comp.dict#scope'(X_658, X_659, X_660, X_661, XImpls), X_662), XDec) :- ocall('present%3'(XImpls, XNm, XDec),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@impInD'(XNm, 'lo.core#,..'(X_663, XEnv), XCon) :- 'lo.comp.dict@impInD'(XNm, XEnv, XCon).
'lo.comp.dict@isImplemented'('lo.comp.types#conTract'(XNm, XArgs, XDeps), XEnv, XImpl) :- 'lo.comp.dict@impInD'(XNm, XEnv, XImpls),
    'lo.comp.types@implementationName'('lo.comp.types#conTract'(XNm, XArgs, XDeps), XX9297),
    ocall('present%3'(XImpls, XX9297, XImpl),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@stdDict'(XX9308) :- !,
    'lo.comp.dict@pushScope'('lo.core#[]', XX9305),
    'lo.comp.dict@standardType'("float", 'lo.comp.types#tipe'("lo.core*float"), XX9305, XX9306),
    'lo.comp.dict@standardType'("integer", 'lo.comp.types#tipe'("lo.core*integer"), XX9306, XX9307),
    'lo.comp.dict@standardType'("string", 'lo.comp.types#tipe'("lo.core*string"), XX9307, XX9308).
'lo.comp.dict^showEnv'('_call%2'(XV1404, XV1405), 'lo.comp.dict^showEnv', _) :- 'lo.comp.dict@showEnv'(XV1404, XV1405).
'lo.core$display$lo.comp.dict*env^disp'('_call%2'(XV1406, XV1407), 'lo.core$display$lo.comp.dict*env^disp'(XLbV227, XThV227), _) :- 'lo.core$display$lo.comp.dict*env@disp'(XV1406, XV1407, XLbV227, XThV227).
'lo.core$display$lo.comp.dict*env^disp'('_call%2'(XV1410, XV1411), 'lo.core$display$lo.comp.dict*env^disp'(XLbV227, XThV227), _) :- 'lo.core$display$lo.comp.dict*env@disp'(XV1410, XV1411, XLbV227, XThV227).
'lo.comp.dict^showTpDef'('_call%2'(XV1412, XV1413), 'lo.comp.dict^showTpDef', _) :- 'lo.comp.dict@showTpDef'(XV1412, XV1413).
'lo.core$display$lo.comp.dict*typeDef^disp'('_call%2'(XV1414, XV1415), 'lo.core$display$lo.comp.dict*typeDef^disp'(XLbV228, XThV228), _) :- 'lo.core$display$lo.comp.dict*typeDef@disp'(XV1414, XV1415, XLbV228, XThV228).
'lo.core$display$lo.comp.dict*typeDef^disp'('_call%2'(XV1418, XV1419), 'lo.core$display$lo.comp.dict*typeDef^disp'(XLbV228, XThV228), _) :- 'lo.core$display$lo.comp.dict*typeDef@disp'(XV1418, XV1419, XLbV228, XThV228).
'lo.comp.dict^showVr'('_call%2'(XV1420, XV1421), 'lo.comp.dict^showVr', _) :- 'lo.comp.dict@showVr'(XV1420, XV1421).
'lo.core$display$lo.comp.dict*vr^disp'('_call%2'(XV1422, XV1423), 'lo.core$display$lo.comp.dict*vr^disp'(XLbV229, XThV229), _) :- 'lo.core$display$lo.comp.dict*vr@disp'(XV1422, XV1423, XLbV229, XThV229).
'lo.core$display$lo.comp.dict*vr^disp'('_call%2'(XV1426, XV1427), 'lo.core$display$lo.comp.dict*vr^disp'(XLbV229, XThV229), _) :- 'lo.core$display$lo.comp.dict*vr@disp'(XV1426, XV1427, XLbV229, XThV229).
'lo.comp.dict^typeInD'('_call%3'(XV1428, XV1429, XV1430), 'lo.comp.dict^typeInD', _) :- 'lo.comp.dict@typeInD'(XV1428, XV1429, XV1430).
'lo.comp.dict^localTypeName'('_call%2'(XV1431, XV1432), 'lo.comp.dict^localTypeName', _) :- 'lo.comp.dict@localTypeName'(XV1431, XV1432).
'lo.comp.dict@one3'(XTp, X_628, X_627, XEnv, XX8917, XNm) :- 'lo.comp.dict@localTypeName'(XNm, XX8917),
    'lo.comp.dict@typeInD'(XX8917, XEnv, 'lo.comp.dict#tpDef'(X_627, X_628, XTp)),
    !.
'lo.comp.dict^typeInDict'('_call%3'(XV1433, XV1434, XV1435), 'lo.comp.dict^typeInDict', _) :- 'lo.comp.dict@typeInDict'(XV1433, XV1434, XV1435).
'lo.comp.dict@one4'(X_629, XTp, XLc, XEnv, XX8928, XNm) :- 'lo.comp.dict@localTypeName'(XNm, XX8928),
    'lo.comp.dict@typeInD'(XX8928, XEnv, 'lo.comp.dict#tpDef'(XLc, XTp, X_629)),
    !.
'lo.comp.dict^isType'('_call%4'(XV1436, XV1437, XV1438, XV1439), 'lo.comp.dict^isType', _) :- 'lo.comp.dict@isType'(XV1436, XV1437, XV1438, XV1439).
'lo.comp.dict^tpInDict'('_call%3'(XV1440, XV1441, XV1442), 'lo.comp.dict^tpInDict', _) :- 'lo.comp.dict@tpInDict'(XV1440, XV1441, XV1442).
'lo.comp.dict^declareType'('_call%4'(XV1443, XV1444, XV1445, XV1446), 'lo.comp.dict^declareType', _) :- 'lo.comp.dict@declareType'(XV1443, XV1444, XV1445, XV1446).
'lo.comp.dict^declareTypeExists'('_call%5'(XV1447, XV1448, XV1449, XV1450, XV1451), 'lo.comp.dict^declareTypeExists', _) :- 'lo.comp.dict@declareTypeExists'(XV1447, XV1448, XV1449, XV1450, XV1451).
'lo.comp.dict^standardType'('_call%4'(XV1452, XV1453, XV1454, XV1455), 'lo.comp.dict^standardType', _) :- 'lo.comp.dict@standardType'(XV1452, XV1453, XV1454, XV1455).
'lo.comp.dict^varInD'('_call%3'(XV1456, XV1457, XV1458), 'lo.comp.dict^varInD', _) :- 'lo.comp.dict@varInD'(XV1456, XV1457, XV1458).
'lo.comp.dict^typeOfVar'('_call%3'(XV1459, XV1460, XV1461), 'lo.comp.dict^typeOfVar', _) :- 'lo.comp.dict@typeOfVar'(XV1459, XV1460, XV1461).
'lo.comp.dict@one5'(XDef, XEnv, XNm) :- 'lo.comp.dict@varInD'(XNm, XEnv, XDef),
    !.
'lo.comp.dict^isVar'('_call%3'(XV1462, XV1463, XV1464), 'lo.comp.dict^isVar', _) :- 'lo.comp.dict@isVar'(XV1462, XV1463, XV1464).
'lo.comp.dict^declareVar'('_call%4'(XV1465, XV1466, XV1467, XV1468), 'lo.comp.dict^declareVar', _) :- 'lo.comp.dict@declareVar'(XV1465, XV1466, XV1467, XV1468).
'lo.comp.dict^topVars'('_call%2'(XV1469, XV1470), 'lo.comp.dict^topVars', _) :- 'lo.comp.dict@topVars'(XV1469, XV1470).
'lo.comp.dict^pushScope'('_call%2'(XV1471, XV1472), 'lo.comp.dict^pushScope', _) :- 'lo.comp.dict@pushScope'(XV1471, XV1472).
'lo.comp.dict^declareConstraint'('_call%3'(XV1473, XV1474, XV1475), 'lo.comp.dict^declareConstraint', _) :- 'lo.comp.dict@declareConstraint'(XV1473, XV1474, XV1475).
'lo.comp.dict^definedConstraint'('_call%2'(XV1476, XV1477), 'lo.comp.dict^definedConstraint', _) :- 'lo.comp.dict@definedConstraint'(XV1476, XV1477).
'lo.comp.dict^declareContract'('_call%4'(XV1478, XV1479, XV1480, XV1481), 'lo.comp.dict^declareContract', _) :- 'lo.comp.dict@declareContract'(XV1478, XV1479, XV1480, XV1481).
'lo.comp.dict^conInD'('_call%3'(XV1482, XV1483, XV1484), 'lo.comp.dict^conInD', _) :- 'lo.comp.dict@conInD'(XV1482, XV1483, XV1484).
'lo.comp.dict^getContract'('_call%3'(XV1485, XV1486, XV1487), 'lo.comp.dict^getContract', _) :- 'lo.comp.dict@getContract'(XV1485, XV1486, XV1487).
'lo.comp.dict@one6'(XDef, XEnv, XNm) :- 'lo.comp.dict@conInD'(XNm, XEnv, XDef),
    !.
'lo.comp.dict^isContract'('_call%3'(XV1488, XV1489, XV1490), 'lo.comp.dict^isContract', _) :- 'lo.comp.dict@isContract'(XV1488, XV1489, XV1490).
'lo.comp.dict^declareImplementation'('_call%5'(XV1491, XV1492, XV1493, XV1494, XV1495), 'lo.comp.dict^declareImplementation', _) :- 'lo.comp.dict@declareImplementation'(XV1491, XV1492, XV1493, XV1494, XV1495).
'lo.comp.dict^impInD'('_call%3'(XV1496, XV1497, XV1498), 'lo.comp.dict^impInD', _) :- 'lo.comp.dict@impInD'(XV1496, XV1497, XV1498).
'lo.comp.dict^isImplemented'('_call%3'(XV1499, XV1500, XV1501), 'lo.comp.dict^isImplemented', _) :- 'lo.comp.dict@isImplemented'(XV1499, XV1500, XV1501).
