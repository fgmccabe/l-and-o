'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.dict's'0.0.1'n6o6'()6'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.escapes'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I20'vr'CT2t'lo.comp.canon*canonTerm't'lo.comp.types*tipe't'lo.comp.dict*vr''scope'CT5Uz2'lo.index*map'2St'lo.comp.dict*typeDef'Uz2'lo.index*map'2St'lo.comp.dict*vr'Lt'lo.comp.types*constraint'Uz2'lo.index*map'2St'lo.comp.types*conEntry'Uz2'lo.index*map'2SUz2'lo.index*map'2St'lo.comp.types*implEntry't'lo.comp.dict*env''tpDef'CT3t'lo.comp.location*location't'lo.comp.types*tipe't'lo.comp.types*tipe't'lo.comp.dict*typeDef''typeInDict'PT3SLt'lo.comp.dict*env't'lo.comp.types*tipe''isType'PT4SLt'lo.comp.dict*env't'lo.comp.location*location't'lo.comp.types*tipe''declareType'FT3St'lo.comp.dict*typeDef'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''declareTypeExists'FT4St'lo.comp.location*location't'lo.comp.types*tipe'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''typeOfVar'FT2SLt'lo.comp.dict*env'Uz1'lo.core*option'1t'lo.comp.dict*vr''isVar'PT3SLt'lo.comp.dict*env't'lo.comp.dict*vr''declareVar'FT3St'lo.comp.dict*vr'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''topVars'FT1Lt'lo.comp.dict*env'Uz2'lo.index*map'2St'lo.comp.dict*vr''pushScope'FT1Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''declareConstraint'FT2t'lo.comp.types*constraint'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''definedConstraint'PT2t'lo.comp.types*constraint'Lt'lo.comp.dict*env''declareContract'FT3St'lo.comp.types*conEntry'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''getContract'PT3SLt'lo.comp.dict*env't'lo.comp.types*conEntry''isContract'PT3SLt'lo.comp.dict*env't'lo.comp.types*conEntry''declareImplementation'FT4SSt'lo.comp.types*implEntry'Lt'lo.comp.dict*env'Lt'lo.comp.dict*env''isImplemented'PT3t'lo.comp.types*constraint'Lt'lo.comp.dict*env't'lo.comp.types*implEntry''stdDict'Lt'lo.comp.dict*env'\"s\"I3'vr'Yt'lo.comp.dict*vr'I0'env'Yt'lo.comp.dict*env'I0'typeDef'Yt'lo.comp.dict*typeDef'I0\"n3o3'()3's'vr's'scope's'tpDef'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.comp.dict*env's\"c'lo.core$display'T1t'lo.comp.dict*env'T0\"n2o2'()2's'lo.core$display$lo.comp.dict*vr's\"c'lo.core$display'T1t'lo.comp.dict*vr'T0\"").
'lo.comp.dict@init'():- !.
'lo.comp.dict#vr'('vr%1'('lo.comp.dict@vr'())):- !.
'lo.comp.dict#scope'('scope%1'('lo.comp.dict@scope'())):- !.
'lo.comp.dict#tpDef'('tpDef%1'('lo.comp.dict@tpDef'())):- !.
'lo.comp.dict@showEnv'('lo.comp.dict#scope'(XTps, XVars, XConstraints, XContracts, XImplementations), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("scope types: "), 'lo.core#,..'(XXe4851, 'lo.core#,..'('lo.core#ss'("
vars: "), 'lo.core#,..'(XXe4852, 'lo.core#[]')))))):- !,
    ocall('disp%1'(XXV5193),'lo.core$display$lo.index*map'('lo.core$display$lo.comp.dict*typeDef', 'lo.core$display$lo.core*string'),'lo.core$display$lo.index*map'('lo.core$display$lo.comp.dict*typeDef', 'lo.core$display$lo.core*string')),
    ocall('disp%1'(XXV5194),'lo.core$display$lo.index*map'('lo.core$display$lo.comp.dict*vr', 'lo.core$display$lo.core*string'),'lo.core$display$lo.index*map'('lo.core$display$lo.comp.dict*vr', 'lo.core$display$lo.core*string')),
    ocall('_call%2'(XTps, XXe4851),XXV5193,XXV5193),
    ocall('_call%2'(XVars, XXe4852),XXV5194,XXV5194).
'lo.comp.dict@showEnv'(_, _):- raise_exception('error'("lo.comp.dict@showEnv", 23, 3, 130)).
'lo.core$display$lo.comp.dict*env'('lo.core$display$lo.comp.dict*env%1'('lo.core$display$lo.comp.dict*env')):- !.
'lo.core$display$lo.comp.dict*env'('disp%2'(XV29942, XV29943), XLbl2182, XThis2182):- !,
    'lo.core$display$lo.comp.dict*env@disp'(XV29942, XV29943, XLbl2182, XThis2182).
'lo.core$display$lo.comp.dict*env'('disp%1'('lo.core$display$lo.comp.dict*env^disp'(XLbl2183, XThis2183)), XLbl2183, XThis2183).
'lo.core$display$lo.comp.dict*env@disp'(XE, XXd38805, XLbV2476, XThV2476):- !,
    'lo.comp.dict@showEnv'(XE, XXd38805).
'lo.core$display$lo.comp.dict*env@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.dict*env@disp", 19, 5, 21)).
'lo.comp.dict@showTpDef'('lo.comp.dict#tpDef'(XLc, XTp, XFace), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type @"), 'lo.core#,..'(XXe4853, 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XXe4854, 'lo.core#,..'('lo.core#ss'("
Type: "), 'lo.core#,..'(XXe4855, 'lo.core#,..'('lo.core#ss'(" face "), 'lo.core#,..'(XXe4856, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))))))):- !,
    ocall('disp%1'(XXV5195),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location'),
    ocall('disp%1'(XXV5196),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%1'(XXV5197),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%1'(XXV5198),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XLc, XXe4853),XXV5195,XXV5195),
    ocall('_call%2'(XTp, XXe4854),XXV5196,XXV5196),
    ocall('_call%2'(XTp, XXe4855),XXV5197,XXV5197),
    ocall('_call%2'(XFace, XXe4856),XXV5198,XXV5198).
'lo.comp.dict@showTpDef'(_, _):- raise_exception('error'("lo.comp.dict@showTpDef", 30, 3, 136)).
'lo.core$display$lo.comp.dict*typeDef'('lo.core$display$lo.comp.dict*typeDef%1'('lo.core$display$lo.comp.dict*typeDef')):- !.
'lo.core$display$lo.comp.dict*typeDef'('disp%2'(XV29948, XV29949), XLbl2184, XThis2184):- !,
    'lo.core$display$lo.comp.dict*typeDef@disp'(XV29948, XV29949, XLbl2184, XThis2184).
'lo.core$display$lo.comp.dict*typeDef'('disp%1'('lo.core$display$lo.comp.dict*typeDef^disp'(XLbl2185, XThis2185)), XLbl2185, XThis2185).
'lo.core$display$lo.comp.dict*typeDef@disp'(XD, XXd38821, XLbV2477, XThV2477):- !,
    'lo.comp.dict@showTpDef'(XD, XXd38821).
'lo.core$display$lo.comp.dict*typeDef@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.dict*typeDef@disp", 26, 5, 23)).
'lo.comp.dict@showVr'('lo.comp.dict#vr'(XT, XTp), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XXe4857, 'lo.core#,..'('lo.core#ss'(" |= "), 'lo.core#,..'(XXe4858, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))):- !,
    ocall('disp%1'(XXV5199),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    ocall('disp%1'(XXV5200),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XT, XXe4857),XXV5199,XXV5199),
    ocall('_call%2'(XTp, XXe4858),XXV5200,XXV5200).
'lo.comp.dict@showVr'(_, _):- raise_exception('error'("lo.comp.dict@showVr", 37, 3, 73)).
'lo.core$display$lo.comp.dict*vr'('lo.core$display$lo.comp.dict*vr%1'('lo.core$display$lo.comp.dict*vr')):- !.
'lo.core$display$lo.comp.dict*vr'('disp%2'(XV29954, XV29955), XLbl2186, XThis2186):- !,
    'lo.core$display$lo.comp.dict*vr@disp'(XV29954, XV29955, XLbl2186, XThis2186).
'lo.core$display$lo.comp.dict*vr'('disp%1'('lo.core$display$lo.comp.dict*vr^disp'(XLbl2187, XThis2187)), XLbl2187, XThis2187).
'lo.core$display$lo.comp.dict*vr@disp'(XD, XXd38831, XLbV2478, XThV2478):- !,
    'lo.comp.dict@showVr'(XD, XXd38831).
'lo.core$display$lo.comp.dict*vr@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.dict*vr@disp", 33, 5, 20)).
'lo.comp.dict@localTypeName'(XNm, XXd38832):- !,
    'lo.comp.misc@pathSuffix'(XNm, "*", XXd38832).
'lo.comp.dict@localTypeName'(_, _):- raise_exception('error'("lo.comp.dict@localTypeName", 65, 3, 39)).
'lo.comp.dict@typeInD'(XKey, 'lo.core#,..'('lo.comp.dict#scope'(XDefs, X_33321, X_33322, X_33323, X_33324), X_33325), XDef):- ocall('present%3'(XDefs, XKey, XDef),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@typeInD'(XKey, 'lo.core#,..'(X_33327, XEnv), XDef):- 'lo.comp.dict@typeInD'(XKey, XEnv, XDef).
'lo.comp.dict@typeInDict'(XNm, XEnv, XTp):- 'lo.comp.dict@one276'(XXd38835, XTp, X_33329, X_33328, XEnv, XXd38834, XNm).
'lo.comp.dict@isType'(XNm, XEnv, XLc, XTp):- 'lo.comp.dict@one277'(XXd38837, X_33330, XTp, XLc, XEnv, XXd38836, XNm).
'lo.comp.dict@tpInDict'(XNm, XEnv, 'lo.core#some'(XD)):- 'lo.comp.dict@localTypeName'(XNm, XXd38838),
    'lo.comp.dict@typeInD'(XXd38838, XEnv, XD),
    !.
'lo.comp.dict@tpInDict'(X_33331, X_33332, 'lo.core#none'):- !.
'lo.comp.dict@tpInDict'(_, _, _):- raise_exception('error'("lo.comp.dict@tpInDict", 48, 3, 63)).
'lo.comp.dict@declareType'(XNm, XTp, 'lo.core#,..'('lo.comp.dict#scope'(XTypes, XNames, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XXe4859, XNames, XCons, XContracts, XImps), XE)):- !,
    ocall('_put%1'(XXV5201),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XTypes, XNm, XTp, XXe4859),XXV5201,XXV5201).
'lo.comp.dict@declareType'(_, _, _, _):- raise_exception('error'("lo.comp.dict@declareType", 56, 3, 125)).
'lo.comp.dict@declareTypeExists'(XNm, XLc, XTp, XEnv, XXd38844):- !,
    'lo.comp.dict@declareType'(XNm, 'lo.comp.dict#tpDef'(XLc, XTp, 'lo.comp.types#voidType'), XEnv, XXd38844).
'lo.comp.dict@declareTypeExists'(_, _, _, _, _):- raise_exception('error'("lo.comp.dict@declareTypeExists", 53, 3, 76)).
'lo.comp.dict@standardType'(XNm, XTp, 'lo.core#,..'('lo.comp.dict#scope'(XTypes, XNames, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XXe4860, XNames, XCons, XContracts, XImps), XE)):- !,
    ocall('_put%1'(XXV5202),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XTypes, XNm, 'lo.comp.dict#tpDef'('lo.comp.location#std', XTp, 'lo.comp.types#typeRule'(XTp, 'lo.comp.types#faceType'('lo.core#[]'))), XXe4860),XXV5202,XXV5202).
'lo.comp.dict@standardType'(_, _, _, _):- raise_exception('error'("lo.comp.dict@standardType", 60, 3, 164)).
'lo.comp.dict@varInD'(XNm, 'lo.core#,..'('lo.comp.dict#scope'(X_33338, XVars, X_33339, X_33340, X_33341), X_33342), XD):- ocall('present%3'(XVars, XNm, XD),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@varInD'(XNm, 'lo.core#,..'(X_33344, XE), XD):- 'lo.comp.dict@varInD'(XNm, XE, XD).
'lo.comp.dict@typeOfVar'(XNm, XEnv, 'lo.core#some'(XD)):- 'lo.comp.dict@varInD'(XNm, XEnv, XD),
    !.
'lo.comp.dict@typeOfVar'(X_33345, X_33346, 'lo.core#none'):- !.
'lo.comp.dict@typeOfVar'(_, _, _):- raise_exception('error'("lo.comp.dict@typeOfVar", 74, 3, 50)).
'lo.comp.dict@isVar'(XNm, X_33347, 'lo.comp.dict#vr'('lo.comp.canon#v'('lo.comp.location#std', XNm), XXb18861)):- 'lo.comp.escapes@isEscape'(XNm),
    'lo.comp.escapes@escapeType'(XNm, XXb18861).
'lo.comp.dict@isVar'(XNm, XEnv, XDef):- 'lo.comp.dict@one278'(XDef, XEnv, XNm).
'lo.comp.dict@declareVar'(XNm, XV, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XXe4861, XCons, XContracts, XImps), XE)):- !,
    ocall('_put%1'(XXV5203),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XNms, XNm, XV, XXe4861),XXV5203,XXV5203).
'lo.comp.dict@declareVar'(_, _, _, _):- raise_exception('error'("lo.comp.dict@declareVar", 89, 3, 108)).
'lo.comp.dict@topVars'('lo.core#,..'('lo.comp.dict#scope'(X_33351, XVars, X_33352, X_33353, X_33354), X_33355), XVars):- !.
'lo.comp.dict@topVars'(_, _):- raise_exception('error'("lo.comp.dict@topVars", 93, 3, 42)).
'lo.comp.dict@pushScope'(XE, 'lo.core#,..'('lo.comp.dict#scope'(XXV5204, XXV5205, 'lo.core#[]', XXV5206, XXV5207), XE)):- !,
    ocall('_empty%1'(XXV5204),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV5205),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV5206),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV5207),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@pushScope'(_, _):- raise_exception('error'("lo.comp.dict@pushScope", 96, 3, 43)).
'lo.comp.dict@declareConstraint'(XC, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, 'lo.core#,..'(XC, XCons), XContracts, XImps), XE)):- !.
'lo.comp.dict@declareConstraint'(_, _, _):- raise_exception('error'("lo.comp.dict@declareConstraint", 99, 3, 111)).
'lo.comp.dict@definedConstraint'(XCx, 'lo.core#,..'('lo.comp.dict#scope'(X_33361, X_33362, XCons, X_33363, X_33364), X_33365)):- ocall('in%2'(XCx, XCons),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.dict@definedConstraint'(XCx, 'lo.core#,..'(X_33367, XE)):- 'lo.comp.dict@definedConstraint'(XCx, XE).
'lo.comp.dict@declareContract'(XNm, XCon, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XXe4862, XImps), XE)):- !,
    ocall('_put%1'(XXV5208),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XContracts, XNm, XCon, XXe4862),XXV5208,XXV5208).
'lo.comp.dict@declareContract'(_, _, _, _):- raise_exception('error'("lo.comp.dict@declareContract", 109, 3, 117)).
'lo.comp.dict@conInD'(XNm, 'lo.core#,..'('lo.comp.dict#scope'(X_33371, X_33372, X_33373, XCons, X_33374), X_33375), XDec):- ocall('present%3'(XCons, XNm, XDec),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@conInD'(XNm, 'lo.core#,..'(X_33377, XEnv), XCon):- 'lo.comp.dict@conInD'(XNm, XEnv, XCon).
'lo.comp.dict@getContract'(XNm, XEnv, XDef):- 'lo.comp.dict@conInD'(XNm, XEnv, XDef).
'lo.comp.dict@isContract'(XNm, XEnv, XDef):- 'lo.comp.dict@one279'(XDef, XEnv, XNm).
'lo.comp.dict@declareImplementation'(XNm, XImplNm, XCon, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XXe4864), XE)):- ocall('present%3'(XImps, XNm, XI),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !,
    ocall('_put%1'(XXV5209),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%1'(XXV5210),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XI, XImplNm, XCon, XXe4863),XXV5209,XXV5209),
    ocall('_call%4'(XImps, XNm, XXe4863, XXe4864),XXV5210,XXV5210).
'lo.comp.dict@declareImplementation'(XNm, XImplNm, XCon, 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XImps), XE), 'lo.core#,..'('lo.comp.dict#scope'(XTps, XNms, XCons, XContracts, XXe4866), XE)):- !,
    ocall('_put%1'(XXV5212),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%1'(XXV5213),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV5211),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XXV5211, XImplNm, XCon, XXe4865),XXV5212,XXV5212),
    ocall('_call%4'(XImps, XNm, XXe4865, XXe4866),XXV5213,XXV5213).
'lo.comp.dict@declareImplementation'(_, _, _, _, _):- raise_exception('error'("lo.comp.dict@declareImplementation", 125, 3, 164)).
'lo.comp.dict@impInD'(XNm, 'lo.core#,..'('lo.comp.dict#scope'(X_33383, X_33384, X_33385, X_33386, XImpls), X_33387), XDec):- ocall('present%3'(XImpls, XNm, XDec),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@impInD'(XNm, 'lo.core#,..'(X_33389, XEnv), XCon):- 'lo.comp.dict@impInD'(XNm, XEnv, XCon).
'lo.comp.dict@isImplemented'('lo.comp.types#conTract'(XNm, XArgs, XDeps), XEnv, XImpl):- 'lo.comp.dict@impInD'(XNm, XEnv, XImpls),
    'lo.comp.types@implementationName'('lo.comp.types#conTract'(XNm, XArgs, XDeps), XXd38881),
    ocall('present%3'(XImpls, XXd38881, XImpl),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.dict@stdDict'(XXd38889):- !,
    'lo.comp.dict@pushScope'('lo.core#[]', XXd38886),
    'lo.comp.dict@standardType'("float", 'lo.comp.types#tipe'("lo.core*float"), XXd38886, XXd38887),
    'lo.comp.dict@standardType'("integer", 'lo.comp.types#tipe'("lo.core*integer"), XXd38887, XXd38888),
    'lo.comp.dict@standardType'("string", 'lo.comp.types#tipe'("lo.core*string"), XXd38888, XXd38889).
'lo.comp.dict^showEnv'('_call%2'(XV29938, XV29939), 'lo.comp.dict^showEnv', _):- 'lo.comp.dict@showEnv'(XV29938, XV29939).
'lo.core$display$lo.comp.dict*env^disp'('_call%2'(XV29940, XV29941), 'lo.core$display$lo.comp.dict*env^disp'(XLbV2476, XThV2476), _):- 'lo.core$display$lo.comp.dict*env@disp'(XV29940, XV29941, XLbV2476, XThV2476).
'lo.comp.dict^showTpDef'('_call%2'(XV29944, XV29945), 'lo.comp.dict^showTpDef', _):- 'lo.comp.dict@showTpDef'(XV29944, XV29945).
'lo.core$display$lo.comp.dict*typeDef^disp'('_call%2'(XV29946, XV29947), 'lo.core$display$lo.comp.dict*typeDef^disp'(XLbV2477, XThV2477), _):- 'lo.core$display$lo.comp.dict*typeDef@disp'(XV29946, XV29947, XLbV2477, XThV2477).
'lo.comp.dict^showVr'('_call%2'(XV29950, XV29951), 'lo.comp.dict^showVr', _):- 'lo.comp.dict@showVr'(XV29950, XV29951).
'lo.core$display$lo.comp.dict*vr^disp'('_call%2'(XV29952, XV29953), 'lo.core$display$lo.comp.dict*vr^disp'(XLbV2478, XThV2478), _):- 'lo.core$display$lo.comp.dict*vr@disp'(XV29952, XV29953, XLbV2478, XThV2478).
'lo.comp.dict^localTypeName'('_call%2'(XV29956, XV29957), 'lo.comp.dict^localTypeName', _):- 'lo.comp.dict@localTypeName'(XV29956, XV29957).
'lo.comp.dict^typeInD'('_call%3'(XV29958, XV29959, XV29960), 'lo.comp.dict^typeInD', _):- 'lo.comp.dict@typeInD'(XV29958, XV29959, XV29960).
'lo.comp.dict@one276'(XXd38835, XTp, X_33329, X_33328, XEnv, XXd38834, XNm):- 'lo.comp.dict@localTypeName'(XNm, XXd38834),
    'lo.comp.dict@typeInD'(XXd38834, XEnv, 'lo.comp.dict#tpDef'(X_33328, X_33329, XTp)),
    !.
'lo.comp.dict^typeInDict'('_call%3'(XV29961, XV29962, XV29963), 'lo.comp.dict^typeInDict', _):- 'lo.comp.dict@typeInDict'(XV29961, XV29962, XV29963).
'lo.comp.dict@one277'(XXd38837, X_33330, XTp, XLc, XEnv, XXd38836, XNm):- 'lo.comp.dict@localTypeName'(XNm, XXd38836),
    'lo.comp.dict@typeInD'(XXd38836, XEnv, 'lo.comp.dict#tpDef'(XLc, XTp, X_33330)),
    !.
'lo.comp.dict^isType'('_call%4'(XV29964, XV29965, XV29966, XV29967), 'lo.comp.dict^isType', _):- 'lo.comp.dict@isType'(XV29964, XV29965, XV29966, XV29967).
'lo.comp.dict^tpInDict'('_call%3'(XV29968, XV29969, XV29970), 'lo.comp.dict^tpInDict', _):- 'lo.comp.dict@tpInDict'(XV29968, XV29969, XV29970).
'lo.comp.dict^declareType'('_call%4'(XV29971, XV29972, XV29973, XV29974), 'lo.comp.dict^declareType', _):- 'lo.comp.dict@declareType'(XV29971, XV29972, XV29973, XV29974).
'lo.comp.dict^declareTypeExists'('_call%5'(XV29975, XV29976, XV29977, XV29978, XV29979), 'lo.comp.dict^declareTypeExists', _):- 'lo.comp.dict@declareTypeExists'(XV29975, XV29976, XV29977, XV29978, XV29979).
'lo.comp.dict^standardType'('_call%4'(XV29980, XV29981, XV29982, XV29983), 'lo.comp.dict^standardType', _):- 'lo.comp.dict@standardType'(XV29980, XV29981, XV29982, XV29983).
'lo.comp.dict^varInD'('_call%3'(XV29984, XV29985, XV29986), 'lo.comp.dict^varInD', _):- 'lo.comp.dict@varInD'(XV29984, XV29985, XV29986).
'lo.comp.dict^typeOfVar'('_call%3'(XV29987, XV29988, XV29989), 'lo.comp.dict^typeOfVar', _):- 'lo.comp.dict@typeOfVar'(XV29987, XV29988, XV29989).
'lo.comp.dict@one278'(XDef, XEnv, XNm):- 'lo.comp.dict@varInD'(XNm, XEnv, XDef),
    !.
'lo.comp.dict^isVar'('_call%3'(XV29990, XV29991, XV29992), 'lo.comp.dict^isVar', _):- 'lo.comp.dict@isVar'(XV29990, XV29991, XV29992).
'lo.comp.dict^declareVar'('_call%4'(XV29993, XV29994, XV29995, XV29996), 'lo.comp.dict^declareVar', _):- 'lo.comp.dict@declareVar'(XV29993, XV29994, XV29995, XV29996).
'lo.comp.dict^topVars'('_call%2'(XV29997, XV29998), 'lo.comp.dict^topVars', _):- 'lo.comp.dict@topVars'(XV29997, XV29998).
'lo.comp.dict^pushScope'('_call%2'(XV29999, XV30000), 'lo.comp.dict^pushScope', _):- 'lo.comp.dict@pushScope'(XV29999, XV30000).
'lo.comp.dict^declareConstraint'('_call%3'(XV30001, XV30002, XV30003), 'lo.comp.dict^declareConstraint', _):- 'lo.comp.dict@declareConstraint'(XV30001, XV30002, XV30003).
'lo.comp.dict^definedConstraint'('_call%2'(XV30004, XV30005), 'lo.comp.dict^definedConstraint', _):- 'lo.comp.dict@definedConstraint'(XV30004, XV30005).
'lo.comp.dict^declareContract'('_call%4'(XV30006, XV30007, XV30008, XV30009), 'lo.comp.dict^declareContract', _):- 'lo.comp.dict@declareContract'(XV30006, XV30007, XV30008, XV30009).
'lo.comp.dict^conInD'('_call%3'(XV30010, XV30011, XV30012), 'lo.comp.dict^conInD', _):- 'lo.comp.dict@conInD'(XV30010, XV30011, XV30012).
'lo.comp.dict^getContract'('_call%3'(XV30013, XV30014, XV30015), 'lo.comp.dict^getContract', _):- 'lo.comp.dict@getContract'(XV30013, XV30014, XV30015).
'lo.comp.dict@one279'(XDef, XEnv, XNm):- 'lo.comp.dict@conInD'(XNm, XEnv, XDef),
    !.
'lo.comp.dict^isContract'('_call%3'(XV30016, XV30017, XV30018), 'lo.comp.dict^isContract', _):- 'lo.comp.dict@isContract'(XV30016, XV30017, XV30018).
'lo.comp.dict^declareImplementation'('_call%5'(XV30019, XV30020, XV30021, XV30022, XV30023), 'lo.comp.dict^declareImplementation', _):- 'lo.comp.dict@declareImplementation'(XV30019, XV30020, XV30021, XV30022, XV30023).
'lo.comp.dict^impInD'('_call%3'(XV30024, XV30025, XV30026), 'lo.comp.dict^impInD', _):- 'lo.comp.dict@impInD'(XV30024, XV30025, XV30026).
'lo.comp.dict^isImplemented'('_call%3'(XV30027, XV30028, XV30029), 'lo.comp.dict^isImplemented', _):- 'lo.comp.dict@isImplemented'(XV30027, XV30028, XV30029).
