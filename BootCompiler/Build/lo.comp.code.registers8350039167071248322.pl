'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.registers'e'*'n13o13'()13'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.instructions'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I30'voidAddr't'lo.comp.code.registers*addr''aReg'CT1it'lo.comp.code.registers*addr''yReg'CT2ilt'lo.comp.code.registers*addr''sReg't'lo.comp.code.registers*addr''notAlloc't'lo.comp.code.registers*addr''varSet'CT3Uz1'lo.sets*set'1iUz1'lo.sets*set'1iit'lo.comp.code.registers*varSet''notInited't'lo.comp.code.registers*initialized''inited't'lo.comp.code.registers*initialized''notDefined't'lo.comp.code.registers*defined''isDefined't'lo.comp.code.registers*defined''varDesc'CT5St'lo.comp.code.registers*initialized't'lo.comp.code.registers*defined't'lo.comp.code.registers*addr'Uz1'lo.core*option'1T2iit'lo.comp.code.registers*varDesc''isInitialized'PT1t'lo.comp.code.registers*varDesc''markInited'FT2t'lo.comp.code.registers*varDesc't'lo.comp.code.registers*addr't'lo.comp.code.registers*varDesc''varAddr'FT1t'lo.comp.code.registers*varDesc't'lo.comp.code.registers*addr''locUsed'FT2t'lo.comp.code.registers*varSet'it'lo.comp.code.registers*varSet''regUsed'FT2t'lo.comp.code.registers*varSet'it'lo.comp.code.registers*varSet''usedRegs'FT1t'lo.comp.code.registers*varSet'Uz1'lo.sets*set'1i'resetUsed'FT1t'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet''maxReg'FT1t'lo.comp.code.registers*varSet'i'freeAReg'FT2t'lo.comp.code.registers*varSet'it'lo.comp.code.registers*varSet''numRegisters'i'pickAreg'PT3t'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet't'lo.comp.code.registers*addr''lclGc'FT2t'lo.comp.code.registers*varSet'it'lo.comp.code.registers*varSet''gcPredict'FT1t'lo.comp.code.registers*varSet'i'resetGc'FT1t'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet''clearAs'FT3iiUz1'lo.sets*set'1iLt'lo.comp.code.instructions*instruction''gcMap'PT4t'lo.comp.code.registers*varSet'iLt'lo.comp.code.instructions*instruction't'lo.comp.code.instructions*instruction''usedLocals'FT1t'lo.comp.code.registers*varSet'Uz1'lo.sets*set'1i'maxLocals'FT1t'lo.comp.code.registers*varSet'i'varSlotSz'i\"s\"I5'addr'Yt'lo.comp.code.registers*addr'I0'varSet'Yt'lo.comp.code.registers*varSet'I0'initialized'Yt'lo.comp.code.registers*initialized'I0'defined'Yt'lo.comp.code.registers*defined'I0'varDesc'Yt'lo.comp.code.registers*varDesc'I0\"n7o7'()7's'voidAddr's'sReg's'notAlloc's'notInited's'inited's'notDefined's'isDefined'n0o0'()0'n4o4'()4'n2o2'()2's'lo.core$display$lo.comp.code.registers*addr's\"c'lo.core$display'T1t'lo.comp.code.registers*addr'T0\"n2o2'()2's'lo.core$display$lo.comp.code.registers*varDesc's\"c'lo.core$display'T1t'lo.comp.code.registers*varDesc'T0\"n2o2'()2's'lo.core$display$lo.comp.code.registers*initialized's\"c'lo.core$display'T1t'lo.comp.code.registers*initialized'T0\"n2o2'()2's'lo.core$display$lo.comp.code.registers*defined's\"c'lo.core$display'T1t'lo.comp.code.registers*defined'T0\"").
'lo.comp.code.registers@init'() :- !.
'lo.comp.code.registers#aReg'('aReg%1'('lo.comp.code.registers@aReg'())) :- !.
'lo.comp.code.registers#yReg'('yReg%1'('lo.comp.code.registers@yReg'())) :- !.
'lo.comp.code.registers#varSet'('varSet%1'('lo.comp.code.registers@varSet'())) :- !.
'lo.comp.code.registers#varDesc'('varDesc%1'('lo.comp.code.registers@varDesc'())) :- !.
'lo.comp.code.registers@isInitialized'('lo.comp.code.registers#varDesc'(X_2499, 'lo.comp.code.registers#inited', X_2500, X_2501, X_2502)).
'lo.comp.code.registers@markInited'('lo.comp.code.registers#varDesc'(XNm, X_2503, X_2504, X_2505, XU), XAddr, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#inited', 'lo.comp.code.registers#isDefined', XAddr, XU)) :- !.
'lo.comp.code.registers@markInited'(_, _, _) :- raise_exception('error'("markInited", 21, 3, 75)).
'lo.comp.code.registers@varAddr'('lo.comp.code.registers#varDesc'(X_2506, X_2507, X_2508, XA, X_2509), XA) :- !.
'lo.comp.code.registers@varAddr'(_, _) :- raise_exception('error'("varAddr", 24, 3, 30)).
'lo.comp.code.registers@locUsed'('lo.comp.code.registers#varSet'(XA, XL, XG), XLc, 'lo.comp.code.registers#varSet'(XA, XX38001, XG)) :- !,
    ocall('addMem%3'(XL, XLc, XX38001),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')).
'lo.comp.code.registers@locUsed'(_, _, _) :- raise_exception('error'("locUsed", 27, 3, 53)).
'lo.comp.code.registers@regUsed'('lo.comp.code.registers#varSet'(XA, XL, XG), XRg, 'lo.comp.code.registers#varSet'(XX38013, XL, XG)) :- !,
    ocall('addMem%3'(XA, XRg, XX38013),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')).
'lo.comp.code.registers@regUsed'(_, _, _) :- raise_exception('error'("regUsed", 30, 3, 53)).
'lo.comp.code.registers@usedRegs'('lo.comp.code.registers#varSet'(XA, X_2510, X_2511), XA) :- !.
'lo.comp.code.registers@usedRegs'(_, _) :- raise_exception('error'("usedRegs", 33, 3, 28)).
'lo.comp.code.registers@resetUsed'('lo.comp.code.registers#varSet'(X_2512, XL, XG), 'lo.comp.code.registers#varSet'('lo.sets#set'(XXV77), XL, XG)) :- !,
    ocall('_empty%1'(XXV77),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')).
'lo.comp.code.registers@resetUsed'(_, _) :- raise_exception('error'("resetUsed", 36, 3, 47)).
'lo.comp.code.registers@maxReg'('lo.comp.code.registers#varSet'(XA, X_2513, X_2514), XX38045) :- !,
    ocall('foldRight%4'('lo.comp.code.registers@$9', 0, XA, XX38045),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer')).
'lo.comp.code.registers@maxReg'(_, _) :- raise_exception('error'("maxReg", 39, 3, 55)).
'lo.comp.code.registers@freeAReg'('lo.comp.code.registers#varSet'(XA, XL, XG), XRg, 'lo.comp.code.registers#varSet'(XX38055, XL, XG)) :- !,
    ocall('delMem%3'(XA, XRg, XX38055),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')).
'lo.comp.code.registers@freeAReg'(_, _, _) :- raise_exception('error'("freeAReg", 42, 3, 54)).
'lo.comp.code.registers@inRange'(XLo, XHi, XLo).
'lo.comp.code.registers@inRange'(XLo, XHi, XEl) :- ocall('+%3'(XLo, 1, XX38068),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.registers@inRange'(XX38068, XHi, XEl).
'lo.comp.code.registers@numRegisters'(64) :- !.
'lo.comp.code.registers@pickAreg'('lo.comp.code.registers#varSet'(XA, XL, XG), 'lo.comp.code.registers#varSet'(XX38078, XL, XG), 'lo.comp.code.registers#aReg'(XRg)) :- 'lo.comp.code.registers@one46'(XA, XRg, XX38086),
    ocall('addMem%3'(XA, XRg, XX38078),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')).
'lo.comp.code.registers@lclGc'('lo.comp.code.registers#varSet'(XA, XL, XG), XSz, 'lo.comp.code.registers#varSet'(XA, XL, XX38101)) :- !,
    ocall('+%3'(XG, XSz, XX38101),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.code.registers@lclGc'(_, _, _) :- raise_exception('error'("lclGc", 53, 3, 43)).
'lo.comp.code.registers@gcPredict'('lo.comp.code.registers#varSet'(X_2515, X_2516, XG), XG) :- !.
'lo.comp.code.registers@gcPredict'(_, _) :- raise_exception('error'("gcPredict", 56, 3, 29)).
'lo.comp.code.registers@resetGc'('lo.comp.code.registers#varSet'(XA, XL, X_2517), 'lo.comp.code.registers#varSet'(XA, XL, 0)) :- !.
'lo.comp.code.registers@resetGc'(_, _) :- raise_exception('error'("resetGc", 59, 3, 39)).
'lo.comp.code.registers@clearYs'(XRg, XMx, X_2518, 'lo.core#[]') :- 'lo.core@>'('lo.core$comp$lo.core*integer', XRg, XMx),
    !.
'lo.comp.code.registers@clearYs'(XRg, XMx, XLocs, 'lo.core#,..'('lo.comp.code.instructions#iClY'(XRg), XX38137)) :- 'lo.comp.code.registers@neg42'(XLocs, XRg),
    !,
    ocall('+%3'(XRg, 1, XX38133),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.registers@clearYs'(XX38133, XMx, XLocs, XX38137).
'lo.comp.code.registers@clearYs'(XRg, XMx, XLocs, XX38147) :- !,
    ocall('+%3'(XRg, 1, XX38143),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.registers@clearYs'(XX38143, XMx, XLocs, XX38147).
'lo.comp.code.registers@clearYs'(_, _, _, _) :- raise_exception('error'("clearYs", 66, 3, 31)).
'lo.comp.code.registers@clearAs'(XRg, XMx, X_2519, 'lo.core#[]') :- 'lo.core@>'('lo.core$comp$lo.core*integer', XRg, XMx),
    !.
'lo.comp.code.registers@clearAs'(XRg, XMx, XRegs, 'lo.core#,..'('lo.comp.code.instructions#iVdA'(XRg), XX38169)) :- 'lo.comp.code.registers@neg43'(XRegs, XRg),
    !,
    ocall('+%3'(XRg, 1, XX38165),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.registers@clearAs'(XX38165, XMx, XRegs, XX38169).
'lo.comp.code.registers@clearAs'(XRg, XMx, XRegs, XX38179) :- !,
    ocall('+%3'(XRg, 1, XX38175),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.registers@clearAs'(XX38175, XMx, XRegs, XX38179).
'lo.comp.code.registers@clearAs'(_, _, _, _) :- raise_exception('error'("clearAs", 71, 3, 31)).
'lo.comp.code.registers@gcMap'('lo.comp.code.registers#varSet'(XRgs, XLocs, X_2520), XArity, XX38191, 'lo.comp.code.instructions#iGcmap'(XArity, XEsize)) :- ocall('foldRight%4'('lo.comp.code.registers@$10', 0, XLocs, XX38203),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer')),
    XEsize = XX38203,
    'lo.comp.code.registers@clearYs'(1, XEsize, XLocs, XX38187),
    'lo.comp.code.registers@clearAs'(1, XArity, XRgs, XX38190),
    'lo.list@<>'(XX38187, XX38190, XX38191).
'lo.comp.code.registers@usedLocals'('lo.comp.code.registers#varSet'(X_2521, XL, X_2522), XL) :- !.
'lo.comp.code.registers@usedLocals'(_, _) :- raise_exception('error'("usedLocals", 76, 3, 30)).
'lo.comp.code.registers@maxLocals'('lo.comp.code.registers#varSet'(X_2523, XL, X_2524), XX38222) :- !,
    ocall('foldRight%4'('lo.comp.code.registers@$11', 0, XL, XX38222),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer')).
'lo.comp.code.registers@maxLocals'(_, _) :- raise_exception('error'("maxLocals", 79, 3, 58)).
'lo.comp.code.registers@varSlotSz'(2) :- !.
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#voidAddr', 'lo.core#ss'("void")) :- !.
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#aReg'(XR), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("A["), 'lo.core#,..'(XX38231, 'lo.core#,..'('lo.core#ss'("]"), 'lo.core#[]'))))) :- !,
    ocall('disp%2'(XR, XX38231),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer').
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#yReg'(XLc, XSf), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Y["), 'lo.core#,..'(XX38244, 'lo.core#,..'('lo.core#ss'(XCndV5), 'lo.core#,..'('lo.core#ss'("]"), 'lo.core#[]')))))) :- !,
    'lo.comp.code.registers@condExp5'(XCndV5, XSf),
    ocall('disp%2'(XLc, XX38244),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer').
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#sReg', 'lo.core#ss'("S")) :- !.
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#notAlloc', 'lo.core#ss'("not allocated")) :- !.
'lo.comp.code.registers@dispAdd'(_, _) :- raise_exception('error'("dispAdd", 92, 3, 31)).
'lo.core$display$lo.comp.code.registers*addr'('lo.core$display$lo.comp.code.registers*addr%1'('lo.core$display$lo.comp.code.registers*addr')) :- !.
'lo.core$display$lo.comp.code.registers*addr'('disp%2'(XV4774, XV4775), XLbl307, XThis307) :- !,
    'lo.core$display$lo.comp.code.registers*addr@disp'(XV4774, XV4775, XLbl307, XThis307).
'lo.core$display$lo.comp.code.registers*addr'('disp%1'('lo.core$display$lo.comp.code.registers*addr^disp'(XLbl308, XThis308)), XLbl308, XThis308).
'lo.core$display$lo.comp.code.registers*addr@disp'(XA, XX38261, XLbV415, XThV415) :- !,
    'lo.comp.code.registers@dispAdd'(XA, XX38261).
'lo.core$display$lo.comp.code.registers*addr@disp'(_, _, _, _) :- raise_exception('error'("disp", 88, 5, 21)).
'lo.comp.code.registers@dispVarDesc'('lo.comp.code.registers#varDesc'(XNm, XI, XD, XA, X_2525), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XNm), 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XX38272, 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XX38276, 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XX38280, 'lo.core#[]'))))))))) :- !,
    ocall('disp%2'(XI, XX38272),'lo.core$display$lo.comp.code.registers*initialized','lo.core$display$lo.comp.code.registers*initialized'),
    ocall('disp%2'(XD, XX38276),'lo.core$display$lo.comp.code.registers*defined','lo.core$display$lo.comp.code.registers*defined'),
    ocall('disp%2'(XA, XX38280),'lo.core$display$lo.comp.code.registers*addr','lo.core$display$lo.comp.code.registers*addr').
'lo.comp.code.registers@dispVarDesc'(_, _) :- raise_exception('error'("dispVarDesc", 103, 3, 99)).
'lo.core$display$lo.comp.code.registers*varDesc'('lo.core$display$lo.comp.code.registers*varDesc%1'('lo.core$display$lo.comp.code.registers*varDesc')) :- !.
'lo.core$display$lo.comp.code.registers*varDesc'('disp%2'(XV4782, XV4783), XLbl309, XThis309) :- !,
    'lo.core$display$lo.comp.code.registers*varDesc@disp'(XV4782, XV4783, XLbl309, XThis309).
'lo.core$display$lo.comp.code.registers*varDesc'('disp%1'('lo.core$display$lo.comp.code.registers*varDesc^disp'(XLbl310, XThis310)), XLbl310, XThis310).
'lo.core$display$lo.comp.code.registers*varDesc@disp'(XV, XX38293, XLbV416, XThV416) :- !,
    'lo.comp.code.registers@dispVarDesc'(XV, XX38293).
'lo.core$display$lo.comp.code.registers*varDesc@disp'(_, _, _, _) :- raise_exception('error'("disp", 99, 5, 25)).
'lo.core$display$lo.comp.code.registers*initialized'('lo.core$display$lo.comp.code.registers*initialized%1'('lo.core$display$lo.comp.code.registers*initialized')) :- !.
'lo.core$display$lo.comp.code.registers*initialized'('disp%2'(XV4788, XV4789), XLbl311, XThis311) :- !,
    'lo.core$display$lo.comp.code.registers*initialized@disp'(XV4788, XV4789, XLbl311, XThis311).
'lo.core$display$lo.comp.code.registers*initialized'('disp%1'('lo.core$display$lo.comp.code.registers*initialized^disp'(XLbl312, XThis312)), XLbl312, XThis312).
'lo.core$display$lo.comp.code.registers*initialized@disp'('lo.comp.code.registers#notInited', 'lo.core#ss'("not inited"), XLbV417, XThV417) :- !.
'lo.core$display$lo.comp.code.registers*initialized@disp'('lo.comp.code.registers#inited', 'lo.core#ss'("inited"), XLbV417, XThV417) :- !.
'lo.core$display$lo.comp.code.registers*initialized@disp'(_, _, _, _) :- raise_exception('error'("disp", 106, 5, 35)).
'lo.core$display$lo.comp.code.registers*defined'('lo.core$display$lo.comp.code.registers*defined%1'('lo.core$display$lo.comp.code.registers*defined')) :- !.
'lo.core$display$lo.comp.code.registers*defined'('disp%2'(XV4794, XV4795), XLbl313, XThis313) :- !,
    'lo.core$display$lo.comp.code.registers*defined@disp'(XV4794, XV4795, XLbl313, XThis313).
'lo.core$display$lo.comp.code.registers*defined'('disp%1'('lo.core$display$lo.comp.code.registers*defined^disp'(XLbl314, XThis314)), XLbl314, XThis314).
'lo.core$display$lo.comp.code.registers*defined@disp'('lo.comp.code.registers#notDefined', 'lo.core#ss'("not defined"), XLbV418, XThV418) :- !.
'lo.core$display$lo.comp.code.registers*defined@disp'('lo.comp.code.registers#isDefined', 'lo.core#ss'("defined"), XLbV418, XThV418) :- !.
'lo.core$display$lo.comp.code.registers*defined@disp'(_, _, _, _) :- raise_exception('error'("disp", 111, 5, 37)).
'lo.comp.code.registers@voidAddr'('lo.comp.code.registers#voidAddr') :- !.
'lo.comp.code.registers@sReg'('lo.comp.code.registers#sReg') :- !.
'lo.comp.code.registers@notAlloc'('lo.comp.code.registers#notAlloc') :- !.
'lo.comp.code.registers@notInited'('lo.comp.code.registers#notInited') :- !.
'lo.comp.code.registers@inited'('lo.comp.code.registers#inited') :- !.
'lo.comp.code.registers@notDefined'('lo.comp.code.registers#notDefined') :- !.
'lo.comp.code.registers@isDefined'('lo.comp.code.registers#isDefined') :- !.
'lo.comp.code.registers^isInitialized'('_call%1'(XV4720), 'lo.comp.code.registers^isInitialized', _) :- 'lo.comp.code.registers@isInitialized'(XV4720).
'lo.comp.code.registers^markInited'('_call%3'(XV4721, XV4722, XV4723), 'lo.comp.code.registers^markInited', _) :- 'lo.comp.code.registers@markInited'(XV4721, XV4722, XV4723).
'lo.comp.code.registers^varAddr'('_call%2'(XV4724, XV4725), 'lo.comp.code.registers^varAddr', _) :- 'lo.comp.code.registers@varAddr'(XV4724, XV4725).
'lo.comp.code.registers^locUsed'('_call%3'(XV4726, XV4727, XV4728), 'lo.comp.code.registers^locUsed', _) :- 'lo.comp.code.registers@locUsed'(XV4726, XV4727, XV4728).
'lo.comp.code.registers^regUsed'('_call%3'(XV4729, XV4730, XV4731), 'lo.comp.code.registers^regUsed', _) :- 'lo.comp.code.registers@regUsed'(XV4729, XV4730, XV4731).
'lo.comp.code.registers^usedRegs'('_call%2'(XV4732, XV4733), 'lo.comp.code.registers^usedRegs', _) :- 'lo.comp.code.registers@usedRegs'(XV4732, XV4733).
'lo.comp.code.registers^resetUsed'('_call%2'(XV4734, XV4735), 'lo.comp.code.registers^resetUsed', _) :- 'lo.comp.code.registers@resetUsed'(XV4734, XV4735).
'lo.comp.code.registers@$9'('_call%3'(XX, XY, XX38043), 'lo.comp.code.registers@$9', _) :- !,
    'lo.core@max'('lo.core$comp$lo.core*integer', XX, XY, XX38043).
'lo.comp.code.registers@$9'(_, _, _) :- raise_exception('error'("lambda", 39, 38, 15)).
'lo.comp.code.registers^maxReg'('_call%2'(XV4736, XV4737), 'lo.comp.code.registers^maxReg', _) :- 'lo.comp.code.registers@maxReg'(XV4736, XV4737).
'lo.comp.code.registers^freeAReg'('_call%3'(XV4738, XV4739, XV4740), 'lo.comp.code.registers^freeAReg', _) :- 'lo.comp.code.registers@freeAReg'(XV4738, XV4739, XV4740).
'lo.comp.code.registers^inRange'('_call%3'(XV4741, XV4742, XV4743), 'lo.comp.code.registers^inRange', _) :- 'lo.comp.code.registers@inRange'(XV4741, XV4742, XV4743).
'lo.comp.code.registers@neg41'(XA, XRg) :- ocall('in%2'(XRg, XA),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.registers@neg41'(XA, XRg).
'lo.comp.code.registers@one46'(XA, XRg, XX38086) :- 'lo.comp.code.registers@numRegisters'(XX38086),
    'lo.comp.code.registers@inRange'(1, XX38086, XRg),
    'lo.comp.code.registers@neg41'(XA, XRg),
    !.
'lo.comp.code.registers^pickAreg'('_call%3'(XV4744, XV4745, XV4746), 'lo.comp.code.registers^pickAreg', _) :- 'lo.comp.code.registers@pickAreg'(XV4744, XV4745, XV4746).
'lo.comp.code.registers^lclGc'('_call%3'(XV4747, XV4748, XV4749), 'lo.comp.code.registers^lclGc', _) :- 'lo.comp.code.registers@lclGc'(XV4747, XV4748, XV4749).
'lo.comp.code.registers^gcPredict'('_call%2'(XV4750, XV4751), 'lo.comp.code.registers^gcPredict', _) :- 'lo.comp.code.registers@gcPredict'(XV4750, XV4751).
'lo.comp.code.registers^resetGc'('_call%2'(XV4752, XV4753), 'lo.comp.code.registers^resetGc', _) :- 'lo.comp.code.registers@resetGc'(XV4752, XV4753).
'lo.comp.code.registers@neg42'(XLocs, XRg) :- ocall('in%2'(XRg, XLocs),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.registers@neg42'(XLocs, XRg).
'lo.comp.code.registers^clearYs'('_call%4'(XV4754, XV4755, XV4756, XV4757), 'lo.comp.code.registers^clearYs', _) :- 'lo.comp.code.registers@clearYs'(XV4754, XV4755, XV4756, XV4757).
'lo.comp.code.registers@neg43'(XRegs, XRg) :- ocall('in%2'(XRg, XRegs),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.registers@neg43'(XRegs, XRg).
'lo.comp.code.registers^clearAs'('_call%4'(XV4758, XV4759, XV4760, XV4761), 'lo.comp.code.registers^clearAs', _) :- 'lo.comp.code.registers@clearAs'(XV4758, XV4759, XV4760, XV4761).
'lo.comp.code.registers@$10'('_call%3'(XX, XY, XX38201), 'lo.comp.code.registers@$10', _) :- !,
    'lo.core@max'('lo.core$comp$lo.core*integer', XX, XY, XX38201).
'lo.comp.code.registers@$10'(_, _, _) :- raise_exception('error'("lambda", 63, 23, 15)).
'lo.comp.code.registers^gcMap'('_call%4'(XV4762, XV4763, XV4764, XV4765), 'lo.comp.code.registers^gcMap', _) :- 'lo.comp.code.registers@gcMap'(XV4762, XV4763, XV4764, XV4765).
'lo.comp.code.registers^usedLocals'('_call%2'(XV4766, XV4767), 'lo.comp.code.registers^usedLocals', _) :- 'lo.comp.code.registers@usedLocals'(XV4766, XV4767).
'lo.comp.code.registers@$11'('_call%3'(XX, XY, XX38220), 'lo.comp.code.registers@$11', _) :- !,
    'lo.core@max'('lo.core$comp$lo.core*integer', XX, XY, XX38220).
'lo.comp.code.registers@$11'(_, _, _) :- raise_exception('error'("lambda", 79, 41, 15)).
'lo.comp.code.registers^maxLocals'('_call%2'(XV4768, XV4769), 'lo.comp.code.registers^maxLocals', _) :- 'lo.comp.code.registers@maxLocals'(XV4768, XV4769).
'lo.comp.code.registers@condExp5'("/safe", XSf) :- 'lo.core@true'(XSf),
    !.
'lo.comp.code.registers@condExp5'("/not safe", XSf).
'lo.comp.code.registers^dispAdd'('_call%2'(XV4770, XV4771), 'lo.comp.code.registers^dispAdd', _) :- 'lo.comp.code.registers@dispAdd'(XV4770, XV4771).
'lo.core$display$lo.comp.code.registers*addr^disp'('_call%2'(XV4772, XV4773), 'lo.core$display$lo.comp.code.registers*addr^disp'(XLbV415, XThV415), _) :- 'lo.core$display$lo.comp.code.registers*addr@disp'(XV4772, XV4773, XLbV415, XThV415).
'lo.core$display$lo.comp.code.registers*addr^disp'('_call%2'(XV4776, XV4777), 'lo.core$display$lo.comp.code.registers*addr^disp'(XLbV415, XThV415), _) :- 'lo.core$display$lo.comp.code.registers*addr@disp'(XV4776, XV4777, XLbV415, XThV415).
'lo.comp.code.registers^dispVarDesc'('_call%2'(XV4778, XV4779), 'lo.comp.code.registers^dispVarDesc', _) :- 'lo.comp.code.registers@dispVarDesc'(XV4778, XV4779).
'lo.core$display$lo.comp.code.registers*varDesc^disp'('_call%2'(XV4780, XV4781), 'lo.core$display$lo.comp.code.registers*varDesc^disp'(XLbV416, XThV416), _) :- 'lo.core$display$lo.comp.code.registers*varDesc@disp'(XV4780, XV4781, XLbV416, XThV416).
'lo.core$display$lo.comp.code.registers*varDesc^disp'('_call%2'(XV4784, XV4785), 'lo.core$display$lo.comp.code.registers*varDesc^disp'(XLbV416, XThV416), _) :- 'lo.core$display$lo.comp.code.registers*varDesc@disp'(XV4784, XV4785, XLbV416, XThV416).
'lo.core$display$lo.comp.code.registers*initialized^disp'('_call%2'(XV4786, XV4787), 'lo.core$display$lo.comp.code.registers*initialized^disp'(XLbV417, XThV417), _) :- 'lo.core$display$lo.comp.code.registers*initialized@disp'(XV4786, XV4787, XLbV417, XThV417).
'lo.core$display$lo.comp.code.registers*initialized^disp'('_call%2'(XV4790, XV4791), 'lo.core$display$lo.comp.code.registers*initialized^disp'(XLbV417, XThV417), _) :- 'lo.core$display$lo.comp.code.registers*initialized@disp'(XV4790, XV4791, XLbV417, XThV417).
'lo.core$display$lo.comp.code.registers*defined^disp'('_call%2'(XV4792, XV4793), 'lo.core$display$lo.comp.code.registers*defined^disp'(XLbV418, XThV418), _) :- 'lo.core$display$lo.comp.code.registers*defined@disp'(XV4792, XV4793, XLbV418, XThV418).
'lo.core$display$lo.comp.code.registers*defined^disp'('_call%2'(XV4796, XV4797), 'lo.core$display$lo.comp.code.registers*defined^disp'(XLbV418, XThV418), _) :- 'lo.core$display$lo.comp.code.registers*defined@disp'(XV4796, XV4797, XLbV418, XThV418).
