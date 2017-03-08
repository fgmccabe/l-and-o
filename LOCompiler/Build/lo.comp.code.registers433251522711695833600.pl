'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.registers's'0.0.1'n3o3'()3'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.instructions'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'s\"I30'voidAddr't'lo.comp.code.registers*addr''aReg'CT1it'lo.comp.code.registers*addr''yReg'CT2ilt'lo.comp.code.registers*addr''sReg't'lo.comp.code.registers*addr''notAlloc't'lo.comp.code.registers*addr''varSet'CT3Uz1'lo.sets*set'1iUz1'lo.sets*set'1iit'lo.comp.code.registers*varSet''notInited't'lo.comp.code.registers*initialized''inited't'lo.comp.code.registers*initialized''notDefined't'lo.comp.code.registers*defined''isDefined't'lo.comp.code.registers*defined''varDesc'CT5St'lo.comp.code.registers*initialized't'lo.comp.code.registers*defined't'lo.comp.code.registers*addr'Uz1'lo.core*option'1T2iit'lo.comp.code.registers*varDesc''isInitialized'PT1t'lo.comp.code.registers*varDesc''markInited'FT2t'lo.comp.code.registers*varDesc't'lo.comp.code.registers*addr't'lo.comp.code.registers*varDesc''varAddr'FT1t'lo.comp.code.registers*varDesc't'lo.comp.code.registers*addr''locUsed'FT2t'lo.comp.code.registers*varSet'it'lo.comp.code.registers*varSet''regUsed'FT2t'lo.comp.code.registers*varSet'it'lo.comp.code.registers*varSet''usedRegs'FT1t'lo.comp.code.registers*varSet'Uz1'lo.sets*set'1i'resetUsed'FT1t'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet''maxReg'FT1t'lo.comp.code.registers*varSet'i'freeAReg'FT2t'lo.comp.code.registers*varSet'it'lo.comp.code.registers*varSet''numRegisters'i'pickAreg'PT3t'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet't'lo.comp.code.registers*addr''lclGc'FT2t'lo.comp.code.registers*varSet'it'lo.comp.code.registers*varSet''gcPredict'FT1t'lo.comp.code.registers*varSet'i'resetGc'FT1t'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet''clearAs'FT3iiUz1'lo.sets*set'1iLt'lo.comp.code.instructions*instruction''gcMap'PT4t'lo.comp.code.registers*varSet'iLt'lo.comp.code.instructions*instruction't'lo.comp.code.instructions*instruction''usedLocals'FT1t'lo.comp.code.registers*varSet'Uz1'lo.sets*set'1i'maxLocals'FT1t'lo.comp.code.registers*varSet'i'varSlotSz'i\"s\"I5'addr'Yt'lo.comp.code.registers*addr'I0'varSet'Yt'lo.comp.code.registers*varSet'I0'initialized'Yt'lo.comp.code.registers*initialized'I0'defined'Yt'lo.comp.code.registers*defined'I0'varDesc'Yt'lo.comp.code.registers*varDesc'I0\"n11o11'()11's'voidAddr's'aReg's'yReg's'sReg's'notAlloc's'varSet's'notInited's'inited's'notDefined's'isDefined's'varDesc'n0o0'()0'n4o4'()4'n2o2'()2's'lo.core$display$lo.comp.code.registers*addr's\"c'lo.core$display'T1t'lo.comp.code.registers*addr'T0\"n2o2'()2's'lo.core$display$lo.comp.code.registers*varDesc's\"c'lo.core$display'T1t'lo.comp.code.registers*varDesc'T0\"n2o2'()2's'lo.core$display$lo.comp.code.registers*initialized's\"c'lo.core$display'T1t'lo.comp.code.registers*initialized'T0\"n2o2'()2's'lo.core$display$lo.comp.code.registers*defined's\"c'lo.core$display'T1t'lo.comp.code.registers*defined'T0\"").
'lo.comp.code.registers@init'():- !.
'lo.comp.code.registers#voidAddr'('voidAddr%1'('lo.comp.code.registers@voidAddr')):- !.
'lo.comp.code.registers#aReg'('aReg%1'('lo.comp.code.registers@aReg'())):- !.
'lo.comp.code.registers#yReg'('yReg%1'('lo.comp.code.registers@yReg'())):- !.
'lo.comp.code.registers#sReg'('sReg%1'('lo.comp.code.registers@sReg')):- !.
'lo.comp.code.registers#notAlloc'('notAlloc%1'('lo.comp.code.registers@notAlloc')):- !.
'lo.comp.code.registers#varSet'('varSet%1'('lo.comp.code.registers@varSet'())):- !.
'lo.comp.code.registers#notInited'('notInited%1'('lo.comp.code.registers@notInited')):- !.
'lo.comp.code.registers#inited'('inited%1'('lo.comp.code.registers@inited')):- !.
'lo.comp.code.registers#notDefined'('notDefined%1'('lo.comp.code.registers@notDefined')):- !.
'lo.comp.code.registers#isDefined'('isDefined%1'('lo.comp.code.registers@isDefined')):- !.
'lo.comp.code.registers#varDesc'('varDesc%1'('lo.comp.code.registers@varDesc'())):- !.
'lo.comp.code.registers@isInitialized'('lo.comp.code.registers#varDesc'(X_31868, 'lo.comp.code.registers#inited', X_31869, X_31870, X_31871)).
'lo.comp.code.registers@markInited'('lo.comp.code.registers#varDesc'(XNm, X_31872, X_31873, X_31874, XU), XAddr, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#inited', 'lo.comp.code.registers#isDefined', XAddr, XU)):- !.
'lo.comp.code.registers@markInited'(_, _, _):- raise_exception('error'("lo.comp.code.registers@markInited", 21, 3, 75)).
'lo.comp.code.registers@varAddr'('lo.comp.code.registers#varDesc'(X_31875, X_31876, X_31877, XA, X_31878), XA):- !.
'lo.comp.code.registers@varAddr'(_, _):- raise_exception('error'("lo.comp.code.registers@varAddr", 24, 3, 30)).
'lo.comp.code.registers@locUsed'('lo.comp.code.registers#varSet'(XA, XL, XG), XLc, 'lo.comp.code.registers#varSet'(XA, XXe4610, XG)):- !,
    ocall('addMem%1'(XXV4952),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XL, XLc, XXe4610),XXV4952,XXV4952).
'lo.comp.code.registers@locUsed'(_, _, _):- raise_exception('error'("lo.comp.code.registers@locUsed", 27, 3, 53)).
'lo.comp.code.registers@regUsed'('lo.comp.code.registers#varSet'(XA, XL, XG), XRg, 'lo.comp.code.registers#varSet'(XXe4611, XL, XG)):- !,
    ocall('addMem%1'(XXV4953),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XA, XRg, XXe4611),XXV4953,XXV4953).
'lo.comp.code.registers@regUsed'(_, _, _):- raise_exception('error'("lo.comp.code.registers@regUsed", 30, 3, 53)).
'lo.comp.code.registers@usedRegs'('lo.comp.code.registers#varSet'(XA, X_31879, X_31880), XA):- !.
'lo.comp.code.registers@usedRegs'(_, _):- raise_exception('error'("lo.comp.code.registers@usedRegs", 33, 3, 28)).
'lo.comp.code.registers@resetUsed'('lo.comp.code.registers#varSet'(X_31881, XL, XG), 'lo.comp.code.registers#varSet'('lo.sets#set'(XXV4954), XL, XG)):- !,
    ocall('_empty%1'(XXV4954),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')).
'lo.comp.code.registers@resetUsed'(_, _):- raise_exception('error'("lo.comp.code.registers@resetUsed", 36, 3, 47)).
'lo.comp.code.registers@maxReg'('lo.comp.code.registers#varSet'(XA, X_31882, X_31883), XXe4612):- !,
    ocall('foldRight%1'(XXV4955),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%4'('lo.comp.code.registers@fun109', 0, XA, XXe4612),XXV4955,XXV4955).
'lo.comp.code.registers@maxReg'(_, _):- raise_exception('error'("lo.comp.code.registers@maxReg", 39, 3, 55)).
'lo.comp.code.registers@freeAReg'('lo.comp.code.registers#varSet'(XA, XL, XG), XRg, 'lo.comp.code.registers#varSet'(XXe4613, XL, XG)):- !,
    ocall('delMem%1'(XXV4956),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XA, XRg, XXe4613),XXV4956,XXV4956).
'lo.comp.code.registers@freeAReg'(_, _, _):- raise_exception('error'("lo.comp.code.registers@freeAReg", 42, 3, 54)).
'lo.comp.code.registers@numRegisters'(64):- !.
'lo.comp.code.registers@inRange'(XLo, XHi, XLo).
'lo.comp.code.registers@inRange'(XLo, XHi, XEl):- ocall('+%1'(XXV4957),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XLo, 1, XXe4614),XXV4957,XXV4957),
    'lo.comp.code.registers@inRange'(XXe4614, XHi, XEl).
'lo.comp.code.registers@pickAreg'('lo.comp.code.registers#varSet'(XA, XL, XG), 'lo.comp.code.registers#varSet'(XXe4615, XL, XG), 'lo.comp.code.registers#aReg'(XRg)):- 'lo.comp.code.registers@one272'(XXd36938, XA, XRg, XnumRegisters10),
    ocall('addMem%1'(XXV4958),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XA, XRg, XXe4615),XXV4958,XXV4958).
'lo.comp.code.registers@lclGc'('lo.comp.code.registers#varSet'(XA, XL, XG), XSz, 'lo.comp.code.registers#varSet'(XA, XL, XXe4616)):- !,
    ocall('+%1'(XXV4959),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XG, XSz, XXe4616),XXV4959,XXV4959).
'lo.comp.code.registers@lclGc'(_, _, _):- raise_exception('error'("lo.comp.code.registers@lclGc", 53, 3, 43)).
'lo.comp.code.registers@gcPredict'('lo.comp.code.registers#varSet'(X_31884, X_31885, XG), XG):- !.
'lo.comp.code.registers@gcPredict'(_, _):- raise_exception('error'("lo.comp.code.registers@gcPredict", 56, 3, 29)).
'lo.comp.code.registers@resetGc'('lo.comp.code.registers#varSet'(XA, XL, X_31886), 'lo.comp.code.registers#varSet'(XA, XL, 0)):- !.
'lo.comp.code.registers@resetGc'(_, _):- raise_exception('error'("lo.comp.code.registers@resetGc", 59, 3, 39)).
'lo.comp.code.registers@clearAs'(XRg, XMx, X_31887, 'lo.core#[]'):- 'lo.core@>'('lo.core$comp$lo.core*integer', XRg, XMx),
    !.
'lo.comp.code.registers@clearAs'(XRg, XMx, XRegs, 'lo.core#,..'('lo.comp.code.instructions#iVdA'(XRg), XXd36943)):- 'lo.comp.code.registers@neg301'(XXd36941, XRegs, XRg),
    !,
    ocall('+%1'(XXV4960),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XRg, 1, XXe4617),XXV4960,XXV4960),
    'lo.comp.code.registers@clearAs'(XXe4617, XMx, XRegs, XXd36943).
'lo.comp.code.registers@clearAs'(XRg, XMx, XRegs, XXd36945):- !,
    ocall('+%1'(XXV4961),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XRg, 1, XXe4618),XXV4961,XXV4961),
    'lo.comp.code.registers@clearAs'(XXe4618, XMx, XRegs, XXd36945).
'lo.comp.code.registers@clearAs'(_, _, _, _):- raise_exception('error'("lo.comp.code.registers@clearAs", 71, 3, 31)).
'lo.comp.code.registers@clearYs'(XRg, XMx, X_31889, 'lo.core#[]'):- 'lo.core@>'('lo.core$comp$lo.core*integer', XRg, XMx),
    !.
'lo.comp.code.registers@clearYs'(XRg, XMx, XLocs, 'lo.core#,..'('lo.comp.code.instructions#iClY'(XRg), XXd36948)):- 'lo.comp.code.registers@neg302'(XXd36946, XLocs, XRg),
    !,
    ocall('+%1'(XXV4962),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XRg, 1, XXe4619),XXV4962,XXV4962),
    'lo.comp.code.registers@clearYs'(XXe4619, XMx, XLocs, XXd36948).
'lo.comp.code.registers@clearYs'(XRg, XMx, XLocs, XXd36950):- !,
    ocall('+%1'(XXV4963),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XRg, 1, XXe4620),XXV4963,XXV4963),
    'lo.comp.code.registers@clearYs'(XXe4620, XMx, XLocs, XXd36950).
'lo.comp.code.registers@clearYs'(_, _, _, _):- raise_exception('error'("lo.comp.code.registers@clearYs", 66, 3, 31)).
'lo.comp.code.registers@gcMap'('lo.comp.code.registers#varSet'(XRgs, XLocs, X_31891), XArity, XXb17996, 'lo.comp.code.instructions#iGcmap'(XArity, XEsize)):- ocall('foldRight%1'(XXV4964),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%4'('lo.comp.code.registers@fun110', 0, XLocs, XXe4621),XXV4964,XXV4964),
    XEsize = XXe4621,
    'lo.comp.code.registers@clearYs'(1, XEsize, XLocs, XXb17994),
    'lo.comp.code.registers@clearAs'(1, XArity, XRgs, XXb17995),
    'lo.list@<>'(XXb17994, XXb17995, XXb17996).
'lo.comp.code.registers@usedLocals'('lo.comp.code.registers#varSet'(X_31892, XL, X_31893), XL):- !.
'lo.comp.code.registers@usedLocals'(_, _):- raise_exception('error'("lo.comp.code.registers@usedLocals", 76, 3, 30)).
'lo.comp.code.registers@maxLocals'('lo.comp.code.registers#varSet'(X_31894, XL, X_31895), XXe4622):- !,
    ocall('foldRight%1'(XXV4965),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$folding$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%4'('lo.comp.code.registers@fun111', 0, XL, XXe4622),XXV4965,XXV4965).
'lo.comp.code.registers@maxLocals'(_, _):- raise_exception('error'("lo.comp.code.registers@maxLocals", 79, 3, 58)).
'lo.comp.code.registers@varSlotSz'(2):- !.
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#voidAddr', 'lo.core#ss'("void")):- !.
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#aReg'(XR), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("A["), 'lo.core#,..'(XXe4623, 'lo.core#,..'('lo.core#ss'("]"), 'lo.core#[]'))))):- !,
    ocall('disp%1'(XXV4966),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('_call%2'(XR, XXe4623),XXV4966,XXV4966).
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#yReg'(XLc, XSf), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Y["), 'lo.core#,..'(XXe4624, 'lo.core#,..'('lo.core#ss'(XCndV118), 'lo.core#,..'('lo.core#ss'("]"), 'lo.core#[]')))))):- !,
    ocall('disp%1'(XXV4967),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo.comp.code.registers@condExp118'(XCndV118, XSf),
    ocall('_call%2'(XLc, XXe4624),XXV4967,XXV4967).
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#sReg', 'lo.core#ss'("S")):- !.
'lo.comp.code.registers@dispAdd'('lo.comp.code.registers#notAlloc', 'lo.core#ss'("not allocated")):- !.
'lo.comp.code.registers@dispAdd'(_, _):- raise_exception('error'("lo.comp.code.registers@dispAdd", 92, 3, 31)).
'lo.core$display$lo.comp.code.registers*addr'('lo.core$display$lo.comp.code.registers*addr%1'('lo.core$display$lo.comp.code.registers*addr')):- !.
'lo.core$display$lo.comp.code.registers*addr'('disp%2'(XV29137, XV29138), XLbl2107, XThis2107):- !,
    'lo.core$display$lo.comp.code.registers*addr@disp'(XV29137, XV29138, XLbl2107, XThis2107).
'lo.core$display$lo.comp.code.registers*addr'('disp%1'('lo.core$display$lo.comp.code.registers*addr^disp'(XLbl2108, XThis2108)), XLbl2108, XThis2108).
'lo.core$display$lo.comp.code.registers*addr@disp'(XA, XXd36972, XLbV2385, XThV2385):- !,
    'lo.comp.code.registers@dispAdd'(XA, XXd36972).
'lo.core$display$lo.comp.code.registers*addr@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.code.registers*addr@disp", 88, 5, 21)).
'lo.comp.code.registers@dispVarDesc'('lo.comp.code.registers#varDesc'(XNm, XI, XD, XA, X_31903), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XNm), 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XXe4625, 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XXe4626, 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XXe4627, 'lo.core#[]'))))))))):- !,
    ocall('disp%1'(XXV4968),'lo.core$display$lo.comp.code.registers*initialized','lo.core$display$lo.comp.code.registers*initialized'),
    ocall('disp%1'(XXV4969),'lo.core$display$lo.comp.code.registers*defined','lo.core$display$lo.comp.code.registers*defined'),
    ocall('disp%1'(XXV4970),'lo.core$display$lo.comp.code.registers*addr','lo.core$display$lo.comp.code.registers*addr'),
    ocall('_call%2'(XI, XXe4625),XXV4968,XXV4968),
    ocall('_call%2'(XD, XXe4626),XXV4969,XXV4969),
    ocall('_call%2'(XA, XXe4627),XXV4970,XXV4970).
'lo.comp.code.registers@dispVarDesc'(_, _):- raise_exception('error'("lo.comp.code.registers@dispVarDesc", 103, 3, 99)).
'lo.core$display$lo.comp.code.registers*varDesc'('lo.core$display$lo.comp.code.registers*varDesc%1'('lo.core$display$lo.comp.code.registers*varDesc')):- !.
'lo.core$display$lo.comp.code.registers*varDesc'('disp%2'(XV29143, XV29144), XLbl2109, XThis2109):- !,
    'lo.core$display$lo.comp.code.registers*varDesc@disp'(XV29143, XV29144, XLbl2109, XThis2109).
'lo.core$display$lo.comp.code.registers*varDesc'('disp%1'('lo.core$display$lo.comp.code.registers*varDesc^disp'(XLbl2110, XThis2110)), XLbl2110, XThis2110).
'lo.core$display$lo.comp.code.registers*varDesc@disp'(XV, XXd36985, XLbV2386, XThV2386):- !,
    'lo.comp.code.registers@dispVarDesc'(XV, XXd36985).
'lo.core$display$lo.comp.code.registers*varDesc@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.code.registers*varDesc@disp", 99, 5, 25)).
'lo.core$display$lo.comp.code.registers*initialized'('lo.core$display$lo.comp.code.registers*initialized%1'('lo.core$display$lo.comp.code.registers*initialized')):- !.
'lo.core$display$lo.comp.code.registers*initialized'('disp%2'(XV29147, XV29148), XLbl2111, XThis2111):- !,
    'lo.core$display$lo.comp.code.registers*initialized@disp'(XV29147, XV29148, XLbl2111, XThis2111).
'lo.core$display$lo.comp.code.registers*initialized'('disp%1'('lo.core$display$lo.comp.code.registers*initialized^disp'(XLbl2112, XThis2112)), XLbl2112, XThis2112).
'lo.core$display$lo.comp.code.registers*initialized@disp'('lo.comp.code.registers#notInited', 'lo.core#ss'("not inited"), XLbV2387, XThV2387):- !.
'lo.core$display$lo.comp.code.registers*initialized@disp'('lo.comp.code.registers#inited', 'lo.core#ss'("inited"), XLbV2387, XThV2387):- !.
'lo.core$display$lo.comp.code.registers*initialized@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.code.registers*initialized@disp", 106, 5, 35)).
'lo.core$display$lo.comp.code.registers*defined'('lo.core$display$lo.comp.code.registers*defined%1'('lo.core$display$lo.comp.code.registers*defined')):- !.
'lo.core$display$lo.comp.code.registers*defined'('disp%2'(XV29151, XV29152), XLbl2113, XThis2113):- !,
    'lo.core$display$lo.comp.code.registers*defined@disp'(XV29151, XV29152, XLbl2113, XThis2113).
'lo.core$display$lo.comp.code.registers*defined'('disp%1'('lo.core$display$lo.comp.code.registers*defined^disp'(XLbl2114, XThis2114)), XLbl2114, XThis2114).
'lo.core$display$lo.comp.code.registers*defined@disp'('lo.comp.code.registers#notDefined', 'lo.core#ss'("not defined"), XLbV2388, XThV2388):- !.
'lo.core$display$lo.comp.code.registers*defined@disp'('lo.comp.code.registers#isDefined', 'lo.core#ss'("defined"), XLbV2388, XThV2388):- !.
'lo.core$display$lo.comp.code.registers*defined@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.code.registers*defined@disp", 111, 5, 37)).
'lo.comp.code.registers^isInitialized'('_call%1'(XV29083), 'lo.comp.code.registers^isInitialized', _):- 'lo.comp.code.registers@isInitialized'(XV29083).
'lo.comp.code.registers^markInited'('_call%3'(XV29084, XV29085, XV29086), 'lo.comp.code.registers^markInited', _):- 'lo.comp.code.registers@markInited'(XV29084, XV29085, XV29086).
'lo.comp.code.registers^varAddr'('_call%2'(XV29087, XV29088), 'lo.comp.code.registers^varAddr', _):- 'lo.comp.code.registers@varAddr'(XV29087, XV29088).
'lo.comp.code.registers^locUsed'('_call%3'(XV29089, XV29090, XV29091), 'lo.comp.code.registers^locUsed', _):- 'lo.comp.code.registers@locUsed'(XV29089, XV29090, XV29091).
'lo.comp.code.registers^regUsed'('_call%3'(XV29092, XV29093, XV29094), 'lo.comp.code.registers^regUsed', _):- 'lo.comp.code.registers@regUsed'(XV29092, XV29093, XV29094).
'lo.comp.code.registers^usedRegs'('_call%2'(XV29095, XV29096), 'lo.comp.code.registers^usedRegs', _):- 'lo.comp.code.registers@usedRegs'(XV29095, XV29096).
'lo.comp.code.registers^resetUsed'('_call%2'(XV29097, XV29098), 'lo.comp.code.registers^resetUsed', _):- 'lo.comp.code.registers@resetUsed'(XV29097, XV29098).
'lo.comp.code.registers@fun109'('_call%3'(XX, XY, XXd36933), 'lo.comp.code.registers@fun109', _):- !,
    'lo.core@max'('lo.core$comp$lo.core*integer', XX, XY, XXd36933).
'lo.comp.code.registers@fun109'(_, _, _):- raise_exception('error'("lo.comp.code.registers@fun109", 39, 38, 15)).
'lo.comp.code.registers^maxReg'('_call%2'(XV29099, XV29100), 'lo.comp.code.registers^maxReg', _):- 'lo.comp.code.registers@maxReg'(XV29099, XV29100).
'lo.comp.code.registers^freeAReg'('_call%3'(XV29101, XV29102, XV29103), 'lo.comp.code.registers^freeAReg', _):- 'lo.comp.code.registers@freeAReg'(XV29101, XV29102, XV29103).
'lo.comp.code.registers^inRange'('_call%3'(XV29104, XV29105, XV29106), 'lo.comp.code.registers^inRange', _):- 'lo.comp.code.registers@inRange'(XV29104, XV29105, XV29106).
'lo.comp.code.registers@neg300'(XXd36938, XA, XRg):- ocall('in%2'(XRg, XA),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.registers@neg300'(XXd36938, XA, XRg).
'lo.comp.code.registers@one272'(XXd36938, XA, XRg, XnumRegisters10):- 'lo.comp.code.registers@numRegisters'(XnumRegisters10),
    'lo.comp.code.registers@inRange'(1, XnumRegisters10, XRg),
    'lo.comp.code.registers@neg300'(XXd36938, XA, XRg),
    !.
'lo.comp.code.registers^pickAreg'('_call%3'(XV29107, XV29108, XV29109), 'lo.comp.code.registers^pickAreg', _):- 'lo.comp.code.registers@pickAreg'(XV29107, XV29108, XV29109).
'lo.comp.code.registers^lclGc'('_call%3'(XV29110, XV29111, XV29112), 'lo.comp.code.registers^lclGc', _):- 'lo.comp.code.registers@lclGc'(XV29110, XV29111, XV29112).
'lo.comp.code.registers^gcPredict'('_call%2'(XV29113, XV29114), 'lo.comp.code.registers^gcPredict', _):- 'lo.comp.code.registers@gcPredict'(XV29113, XV29114).
'lo.comp.code.registers^resetGc'('_call%2'(XV29115, XV29116), 'lo.comp.code.registers^resetGc', _):- 'lo.comp.code.registers@resetGc'(XV29115, XV29116).
'lo.comp.code.registers@neg301'(XXd36941, XRegs, XRg):- ocall('in%2'(XRg, XRegs),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.registers@neg301'(XXd36941, XRegs, XRg).
'lo.comp.code.registers^clearAs'('_call%4'(XV29117, XV29118, XV29119, XV29120), 'lo.comp.code.registers^clearAs', _):- 'lo.comp.code.registers@clearAs'(XV29117, XV29118, XV29119, XV29120).
'lo.comp.code.registers@neg302'(XXd36946, XLocs, XRg):- ocall('in%2'(XRg, XLocs),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.registers@neg302'(XXd36946, XLocs, XRg).
'lo.comp.code.registers^clearYs'('_call%4'(XV29121, XV29122, XV29123, XV29124), 'lo.comp.code.registers^clearYs', _):- 'lo.comp.code.registers@clearYs'(XV29121, XV29122, XV29123, XV29124).
'lo.comp.code.registers@fun110'('_call%3'(XX, XY, XXd36951), 'lo.comp.code.registers@fun110', _):- !,
    'lo.core@max'('lo.core$comp$lo.core*integer', XX, XY, XXd36951).
'lo.comp.code.registers@fun110'(_, _, _):- raise_exception('error'("lo.comp.code.registers@fun110", 63, 23, 15)).
'lo.comp.code.registers^gcMap'('_call%4'(XV29125, XV29126, XV29127, XV29128), 'lo.comp.code.registers^gcMap', _):- 'lo.comp.code.registers@gcMap'(XV29125, XV29126, XV29127, XV29128).
'lo.comp.code.registers^usedLocals'('_call%2'(XV29129, XV29130), 'lo.comp.code.registers^usedLocals', _):- 'lo.comp.code.registers@usedLocals'(XV29129, XV29130).
'lo.comp.code.registers@fun111'('_call%3'(XX, XY, XXd36953), 'lo.comp.code.registers@fun111', _):- !,
    'lo.core@max'('lo.core$comp$lo.core*integer', XX, XY, XXd36953).
'lo.comp.code.registers@fun111'(_, _, _):- raise_exception('error'("lo.comp.code.registers@fun111", 79, 41, 15)).
'lo.comp.code.registers^maxLocals'('_call%2'(XV29131, XV29132), 'lo.comp.code.registers^maxLocals', _):- 'lo.comp.code.registers@maxLocals'(XV29131, XV29132).
'lo.comp.code.registers@condExp118'("/safe", XSf):- XSf = 'lo.core#true',
    !.
'lo.comp.code.registers@condExp118'("/not safe", XSf).
'lo.comp.code.registers^dispAdd'('_call%2'(XV29133, XV29134), 'lo.comp.code.registers^dispAdd', _):- 'lo.comp.code.registers@dispAdd'(XV29133, XV29134).
'lo.core$display$lo.comp.code.registers*addr^disp'('_call%2'(XV29135, XV29136), 'lo.core$display$lo.comp.code.registers*addr^disp'(XLbV2385, XThV2385), _):- 'lo.core$display$lo.comp.code.registers*addr@disp'(XV29135, XV29136, XLbV2385, XThV2385).
'lo.comp.code.registers^dispVarDesc'('_call%2'(XV29139, XV29140), 'lo.comp.code.registers^dispVarDesc', _):- 'lo.comp.code.registers@dispVarDesc'(XV29139, XV29140).
'lo.core$display$lo.comp.code.registers*varDesc^disp'('_call%2'(XV29141, XV29142), 'lo.core$display$lo.comp.code.registers*varDesc^disp'(XLbV2386, XThV2386), _):- 'lo.core$display$lo.comp.code.registers*varDesc@disp'(XV29141, XV29142, XLbV2386, XThV2386).
'lo.core$display$lo.comp.code.registers*initialized^disp'('_call%2'(XV29145, XV29146), 'lo.core$display$lo.comp.code.registers*initialized^disp'(XLbV2387, XThV2387), _):- 'lo.core$display$lo.comp.code.registers*initialized@disp'(XV29145, XV29146, XLbV2387, XThV2387).
'lo.core$display$lo.comp.code.registers*defined^disp'('_call%2'(XV29149, XV29150), 'lo.core$display$lo.comp.code.registers*defined^disp'(XLbV2388, XThV2388), _):- 'lo.core$display$lo.comp.code.registers*defined@disp'(XV29149, XV29150, XLbV2388, XThV2388).
