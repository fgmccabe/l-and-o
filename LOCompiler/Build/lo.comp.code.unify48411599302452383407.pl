'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.unify's'0.0.1'n6o6'()6'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.code'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.instructions'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.registers'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'s\"I12'moveVr'PT6t'lo.comp.code.registers*addr't'lo.comp.code.registers*addr't'lo.comp.code.registers*varSet'lLt'lo.comp.code.instructions*instruction't'lo.comp.code.registers*varSet''unifyVr'PT5t'lo.comp.code.registers*addr't'lo.comp.code.registers*addr't'lo.comp.code.registers*varSet'Lt'lo.comp.code.instructions*instruction't'lo.comp.code.registers*varSet''accessLiteral'PT4t'lo.comp.term*term'SLt'lo.comp.code.code*litrl'Lt'lo.comp.code.code*litrl''unifyTerm'FT10t'lo.comp.term*term'lt'lo.comp.code.registers*addr'Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc'Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc't'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet'Lt'lo.comp.code.code*litrl'Lt'lo.comp.code.code*litrl'lLt'lo.comp.code.instructions*instruction''unifyHead'PT9Lt'lo.comp.term*term'Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc'Lt'lo.comp.code.code*litrl't'lo.comp.code.registers*varSet'lt'lo.comp.code.registers*varSet'Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc'Lt'lo.comp.code.code*litrl'Lt'lo.comp.code.instructions*instruction''matchVr'PT5t'lo.comp.code.registers*addr't'lo.comp.code.registers*addr't'lo.comp.code.registers*varSet'Lt'lo.comp.code.instructions*instruction't'lo.comp.code.registers*varSet''matchTerm'FT10t'lo.comp.term*term'lt'lo.comp.code.registers*addr'Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc'Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc't'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet'Lt'lo.comp.code.code*litrl'Lt'lo.comp.code.code*litrl'lLt'lo.comp.code.instructions*instruction''clearTrm'FT3t'lo.comp.code.registers*addr't'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet'Lt'lo.comp.code.instructions*instruction''clearVr'PT5t'lo.comp.code.registers*addr't'lo.comp.code.registers*addr't'lo.comp.code.registers*varSet'Lt'lo.comp.code.instructions*instruction't'lo.comp.code.registers*varSet''buildTerm'FT10t'lo.comp.term*term'lt'lo.comp.code.registers*addr'Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc'Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc't'lo.comp.code.registers*varSet't'lo.comp.code.registers*varSet'Lt'lo.comp.code.code*litrl'Lt'lo.comp.code.code*litrl'lLt'lo.comp.code.instructions*instruction''callArgs'PT10Lt'lo.comp.term*term'iUz2'lo.index*map'2St'lo.comp.code.registers*varDesc'Lt'lo.comp.code.code*litrl't'lo.comp.code.registers*varSet'lLt'lo.comp.code.instructions*instruction'Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc'Lt'lo.comp.code.code*litrl't'lo.comp.code.registers*varSet''overVr'PT6t'lo.comp.code.registers*addr't'lo.comp.code.registers*addr't'lo.comp.code.registers*varSet'lLt'lo.comp.code.instructions*instruction't'lo.comp.code.registers*varSet'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.code.unify@init'():- !.
'lo.comp.code.unify@unifyOp'(XLb, 'lo.comp.code.registers#aReg'(XRg), 'lo.core#true', XV, XXb18100, 'lo.core#,..'('lo.comp.code.instructions#iUAcns'(XRg, XLb), 'lo.core#[]')):- !,
    'lo.comp.code.registers@freeAReg'(XV, XRg, XXb18100).
'lo.comp.code.unify@unifyOp'(XLb, 'lo.comp.code.registers#aReg'(XRg), 'lo.core#false', XV, XV, 'lo.core#,..'('lo.comp.code.instructions#iUAcns'(XRg, XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@unifyOp'(XLb, 'lo.comp.code.registers#yReg'(XLc, X_32122), X_32123, XV, XV, 'lo.core#,..'('lo.comp.code.instructions#iMAY'(0, XLc), 'lo.core#,..'('lo.comp.code.instructions#iUAcns'(0, XLb), 'lo.core#[]'))):- !.
'lo.comp.code.unify@unifyOp'(XLb, 'lo.comp.code.registers#sReg', X_32126, XV, XV, 'lo.core#,..'('lo.comp.code.instructions#iUScns'(XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@unifyOp'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@unifyOp", 65, 3, 61)).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#voidAddr', X_32128, XUsed, X_32129, 'lo.core#[]', XUsed).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#aReg'(XRg), XUsed, X_32130, 'lo.core#[]', XUsed).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#aReg'(XReg), XUsed, X_32131, 'lo.core#,..'('lo.comp.code.instructions#iMAA'(XRg, XReg), 'lo.core#[]'), XXb18109):- 'lo.comp.code.registers@regUsed'(XUsed, XRg, XXb18109).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#yReg'(XLc, XSafe), XUsed, XLast, 'lo.core#,..'('lo.comp.code.instructions#iMAY'(XRg, XLc), 'lo.core#[]'), XXb18114):- 'lo.comp.code.unify@or174'(XLast, XSafe),
    'lo.comp.code.registers@regUsed'(XUsed, XRg, XXb18114).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#yReg'(XLc, X_32134), XUsed, X_32135, 'lo.core#,..'('lo.comp.code.instructions#iMuAY'(XRg, XLc), 'lo.core#[]'), XXb18120):- 'lo.comp.code.registers@varSlotSz'(XvarSlotSz17),
    'lo.comp.code.registers@lclGc'(XUsed, XvarSlotSz17, XXb18119),
    'lo.comp.code.registers@regUsed'(XXb18119, XRg, XXb18120).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#sReg', XUsed, X_32137, 'lo.core#,..'('lo.comp.code.instructions#iMAS'(XRg), 'lo.core#[]'), XXb18124):- 'lo.comp.code.registers@regUsed'(XUsed, XRg, XXb18124).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#yReg'(XLc, X_32139), 'lo.comp.code.registers#aReg'(XRg), XUsed, X_32140, 'lo.core#,..'('lo.comp.code.instructions#iMYA'(XLc, XRg), 'lo.core#[]'), XXb18129):- 'lo.comp.code.registers@locUsed'(XUsed, XLc, XXb18129).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#yReg'(XLc, X_32142), 'lo.comp.code.registers#yReg'(XLc, X_32143), XUsed, X_32144, 'lo.core#[]', XUsed).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#yReg'(XLc, X_32145), 'lo.comp.code.registers#yReg'(XLoc, X_32146), XUsed, X_32147, 'lo.core#,..'('lo.comp.code.instructions#iMYY'(XLc, XLoc), 'lo.core#[]'), XXb18136):- ocall('<%2'(XLc, 255),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    ocall('<%2'(XLoc, 255),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    'lo.comp.code.registers@locUsed'(XUsed, XLc, XXb18136).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#yReg'(XLc, X_32149), 'lo.comp.code.registers#yReg'(XLoc, X_32150), XUsed, X_32151, 'lo.core#,..'('lo.comp.code.instructions#iMAY'(0, XLoc), 'lo.core#,..'('lo.comp.code.instructions#iMYA'(XLc, 0), 'lo.core#[]')), XXb18143):- 'lo.comp.code.registers@locUsed'(XUsed, XLc, XXb18143).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#yReg'(XLc, X_32154), 'lo.comp.code.registers#sReg', XUsed, X_32155, 'lo.core#,..'('lo.comp.code.instructions#iMYS'(XLc), 'lo.core#[]'), XXb18147):- 'lo.comp.code.registers@locUsed'(XUsed, XLc, XXb18147).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#sReg', 'lo.comp.code.registers#aReg'(XRg), XUsed, X_32157, 'lo.core#,..'('lo.comp.code.instructions#iMSA'(XRg), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@moveVr'('lo.comp.code.registers#sReg', 'lo.comp.code.registers#yReg'(XLc, X_32159), XUsed, X_32160, 'lo.core#,..'('lo.comp.code.instructions#iMSY'(XLc), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#voidAddr', X_32162, XUsed, 'lo.core#[]', XUsed).
'lo.comp.code.unify@unifyVr'(X_32163, 'lo.comp.code.registers#voidAddr', XUsed, 'lo.core#[]', XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#aReg'(XReg), 'lo.comp.code.registers#aReg'(XReg), XUsed, 'lo.core#[]', XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#aReg'(XReg), 'lo.comp.code.registers#aReg'(XRg), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iUAA'(XReg, XRg), 'lo.core#[]'), XUsed):- 'lo.comp.code.unify@neg316'(XRg, XReg).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#aReg'(XReg), 'lo.comp.code.registers#yReg'(XLc, X_32165), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iUAY'(XReg, XLc), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#aReg'(XReg), 'lo.comp.code.registers#sReg', XUsed, 'lo.core#,..'('lo.comp.code.instructions#iUAS'(XReg), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#yReg'(XLc, X_32168), 'lo.comp.code.registers#aReg'(XReg), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iUAY'(XReg, XLc), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#yReg'(XLc, X_32170), 'lo.comp.code.registers#yReg'(XLc, X_32171), XUsed, 'lo.core#[]', XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#yReg'(XLc, X_32172), 'lo.comp.code.registers#yReg'(XLoc, X_32173), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iUYY'(XLc, XLoc), 'lo.core#[]'), XUsed):- ocall('<%2'(XLc, 255),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    ocall('<%2'(XLoc, 255),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer').
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#yReg'(XLc, X_32175), 'lo.comp.code.registers#yReg'(XLoc, X_32176), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iMAY'(0, XLc), 'lo.core#,..'('lo.comp.code.instructions#iUAY'(0, XLoc), 'lo.core#[]')), XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#yReg'(XLc, X_32179), 'lo.comp.code.registers#sReg', XUsed, 'lo.core#,..'('lo.comp.code.instructions#iUYS'(XLc), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#sReg', 'lo.comp.code.registers#aReg'(XRg), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iUAS'(XRg), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@unifyVr'('lo.comp.code.registers#sReg', 'lo.comp.code.registers#yReg'(XLc, X_32182), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iUYS'(XLc), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@unifyVar'('lo.comp.code.registers#varDesc'(X_32184, X_32185, X_32186, 'lo.comp.code.registers#voidAddr', X_32187), 'lo.comp.code.registers#sReg', XD, XD, XV, XV, X_32188, 'lo.core#,..'('lo.comp.code.instructions#iClS', 'lo.core#[]')):- !.
'lo.comp.code.unify@unifyVar'('lo.comp.code.registers#varDesc'(X_32190, X_32191, X_32192, 'lo.comp.code.registers#voidAddr', X_32193), X_32194, XD, XD, XV, XV, X_32195, 'lo.core#[]'):- !.
'lo.comp.code.unify@unifyVar'('lo.comp.code.registers#varDesc'(X_32196, 'lo.comp.code.registers#inited', X_32197, XSrc, X_32198), XDst, XD, XD, XV, XVx, XLast, XIns):- 'lo.comp.code.unify@unifyVr'(XSrc, XDst, XV, XIns, XVx),
    !.
'lo.comp.code.unify@unifyVar'('lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#notInited', XDef, XSrc, XWh), XDst, XD, XXe4700, XV, XVx, XLast, XIns):- 'lo.comp.code.unify@moveVr'(XSrc, XDst, XV, XLast, XIns, XVx),
    !,
    ocall('_put%1'(XXV5038),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XD, XNm, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#inited', XDef, XSrc, XWh), XXe4700),XXV5038,XXV5038).
'lo.comp.code.unify@unifyVar'(_, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@unifyVar", 40, 3, 60)).
'lo.comp.code.unify@unifyLit'(X_32199, X_32200, 'lo.comp.code.registers#voidAddr', XVs, XVs, 'lo.core#[]'):- !.
'lo.comp.code.unify@unifyLit'(XLb, 'lo.core#true', 'lo.comp.code.registers#aReg'(XRg), XVS, XXb18197, 'lo.core#,..'('lo.comp.code.instructions#iUAlit'(XRg, XLb), 'lo.core#[]')):- !,
    'lo.comp.code.registers@freeAReg'(XVS, XRg, XXb18197).
'lo.comp.code.unify@unifyLit'(XLb, 'lo.core#false', 'lo.comp.code.registers#aReg'(XRg), XVS, XVS, 'lo.core#,..'('lo.comp.code.instructions#iUAlit'(XRg, XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@unifyLit'(XLb, X_32203, 'lo.comp.code.registers#yReg'(XLc, X_32204), XVS, XVS, 'lo.core#,..'('lo.comp.code.instructions#iMAY'(0, XLc), 'lo.core#,..'('lo.comp.code.instructions#iUAlit'(0, XLb), 'lo.core#[]'))):- !.
'lo.comp.code.unify@unifyLit'(XLb, X_32207, 'lo.comp.code.registers#sReg', XVS, XVS, 'lo.core#,..'('lo.comp.code.instructions#iUSlit'(XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@unifyLit'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@unifyLit", 52, 3, 34)).
'lo.comp.code.unify@accessLiteral'(XT, XLb, XLt, XLt):- ocall('in%2'('lo.comp.code.code#litrl'(XLb, XT), XLt),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.unify@accessLiteral'('lo.comp.term#strng'(XSx), XLb, XLts, 'lo.core#,..'('lo.comp.code.code#litrl'(XLb, 'lo.comp.term#strng'(XSx)), XLts)):- '_str_gen'(XSx, XXc507),
    XLb = XXc507.
'lo.comp.code.unify@accessLiteral'('lo.comp.term#enum'(XSx), XLb, XLts, 'lo.core#,..'('lo.comp.code.code#litrl'(XLb, 'lo.comp.term#enum'(XSx)), XLts)):- '_str_gen'(XSx, XXc508),
    XLb = XXc508.
'lo.comp.code.unify@accessLiteral'(XT, XLb, XLts, 'lo.core#,..'('lo.comp.code.code#litrl'(XLb, XT), XLts)):- '_str_gen'("L", XXc509),
    XLb = XXc509.
'lo.comp.code.unify@skipOver'('lo.comp.code.registers#voidAddr', 'lo.core#[]'):- !.
'lo.comp.code.unify@skipOver'('lo.comp.code.registers#aReg'(X_32212), 'lo.core#[]'):- !.
'lo.comp.code.unify@skipOver'('lo.comp.code.registers#yReg'(X_32213, X_32214), 'lo.core#[]'):- !.
'lo.comp.code.unify@skipOver'('lo.comp.code.registers#sReg', 'lo.core#,..'('lo.comp.code.instructions#iClS', 'lo.core#[]')):- !.
'lo.comp.code.unify@skipOver'(_, _):- raise_exception('error'("lo.comp.code.unify@skipOver", 46, 3, 24)).
'lo.comp.code.unify@unifyEls'('lo.core#[]', X_32216, XD, XD, XV, XV, XLt, XLt, 'lo.core#[]'):- !.
'lo.comp.code.unify@unifyEls'('lo.core#,..'(Xe, 'lo.core#[]'), XTop, XD, XDx, XV, XVx, XLt, XLtx, XXd37229):- !,
    'lo.comp.code.unify@unifyTerm'(Xe, XTop, 'lo.comp.code.registers#sReg', XD, XDx, XV, XVx, XLt, XLtx, 'lo.core#true', XXd37229).
'lo.comp.code.unify@unifyEls'('lo.core#,..'(Xe, Xl), XTop, XD, XDx, XV, XVx, XLt, XLtx, XXd37232):- !,
    'lo.comp.code.unify@unifyTerm'(Xe, XTop, 'lo.comp.code.registers#sReg', XD, XD0, XV, XV0, XLt, XLt0, 'lo.core#false', XXd37230),
    'lo.comp.code.unify@unifyEls'(Xl, XTop, XD0, XDx, XV0, XVx, XLt0, XLtx, XXd37231),
    'lo.list@<>'(XXd37230, XXd37231, XXd37232).
'lo.comp.code.unify@unifyEls'(_, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@unifyEls", 35, 3, 34)).
'lo.comp.code.unify@unifyTerm'('lo.comp.term#anon', X_32219, XSrc, XD, XD, XVS, XVS, XLts, XLts, X_32220, XXd37233):- !,
    'lo.comp.code.unify@skipOver'(XSrc, XXd37233).
'lo.comp.code.unify@unifyTerm'(XT, XTop, XSrc, XD, XD, XVS, XVSx, XLt, XLtx, X_32221, XXd37234):- 'lo.comp.term@isGroundTerm'(XT),
    'lo.comp.code.unify@accessLiteral'(XT, XLb, XLt, XLtx),
    !,
    'lo.comp.code.unify@unifyLit'(XLb, XTop, XSrc, XVS, XVSx, XXd37234).
'lo.comp.code.unify@unifyTerm'('lo.comp.term#varbl'(XNm), X_32222, XSrc, XD, XDx, XVS, XVx, XLt, XLt, Xlast, XXd37236):- ocall('present%3'(XD, XNm, XEntry),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !,
    'lo.comp.code.unify@unifyVar'(XEntry, XSrc, XD, XDx, XVS, XVx, Xlast, XXd37236).
'lo.comp.code.unify@unifyTerm'('lo.comp.term#cons'(XOp, XEls), XTop, XSrc, XD, XDx, XV, XVx, XLt, XLtx, XLast, XXd37242):- 'lo.comp.code.unify@accessLiteral'(XOp, XLb, XLt, XLt0),
    'lo.comp.code.unify@unifyOp'(XLb, XSrc, XTop, XV, XV0, XXd37237),
    XHdCode = XXd37237,
    'lo.comp.code.unify@cond384'(XXd37239, XV0, XV1, XXd37238, XRg, XSrc, XTop),
    ocall('size%1'(XXV5039),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5040),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XEls, XXe4701),XXV5039,XXV5039),
    ocall('_call%3'(XXe4701, 2, XXe4702),XXV5040,XXV5040),
    'lo.comp.code.registers@lclGc'(XV1, XXe4702, XXd37240),
    XV2 = XXd37240,
    !,
    'lo.comp.code.unify@unifyEls'(XEls, 'lo.core#false', XD, XDx, XV2, XVx, XLt0, XLtx, XXd37241),
    'lo.list@<>'(XHdCode, XXd37241, XXd37242).
'lo.comp.code.unify@unifyTerm'(_, _, _, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@unifyTerm", 23, 3, 58)).
'lo.comp.code.unify@unifyArgs'('lo.core#[]', X_32223, XD, XD, XV, X_32224, XV, XLt, XLt, 'lo.core#[]').
'lo.comp.code.unify@unifyArgs'('lo.core#,..'(Xe, 'lo.core#[]'), XRg, XD, XDx, XV, XLast, XVx, XLt, XLtx, XXb18218):- 'lo.comp.code.unify@unifyTerm'(Xe, 'lo.core#true', 'lo.comp.code.registers#aReg'(XRg), XD, XDx, XV, XVx, XLt, XLtx, XLast, XXb18218).
'lo.comp.code.unify@unifyArgs'('lo.core#,..'(Xe, Xl), XRg, XD, XDx, XV, XLast, XVx, XLt, XLtx, XXb18220):- 'lo.comp.code.unify@unifyTerm'(Xe, 'lo.core#true', 'lo.comp.code.registers#aReg'(XRg), XD, XD0, XV, XV0, XLt, XLt0, XLast, XXd37244),
    XHCode = XXd37244,
    ocall('+%1'(XXV5041),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XRg, 1, XXe4703),XXV5041,XXV5041),
    'lo.comp.code.unify@unifyArgs'(Xl, XXe4703, XD0, XDx, XV0, XLast, XVx, XLt0, XLtx, XRCode),
    'lo.list@<>'(XHCode, XRCode, XXb18220).
'lo.comp.code.unify@unifyHead'(XArgs, XD0, XRefs, XUsed, XLast, XUsx, XDx, XRfx, XInx):- 'lo.comp.code.unify@unifyArgs'(XArgs, 1, XD0, XDx, XUsed, XLast, XUsx, XRefs, XRfx, XInx).
'lo.comp.code.unify@matchOp'(XLb, 'lo.comp.code.registers#aReg'(XRg), 'lo.core#true', XV, XXb18222, 'lo.core#,..'('lo.comp.code.instructions#iCAcns'(XRg, XLb), 'lo.core#[]')):- !,
    'lo.comp.code.registers@freeAReg'(XV, XRg, XXb18222).
'lo.comp.code.unify@matchOp'(XLb, 'lo.comp.code.registers#aReg'(XRg), 'lo.core#false', XV, XV, 'lo.core#,..'('lo.comp.code.instructions#iCAcns'(XRg, XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@matchOp'(XLb, 'lo.comp.code.registers#yReg'(XLc, X_32229), X_32230, XV, XV, 'lo.core#,..'('lo.comp.code.instructions#iMAY'(0, XLc), 'lo.core#,..'('lo.comp.code.instructions#iCAcns'(0, XLb), 'lo.core#[]'))):- !.
'lo.comp.code.unify@matchOp'(XLb, 'lo.comp.code.registers#sReg', X_32233, XV, XV, 'lo.core#,..'('lo.comp.code.instructions#iCScns'(XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@matchOp'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@matchOp", 97, 3, 61)).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#voidAddr', X_32235, XUsed, 'lo.core#[]', XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#aReg'(XRg), XUsed, 'lo.core#[]', XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#aReg'(XReg), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iNvrA'(XRg), 'lo.core#,..'('lo.comp.code.instructions#iCAA'(XRg, XReg), 'lo.core#[]')), XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#yReg'(XLc, XSafe), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iNvrA'(XRg), 'lo.core#,..'('lo.comp.code.instructions#iCAY'(XRg, XLc), 'lo.core#[]')), XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#sReg', XUsed, 'lo.core#,..'('lo.comp.code.instructions#iNvrA'(XRg), 'lo.core#,..'('lo.comp.code.instructions#iCAS'(XRg), 'lo.core#[]')), XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#yReg'(XLc, X_32242), 'lo.comp.code.registers#aReg'(XRg), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iNvrY'(XLc), 'lo.core#,..'('lo.comp.code.instructions#iCYA'(XLc, XRg), 'lo.core#[]')), XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#yReg'(XLc, X_32245), 'lo.comp.code.registers#yReg'(XLc, X_32246), XUsed, 'lo.core#[]', XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#yReg'(XLc, X_32247), 'lo.comp.code.registers#yReg'(XLoc, X_32248), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iNvrY'(XLc), 'lo.core#,..'('lo.comp.code.instructions#iMAY'(0, XLoc), 'lo.core#,..'('lo.comp.code.instructions#iCAY'(0, XLc), 'lo.core#[]'))), XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#yReg'(XLc, X_32252), 'lo.comp.code.registers#sReg', XUsed, 'lo.core#,..'('lo.comp.code.instructions#iNvrY'(XLc), 'lo.core#,..'('lo.comp.code.instructions#iCYS'(XLc), 'lo.core#[]')), XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#sReg', 'lo.comp.code.registers#aReg'(XRg), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iCSA'(XRg), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@matchVr'('lo.comp.code.registers#sReg', 'lo.comp.code.registers#yReg'(XLc, X_32256), XUsed, 'lo.core#,..'('lo.comp.code.instructions#iCSY'(XLc), 'lo.core#[]'), XUsed).
'lo.comp.code.unify@matchVar'('lo.comp.code.registers#varDesc'(X_32258, X_32259, X_32260, 'lo.comp.code.registers#voidAddr', X_32261), 'lo.comp.code.registers#sReg', XD, XD, XV, XV, X_32262, 'lo.core#,..'('lo.comp.code.instructions#iClS', 'lo.core#[]')):- !.
'lo.comp.code.unify@matchVar'('lo.comp.code.registers#varDesc'(X_32264, X_32265, X_32266, 'lo.comp.code.registers#voidAddr', X_32267), X_32268, XD, XD, XV, XV, X_32269, 'lo.core#[]'):- !.
'lo.comp.code.unify@matchVar'('lo.comp.code.registers#varDesc'(X_32270, 'lo.comp.code.registers#inited', X_32271, XSrc, X_32272), XDst, XD, XD, XV, XVx, X_32273, XIns):- 'lo.comp.code.unify@matchVr'(XSrc, XDst, XV, XIns, XVx),
    !.
'lo.comp.code.unify@matchVar'('lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#notInited', XDef, XSrc, XLst), XDst, XD, XXe4704, XV, XVx, XLast, XIns):- 'lo.comp.code.unify@moveVr'(XSrc, XDst, XV, XLast, XIns, XVx),
    !,
    ocall('_put%1'(XXV5042),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XD, XNm, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#inited', XDef, XSrc, XLst), XXe4704),XXV5042,XXV5042).
'lo.comp.code.unify@matchVar'(_, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@matchVar", 84, 3, 60)).
'lo.comp.code.unify@matchLit'(X_32274, X_32275, 'lo.comp.code.registers#voidAddr', XVs, XVs, 'lo.core#[]'):- !.
'lo.comp.code.unify@matchLit'(XLb, 'lo.core#true', 'lo.comp.code.registers#aReg'(XRg), XVS, XXb18276, 'lo.core#,..'('lo.comp.code.instructions#iCAlit'(XRg, XLb), 'lo.core#[]')):- !,
    'lo.comp.code.registers@freeAReg'(XVS, XRg, XXb18276).
'lo.comp.code.unify@matchLit'(XLb, 'lo.core#false', 'lo.comp.code.registers#aReg'(XRg), XVS, XVS, 'lo.core#,..'('lo.comp.code.instructions#iCAlit'(XRg, XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@matchLit'(XLb, X_32278, 'lo.comp.code.registers#yReg'(XLc, X_32279), XVS, XVS, 'lo.core#,..'('lo.comp.code.instructions#iMAY'(0, XLc), 'lo.core#,..'('lo.comp.code.instructions#iCAlit'(0, XLb), 'lo.core#[]'))):- !.
'lo.comp.code.unify@matchLit'(XLb, X_32282, 'lo.comp.code.registers#sReg', XVS, XVS, 'lo.core#,..'('lo.comp.code.instructions#iCSlit'(XLcb), 'lo.core#[]')):- !.
'lo.comp.code.unify@matchLit'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@matchLit", 90, 3, 34)).
'lo.comp.code.unify@matchEls'('lo.core#[]', XD, XD, XV, XV, XLt, XLt, 'lo.core#[]'):- !.
'lo.comp.code.unify@matchEls'('lo.core#,..'(Xe, 'lo.core#[]'), XD, XDx, XV, XVx, XLt, XLtx, XXd37268):- !,
    'lo.comp.code.unify@matchTerm'(Xe, 'lo.core#false', 'lo.comp.code.registers#sReg', XD, XDx, XV, XVx, XLt, XLtx, 'lo.core#true', XXd37268).
'lo.comp.code.unify@matchEls'('lo.core#,..'(Xe, Xl), XD, XDx, XV, XVx, XLt, XLtx, XXd37271):- !,
    'lo.comp.code.unify@matchTerm'(Xe, 'lo.core#false', 'lo.comp.code.registers#sReg', XD, XD0, XV, XV0, XLt, XLt0, 'lo.core#false', XXd37269),
    'lo.comp.code.unify@matchEls'(Xl, XD0, XDx, XV0, XVx, XLt0, XLtx, XXd37270),
    'lo.list@<>'(XXd37269, XXd37270, XXd37271).
'lo.comp.code.unify@matchEls'(_, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@matchEls", 79, 3, 32)).
'lo.comp.code.unify@matchTerm'('lo.comp.term#anon', X_32286, XSrc, XD, XD, XVS, XVS, XLts, XLts, X_32287, XXd37272):- !,
    'lo.comp.code.unify@skipOver'(XSrc, XXd37272).
'lo.comp.code.unify@matchTerm'(XT, XTop, XSrc, XD, XDx, XVS, XVSx, XLt, XLtx, X_32288, XXd37273):- 'lo.comp.term@isGroundTerm'(XT),
    'lo.comp.code.unify@accessLiteral'(XT, XLb, XLt, XLtx),
    !,
    'lo.comp.code.unify@matchLit'(XLb, XTop, XSrc, XVS, XVSx, XXd37273).
'lo.comp.code.unify@matchTerm'('lo.comp.term#varbl'(XNm), X_32289, XSrc, XD, XDx, XVS, XVx, XLt, XLt, Xlast, XXd37275):- ocall('present%3'(XD, XNm, XEntry),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !,
    'lo.comp.code.unify@matchVar'(XEntry, XSrc, XD, XDx, XVS, XVx, Xlast, XXd37275).
'lo.comp.code.unify@matchTerm'('lo.comp.term#cons'(XOp, XEls), XTop, XSrc, XD, XDx, XV, XVx, XLt, XLtx, XLast, XXd37278):- 'lo.comp.code.unify@accessLiteral'(XOp, XLb, XLt, XLt0),
    !,
    'lo.comp.code.unify@matchOp'(XLb, XSrc, XTop, XV, XV0, XXd37276),
    'lo.comp.code.unify@matchEls'(XEls, XD, XDx, XV, XVx, XLt0, XLtx, XXd37277),
    'lo.list@<>'(XXd37276, XXd37277, XXd37278).
'lo.comp.code.unify@matchTerm'(_, _, _, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@matchTerm", 72, 3, 58)).
'lo.comp.code.unify@buildOp'(XLb, 'lo.comp.code.registers#aReg'(XRg), 'lo.core#true', XV, XXb18284, 'lo.core#,..'('lo.comp.code.instructions#iMAcns'(XRg, XLb), 'lo.core#[]')):- !,
    'lo.comp.code.registers@regUsed'(XV, XRg, XXb18284).
'lo.comp.code.unify@buildOp'(XLb, 'lo.comp.code.registers#aReg'(XRg), 'lo.core#false', XV, XV, 'lo.core#,..'('lo.comp.code.instructions#iMAcns'(XRg, XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@buildOp'(XLb, 'lo.comp.code.registers#yReg'(XLc, X_32292), X_32293, XV, XXb18287, 'lo.core#,..'('lo.comp.code.instructions#iMAcns'(0, XLb), 'lo.core#,..'('lo.comp.code.instructions#iMYA'(XLc, 0), 'lo.core#[]'))):- !,
    'lo.comp.code.registers@locUsed'(XV, XLc, XXb18287).
'lo.comp.code.unify@buildOp'(XLb, 'lo.comp.code.registers#sReg', X_32296, XV, XV, 'lo.core#,..'('lo.comp.code.instructions#iMScns'(XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@buildOp'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@buildOp", 146, 3, 60)).
'lo.comp.code.unify@clearTrm'('lo.comp.code.registers#voidAddr', XV, XV, 'lo.core#[]'):- !.
'lo.comp.code.unify@clearTrm'('lo.comp.code.registers#aReg'(XRg), XV, XXb18289, 'lo.core#,..'('lo.comp.code.instructions#iClA'(XRg), 'lo.core#[]')):- !,
    'lo.comp.code.registers@regUsed'(XV, XRg, XXb18289).
'lo.comp.code.unify@clearTrm'('lo.comp.code.registers#yReg'(XLc, X_32299), XV, XXb18291, 'lo.core#,..'('lo.comp.code.instructions#iClY'(XLc), 'lo.core#[]')):- !,
    'lo.comp.code.registers@locUsed'(XV, XLc, XXb18291).
'lo.comp.code.unify@clearTrm'('lo.comp.code.registers#sReg', XV, XV, 'lo.core#,..'(XiCLS, 'lo.core#[]')):- !.
'lo.comp.code.unify@clearTrm'(_, _, _, _):- raise_exception('error'("lo.comp.code.unify@clearTrm", 123, 3, 26)).
'lo.comp.code.unify@clearVr'('lo.comp.code.registers#voidAddr', XSrc, XV, XXb18292, XVx):- 'lo.comp.code.unify@clearTrm'(XSrc, XV, XVx, XXb18292).
'lo.comp.code.unify@clearVr'(XDst, 'lo.comp.code.registers#voidAddr', XV, XXb18293, XVx):- 'lo.comp.code.unify@clearTrm'(XDst, XV, XVx, XXb18293).
'lo.comp.code.unify@clearVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#aReg'(XRg), XV, 'lo.core#,..'('lo.comp.code.instructions#iClA'(XRg), 'lo.core#[]'), XXb18299):- 'lo.comp.code.registers@varSlotSz'(XvarSlotSz18),
    'lo.comp.code.registers@lclGc'(XV, XvarSlotSz18, XXb18298),
    'lo.comp.code.registers@regUsed'(XXb18298, XRg, XXb18299).
'lo.comp.code.unify@clearVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#aReg'(XReg), XV, 'lo.core#,..'('lo.comp.code.instructions#iClAA'(XRg, XReg), 'lo.core#[]'), XXb18306):- 'lo.comp.code.registers@varSlotSz'(XvarSlotSz19),
    'lo.comp.code.registers@lclGc'(XV, XvarSlotSz19, XXb18304),
    'lo.comp.code.registers@regUsed'(XXb18304, XReg, XXb18305),
    'lo.comp.code.registers@regUsed'(XXb18305, XRg, XXb18306).
'lo.comp.code.unify@clearVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#yReg'(XLc, XSafe), XV, 'lo.core#,..'('lo.comp.code.instructions#iClAY'(XRg, XLc), 'lo.core#[]'), XXb18312):- 'lo.comp.code.registers@regUsed'(XV, XRg, XXb18311),
    'lo.comp.code.registers@locUsed'(XXb18311, XLc, XXb18312).
'lo.comp.code.unify@clearVr'('lo.comp.code.registers#yReg'(XLc, X_32305), 'lo.comp.code.registers#aReg'(XRg), XV, 'lo.core#,..'('lo.comp.code.instructions#iClAY'(XRg, XLc), 'lo.core#[]'), XXb18318):- 'lo.comp.code.registers@regUsed'(XV, XRg, XXb18317),
    'lo.comp.code.registers@locUsed'(XXb18317, XLc, XXb18318).
'lo.comp.code.unify@clearVr'('lo.comp.code.registers#yReg'(XLc, X_32307), 'lo.comp.code.registers#yReg'(XLc, X_32308), XV, 'lo.core#,..'('lo.comp.code.instructions#iClY'(XLc), 'lo.core#[]'), XXb18323):- 'lo.comp.code.registers@locUsed'(XV, XLc, XXb18323).
'lo.comp.code.unify@clearVr'('lo.comp.code.registers#yReg'(XLc, X_32310), 'lo.comp.code.registers#yReg'(XLoc, X_32311), XV, 'lo.core#,..'('lo.comp.code.instructions#iClY'(XLc), 'lo.core#,..'('lo.comp.code.instructions#iMAY'(0, XLc), 'lo.core#,..'('lo.comp.code.instructions#iMYA'(XLoc, 0), 'lo.core#[]'))), XXb18333):- 'lo.comp.code.registers@locUsed'(XV, XLoc, XXb18332),
    'lo.comp.code.registers@locUsed'(XXb18332, XLc, XXb18333).
'lo.comp.code.unify@clearVr'('lo.comp.code.registers#sReg', 'lo.comp.code.registers#aReg'(XRg), XV, 'lo.core#,..'('lo.comp.code.instructions#iClSA'(XRg), 'lo.core#[]'), XXb18337):- 'lo.comp.code.registers@regUsed'(XV, XRg, XXb18337).
'lo.comp.code.unify@clearVr'('lo.comp.code.registers#sReg', 'lo.comp.code.registers#yReg'(XLc, X_32316), XV, 'lo.core#,..'('lo.comp.code.instructions#iClSY'(XLc), 'lo.core#[]'), XXb18341):- 'lo.comp.code.registers@locUsed'(XV, XLc, XXb18341).
'lo.comp.code.unify@buildVar'('lo.comp.code.registers#varDesc'(X_32318, X_32319, X_32320, 'lo.comp.code.registers#voidAddr', X_32321), 'lo.comp.code.registers#sReg', XD, XD, XV, XV, X_32322, 'lo.core#,..'('lo.comp.code.instructions#iClS', 'lo.core#[]')):- !.
'lo.comp.code.unify@buildVar'('lo.comp.code.registers#varDesc'(X_32324, X_32325, X_32326, 'lo.comp.code.registers#voidAddr', X_32327), X_32328, XD, XD, XV, XV, X_32329, 'lo.core#[]'):- !.
'lo.comp.code.unify@buildVar'('lo.comp.code.registers#varDesc'(X_32330, 'lo.comp.code.registers#inited', X_32331, XSrc, X_32332), XDst, XD, XD, XV, XVx, XLast, XIns):- 'lo.comp.code.unify@moveVr'(XDst, XSrc, XV, XLast, XIns, XVx),
    !.
'lo.comp.code.unify@buildVar'('lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#notInited', XDef, XSrc, XLst), XDst, XD, XXe4705, XV, XVx, XLast, XIns):- 'lo.comp.code.unify@clearVr'(XDst, XSrc, XV, XIns, XVx),
    !,
    ocall('_put%1'(XXV5043),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XD, XNm, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#inited', XDef, XSrc, XLst), XXe4705),XXV5043,XXV5043).
'lo.comp.code.unify@buildVar'(_, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@buildVar", 134, 3, 60)).
'lo.comp.code.unify@buildLit'(X_32333, X_32334, 'lo.comp.code.registers#voidAddr', XVs, XVs, 'lo.core#[]'):- !.
'lo.comp.code.unify@buildLit'(XLb, X_32335, 'lo.comp.code.registers#aReg'(XRg), XVS, XXb18347, 'lo.core#,..'('lo.comp.code.instructions#iMAlit'(XRg, XLb), 'lo.core#[]')):- !,
    'lo.comp.code.registers@regUsed'(XVS, XRg, XXb18347).
'lo.comp.code.unify@buildLit'(XLb, X_32337, 'lo.comp.code.registers#yReg'(XLc, X_32338), XVS, XXb18349, 'lo.core#,..'('lo.comp.code.instructions#iMAlit'(0, XLb), 'lo.core#,..'('lo.comp.code.instructions#iMYA'(XLc, 0), 'lo.core#[]'))):- !,
    'lo.comp.code.registers@locUsed'(XVS, XLc, XXb18349).
'lo.comp.code.unify@buildLit'(XLb, X_32341, 'lo.comp.code.registers#sReg', XVS, XVS, 'lo.core#,..'('lo.comp.code.instructions#iMSlit'(XLb), 'lo.core#[]')):- !.
'lo.comp.code.unify@buildLit'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@buildLit", 140, 3, 34)).
'lo.comp.code.unify@buildEls'('lo.core#[]', X_32343, XD, XD, XV, XV, XLt, XLt, 'lo.core#[]'):- !.
'lo.comp.code.unify@buildEls'('lo.core#,..'(Xe, 'lo.core#[]'), XTop, XD, XDx, XV, XVx, XLt, XLtx, XXd37305):- !,
    'lo.comp.code.unify@buildTerm'(Xe, XTop, 'lo.comp.code.registers#sReg', XD, XDx, XV, XVx, XLt, XLtx, 'lo.core#true', XXd37305).
'lo.comp.code.unify@buildEls'('lo.core#,..'(Xe, Xl), XTop, XD, XDx, XV, XVx, XLt, XLtx, XXd37308):- !,
    'lo.comp.code.unify@buildTerm'(Xe, XTop, 'lo.comp.code.registers#sReg', XD, XD0, XV, XV0, XLt, XLt0, 'lo.core#false', XXd37306),
    'lo.comp.code.unify@buildEls'(Xl, XTop, XD0, XDx, XV0, XVx, XLt0, XLtx, XXd37307),
    'lo.list@<>'(XXd37306, XXd37307, XXd37308).
'lo.comp.code.unify@buildEls'(_, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@buildEls", 129, 3, 34)).
'lo.comp.code.unify@buildTerm'('lo.comp.term#anon', X_32346, XDst, XD, XD, XV, XVx, XLts, XLts, X_32347, XXd37309):- !,
    'lo.comp.code.unify@clearTrm'(XDst, XV, XVx, XXd37309).
'lo.comp.code.unify@buildTerm'(XT, XTop, XDst, XD, XD, XV, XVx, XLt, XLtx, X_32348, XXd37310):- 'lo.comp.term@isGroundTerm'(XT),
    'lo.comp.code.unify@accessLiteral'(XT, XLb, XLt, XLtx),
    !,
    'lo.comp.code.unify@buildLit'(XLb, XTop, XDst, XV, XVx, XXd37310).
'lo.comp.code.unify@buildTerm'('lo.comp.term#varbl'(XNm), X_32349, XDst, XD, XDx, XV, XVx, XLt, XLt, Xlast, XXd37312):- ocall('present%3'(XD, XNm, XEntry),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !,
    'lo.comp.code.unify@buildVar'(XEntry, XDst, XD, XDx, XV, XVx, Xlast, XXd37312).
'lo.comp.code.unify@buildTerm'('lo.comp.term#cons'(XOp, XEls), XTop, XDst, XD, XDx, XV, XVx, XLt, XLtx, XLast, XXd37316):- 'lo.comp.code.unify@accessLiteral'(XOp, XLb, XLt, XLt0),
    ocall('size%1'(XXV5044),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5045),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XEls, XXe4706),XXV5044,XXV5044),
    ocall('_call%3'(XXe4706, 2, XXe4707),XXV5045,XXV5045),
    'lo.comp.code.registers@lclGc'(XV, XXe4707, XXd37313),
    XV0 = XXd37313,
    !,
    'lo.comp.code.unify@buildOp'(XLb, XDst, XTop, XV0, XV1, XXd37314),
    'lo.comp.code.unify@buildEls'(XEls, 'lo.core#false', XD, XDx, XV1, XVx, XLt0, XLtx, XXd37315),
    'lo.list@<>'(XXd37314, XXd37315, XXd37316).
'lo.comp.code.unify@buildTerm'(_, _, _, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.unify@buildTerm", 115, 3, 62)).
'lo.comp.code.unify@buildArgs'('lo.core#[]', X_32350, X_32351, XD, XD, XV, X_32352, XV, XLt, XLt, 'lo.core#[]').
'lo.comp.code.unify@buildArgs'('lo.core#,..'(Xe, Xl), XRg, XGp, XD, XDx, XV, XLast, XVx, XLt, XLtx, XCode):- 'lo.comp.code.unify@buildTerm'(Xe, 'lo.core#true', 'lo.comp.code.registers#aReg'(XRg), XD, XD0, XV, XV0, XLt, XLt0, XLast, XXd37318),
    XACode = XXd37318,
    ocall('+%1'(XXV5046),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XRg, 1, XXe4708),XXV5046,XXV5046),
    'lo.comp.code.unify@buildArgs'(Xl, XXe4708, XGp, XD0, XDx, XV0, XLast, XVx, XLt0, XLtx, XBCode),
    'lo.list@<>'(XACode, XBCode, XXd37319),
    XCode = XXd37319.
'lo.comp.code.unify@callArgs'(XArgs, XGp, XD, XLt, XVs, XLast, XCode, XDx, XLtx, XVsx):- 'lo.comp.code.unify@buildArgs'(XArgs, 1, XGp, XD, XDx, XVs, XLast, XVsx, XLt, XLtx, XCode).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#voidAddr', X_32354, XUsed, X_32355, 'lo.core#[]', XUsed).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#aReg'(XRg), XUsed, X_32356, 'lo.core#[]', XUsed).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#aReg'(XReg), XUsed, X_32357, 'lo.core#,..'('lo.comp.code.instructions#iMAA'(XRg, XReg), 'lo.core#[]'), XXb18361):- 'lo.comp.code.registers@regUsed'(XUsed, XRg, XXb18361).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#yReg'(XLc, XSafe), XUsed, XLast, 'lo.core#,..'('lo.comp.code.instructions#iMAY'(XRg, XLc), 'lo.core#[]'), XXb18366):- 'lo.comp.code.unify@or175'(XLast, XSafe),
    'lo.comp.code.registers@regUsed'(XUsed, XRg, XXb18366).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#yReg'(XLc, X_32360), XUsed, X_32361, 'lo.core#,..'('lo.comp.code.instructions#iMuAY'(XRg, XLc), 'lo.core#[]'), XXb18372):- 'lo.comp.code.registers@varSlotSz'(XvarSlotSz20),
    'lo.comp.code.registers@lclGc'(XUsed, XvarSlotSz20, XXb18371),
    'lo.comp.code.registers@regUsed'(XXb18371, XRg, XXb18372).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#aReg'(XRg), 'lo.comp.code.registers#sReg', XUsed, X_32363, 'lo.core#,..'('lo.comp.code.instructions#iMAS'(XRg), 'lo.core#[]'), XXb18376):- 'lo.comp.code.registers@regUsed'(XUsed, XRg, XXb18376).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#yReg'(XLc, X_32365), 'lo.comp.code.registers#aReg'(XRg), XUsed, X_32366, 'lo.core#,..'('lo.comp.code.instructions#iOYA'(XLc, XRg), 'lo.core#[]'), XXb18381):- 'lo.comp.code.registers@locUsed'(XUsed, XLc, XXb18381).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#yReg'(XLc, X_32368), 'lo.comp.code.registers#yReg'(XLc, X_32369), XUsed, X_32370, 'lo.core#[]', XUsed).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#yReg'(XLc, X_32371), 'lo.comp.code.registers#yReg'(XLoc, X_32372), XUsed, X_32373, 'lo.core#,..'('lo.comp.code.instructions#iMAY'(0, XLc), 'lo.core#,..'('lo.comp.code.instructions#iOYA'(XLoc, 0), 'lo.core#[]')), XXb18390):- 'lo.comp.code.registers@locUsed'(XUsed, XLc, XXb18390).
'lo.comp.code.unify@overVr'('lo.comp.code.registers#yReg'(XLc, X_32376), 'lo.comp.code.registers#sReg', XUsed, X_32377, 'lo.core#,..'('lo.comp.code.instructions#iMYS'(XLc), 'lo.core#[]'), XXb18394):- 'lo.comp.code.registers@locUsed'(XUsed, XLc, XXb18394).
'lo.comp.code.unify^unifyOp'('_call%6'(XV29362, XV29363, XV29364, XV29365, XV29366, XV29367), 'lo.comp.code.unify^unifyOp', _):- 'lo.comp.code.unify@unifyOp'(XV29362, XV29363, XV29364, XV29365, XV29366, XV29367).
'lo.comp.code.unify@or174'(XLast, XSafe):- XSafe = 'lo.core#true'.
'lo.comp.code.unify@or174'(XLast, XSafe):- XLast = 'lo.core#true'.
'lo.comp.code.unify^moveVr'('_call%6'(XV29368, XV29369, XV29370, XV29371, XV29372, XV29373), 'lo.comp.code.unify^moveVr', _):- 'lo.comp.code.unify@moveVr'(XV29368, XV29369, XV29370, XV29371, XV29372, XV29373).
'lo.comp.code.unify@neg316'(XRg, XReg):- XReg = XRg,
    !,
    fail.
'lo.comp.code.unify@neg316'(XRg, XReg).
'lo.comp.code.unify^unifyVr'('_call%5'(XV29374, XV29375, XV29376, XV29377, XV29378), 'lo.comp.code.unify^unifyVr', _):- 'lo.comp.code.unify@unifyVr'(XV29374, XV29375, XV29376, XV29377, XV29378).
'lo.comp.code.unify^unifyVar'('_call%8'(XV29379, XV29380, XV29381, XV29382, XV29383, XV29384, XV29385, XV29386), 'lo.comp.code.unify^unifyVar', _):- 'lo.comp.code.unify@unifyVar'(XV29379, XV29380, XV29381, XV29382, XV29383, XV29384, XV29385, XV29386).
'lo.comp.code.unify^unifyLit'('_call%6'(XV29387, XV29388, XV29389, XV29390, XV29391, XV29392), 'lo.comp.code.unify^unifyLit', _):- 'lo.comp.code.unify@unifyLit'(XV29387, XV29388, XV29389, XV29390, XV29391, XV29392).
'lo.comp.code.unify^accessLiteral'('_call%4'(XV29393, XV29394, XV29395, XV29396), 'lo.comp.code.unify^accessLiteral', _):- 'lo.comp.code.unify@accessLiteral'(XV29393, XV29394, XV29395, XV29396).
'lo.comp.code.unify^skipOver'('_call%2'(XV29397, XV29398), 'lo.comp.code.unify^skipOver', _):- 'lo.comp.code.unify@skipOver'(XV29397, XV29398).
'lo.comp.code.unify^unifyEls'('_call%9'(XV29399, XV29400, XV29401, XV29402, XV29403, XV29404, XV29405, XV29406, XV29407), 'lo.comp.code.unify^unifyEls', _):- 'lo.comp.code.unify@unifyEls'(XV29399, XV29400, XV29401, XV29402, XV29403, XV29404, XV29405, XV29406, XV29407).
'lo.comp.code.unify@cond384'(XXd37239, XV0, XV1, XXd37238, XRg, XSrc, XTop):- XTop = 'lo.core#true',
    XSrc = 'lo.comp.code.registers#aReg'(XRg),
    !,
    'lo.comp.code.registers@freeAReg'(XV0, XRg, XXd37239),
    XV1 = XXd37239.
'lo.comp.code.unify@cond384'(XXd37239, XV0, XV1, XXd37238, XRg, XSrc, XTop):- XV1 = XV0.
'lo.comp.code.unify^unifyTerm'('_call%11'(XV29408, XV29409, XV29410, XV29411, XV29412, XV29413, XV29414, XV29415, XV29416, XV29417, XV29418), 'lo.comp.code.unify^unifyTerm', _):- 'lo.comp.code.unify@unifyTerm'(XV29408, XV29409, XV29410, XV29411, XV29412, XV29413, XV29414, XV29415, XV29416, XV29417, XV29418).
'lo.comp.code.unify^unifyArgs'('_call%10'(XV29419, XV29420, XV29421, XV29422, XV29423, XV29424, XV29425, XV29426, XV29427, XV29428), 'lo.comp.code.unify^unifyArgs', _):- 'lo.comp.code.unify@unifyArgs'(XV29419, XV29420, XV29421, XV29422, XV29423, XV29424, XV29425, XV29426, XV29427, XV29428).
'lo.comp.code.unify^unifyHead'('_call%9'(XV29429, XV29430, XV29431, XV29432, XV29433, XV29434, XV29435, XV29436, XV29437), 'lo.comp.code.unify^unifyHead', _):- 'lo.comp.code.unify@unifyHead'(XV29429, XV29430, XV29431, XV29432, XV29433, XV29434, XV29435, XV29436, XV29437).
'lo.comp.code.unify^matchOp'('_call%6'(XV29438, XV29439, XV29440, XV29441, XV29442, XV29443), 'lo.comp.code.unify^matchOp', _):- 'lo.comp.code.unify@matchOp'(XV29438, XV29439, XV29440, XV29441, XV29442, XV29443).
'lo.comp.code.unify^matchVr'('_call%5'(XV29444, XV29445, XV29446, XV29447, XV29448), 'lo.comp.code.unify^matchVr', _):- 'lo.comp.code.unify@matchVr'(XV29444, XV29445, XV29446, XV29447, XV29448).
'lo.comp.code.unify^matchVar'('_call%8'(XV29449, XV29450, XV29451, XV29452, XV29453, XV29454, XV29455, XV29456), 'lo.comp.code.unify^matchVar', _):- 'lo.comp.code.unify@matchVar'(XV29449, XV29450, XV29451, XV29452, XV29453, XV29454, XV29455, XV29456).
'lo.comp.code.unify^matchLit'('_call%6'(XV29457, XV29458, XV29459, XV29460, XV29461, XV29462), 'lo.comp.code.unify^matchLit', _):- 'lo.comp.code.unify@matchLit'(XV29457, XV29458, XV29459, XV29460, XV29461, XV29462).
'lo.comp.code.unify^matchEls'('_call%8'(XV29463, XV29464, XV29465, XV29466, XV29467, XV29468, XV29469, XV29470), 'lo.comp.code.unify^matchEls', _):- 'lo.comp.code.unify@matchEls'(XV29463, XV29464, XV29465, XV29466, XV29467, XV29468, XV29469, XV29470).
'lo.comp.code.unify^matchTerm'('_call%11'(XV29471, XV29472, XV29473, XV29474, XV29475, XV29476, XV29477, XV29478, XV29479, XV29480, XV29481), 'lo.comp.code.unify^matchTerm', _):- 'lo.comp.code.unify@matchTerm'(XV29471, XV29472, XV29473, XV29474, XV29475, XV29476, XV29477, XV29478, XV29479, XV29480, XV29481).
'lo.comp.code.unify^buildOp'('_call%6'(XV29482, XV29483, XV29484, XV29485, XV29486, XV29487), 'lo.comp.code.unify^buildOp', _):- 'lo.comp.code.unify@buildOp'(XV29482, XV29483, XV29484, XV29485, XV29486, XV29487).
'lo.comp.code.unify^clearTrm'('_call%4'(XV29488, XV29489, XV29490, XV29491), 'lo.comp.code.unify^clearTrm', _):- 'lo.comp.code.unify@clearTrm'(XV29488, XV29489, XV29490, XV29491).
'lo.comp.code.unify^clearVr'('_call%5'(XV29492, XV29493, XV29494, XV29495, XV29496), 'lo.comp.code.unify^clearVr', _):- 'lo.comp.code.unify@clearVr'(XV29492, XV29493, XV29494, XV29495, XV29496).
'lo.comp.code.unify^buildVar'('_call%8'(XV29497, XV29498, XV29499, XV29500, XV29501, XV29502, XV29503, XV29504), 'lo.comp.code.unify^buildVar', _):- 'lo.comp.code.unify@buildVar'(XV29497, XV29498, XV29499, XV29500, XV29501, XV29502, XV29503, XV29504).
'lo.comp.code.unify^buildLit'('_call%6'(XV29505, XV29506, XV29507, XV29508, XV29509, XV29510), 'lo.comp.code.unify^buildLit', _):- 'lo.comp.code.unify@buildLit'(XV29505, XV29506, XV29507, XV29508, XV29509, XV29510).
'lo.comp.code.unify^buildEls'('_call%9'(XV29511, XV29512, XV29513, XV29514, XV29515, XV29516, XV29517, XV29518, XV29519), 'lo.comp.code.unify^buildEls', _):- 'lo.comp.code.unify@buildEls'(XV29511, XV29512, XV29513, XV29514, XV29515, XV29516, XV29517, XV29518, XV29519).
'lo.comp.code.unify^buildTerm'('_call%11'(XV29520, XV29521, XV29522, XV29523, XV29524, XV29525, XV29526, XV29527, XV29528, XV29529, XV29530), 'lo.comp.code.unify^buildTerm', _):- 'lo.comp.code.unify@buildTerm'(XV29520, XV29521, XV29522, XV29523, XV29524, XV29525, XV29526, XV29527, XV29528, XV29529, XV29530).
'lo.comp.code.unify^buildArgs'('_call%11'(XV29531, XV29532, XV29533, XV29534, XV29535, XV29536, XV29537, XV29538, XV29539, XV29540, XV29541), 'lo.comp.code.unify^buildArgs', _):- 'lo.comp.code.unify@buildArgs'(XV29531, XV29532, XV29533, XV29534, XV29535, XV29536, XV29537, XV29538, XV29539, XV29540, XV29541).
'lo.comp.code.unify^callArgs'('_call%10'(XV29542, XV29543, XV29544, XV29545, XV29546, XV29547, XV29548, XV29549, XV29550, XV29551), 'lo.comp.code.unify^callArgs', _):- 'lo.comp.code.unify@callArgs'(XV29542, XV29543, XV29544, XV29545, XV29546, XV29547, XV29548, XV29549, XV29550, XV29551).
'lo.comp.code.unify@or175'(XLast, XSafe):- XSafe = 'lo.core#true'.
'lo.comp.code.unify@or175'(XLast, XSafe):- XLast = 'lo.core#true'.
'lo.comp.code.unify^overVr'('_call%6'(XV29552, XV29553, XV29554, XV29555, XV29556, XV29557), 'lo.comp.code.unify^overVr', _):- 'lo.comp.code.unify@overVr'(XV29552, XV29553, XV29554, XV29555, XV29556, XV29557).
