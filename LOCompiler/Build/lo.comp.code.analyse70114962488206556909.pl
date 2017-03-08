'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.analyse's'0.0.1'n6o6'()6'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.sort'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.registers'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.escapes'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'s\"I2'partitionGoals'FT2Lt'lo.comp.term*pred'Lt'lo.comp.term*pred'LLt'lo.comp.term*pred''varAnalysis'FT3Lt'lo.comp.term*term'Lt'lo.comp.term*term'LLt'lo.comp.term*pred'T3Uz2'lo.index*map'2St'lo.comp.code.registers*varDesc'it'lo.comp.code.registers*varSet'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.code.analyse@init'():- !.
'lo.comp.code.analyse@isSafeGoal'('lo.comp.term#neck').
'lo.comp.code.analyse@isSafeGoal'('lo.comp.term#fail').
'lo.comp.code.analyse@isSafeGoal'('lo.comp.term#unfy'(X_31911, X_31912, X_31913)).
'lo.comp.code.analyse@partitionGoals'('lo.core#[]', 'lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.code.analyse@partitionGoals'('lo.core#[]', XP, 'lo.core#,..'(XXd36990, 'lo.core#[]')):- !,
    'lo.list@reverse'(XP, XXd36990).
'lo.comp.code.analyse@partitionGoals'('lo.core#,..'(XG, XRest), XP, XXd36993):- 'lo.comp.code.analyse@isSafeGoal'(XG),
    !,
    'lo.comp.code.analyse@partitionGoals'(XRest, 'lo.core#,..'(XG, XP), XXd36993).
'lo.comp.code.analyse@partitionGoals'('lo.core#,..'(XG, XRest), XP, 'lo.core#,..'(XXd36995, XXd36996)):- !,
    'lo.list@reverse'('lo.core#,..'(XG, XP), XXd36995),
    'lo.comp.code.analyse@partitionGoals'(XRest, 'lo.core#[]', XXd36996).
'lo.comp.code.analyse@partitionGoals'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@partitionGoals", 12, 3, 27)).
'lo.comp.code.analyse#hArg'('hArg%1'('lo.comp.code.analyse@hArg')):- !.
'lo.comp.code.analyse#hTerm'('hTerm%1'('lo.comp.code.analyse@hTerm')):- !.
'lo.comp.code.analyse#lArg'('lArg%1'('lo.comp.code.analyse@lArg')):- !.
'lo.comp.code.analyse#lTerm'('lTerm%1'('lo.comp.code.analyse@lTerm')):- !.
'lo.comp.code.analyse#oArg'('oArg%1'('lo.comp.code.analyse@oArg')):- !.
'lo.comp.code.analyse#oTerm'('oTerm%1'('lo.comp.code.analyse@oTerm')):- !.
'lo.comp.code.analyse#occ'('occ%1'('lo.comp.code.analyse@occ'())):- !.
'lo.comp.code.analyse@maxArity'('lo.core#[]', 0):- !.
'lo.comp.code.analyse@maxArity'('lo.core#,..'(XC, XL), XXd37000):- !,
    'lo.comp.term@predArity'(XC, XXd36998),
    'lo.comp.code.analyse@maxArity'(XL, XXd36999),
    'lo.core@max'('lo.core$comp$lo.core*integer', XXd36998, XXd36999, XXd37000).
'lo.comp.code.analyse@maxArity'(_, _):- raise_exception('error'("lo.comp.code.analyse@maxArity", 139, 3, 17)).
'lo.comp.code.analyse@maxHeadArity'('lo.core#[]', 0):- !.
'lo.comp.code.analyse@maxHeadArity'('lo.core#,..'(XP, X_31924), XXd37001):- !,
    'lo.comp.code.analyse@maxArity'(XP, XXd37001).
'lo.comp.code.analyse@maxHeadArity'(_, _):- raise_exception('error'("lo.comp.code.analyse@maxHeadArity", 143, 3, 21)).
'lo.comp.code.analyse@compPerm'('()4'(X_31925, 'lo.core#some'('()2'(XG1, XI1)), X_31926, X_31927), '()4'(X_31928, 'lo.core#some'('()2'(XG2, XI2)), X_31929, X_31930)):- 'lo.comp.code.analyse@or167'(XI1, XI2, XG1, XG2).
'lo.comp.code.analyse#usage'('usage%1'('lo.comp.code.analyse@usage'())):- !.
'lo.comp.code.analyse@definedInRegister'('lo.core#[]', 'lo.comp.code.registers#notDefined'):- !.
'lo.comp.code.analyse@definedInRegister'('lo.core#,..'('()3'('lo.comp.code.analyse#hArg', X_31937, X_31938), X_31939), 'lo.comp.code.registers#isDefined'):- !.
'lo.comp.code.analyse@definedInRegister'('lo.core#,..'('()3'('lo.comp.code.analyse#lArg', X_31941, X_31942), X_31943), 'lo.comp.code.registers#isDefined'):- !.
'lo.comp.code.analyse@definedInRegister'('lo.core#,..'(X_31945, XO), XXd37002):- !,
    'lo.comp.code.analyse@definedInRegister'(XO, XXd37002).
'lo.comp.code.analyse@definedInRegister'(_, _):- raise_exception('error'("lo.comp.code.analyse@definedInRegister", 247, 3, 35)).
'lo.comp.code.analyse@lastRgUsage'('lo.core#[]', -1, X_31946, 'lo.core#none'):- !.
'lo.comp.code.analyse@lastRgUsage'('lo.core#[]', XGp, XIx, 'lo.core#some'('()2'(XGp, XIx))):- !.
'lo.comp.code.analyse@lastRgUsage'('lo.core#,..'('()3'(X_31948, XGp, XIx), XU), XG, XI, XXd37004):- 'lo.comp.code.analyse@or168'(XI, XIx, XG, XGp),
    !,
    'lo.comp.code.analyse@lastRgUsage'(XU, XGp, XIx, XXd37004).
'lo.comp.code.analyse@lastRgUsage'('lo.core#,..'(X_31950, XU), XG, XI, XXd37005):- !,
    'lo.comp.code.analyse@lastRgUsage'(XU, XG, XI, XXd37005).
'lo.comp.code.analyse@lastRgUsage'(_, _, _, _):- raise_exception('error'("lo.comp.code.analyse@lastRgUsage", 232, 3, 28)).
'lo.comp.code.analyse@lastOcc'(XO, XXd37006):- !,
    'lo.comp.code.analyse@lastRgUsage'(XO, -1, -1, XXd37006).
'lo.comp.code.analyse@lastOcc'(_, _):- raise_exception('error'("lo.comp.code.analyse@lastOcc", 229, 3, 34)).
'lo.comp.code.analyse@findPerms'('lo.core#[]', XPermNo, XPermNo, XD, XD, 'lo.core#[]'):- !.
'lo.comp.code.analyse@findPerms'('lo.core#,..'('lo.comp.code.analyse#usage'(XNm, 'lo.comp.code.analyse#occ'(X_31952, XOccs), 'lo.comp.code.registers#yReg'(X_31953, XSfe), XUse, X_31954), XTble), XNoP, XNoPx, XD, XDx, 'lo.core#,..'('()4'(XNm, XXd37007, 'lo.comp.code.registers#yReg'(0, XSfe), XXd37009), XXd37010)):- !,
    ocall('+%1'(XXV4971),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.analyse@lastOcc'(XOccs, XXd37007),
    'lo.comp.code.analyse@definedInRegister'(XOccs, XXd37009),
    ocall('_call%3'(XNoP, 1, XXe4628),XXV4971,XXV4971),
    'lo.comp.code.analyse@findPerms'(XTble, XXe4628, XNoPx, XD, XDx, XXd37010).
'lo.comp.code.analyse@findPerms'('lo.core#,..'('lo.comp.code.analyse#usage'(XNm, 'lo.comp.code.analyse#occ'(X_31957, XOccs), 'lo.comp.code.registers#voidAddr', X_31958, X_31959), XTble), XNoP, XNoPx, XD, XDx, XXd37015):- !,
    ocall('_put%1'(XXV4972),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.analyse@definedInRegister'(XOccs, XXd37012),
    ocall('_call%4'(XD, XNm, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#inited', XXd37012, 'lo.comp.code.registers#voidAddr', 'lo.core#none'), XXe4629),XXV4972,XXV4972),
    'lo.comp.code.analyse@findPerms'(XTble, XNoP, XNoPx, XXe4629, XDx, XXd37015).
'lo.comp.code.analyse@findPerms'('lo.core#,..'(X_31961, XTble), XNoP, XNoPx, XD, XDx, XXd37016):- !,
    'lo.comp.code.analyse@findPerms'(XTble, XNoP, XNoPx, XD, XDx, XXd37016).
'lo.comp.code.analyse@findPerms'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@findPerms", 239, 3, 37)).
'lo.comp.code.analyse@allocPerms'('lo.core#[]', XM, X_31962, XLcls, XLcls, XM):- !.
'lo.comp.code.analyse@allocPerms'('lo.core#,..'('()4'(XNm, XU, 'lo.comp.code.registers#yReg'(X_31964, XSfe), XD), XT), XM, XvNo, XLc0, XLcx, XXd37021):- !,
    ocall('_put%1'(XXV4973),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('-%1'(XXV4974),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('addMem%1'(XXV4975),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%4'(XM, XNm, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#notInited', XD, 'lo.comp.code.registers#yReg'(XvNo, XSfe), XU), XXe4630),XXV4973,XXV4973),
    ocall('_call%3'(XvNo, 1, XXe4631),XXV4974,XXV4974),
    ocall('_call%3'(XLc0, XvNo, XXe4632),XXV4975,XXV4975),
    'lo.comp.code.analyse@allocPerms'(XT, XXe4630, XXe4631, XXe4632, XLcx, XXd37021).
'lo.comp.code.analyse@allocPerms'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@allocPerms", 257, 3, 33)).
'lo.comp.code.analyse@allocateHeadRegs'('lo.core#[]', XUsedRgs, XFreeRgs, XDict, XUsedRgs, XFreeRgs, XDict).
'lo.comp.code.analyse@allocateHeadRegs'('lo.core#,..'('lo.comp.code.analyse#usage'(XNm, 'lo.comp.code.analyse#occ'(X_31966, XOccs), 'lo.comp.code.registers#aReg'(X_31967), XUse, XAvoid), XTble), XUr, XFr, XD, XUx, XFx, XDx):- 'lo.comp.code.analyse@cond382'(XXe4635, XXV4978, XFr, XF0, XXe4634, XXV4977, XUr, XU0, XXe4633, XXV4976, XXd37026, XXd37025, XXd37024, XXd37023, XNm, XD, XD0, XXd37022, XAvoid, XOccs, XRg, X_31968),
    'lo.comp.code.analyse@allocateHeadRegs'(XTble, XU0, XF0, XD0, XUx, XFx, XDx).
'lo.comp.code.analyse@allocateHeadRegs'('lo.core#,..'(XU, XL), XUr, XFr, XD, XUx, XFx, XDx):- 'lo.comp.code.analyse@allocateHeadRegs'(XL, XUr, XFr, XD, XUx, XFx, XDx).
'lo.comp.code.analyse@findOccupiedRegs'('lo.core#[]', XUs, XUs, XFr, XFr).
'lo.comp.code.analyse@findOccupiedRegs'('lo.core#,..'('lo.comp.code.analyse#usage'(X_31971, 'lo.comp.code.analyse#occ'(X_31972, XOccs), X_31973, X_31974, X_31975), XUsage), XUs0, XUsx, XFr, XFrx):- 'lo.comp.code.analyse@cond383'(XFrx, XXe4637, XXV4980, XUsx, XXe4636, XXV4979, XUs0, XUsage, XFr, XOccs, XRg, X_31976).
'lo.comp.code.analyse@allocateRegs'('lo.core#[]', XUsedRgs, XFreeRgs, XDict, XUsedRgs, XFreeRgs, XDict).
'lo.comp.code.analyse@allocateRegs'('lo.core#,..'('lo.comp.code.analyse#usage'(XNm, 'lo.comp.code.analyse#occ'(X_31978, XOccs), 'lo.comp.code.registers#aReg'(X_31979), XUse, XConflict), XTble), XUr, XFr, XD, XUx, XFx, XDx):- ocall('present%3'(XD, XNm, X_31980),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.analyse@allocateRegs'(XTble, XUr, XFr, XD, XUx, XFx, XDx).
'lo.comp.code.analyse@allocateRegs'('lo.core#,..'('lo.comp.code.analyse#usage'(XNm, 'lo.comp.code.analyse#occ'(X_31982, XOccs), 'lo.comp.code.registers#aReg'(X_31983), XUse, XConflict), XTble), XUr, XFr, XD, XUx, XFx, XDx):- ocall('in%2'('()3'('lo.comp.code.analyse#lArg', X_31984, XRg), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('in%2'(XRg, XFr),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.code.analyse@neg304'(XXd37028, XConflict, XRg),
    ocall('addMem%1'(XXV4981),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('delMem%1'(XXV4982),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('_put%1'(XXV4983),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XUr, XRg, XXe4638),XXV4981,XXV4981),
    ocall('_call%3'(XFr, XRg, XXe4639),XXV4982,XXV4982),
    'lo.comp.code.analyse@lastOcc'(XOccs, XXd37030),
    ocall('_call%4'(XD, XNm, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#notInited', 'lo.comp.code.registers#isDefined', 'lo.comp.code.registers#aReg'(XRg), XXd37030), XXe4640),XXV4983,XXV4983),
    'lo.comp.code.analyse@allocateRegs'(XTble, XXe4638, XXe4639, XXe4640, XUx, XFx, XDx).
'lo.comp.code.analyse@allocateRegs'('lo.core#,..'('lo.comp.code.analyse#usage'(XNm, 'lo.comp.code.analyse#occ'(X_31986, XOccs), 'lo.comp.code.registers#aReg'(X_31987), XUse, XConflict), XTble), XUr, XFr, XD, XUx, XFx, XDx):- ocall('in%2'(XRg, XFr),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.code.analyse@neg305'(XXd37033, XConflict, XRg),
    ocall('addMem%1'(XXV4984),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('delMem%1'(XXV4985),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('_put%1'(XXV4986),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XUr, XRg, XXe4641),XXV4984,XXV4984),
    ocall('_call%3'(XFr, XRg, XXe4642),XXV4985,XXV4985),
    'lo.comp.code.analyse@lastOcc'(XOccs, XXd37035),
    ocall('_call%4'(XD, XNm, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#notInited', 'lo.comp.code.registers#isDefined', 'lo.comp.code.registers#aReg'(XRg), XXd37035), XXe4643),XXV4986,XXV4986),
    'lo.comp.code.analyse@allocateRegs'(XTble, XXe4641, XXe4642, XXe4643, XUx, XFx, XDx).
'lo.comp.code.analyse@allocateRegs'('lo.core#,..'(XU, XL), XUr, XFr, XD, XUx, XFx, XDx):- 'lo.comp.code.analyse@allocateRegs'(XL, XUr, XFr, XD, XUx, XFx, XDx).
'lo.comp.code.analyse@allocateVars'(XUsage, XHeadArity, '()3'(XXd37043, XNumPerms, 'lo.comp.code.registers#varSet'(XXe4645, XLcls, 0))):- 'lo.comp.code.registers@numRegisters'(XnumRegisters11),
    'lo.list@iota'(1, XnumRegisters11, XXd37038),
    ocall('_empty%1'(XXV4987),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.analyse@allocateHeadRegs'(XUsage, 'lo.core#[]', XXd37038, XXV4987, XUs0, XFr0, XD0),
    'lo.comp.code.analyse@findOccupiedRegs'(XUsage, XUs0, XUs1, XFr0, XFr1),
    'lo.comp.code.analyse@allocateRegs'(XUsage, XUs1, XFr1, XD0, XUs2, X_31989, XD1),
    !,
    'lo.sets@nullSet'(XnullSet41),
    ocall('_coerce%1'(XXV4988),'lo.coerce$coercion$lo.core*list$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.coerce$coercion$lo.core*list$lo.sets*set'('lo.core$equality$lo.core*integer')),
    'lo.comp.code.analyse@findPerms'(XUsage, 0, XNumPerms, XD1, XD, XXd37040),
    'lo.sort@sort'(XXd37040, 'lo.comp.code.analyse^compPerm', XXd37041),
    ocall('_call%2'('lo.core$equality$lo.core*integer', XXe4644),XnullSet41,XnullSet41),
    'lo.comp.code.analyse@allocPerms'(XXd37041, XD1, XNumPerms, XXe4644, XLcls, XXd37043),
    ocall('_call%2'(XUs2, XXe4645),XXV4988,XXV4988).
'lo.comp.code.analyse@allocateVars'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@allocateVars", 173, 3, 320)).
'lo.comp.code.analyse@occMap'('lo.core#[]', XM, XM):- !.
'lo.comp.code.analyse@occMap'('lo.core#,..'('lo.comp.term#varbl'(XNm), XQ), XM, XXd37048):- !,
    ocall('_put%1'(XXV4989),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XM, XNm, 'lo.comp.code.analyse#occ'(XNm, 'lo.core#[]'), XXe4646),XXV4989,XXV4989),
    'lo.comp.code.analyse@occMap'(XQ, XXe4646, XXd37048).
'lo.comp.code.analyse@occMap'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@occMap", 39, 3, 17)).
'lo.comp.code.analyse@updateOccurrence'(XNm, XM, XKey, XGp, XIx, XXe4647):- ocall('present%3'(XM, XNm, 'lo.comp.code.analyse#occ'(XNm, XOccs)),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !,
    ocall('_put%1'(XXV4990),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XM, XNm, 'lo.comp.code.analyse#occ'(XNm, 'lo.core#,..'('()3'(XKey, XGp, XIx), XOccs)), XXe4647),XXV4990,XXV4990).
'lo.comp.code.analyse@updateOccurrence'(XNm, XM, XKey, XGp, XIx, XXe4648):- !,
    ocall('_put%1'(XXV4991),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XM, XNm, 'lo.comp.code.analyse#occ'(XNm, 'lo.core#,..'('()3'(XKey, XGp, XIx), 'lo.core#[]')), XXe4648),XXV4991,XXV4991).
'lo.comp.code.analyse@updateOccurrence'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@updateOccurrence", 78, 3, 101)).
'lo.comp.code.analyse@findOccsInTerms'('lo.core#[]', XM, X_31993, X_31994, X_31995, X_31996, XM):- !.
'lo.comp.code.analyse@findOccsInTerms'('lo.core#,..'(Xt, Xl), XM, XGp, XIx, XKey, XiKey, XXd37058):- !,
    'lo.comp.code.analyse@findOccsInTerm'(Xt, XM, XGp, XIx, XKey, XiKey, XXd37057),
    'lo.comp.code.analyse@findOccsInTerms'(Xl, XXd37057, XGp, XIx, XKey, XiKey, XXd37058).
'lo.comp.code.analyse@findOccsInTerms'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@findOccsInTerms", 74, 3, 34)).
'lo.comp.code.analyse@findOccsInTerm'('lo.comp.term#varbl'(XNm), XM, XGp, XIx, XKey, XiKey, XXd37059):- 'lo.comp.code.analyse@neg306'(XNm),
    !,
    'lo.comp.code.analyse@updateOccurrence'(XNm, XM, XKey, XGp, XIx, XXd37059).
'lo.comp.code.analyse@findOccsInTerm'('lo.comp.term#anon', XM, XGp, XIx, XKey, X_31998, XM):- !.
'lo.comp.code.analyse@findOccsInTerm'(XT, XM, XGp, XIx, XKey, X_31999, XXd37060):- 'lo.comp.term@isGroundTerm'(XT),
    !,
    'lo.comp.code.analyse@updateOccurrence'("", XM, XKey, XGp, XIx, XXd37060).
'lo.comp.code.analyse@findOccsInTerm'('lo.comp.term#cons'(X_32000, XEls), XM, XGp, XIx, X_32001, XiKey, XXd37061):- !,
    'lo.comp.code.analyse@findOccsInTerms'(XEls, XM, XGp, XIx, XiKey, XiKey, XXd37061).
'lo.comp.code.analyse@findOccsInTerm'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@findOccsInTerm", 68, 3, 96)).
'lo.comp.code.analyse@findOccsInArgs'('lo.core#[]', XM, X_32002, X_32003, X_32004, X_32005, XM):- !.
'lo.comp.code.analyse@findOccsInArgs'('lo.core#,..'(Xt, Xl), XM, XGp, XIx, XKey, XiKey, XXd37063):- !,
    ocall('+%1'(XXV4992),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.analyse@findOccsInTerm'(Xt, XM, XGp, XIx, XKey, XiKey, XXd37062),
    ocall('_call%3'(XIx, 1, XXe4649),XXV4992,XXV4992),
    'lo.comp.code.analyse@findOccsInArgs'(Xl, XXd37062, XGp, XXe4649, XKey, XiKey, XXd37063).
'lo.comp.code.analyse@findOccsInArgs'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@findOccsInArgs", 47, 3, 33)).
'lo.comp.code.analyse@findOccsInHead'(XArgs, XM, XXd37064):- !,
    'lo.comp.code.analyse@findOccsInArgs'(XArgs, XM, 1, 1, 'lo.comp.code.analyse#hArg', 'lo.comp.code.analyse#hTerm', XXd37064).
'lo.comp.code.analyse@findOccsInHead'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@findOccsInHead", 44, 3, 63)).
'lo.comp.code.analyse@findOccsInGoal'('lo.comp.term#call'(X_32007, XOp, XArgs), XM, XGp, XXd37065):- !,
    'lo.comp.code.analyse@findOccsInArgs'(XArgs, XM, XGp, 1, 'lo.comp.code.analyse#lArg', 'lo.comp.code.analyse#lTerm', XXd37065).
'lo.comp.code.analyse@findOccsInGoal'('lo.comp.term#ecall'(X_32008, XOp, XArgs), XM, XGp, XXd37066):- !,
    'lo.comp.code.analyse@findOccsInArgs'(XArgs, XM, XGp, 1, 'lo.comp.code.analyse#lArg', 'lo.comp.code.analyse#lTerm', XXd37066).
'lo.comp.code.analyse@findOccsInGoal'('lo.comp.term#ocall'(X_32009, XC, XL, XT), XM, XGp, XXd37070):- !,
    'lo.comp.code.analyse@findOccsInArgs'('lo.core#,..'(XC, 'lo.core#,..'(XL, 'lo.core#,..'(XT, 'lo.core#[]'))), XM, XGp, 1, 'lo.comp.code.analyse#lArg', 'lo.comp.code.analyse#lTerm', XXd37070).
'lo.comp.code.analyse@findOccsInGoal'('lo.comp.term#unfy'(X_32013, XL, XR), XM, XGp, XXd37072):- !,
    'lo.comp.code.analyse@findOccsInTerm'(XL, XM, XGp, 1, 'lo.comp.code.analyse#oArg', 'lo.comp.code.analyse#oTerm', XXd37071),
    'lo.comp.code.analyse@findOccsInTerm'(XR, XXd37071, XGp, 2, 'lo.comp.code.analyse#oArg', 'lo.comp.code.analyse#oTerm', XXd37072).
'lo.comp.code.analyse@findOccsInGoal'('lo.comp.term#except'(X_32014, XT), XM, XGp, XXd37073):- !,
    'lo.comp.code.analyse@findOccsInTerm'(XT, XM, XGp, 1, 'lo.comp.code.analyse#lArg', 'lo.comp.code.analyse#lTerm', XXd37073).
'lo.comp.code.analyse@findOccsInGoal'('lo.comp.term#neck', XM, X_32015, XM):- !.
'lo.comp.code.analyse@findOccsInGoal'('lo.comp.term#fail', XM, X_32016, XM):- !.
'lo.comp.code.analyse@findOccsInGoal'(_, _, _, _):- raise_exception('error'("lo.comp.code.analyse@findOccsInGoal", 59, 3, 78)).
'lo.comp.code.analyse@findOccsInPart'('lo.core#[]', XM, XGp, XM):- !.
'lo.comp.code.analyse@findOccsInPart'('lo.core#,..'(XG, Xl), XM, XGp, XXd37075):- !,
    'lo.comp.code.analyse@findOccsInGoal'(XG, XM, XGp, XXd37074),
    'lo.comp.code.analyse@findOccsInPart'(Xl, XXd37074, XGp, XXd37075).
'lo.comp.code.analyse@findOccsInPart'(_, _, _, _):- raise_exception('error'("lo.comp.code.analyse@findOccsInPart", 55, 3, 28)).
'lo.comp.code.analyse@findOccsInParts'('lo.core#,..'(XP1, XP), XM, XGno, XXd37077):- !,
    ocall('+%1'(XXV4993),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.analyse@findOccsInPart'(XP1, XM, XGno, XXd37076),
    ocall('_call%3'(XGno, 1, XXe4650),XXV4993,XXV4993),
    'lo.comp.code.analyse@findOccsInParts'(XP, XXd37076, XXe4650, XXd37077).
'lo.comp.code.analyse@findOccsInParts'('lo.core#[]', XM, X_32019, XM):- !.
'lo.comp.code.analyse@findOccsInParts'(_, _, _, _):- raise_exception('error'("lo.comp.code.analyse@findOccsInParts", 51, 3, 84)).
'lo.comp.code.analyse@occurranceTable'(XQ, XArgs, XParts, XXd37081):- !,
    ocall('_empty%1'(XXV4994),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.analyse@occMap'(XQ, XXV4994, XXd37079),
    'lo.comp.code.analyse@findOccsInHead'(XArgs, XXd37079, XXd37080),
    'lo.comp.code.analyse@findOccsInParts'(XParts, XXd37080, 1, XXd37081).
'lo.comp.code.analyse@occurranceTable'(_, _, _, _):- raise_exception('error'("lo.comp.code.analyse@occurranceTable", 36, 3, 91)).
'lo.comp.code.analyse@classifyVr'('lo.core#[]', 'lo.comp.code.registers#voidAddr', 'lo.core#none').
'lo.comp.code.analyse@classifyVr'('lo.core#,..'(X_32021, 'lo.core#[]'), 'lo.comp.code.registers#voidAddr', 'lo.core#none').
'lo.comp.code.analyse@classifyVr'('lo.core#,..'('()3'(X_32023, XGp, X_32024), XRest), 'lo.comp.code.registers#aReg'(-1), 'lo.core#some'(XGp)):- 'lo.comp.code.analyse@forallA15'(XGp, XRest, X_32026, XG, X_32025).
'lo.comp.code.analyse@classifyVr'(XOccs, 'lo.comp.code.registers#yReg'(-1, 'lo.core#true'), 'lo.core#none'):- 'lo.comp.code.analyse@one273'(X_32034, X_32033, X_32032, X_32031, X_32030, X_32029, XOccs, X_32028, X_32027).
'lo.comp.code.analyse@classifyVr'(X_32035, 'lo.comp.code.registers#yReg'(-1, 'lo.core#false'), 'lo.core#none').
'lo.comp.code.analyse@argRegUsed'(XRegs, '()3'('lo.comp.code.analyse#hArg', X_32036, XIx), XXe4651):- !,
    ocall('addMem%1'(XXV4995),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XRegs, XIx, XXe4651),XXV4995,XXV4995).
'lo.comp.code.analyse@argRegUsed'(XRegs, '()3'('lo.comp.code.analyse#lArg', X_32037, XIx), XXe4652):- !,
    ocall('addMem%1'(XXV4996),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XRegs, XIx, XXe4652),XXV4996,XXV4996).
'lo.comp.code.analyse@argRegUsed'(XRegs, X_32038, XRegs):- !.
'lo.comp.code.analyse@argRegUsed'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@argRegUsed", 150, 3, 47)).
'lo.comp.code.analyse@registersOccupied'(XOccs, XXe4654):- !,
    'lo.sets@nullSet'(XnullSet42),
    ocall('foldLeft%1'(XXV4997),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%2'('lo.core$equality$lo.core*integer', XXe4653),XnullSet42,XnullSet42),
    ocall('_call%4'('lo.comp.code.analyse^argRegUsed', XXe4653, XOccs, XXe4654),XXV4997,XXV4997).
'lo.comp.code.analyse@registersOccupied'(_, _):- raise_exception('error'("lo.comp.code.analyse@registersOccupied", 147, 3, 60)).
'lo.comp.code.analyse@checkOccForArg'('lo.comp.code.analyse#hArg', XGp, XGp, XRg, XU, XA, XXe4655):- !,
    ocall('addMem%1'(XXV4998),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XA, XRg, XXe4655),XXV4998,XXV4998).
'lo.comp.code.analyse@checkOccForArg'('lo.comp.code.analyse#oTerm', XGp, XGp, XRg, XU, XA, XXe4656):- !,
    ocall('addMem%1'(XXV4999),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XA, XRg, XXe4656),XXV4999,XXV4999).
'lo.comp.code.analyse@checkOccForArg'('lo.comp.code.analyse#hTerm', XGp, XGp, XRg, XU, XA, XXe4657):- 'lo.comp.code.analyse@neg307'(XXd37087, XU, XRg),
    !,
    ocall('addMem%1'(XXV5000),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XA, XRg, XXe4657),XXV5000,XXV5000).
'lo.comp.code.analyse@checkOccForArg'('lo.comp.code.analyse#lTerm', XGp, XGp, XRg, XU, XA, XXe4658):- !,
    ocall('addMem%1'(XXV5001),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XA, XRg, XXe4658),XXV5001,XXV5001).
'lo.comp.code.analyse@checkOccForArg'('lo.comp.code.analyse#lArg', XGp, XGp, XRg, XU, XA, XXe4659):- 'lo.comp.code.analyse@neg308'(XXd37090, XU, XRg),
    !,
    ocall('addMem%1'(XXV5002),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XA, XRg, XXe4659),XXV5002,XXV5002).
'lo.comp.code.analyse@checkOccForArg'('lo.comp.code.analyse#oArg', XGp, XGp, XRg, XU, XA, XXe4660):- 'lo.comp.code.analyse@neg309'(XXd37092, XU, XRg),
    !,
    ocall('addMem%1'(XXV5003),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XA, XRg, XXe4660),XXV5003,XXV5003).
'lo.comp.code.analyse@checkOccForArg'(X_32039, X_32040, X_32041, X_32042, X_32043, XA, XA):- !.
'lo.comp.code.analyse@checkOccForArg'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@checkOccForArg", 102, 3, 49)).
'lo.comp.code.analyse@checkOtherOccs'(XUse, XRGp, XOccs, XAvoid, XXe4661):- !,
    ocall('foldLeft%1'(XXV5004),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'('lo.comp.code.analyse@fun112'(XUse, XRGp), XAvoid, XOccs, XXe4661),XXV5004,XXV5004).
'lo.comp.code.analyse@checkOtherOccs'(_, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@checkOtherOccs", 99, 3, 115)).
'lo.comp.code.analyse@checkOtherRegVars'(XNm, XGp, XU, XO, XXe4663):- !,
    'lo.sets@nullSet'(XnullSet43),
    ocall('foldLeft%1'(XXV5005),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%2'('lo.core$equality$lo.core*integer', XXe4662),XnullSet43,XnullSet43),
    ocall('_call%4'('lo.comp.code.analyse@fun113'(XGp, XU, XNm), XXe4662, XO, XXe4663),XXV5005,XXV5005).
'lo.comp.code.analyse@checkOtherRegVars'(_, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@checkOtherRegVars", 96, 3, 105)).
'lo.comp.code.analyse@checkArgPos'(XGp, XRg, XOccs, XCS, XXe4664):- 'lo.comp.code.analyse@neg310'(XOccs, XRg, XGp),
    'lo.comp.code.analyse@neg311'(XOccs, XRg, XGp),
    !,
    ocall('addMem%1'(XXV5006),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XCS, XRg, XXe4664),XXV5006,XXV5006).
'lo.comp.code.analyse@checkArgPos'(XGp, XRg, XOccs, XCS, XXe4665):- 'lo.comp.code.analyse@or172'(XOccs, XRg, XGp),
    !,
    ocall('addMem%1'(XXV5007),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XCS, XRg, XXe4665),XXV5007,XXV5007).
'lo.comp.code.analyse@checkArgPos'(X_32044, X_32045, X_32046, XCS, XCS):- !.
'lo.comp.code.analyse@checkArgPos'(_, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@checkArgPos", 115, 3, 93)).
'lo.comp.code.analyse@conflictSet'('lo.comp.code.analyse#occ'(XNm, XOccs), XGp, XArity, XXe4667):- 'lo.comp.code.analyse@or173'(X_32048, XOccs, X_32047, XGp),
    !,
    'lo.sets@nullSet'(XnullSet44),
    ocall('foldLeft%1'(XXV5008),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%2'('lo.core$equality$lo.core*integer', XXe4666),XnullSet44,XnullSet44),
    'lo.list@iota'(1, XArity, XXd37101),
    ocall('_call%4'('lo.comp.code.analyse@fun114'(XOccs, XGp), XXe4666, XXd37101, XXe4667),XXV5008,XXV5008).
'lo.comp.code.analyse@conflictSet'('lo.comp.code.analyse#occ'(XNm, XOccs), XGp, XArity, XXe4668):- !,
    'lo.sets@nullSet'(XnullSet45),
    ocall('_call%2'('lo.core$equality$lo.core*integer', XXe4668),XnullSet45,XnullSet45).
'lo.comp.code.analyse@conflictSet'(_, _, _, _):- raise_exception('error'("lo.comp.code.analyse@conflictSet", 111, 3, 152)).
'lo.comp.code.analyse@checkHeadUse'('lo.comp.code.analyse#hTerm', XGp, XGp, XRg, XCs, XUse, XXe4669):- 'lo.comp.code.analyse@neg312'(XUse, XRg, XGp),
    !,
    ocall('addMem%1'(XXV5009),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XCs, XRg, XXe4669),XXV5009,XXV5009).
'lo.comp.code.analyse@checkHeadUse'('lo.comp.code.analyse#hArg', XGp, XGp, XRg, XCs, XUse, XXe4670):- 'lo.comp.code.analyse@neg313'(XUse, XRg, XGp, X_32049),
    !,
    ocall('addMem%1'(XXV5010),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XCs, XRg, XXe4670),XXV5010,XXV5010).
'lo.comp.code.analyse@checkHeadUse'(X_32050, X_32051, X_32052, X_32053, XCs, X_32054, XCs):- !.
'lo.comp.code.analyse@checkHeadUse'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@checkHeadUse", 161, 3, 78)).
'lo.comp.code.analyse@checkHeadUses'('lo.comp.code.analyse#occ'(X_32055, XOccs), XGp, XC, XUse, XXe4671):- !,
    ocall('foldLeft%1'(XXV5011),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'('lo.comp.code.analyse@fun115'(XUse, XGp), XC, XOccs, XXe4671),XXV5011,XXV5011).
'lo.comp.code.analyse@checkHeadUses'(_, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@checkHeadUses", 158, 3, 109)).
'lo.comp.code.analyse@otherUsers'(XOM, XGp, XUse, XXe4673):- !,
    'lo.sets@nullSet'(XnullSet46),
    ocall('foldLeft%1'(XXV5012),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%2'('lo.core$equality$lo.core*integer', XXe4672),XnullSet46,XnullSet46),
    ocall('_call%4'('lo.comp.code.analyse@fun116'(XUse, XGp), XXe4672, XOM, XXe4673),XXV5012,XXV5012).
'lo.comp.code.analyse@otherUsers'(_, _, _, _):- raise_exception('error'("lo.comp.code.analyse@otherUsers", 155, 3, 80)).
'lo.comp.code.analyse@buildRgUsage'('lo.comp.code.analyse#occ'("", X_32056), X_32057, X_32058, X_32059, XOut, XOut):- !.
'lo.comp.code.analyse@buildRgUsage'('lo.comp.code.analyse#occ'(XNm, XOccs), XT, XParts, XArity, XOut, 'lo.core#,..'('lo.comp.code.analyse#usage'(XNm, 'lo.comp.code.analyse#occ'(XNm, XOccs), 'lo.comp.code.registers#aReg'(-1), XUse, XXe4677), XOut)):- 'lo.comp.code.analyse@neg314'(XNm),
    'lo.comp.code.analyse@classifyVr'(XOccs, 'lo.comp.code.registers#aReg'(X_32060), 'lo.core#some'(XG)),
    'lo.comp.code.analyse@registersOccupied'(XOccs, XXd37110),
    XUse = XXd37110,
    'lo.comp.code.analyse@checkOtherRegVars'(XNm, XG, XUse, XT, XXd37111),
    XAvoid = XXd37111,
    'lo.comp.code.analyse@condExp120'(XCndV120, XXd37113, XXd37112, XArity, XGPart, XXe4674, XXV5013, XG, XParts),
    XAr = XCndV120,
    'lo.comp.code.analyse@conflictSet'('lo.comp.code.analyse#occ'(XNm, XOccs), XG, XAr, XXd37115),
    XCS = XXd37115,
    ocall('^/%1'(XXV5014),'lo.collection$filter$lo.core*list','lo.collection$filter$lo.core*list'),
    ocall('_call%3'(XT, 'lo.comp.code.analyse@pred9'(XNm, XG), XXe4675),XXV5014,XXV5014),
    'lo.comp.code.analyse@otherUsers'(XXe4675, XG, XOccs, XXd37116),
    XLater = XXd37116,
    !,
    ocall('+%1'(XXV5015),'lo.core$additive$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.core$additive$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('+%1'(XXV5016),'lo.core$additive$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.core$additive$lo.sets*set'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XAvoid, XCS, XXe4676),XXV5015,XXV5015),
    ocall('_call%3'(XXe4676, XLater, XXe4677),XXV5016,XXV5016).
'lo.comp.code.analyse@buildRgUsage'('lo.comp.code.analyse#occ'(XNm, XOccs), XT, XParts, XArity, XOut, 'lo.core#,..'('lo.comp.code.analyse#usage'(XNm, 'lo.comp.code.analyse#occ'(XNm, XOccs), XAddr, XXe4678, XXe4679), XOut)):- 'lo.comp.code.analyse@classifyVr'(XOccs, XAddr, X_32064),
    !,
    'lo.sets@nullSet'(XnullSet47),
    'lo.sets@nullSet'(XnullSet48),
    ocall('_call%2'('lo.core$equality$lo.core*integer', XXe4678),XnullSet47,XnullSet47),
    ocall('_call%2'('lo.core$equality$lo.core*integer', XXe4679),XnullSet48,XnullSet48).
'lo.comp.code.analyse@buildRgUsage'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@buildRgUsage", 120, 3, 40)).
'lo.comp.code.analyse@buildRegUsage'('lo.core#[]', X_32066, X_32067, X_32068, XU, XU):- !.
'lo.comp.code.analyse@buildRegUsage'('lo.core#,..'(XO, XL), XT, XParts, XArity, XOut, XXd37129):- !,
    'lo.comp.code.analyse@buildRgUsage'(XO, XT, XParts, XArity, XOut, XXd37128),
    'lo.comp.code.analyse@buildRegUsage'(XL, XT, XParts, XArity, XXd37128, XXd37129).
'lo.comp.code.analyse@buildRegUsage'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.code.analyse@buildRegUsage", 134, 3, 30)).
'lo.comp.code.analyse@usageTable'(XOccs, XArity, XParts, XXd37130):- !,
    'lo.comp.code.analyse@buildRegUsage'(XOccs, XOccs, XParts, XArity, 'lo.core#[]', XXd37130).
'lo.comp.code.analyse@usageTable'(_, _, _, _):- raise_exception('error'("lo.comp.code.analyse@usageTable", 93, 3, 71)).
'lo.comp.code.analyse@varAnalysis'(XQ, XArgs, XParts, XXd37135):- ocall('values%1'(XXV5017),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.analyse@occurranceTable'(XQ, XArgs, XParts, XXd37131),
    ocall('_call%2'(XXd37131, XXe4680),XXV5017,XXV5017),
    XOccTble = XXe4680,
    ocall('size%1'(XXV5018),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe4681),XXV5018,XXV5018),
    'lo.comp.code.analyse@usageTable'(XOccTble, XXe4681, XParts, XXd37133),
    XUsage = XXd37133,
    !,
    'lo.comp.code.analyse@maxHeadArity'(XParts, XXd37134),
    'lo.comp.code.analyse@allocateVars'(XUsage, XXd37134, XXd37135).
'lo.comp.code.analyse@varAnalysis'(_, _, _, _):- raise_exception('error'("lo.comp.code.analyse@varAnalysis", 30, 3, 173)).
'lo.comp.code.analyse@dispUsage'('lo.comp.code.analyse#usage'(XNm, XOccs, XAddr, XIn, XConflict), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("usage for "), 'lo.core#,..'('lo.core#ss'(XNm), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXe4682, 'lo.core#,..'('lo.core#ss'(", inset="), 'lo.core#,..'(XXe4683, 'lo.core#,..'('lo.core#ss'(", conflict set="), 'lo.core#,..'(XXe4684, 'lo.core#[]')))))))))):- !,
    ocall('disp%1'(XXV5019),'lo.core$display$lo.comp.code.registers*addr','lo.core$display$lo.comp.code.registers*addr'),
    ocall('disp%1'(XXV5020),'lo.core$display$lo.sets*set'('lo.core$equality$lo.core*integer', 'lo.core$display$lo.core*integer'),'lo.core$display$lo.sets*set'('lo.core$equality$lo.core*integer', 'lo.core$display$lo.core*integer')),
    ocall('disp%1'(XXV5021),'lo.core$display$lo.sets*set'('lo.core$equality$lo.core*integer', 'lo.core$display$lo.core*integer'),'lo.core$display$lo.sets*set'('lo.core$equality$lo.core*integer', 'lo.core$display$lo.core*integer')),
    ocall('_call%2'(XAddr, XXe4682),XXV5019,XXV5019),
    ocall('_call%2'(XIn, XXe4683),XXV5020,XXV5020),
    ocall('_call%2'(XConflict, XXe4684),XXV5021,XXV5021).
'lo.comp.code.analyse@dispUsage'(_, _):- raise_exception('error'("lo.comp.code.analyse@dispUsage", 88, 3, 164)).
'lo.core$display$lo.comp.code.analyse*usageMap'('lo.core$display$lo.comp.code.analyse*usageMap%1'('lo.core$display$lo.comp.code.analyse*usageMap')):- !.
'lo.core$display$lo.comp.code.analyse*usageMap'('disp%2'(XV29330, XV29331), XLbl2115, XThis2115):- !,
    'lo.core$display$lo.comp.code.analyse*usageMap@disp'(XV29330, XV29331, XLbl2115, XThis2115).
'lo.core$display$lo.comp.code.analyse*usageMap'('disp%1'('lo.core$display$lo.comp.code.analyse*usageMap^disp'(XLbl2116, XThis2116)), XLbl2116, XThis2116).
'lo.core$display$lo.comp.code.analyse*usageMap@disp'(XU, XXd37152, XLbV2397, XThV2397):- !,
    'lo.comp.code.analyse@dispUsage'(XU, XXd37152).
'lo.core$display$lo.comp.code.analyse*usageMap@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.code.analyse*usageMap@disp", 84, 5, 23)).
'lo.comp.code.analyse@avoidIota'(XFr, 0, XFr):- !.
'lo.comp.code.analyse@avoidIota'(XFr, XIx, XXd37153):- !,
    ocall('delMem%1'(XXV5022),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('-%1'(XXV5023),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XFr, XIx, XXe4685),XXV5022,XXV5022),
    ocall('_call%3'(XIx, 1, XXe4686),XXV5023,XXV5023),
    'lo.comp.code.analyse@avoidIota'(XXe4685, XXe4686, XXd37153).
'lo.comp.code.analyse@avoidIota'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@avoidIota", 198, 3, 21)).
'lo.comp.code.analyse@compPos'('lo.core#some'('()2'(XG1, X_32078)), 'lo.core#some'('()2'(XG2, X_32079))):- ocall('<%2'(XG1, XG2),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer').
'lo.comp.code.analyse@compPos'('lo.core#some'('()2'(XG, XX1)), 'lo.core#some'('()2'(XG, XX2))):- ocall('<%2'(XX1, XX2),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer').
'lo.comp.code.analyse^isSafeGoal'('_call%1'(XV29155), 'lo.comp.code.analyse^isSafeGoal', _):- 'lo.comp.code.analyse@isSafeGoal'(XV29155).
'lo.comp.code.analyse^partitionGoals'('_call%3'(XV29156, XV29157, XV29158), 'lo.comp.code.analyse^partitionGoals', _):- 'lo.comp.code.analyse@partitionGoals'(XV29156, XV29157, XV29158).
'lo.comp.code.analyse^maxArity'('_call%2'(XV29159, XV29160), 'lo.comp.code.analyse^maxArity', _):- 'lo.comp.code.analyse@maxArity'(XV29159, XV29160).
'lo.comp.code.analyse^maxHeadArity'('_call%2'(XV29161, XV29162), 'lo.comp.code.analyse^maxHeadArity', _):- 'lo.comp.code.analyse@maxHeadArity'(XV29161, XV29162).
'lo.comp.code.analyse@or167'(XI1, XI2, XG1, XG2):- ocall('<%2'(XG2, XG1),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer').
'lo.comp.code.analyse@or167'(XI1, XI2, XG1, XG2):- XG2 = XG1,
    ocall('<%2'(XI2, XI1),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer').
'lo.comp.code.analyse^compPerm'('_call%2'(XV29163, XV29164), 'lo.comp.code.analyse^compPerm', _):- 'lo.comp.code.analyse@compPerm'(XV29163, XV29164).
'lo.comp.code.analyse^definedInRegister'('_call%2'(XV29165, XV29166), 'lo.comp.code.analyse^definedInRegister', _):- 'lo.comp.code.analyse@definedInRegister'(XV29165, XV29166).
'lo.comp.code.analyse@or168'(XI, XIx, XG, XGp):- 'lo.core@>'('lo.core$comp$lo.core*integer', XGp, XG).
'lo.comp.code.analyse@or168'(XI, XIx, XG, XGp):- XGp = XG,
    'lo.core@>'('lo.core$comp$lo.core*integer', XIx, XI).
'lo.comp.code.analyse^lastRgUsage'('_call%4'(XV29167, XV29168, XV29169, XV29170), 'lo.comp.code.analyse^lastRgUsage', _):- 'lo.comp.code.analyse@lastRgUsage'(XV29167, XV29168, XV29169, XV29170).
'lo.comp.code.analyse^lastOcc'('_call%2'(XV29171, XV29172), 'lo.comp.code.analyse^lastOcc', _):- 'lo.comp.code.analyse@lastOcc'(XV29171, XV29172).
'lo.comp.code.analyse^findPerms'('_call%6'(XV29173, XV29174, XV29175, XV29176, XV29177, XV29178), 'lo.comp.code.analyse^findPerms', _):- 'lo.comp.code.analyse@findPerms'(XV29173, XV29174, XV29175, XV29176, XV29177, XV29178).
'lo.comp.code.analyse^allocPerms'('_call%6'(XV29179, XV29180, XV29181, XV29182, XV29183, XV29184), 'lo.comp.code.analyse^allocPerms', _):- 'lo.comp.code.analyse@allocPerms'(XV29179, XV29180, XV29181, XV29182, XV29183, XV29184).
'lo.comp.code.analyse@neg303'(XXd37022, XAvoid, XRg):- ocall('in%2'(XRg, XAvoid),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.analyse@neg303'(XXd37022, XAvoid, XRg).
'lo.comp.code.analyse@cond381'(XXe4635, XXV4978, XFr, XF0, XXe4634, XXV4977, XUr, XU0, XXe4633, XXV4976, XXd37026, XXd37025, XXd37024, XOccs, XXd37023, XNm, XD, XD0, XXd37022, XAvoid, XRg):- 'lo.comp.code.analyse@neg303'(XXd37022, XAvoid, XRg),
    !,
    ocall('_put%1'(XXV4976),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.analyse@lastOcc'(XOccs, XXd37024),
    ocall('_call%4'(XD, XNm, 'lo.comp.code.registers#varDesc'(XNm, 'lo.comp.code.registers#inited', 'lo.comp.code.registers#isDefined', 'lo.comp.code.registers#aReg'(XRg), XXd37024), XXe4633),XXV4976,XXV4976),
    XD0 = XXe4633,
    ocall('addMem%1'(XXV4977),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('_call%3'(XUr, XRg, XXe4634),XXV4977,XXV4977),
    XU0 = XXe4634,
    ocall('delMem%1'(XXV4978),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('_call%3'(XFr, XRg, XXe4635),XXV4978,XXV4978),
    XF0 = XXe4635.
'lo.comp.code.analyse@cond381'(XXe4635, XXV4978, XFr, XF0, XXe4634, XXV4977, XUr, XU0, XXe4633, XXV4976, XXd37026, XXd37025, XXd37024, XOccs, XXd37023, XNm, XD, XD0, XXd37022, XAvoid, XRg):- XD0 = XD,
    XU0 = XUr,
    XF0 = XFr.
'lo.comp.code.analyse@cond382'(XXe4635, XXV4978, XFr, XF0, XXe4634, XXV4977, XUr, XU0, XXe4633, XXV4976, XXd37026, XXd37025, XXd37024, XXd37023, XNm, XD, XD0, XXd37022, XAvoid, XOccs, XRg, X_31968):- ocall('in%2'('()3'('lo.comp.code.analyse#hArg', X_31968, XRg), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    'lo.comp.code.analyse@cond381'(XXe4635, XXV4978, XFr, XF0, XXe4634, XXV4977, XUr, XU0, XXe4633, XXV4976, XXd37026, XXd37025, XXd37024, XOccs, XXd37023, XNm, XD, XD0, XXd37022, XAvoid, XRg).
'lo.comp.code.analyse@cond382'(XXe4635, XXV4978, XFr, XF0, XXe4634, XXV4977, XUr, XU0, XXe4633, XXV4976, XXd37026, XXd37025, XXd37024, XXd37023, XNm, XD, XD0, XXd37022, XAvoid, XOccs, XRg, X_31968):- XD0 = XD,
    XF0 = XFr,
    XU0 = XUr.
'lo.comp.code.analyse^allocateHeadRegs'('_call%7'(XV29185, XV29186, XV29187, XV29188, XV29189, XV29190, XV29191), 'lo.comp.code.analyse^allocateHeadRegs', _):- 'lo.comp.code.analyse@allocateHeadRegs'(XV29185, XV29186, XV29187, XV29188, XV29189, XV29190, XV29191).
'lo.comp.code.analyse@cond383'(XFrx, XXe4637, XXV4980, XUsx, XXe4636, XXV4979, XUs0, XUsage, XFr, XOccs, XRg, X_31976):- ocall('in%2'('()3'('lo.comp.code.analyse#hArg', X_31976, XRg), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('in%2'(XRg, XFr),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    ocall('addMem%1'(XXV4979),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('delMem%1'(XXV4980),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    ocall('_call%3'(XUs0, XRg, XXe4636),XXV4979,XXV4979),
    ocall('_call%3'(XFr, XRg, XXe4637),XXV4980,XXV4980),
    'lo.comp.code.analyse@findOccupiedRegs'(XUsage, XXe4636, XUsx, XXe4637, XFrx).
'lo.comp.code.analyse@cond383'(XFrx, XXe4637, XXV4980, XUsx, XXe4636, XXV4979, XUs0, XUsage, XFr, XOccs, XRg, X_31976):- 'lo.comp.code.analyse@findOccupiedRegs'(XUsage, XUs0, XUsx, XFr, XFrx).
'lo.comp.code.analyse^findOccupiedRegs'('_call%5'(XV29192, XV29193, XV29194, XV29195, XV29196), 'lo.comp.code.analyse^findOccupiedRegs', _):- 'lo.comp.code.analyse@findOccupiedRegs'(XV29192, XV29193, XV29194, XV29195, XV29196).
'lo.comp.code.analyse@neg304'(XXd37028, XConflict, XRg):- ocall('in%2'(XRg, XConflict),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.analyse@neg304'(XXd37028, XConflict, XRg).
'lo.comp.code.analyse@neg305'(XXd37033, XConflict, XRg):- ocall('in%2'(XRg, XConflict),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.analyse@neg305'(XXd37033, XConflict, XRg).
'lo.comp.code.analyse^allocateRegs'('_call%7'(XV29197, XV29198, XV29199, XV29200, XV29201, XV29202, XV29203), 'lo.comp.code.analyse^allocateRegs', _):- 'lo.comp.code.analyse@allocateRegs'(XV29197, XV29198, XV29199, XV29200, XV29201, XV29202, XV29203).
'lo.comp.code.analyse^allocateVars'('_call%3'(XV29204, XV29205, XV29206), 'lo.comp.code.analyse^allocateVars', _):- 'lo.comp.code.analyse@allocateVars'(XV29204, XV29205, XV29206).
'lo.comp.code.analyse^occMap'('_call%3'(XV29207, XV29208, XV29209), 'lo.comp.code.analyse^occMap', _):- 'lo.comp.code.analyse@occMap'(XV29207, XV29208, XV29209).
'lo.comp.code.analyse^updateOccurrence'('_call%6'(XV29210, XV29211, XV29212, XV29213, XV29214, XV29215), 'lo.comp.code.analyse^updateOccurrence', _):- 'lo.comp.code.analyse@updateOccurrence'(XV29210, XV29211, XV29212, XV29213, XV29214, XV29215).
'lo.comp.code.analyse^findOccsInTerms'('_call%7'(XV29216, XV29217, XV29218, XV29219, XV29220, XV29221, XV29222), 'lo.comp.code.analyse^findOccsInTerms', _):- 'lo.comp.code.analyse@findOccsInTerms'(XV29216, XV29217, XV29218, XV29219, XV29220, XV29221, XV29222).
'lo.comp.code.analyse@neg306'(XNm):- 'lo.comp.escapes@isEscape'(XNm),
    !,
    fail.
'lo.comp.code.analyse@neg306'(XNm).
'lo.comp.code.analyse^findOccsInTerm'('_call%7'(XV29223, XV29224, XV29225, XV29226, XV29227, XV29228, XV29229), 'lo.comp.code.analyse^findOccsInTerm', _):- 'lo.comp.code.analyse@findOccsInTerm'(XV29223, XV29224, XV29225, XV29226, XV29227, XV29228, XV29229).
'lo.comp.code.analyse^findOccsInArgs'('_call%7'(XV29230, XV29231, XV29232, XV29233, XV29234, XV29235, XV29236), 'lo.comp.code.analyse^findOccsInArgs', _):- 'lo.comp.code.analyse@findOccsInArgs'(XV29230, XV29231, XV29232, XV29233, XV29234, XV29235, XV29236).
'lo.comp.code.analyse^findOccsInHead'('_call%3'(XV29237, XV29238, XV29239), 'lo.comp.code.analyse^findOccsInHead', _):- 'lo.comp.code.analyse@findOccsInHead'(XV29237, XV29238, XV29239).
'lo.comp.code.analyse^findOccsInGoal'('_call%4'(XV29240, XV29241, XV29242, XV29243), 'lo.comp.code.analyse^findOccsInGoal', _):- 'lo.comp.code.analyse@findOccsInGoal'(XV29240, XV29241, XV29242, XV29243).
'lo.comp.code.analyse^findOccsInPart'('_call%4'(XV29244, XV29245, XV29246, XV29247), 'lo.comp.code.analyse^findOccsInPart', _):- 'lo.comp.code.analyse@findOccsInPart'(XV29244, XV29245, XV29246, XV29247).
'lo.comp.code.analyse^findOccsInParts'('_call%4'(XV29248, XV29249, XV29250, XV29251), 'lo.comp.code.analyse^findOccsInParts', _):- 'lo.comp.code.analyse@findOccsInParts'(XV29248, XV29249, XV29250, XV29251).
'lo.comp.code.analyse^occurranceTable'('_call%4'(XV29252, XV29253, XV29254, XV29255), 'lo.comp.code.analyse^occurranceTable', _):- 'lo.comp.code.analyse@occurranceTable'(XV29252, XV29253, XV29254, XV29255).
'lo.comp.code.analyse@forallA15'(XGp, XRest, X_32026, XG, X_32025):- ocall('in%2'('()3'(X_32025, XG, X_32026), XRest),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.code.analyse@forallB15'(XGp, XRest, X_32026, XG, X_32025),
    !,
    fail.
'lo.comp.code.analyse@forallA15'(XGp, XRest, X_32026, XG, X_32025).
'lo.comp.code.analyse@forallB15'(XGp, XRest, X_32026, XG, X_32025):- XG = XGp,
    !,
    fail.
'lo.comp.code.analyse@forallB15'(XGp, XRest, X_32026, XG, X_32025).
'lo.comp.code.analyse@or169'(X_32034, X_32033, XOccs, X_32032, X_32031):- ocall('in%2'('()3'('lo.comp.code.analyse#oTerm', X_32031, X_32032), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.analyse@or169'(X_32034, X_32033, XOccs, X_32032, X_32031):- ocall('in%2'('()3'('lo.comp.code.analyse#lTerm', X_32033, X_32034), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.analyse@or170'(X_32034, X_32033, X_32032, X_32031, XOccs, X_32030, X_32029):- ocall('in%2'('()3'('lo.comp.code.analyse#hArg', X_32029, X_32030), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.analyse@or170'(X_32034, X_32033, X_32032, X_32031, XOccs, X_32030, X_32029):- 'lo.comp.code.analyse@or169'(X_32034, X_32033, XOccs, X_32032, X_32031).
'lo.comp.code.analyse@or171'(X_32034, X_32033, X_32032, X_32031, X_32030, X_32029, XOccs, X_32028, X_32027):- ocall('in%2'('()3'('lo.comp.code.analyse#hTerm', X_32027, X_32028), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.analyse@or171'(X_32034, X_32033, X_32032, X_32031, X_32030, X_32029, XOccs, X_32028, X_32027):- 'lo.comp.code.analyse@or170'(X_32034, X_32033, X_32032, X_32031, XOccs, X_32030, X_32029).
'lo.comp.code.analyse@one273'(X_32034, X_32033, X_32032, X_32031, X_32030, X_32029, XOccs, X_32028, X_32027):- 'lo.comp.code.analyse@or171'(X_32034, X_32033, X_32032, X_32031, X_32030, X_32029, XOccs, X_32028, X_32027),
    !.
'lo.comp.code.analyse^classifyVr'('_call%3'(XV29256, XV29257, XV29258), 'lo.comp.code.analyse^classifyVr', _):- 'lo.comp.code.analyse@classifyVr'(XV29256, XV29257, XV29258).
'lo.comp.code.analyse^argRegUsed'('_call%3'(XV29259, XV29260, XV29261), 'lo.comp.code.analyse^argRegUsed', _):- 'lo.comp.code.analyse@argRegUsed'(XV29259, XV29260, XV29261).
'lo.comp.code.analyse^registersOccupied'('_call%2'(XV29262, XV29263), 'lo.comp.code.analyse^registersOccupied', _):- 'lo.comp.code.analyse@registersOccupied'(XV29262, XV29263).
'lo.comp.code.analyse@neg307'(XXd37087, XU, XRg):- ocall('in%2'(XRg, XU),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.analyse@neg307'(XXd37087, XU, XRg).
'lo.comp.code.analyse@neg308'(XXd37090, XU, XRg):- ocall('in%2'(XRg, XU),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.analyse@neg308'(XXd37090, XU, XRg).
'lo.comp.code.analyse@neg309'(XXd37092, XU, XRg):- ocall('in%2'(XRg, XU),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.analyse@neg309'(XXd37092, XU, XRg).
'lo.comp.code.analyse^checkOccForArg'('_call%7'(XV29264, XV29265, XV29266, XV29267, XV29268, XV29269, XV29270), 'lo.comp.code.analyse^checkOccForArg', _):- 'lo.comp.code.analyse@checkOccForArg'(XV29264, XV29265, XV29266, XV29267, XV29268, XV29269, XV29270).
'lo.comp.code.analyse@fun112'('_call%3'(XA, '()3'(XForm, XGp, XRg), XXd37094), 'lo.comp.code.analyse@fun112'(XUse, XRGp), _):- !,
    'lo.comp.code.analyse@checkOccForArg'(XForm, XRGp, XGp, XRg, XUse, XA, XXd37094).
'lo.comp.code.analyse@fun112'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@fun112", 99, 50, 56)).
'lo.comp.code.analyse^checkOtherOccs'('_call%5'(XV29271, XV29272, XV29273, XV29274, XV29275), 'lo.comp.code.analyse^checkOtherOccs', _):- 'lo.comp.code.analyse@checkOtherOccs'(XV29271, XV29272, XV29273, XV29274, XV29275).
'lo.comp.code.analyse@condExp119'(XA, XXd37095, XOccs, XGp, XU, XA, XNm, XN):- XN = XNm,
    !.
'lo.comp.code.analyse@condExp119'(XXd37095, XXd37095, XOccs, XGp, XU, XA, XNm, XN):- 'lo.comp.code.analyse@checkOtherOccs'(XU, XGp, XOccs, XA, XXd37095).
'lo.comp.code.analyse@fun113'('_call%3'(XA, 'lo.comp.code.analyse#occ'(XN, XOccs), XCndV119), 'lo.comp.code.analyse@fun113'(XGp, XU, XNm), _):- !,
    'lo.comp.code.analyse@condExp119'(XCndV119, XXd37095, XOccs, XGp, XU, XA, XNm, XN).
'lo.comp.code.analyse@fun113'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@fun113", 96, 44, 53)).
'lo.comp.code.analyse^checkOtherRegVars'('_call%5'(XV29276, XV29277, XV29278, XV29279, XV29280), 'lo.comp.code.analyse^checkOtherRegVars', _):- 'lo.comp.code.analyse@checkOtherRegVars'(XV29276, XV29277, XV29278, XV29279, XV29280).
'lo.comp.code.analyse@neg310'(XOccs, XRg, XGp):- ocall('in%2'('()3'('lo.comp.code.analyse#lArg', XGp, XRg), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.code.analyse@neg310'(XOccs, XRg, XGp).
'lo.comp.code.analyse@neg311'(XOccs, XRg, XGp):- ocall('in%2'('()3'('lo.comp.code.analyse#oArg', XGp, XRg), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.code.analyse@neg311'(XOccs, XRg, XGp).
'lo.comp.code.analyse@or172'(XOccs, XRg, XGp):- ocall('in%2'('()3'('lo.comp.code.analyse#lTerm', XGp, XRg), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.analyse@or172'(XOccs, XRg, XGp):- ocall('in%2'('()3'('lo.comp.code.analyse#oTerm', XGp, XRg), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.analyse^checkArgPos'('_call%5'(XV29281, XV29282, XV29283, XV29284, XV29285), 'lo.comp.code.analyse^checkArgPos', _):- 'lo.comp.code.analyse@checkArgPos'(XV29281, XV29282, XV29283, XV29284, XV29285).
'lo.comp.code.analyse@or173'(X_32048, XOccs, X_32047, XGp):- ocall('in%2'('()3'('lo.comp.code.analyse#lArg', XGp, X_32047), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.analyse@or173'(X_32048, XOccs, X_32047, XGp):- ocall('in%2'('()3'('lo.comp.code.analyse#oArg', XGp, X_32048), XOccs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.analyse@fun114'('_call%3'(XC, XRg, XXd37099), 'lo.comp.code.analyse@fun114'(XOccs, XGp), _):- !,
    'lo.comp.code.analyse@checkArgPos'(XGp, XRg, XOccs, XC, XXd37099).
'lo.comp.code.analyse@fun114'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@fun114", 111, 50, 35)).
'lo.comp.code.analyse^conflictSet'('_call%4'(XV29286, XV29287, XV29288, XV29289), 'lo.comp.code.analyse^conflictSet', _):- 'lo.comp.code.analyse@conflictSet'(XV29286, XV29287, XV29288, XV29289).
'lo.comp.code.analyse@neg312'(XUse, XRg, XGp):- ocall('in%2'('()3'('lo.comp.code.analyse#hTerm', XGp, XRg), XUse),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.code.analyse@neg312'(XUse, XRg, XGp).
'lo.comp.code.analyse@neg313'(XUse, XRg, XGp, X_32049):- ocall('in%2'('()3'(X_32049, XGp, XRg), XUse),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.code.analyse@neg313'(XUse, XRg, XGp, X_32049).
'lo.comp.code.analyse^checkHeadUse'('_call%7'(XV29290, XV29291, XV29292, XV29293, XV29294, XV29295, XV29296), 'lo.comp.code.analyse^checkHeadUse', _):- 'lo.comp.code.analyse@checkHeadUse'(XV29290, XV29291, XV29292, XV29293, XV29294, XV29295, XV29296).
'lo.comp.code.analyse@fun115'('_call%3'(XCS, '()3'(XForm, XG, XRg), XXd37105), 'lo.comp.code.analyse@fun115'(XUse, XGp), _):- !,
    'lo.comp.code.analyse@checkHeadUse'(XForm, XG, XGp, XRg, XCS, XUse, XXd37105).
'lo.comp.code.analyse@fun115'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@fun115", 158, 51, 53)).
'lo.comp.code.analyse^checkHeadUses'('_call%5'(XV29297, XV29298, XV29299, XV29300, XV29301), 'lo.comp.code.analyse^checkHeadUses', _):- 'lo.comp.code.analyse@checkHeadUses'(XV29297, XV29298, XV29299, XV29300, XV29301).
'lo.comp.code.analyse@fun116'('_call%3'(XC, XM, XXd37106), 'lo.comp.code.analyse@fun116'(XUse, XGp), _):- !,
    'lo.comp.code.analyse@checkHeadUses'(XM, XGp, XC, XUse, XXd37106).
'lo.comp.code.analyse@fun116'(_, _, _):- raise_exception('error'("lo.comp.code.analyse@fun116", 155, 37, 34)).
'lo.comp.code.analyse^otherUsers'('_call%4'(XV29302, XV29303, XV29304, XV29305), 'lo.comp.code.analyse^otherUsers', _):- 'lo.comp.code.analyse@otherUsers'(XV29302, XV29303, XV29304, XV29305).
'lo.comp.code.analyse@neg314'(XNm):- XNm = "",
    !,
    fail.
'lo.comp.code.analyse@neg314'(XNm).
'lo.comp.code.analyse@condExp120'(XXd37113, XXd37113, XXd37112, XArity, XGPart, XXe4674, XXV5013, XG, XParts):- ocall('-%1'(XXV5013),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XG, 1, XXe4674),XXV5013,XXV5013),
    'lo.list@nthEl'(XParts, XXe4674, XGPart),
    !,
    'lo.comp.code.analyse@maxArity'(XGPart, XXd37112),
    'lo.core@max'('lo.core$comp$lo.core*integer', XArity, XXd37112, XXd37113).
'lo.comp.code.analyse@condExp120'(XArity, XXd37113, XXd37112, XArity, XGPart, XXe4674, XXV5013, XG, XParts).
'lo.comp.code.analyse@neg315'(XNm, XN):- XN = XNm,
    !,
    fail.
'lo.comp.code.analyse@neg315'(XNm, XN).
'lo.comp.code.analyse@pred9'('_call%1'('lo.comp.code.analyse#occ'(XN, XOO)), 'lo.comp.code.analyse@pred9'(XNm, XG), _):- 'lo.comp.code.analyse@neg315'(XNm, XN),
    ocall('in%2'('()3'(X_32061, XG, X_32062), XOO),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.code.analyse^buildRgUsage'('_call%6'(XV29306, XV29307, XV29308, XV29309, XV29310, XV29311), 'lo.comp.code.analyse^buildRgUsage', _):- 'lo.comp.code.analyse@buildRgUsage'(XV29306, XV29307, XV29308, XV29309, XV29310, XV29311).
'lo.comp.code.analyse^buildRegUsage'('_call%6'(XV29312, XV29313, XV29314, XV29315, XV29316, XV29317), 'lo.comp.code.analyse^buildRegUsage', _):- 'lo.comp.code.analyse@buildRegUsage'(XV29312, XV29313, XV29314, XV29315, XV29316, XV29317).
'lo.comp.code.analyse^usageTable'('_call%4'(XV29318, XV29319, XV29320, XV29321), 'lo.comp.code.analyse^usageTable', _):- 'lo.comp.code.analyse@usageTable'(XV29318, XV29319, XV29320, XV29321).
'lo.comp.code.analyse^varAnalysis'('_call%4'(XV29322, XV29323, XV29324, XV29325), 'lo.comp.code.analyse^varAnalysis', _):- 'lo.comp.code.analyse@varAnalysis'(XV29322, XV29323, XV29324, XV29325).
'lo.comp.code.analyse^dispUsage'('_call%2'(XV29326, XV29327), 'lo.comp.code.analyse^dispUsage', _):- 'lo.comp.code.analyse@dispUsage'(XV29326, XV29327).
'lo.core$display$lo.comp.code.analyse*usageMap^disp'('_call%2'(XV29328, XV29329), 'lo.core$display$lo.comp.code.analyse*usageMap^disp'(XLbV2397, XThV2397), _):- 'lo.core$display$lo.comp.code.analyse*usageMap@disp'(XV29328, XV29329, XLbV2397, XThV2397).
'lo.comp.code.analyse^avoidIota'('_call%3'(XV29332, XV29333, XV29334), 'lo.comp.code.analyse^avoidIota', _):- 'lo.comp.code.analyse@avoidIota'(XV29332, XV29333, XV29334).
'lo.comp.code.analyse^compPos'('_call%2'(XV29335, XV29336), 'lo.comp.code.analyse^compPos', _):- 'lo.comp.code.analyse@compPos'(XV29335, XV29336).
