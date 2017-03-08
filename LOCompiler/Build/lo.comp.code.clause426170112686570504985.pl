'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.clause's'0.0.1'n11o11'()11'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.analyse'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.code'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.estimate'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.instructions'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.registers'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.unify'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.escapes'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'s\"I2'newLabel'FT1SS'compCl'FT3t'lo.comp.term*clse'Lt'lo.comp.code.code*litrl'LT3SSt'lo.comp.term*tloc't'lo.comp.code.code*assem'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.code.clause@init'():- !.
'lo.comp.code.clause@newLabel'(XPrefix, XXc555):- !,
    '_str_gen'(XPrefix, XXc555).
'lo.comp.code.clause@newLabel'(_, _):- raise_exception('error'("lo.comp.code.clause@newLabel", 168, 3, 36)).
'lo.comp.code.clause@addToSrcMap'('lo.core#some'(XTLc), XSLb, XELb, XM, 'lo.core#,..'('()3'(XSLb, XELb, XTLc), XM)).
'lo.comp.code.clause@addToSrcMap'('lo.core#none', X_36727, X_36728, XM, XM).
'lo.comp.code.clause@compGoal'('lo.comp.term#fail', X_36729, XD, XLt, XSM, XVs, X_36730, X_36731, 'lo.core#,..'('lo.comp.code.instructions#iFayl', 'lo.core#[]'), XD, XLt, XVs, XSM).
'lo.comp.code.clause@compGoal'('lo.comp.term#neck', X_36733, XD, XLt, XSM, XVs, 'lo.core#true', XAlloc, XCode, XD, XLt, XVs, XSM):- 'lo.comp.code.clause@cond494'(XXd41992, XXd41991, XXd41990, XXd41989, XXd41988, XXd41987, XCode, XXd41986, XVs, XAlloc).
'lo.comp.code.clause@compGoal'('lo.comp.term#neck', X_36739, XD, XLt, XSM, XVs, 'lo.core#false', X_36740, 'lo.core#,..'('lo.comp.code.instructions#iCut', 'lo.core#[]'), XD, XLt, XVs, XSM).
'lo.comp.code.clause@compGoal'('lo.comp.term#ecall'(XTLc, XEsc, XArgs), XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XCode, XDx, XLtx, XVsx, XSMx):- 'lo.comp.code.clause@newLabel'("Src", XXd41993),
    XSLb = XXd41993,
    'lo.comp.code.clause@newLabel'("Src", XXd41994),
    XELb = XXd41994,
    'lo.comp.code.unify@callArgs'(XArgs, XGp, XD, XLt, XVs, XLast, XaCode, XDx, XLtx, XVsx),
    ocall('size%1'(XXV5648),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe5225),XXV5648,XXV5648),
    XArity = XXe5225,
    'lo.comp.code.registers@gcMap'(XVsx, XArity, XPre, XMapIns),
    'lo.comp.code.clause@cond496'(XXd42032, XXd42031, XXd42030, XXd42029, XXd42028, XXd42027, XXd42026, XXd42025, XXd42024, XXd42023, XXd42022, XXd42021, XXd42020, XXd42019, XXd42018, XXd42017, XXd42016, XXd42015, XXd42014, XXd42013, XPre, XXd42012, XXd42011, XXd42010, XXd42009, XXd42008, XXd42007, XXd42006, XXd42005, XXd42004, XXd42003, XXd42002, XXd42001, XXd42000, XELb, XXd41999, XMapIns, XXd41998, XEsc, XArity, XXd41997, XaCode, XXd41996, XSLb, XCode, XXd41995, XVs, XAlloc, XLast),
    'lo.comp.code.clause@addToSrcMap'(XTLc, XSLb, XELb, XSm, XSMx).
'lo.comp.code.clause@compGoal'('lo.comp.term#call'(XTLc, XPrd, XArgs), XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XCode, XDx, XLtx, XVsx, XSMx):- 'lo.comp.code.clause@newLabel'("Src", XXd42033),
    XSLb = XXd42033,
    'lo.comp.code.clause@newLabel'("Src", XXd42034),
    XELb = XXd42034,
    'lo.comp.code.unify@callArgs'(XArgs, XGp, XD, XLt, XVs, XLast, XaCode, XDx, XLt0, XVsx),
    ocall('size%1'(XXV5649),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe5226),XXV5649,XXV5649),
    XArity = XXe5226,
    'lo.comp.code.registers@gcMap'(XVsx, XArity, XPre, XMapIns),
    'lo.comp.code.clause@one325'(XLtx, XLt0, XLbl, XPrd),
    'lo.comp.code.clause@cond498'(XXd42060, XXd42059, XXd42058, XXd42057, XXd42056, XXd42055, XMapIns, XXd42054, XPre, XXd42053, XXd42052, XXd42051, XXd42050, XXd42049, XXd42048, XXd42047, XXd42046, XXd42045, XXd42044, XXd42043, XXd42042, XXd42041, XXd42040, XXd42039, XELb, XXd42038, XLbl, XArity, XXd42037, XaCode, XXd42036, XSLb, XCode, XXd42035, XVs, XAlloc, XLast),
    'lo.comp.code.clause@addToSrcMap'(XTLc, XSLb, XELb, XSM, XSMx).
'lo.comp.code.clause@compGoal'('lo.comp.term#ocall'(XTLc, XCl, XLbV, XThV), XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XCode, XDx, XLtx, XVsx, XSMx):- 'lo.comp.code.clause@newLabel'("Src", XXd42061),
    XSLb = XXd42061,
    'lo.comp.code.clause@newLabel'("Src", XXd42062),
    XELb = XXd42062,
    'lo.comp.code.unify@callArgs'('lo.core#,..'(XCl, 'lo.core#,..'(XLbV, 'lo.core#,..'(XThV, 'lo.core#[]'))), XGp, XD, XLt, XVs, XLast, XaCode, XDx, XLtx, XVsx),
    'lo.comp.code.registers@gcMap'(XVsx, 3, XPre, XMapIns),
    'lo.comp.code.clause@cond500'(XXd42090, XXd42089, XXd42088, XXd42087, XXd42086, XXd42085, XXd42084, XPre, XXd42083, XXd42082, XXd42081, XXd42080, XXd42079, XXd42078, XXd42077, XMapIns, XXd42076, XXd42075, XXd42074, XXd42073, XXd42072, XXd42071, XXd42070, XELb, XXd42069, XXd42068, XaCode, XXd42067, XSLb, XCode, XXd42066, XVs, XAlloc, XLast),
    'lo.comp.code.clause@addToSrcMap'(XTLc, XSLb, XELb, XSM, XSMx).
'lo.comp.code.clause@compGoal'('lo.comp.term#unfy'(XTLc, 'lo.comp.term#varbl'(XNm), XR), XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XCode, XDx, XLtx, XVsx, XSMx):- 'lo.comp.code.clause@newLabel'("Src", XXd42091),
    XSLb = XXd42091,
    'lo.comp.code.clause@newLabel'("Src", XXd42092),
    XELb = XXd42092,
    ocall('present%3'(XD, XNm, XV),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.registers@varAddr'(XV, XXd42094),
    XAddr = XXd42094,
    'lo.comp.code.clause@cond501'(XXe5227, XXV5650, XXd42098, XXd42097, XNm, XXd42096, XD1, XXd42095, XLast, XLtx, XLt, XVsx, XVs, XDx, XD, XAddr, XR, XaCode, XV),
    'lo.comp.code.clause@cond503'(XXd42119, XXd42118, XXd42117, XXd42116, XbCode, XXd42115, XXd42114, XXd42113, XXd42112, XXd42111, XXd42110, XXd42109, XXd42108, XXd42107, XXd42106, XXd42105, XXd42104, XXd42103, XELb, XXd42102, XXd42101, XaCode, XXd42100, XSLb, XCode, XXd42099, XVs, XAlloc, XLast),
    'lo.comp.code.clause@addToSrcMap'(XTLc, XSLb, XELb, XSM, XSMx).
'lo.comp.code.clause@compGoal'('lo.comp.term#unfy'(XTLc, XL, 'lo.comp.term#varbl'(XNm)), XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XCode, XDx, XLtx, XVsx, XSMx):- 'lo.comp.code.clause@newLabel'("Src", XXd42120),
    XSLb = XXd42120,
    'lo.comp.code.clause@newLabel'("Src", XXd42121),
    XELb = XXd42121,
    ocall('present%3'(XD, XNm, XV),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.registers@varAddr'(XV, XXd42123),
    XAddr = XXd42123,
    'lo.comp.code.clause@cond504'(XXe5228, XXV5651, XXd42127, XXd42126, XNm, XXd42125, XD1, XXd42124, XLast, XLtx, XLt, XVsx, XVs, XDx, XD, XAddr, XL, XaCode, XV),
    'lo.comp.code.clause@cond506'(XXd42148, XXd42147, XXd42146, XXd42145, XbCode, XXd42144, XXd42143, XXd42142, XXd42141, XXd42140, XXd42139, XXd42138, XXd42137, XXd42136, XXd42135, XXd42134, XXd42133, XXd42132, XELb, XXd42131, XXd42130, XaCode, XXd42129, XSLb, XCode, XXd42128, XVs, XAlloc, XLast),
    'lo.comp.code.clause@addToSrcMap'(XTLc, XSLb, XELb, XSM, XSMx).
'lo.comp.code.clause@compGoal'('lo.comp.term#unfy'(XTLc, XL, XR), XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XCode, XDx, XLtx, XVsx, XSMx):- 'lo.comp.code.clause@newLabel'("Src", XXd42149),
    XSLb = XXd42149,
    'lo.comp.code.clause@newLabel'("Src", XXd42150),
    XELb = XXd42150,
    'lo.comp.code.registers@pickAreg'(XVs, XVs0, XV),
    'lo.comp.code.unify@buildTerm'(XL, 'lo.core#true', XV, XD, XD0, XVs0, XVs1, XLt, XLt0, 'lo.core#false', XXd42151),
    XaCode = XXd42151,
    'lo.comp.code.unify@unifyTerm'(XR, 'lo.core#true', XV, XD0, XDx, XVs1, XVsx, XLt0, XLtx, XLast, XXd42152),
    XbCode = XXd42152,
    'lo.comp.code.clause@cond508'(XXd42175, XXd42174, XXd42173, XXd42172, XXd42171, XXd42170, XXd42169, XXd42168, XXd42167, XXd42166, XXd42165, XXd42164, XXd42163, XXd42162, XXd42161, XXd42160, XXd42159, XXd42158, XXd42157, XELb, XXd42156, XbCode, XXd42155, XaCode, XXd42154, XSLb, XCode, XXd42153, XVs, XAlloc, XLast),
    'lo.comp.code.clause@addToSrcMap'(XTLc, XSLb, XELb, XSM, XSMx).
'lo.comp.code.clause@compGoal'('lo.comp.term#except'(XTLc, XE), XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XCode, XDx, XLtx, XVsx, XSMx):- 'lo.comp.code.clause@newLabel'("Src", XXd42176),
    XSLb = XXd42176,
    'lo.comp.code.clause@newLabel'("Src", XXd42177),
    XELb = XXd42177,
    'lo.comp.code.unify@buildTerm'(XE, 'lo.core#true', 'lo.comp.code.registers#aReg'(0), XD, XDx, XVs, XVsx, XLt, XLtx, XLast, XXd42179),
    XaCode = XXd42179,
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iExcept'(0), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]')), XXd42186),
    XCode = XXd42186,
    'lo.comp.code.clause@addToSrcMap'(XTLc, XSLb, XELb, XSM, XSMx).
'lo.comp.code.clause@compPart'('lo.core#[]', X_36815, XD, XLtx, XSM, XVs, X_36816, X_36817, 'lo.core#[]', XD, XLtx, XVs, XSM).
'lo.comp.code.clause@compPart'('lo.core#,..'(XG, 'lo.core#[]'), XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XCode, XDx, XLtx, XVsx, XSMx):- 'lo.comp.code.clause@compGoal'(XG, XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XCode, XDx, XLtx, XVsx, XSMx).
'lo.comp.code.clause@compPart'('lo.core#,..'(XG, 'lo.core#,..'(XG1, XR)), XGp, XD, XLt, XSM, XVs, XLast, XAlloc, XXb20803, XDx, XLtx, XVsx, XSMx):- 'lo.comp.code.clause@compGoal'(XG, XGp, XD, XLt, XSM, XVs, 'lo.core#false', 'lo.core#false', XpCode, XD0, XLt0, XVs0, XSM0),
    'lo.comp.code.clause@compPart'('lo.core#,..'(XG1, XR), XGp, XD0, XLt0, XSM0, XVs0, XLast, XAlloc, XrCode, XDx, XLtx, XVsx, XSMx),
    'lo.list@<>'(XpCode, XrCode, XXb20803).
'lo.comp.code.clause@compBodyParts'('lo.core#[]', X_36822, XDx, XLts, XVs, XSM0, X_36823, 'lo.core#,..'('lo.comp.code.instructions#iSucc', 'lo.core#[]'), XDx, XLts, XVs, XSM0).
'lo.comp.code.clause@compBodyParts'('lo.core#,..'(XP, 'lo.core#[]'), XGp, XD0, XL0, XVs0, XSM0, XAllo, XCode, XDx, XLx, XVsx, XSMx):- 'lo.comp.code.registers@maxReg'(XVs0, XXd42188),
    XMx = XXd42188,
    'lo.comp.code.registers@resetGc'(XVs0, XXd42189),
    'lo.comp.code.clause@compPart'(XP, XGp, XD0, XL0, XSM0, XXd42189, 'lo.core#true', XAllo, XfCode, XDx, XLx, XVsx, XSMx),
    'lo.comp.code.registers@gcPredict'(XVsx, XXd42190),
    XlSize = XXd42190,
    'lo.comp.code.clause@cond510'(XXd42200, XXd42199, XXd42198, XXd42197, XXd42196, XXd42195, XXd42194, XXd42193, XfCode, XXd42192, XCode, X_36826, XPre, XMx, XVs0, XXd42191, XVsx, XAllo, XlSize).
'lo.comp.code.clause@compBodyParts'('lo.core#,..'(XP, XArts), XGp, XD0, XL0, XVs0, XSM0, XAllo, XCode, XDx, XLx, XVsx, XSMx):- 'lo.comp.code.registers@resetGc'(XVs0, XXd42201),
    'lo.comp.code.clause@compPart'(XP, XGp, XD0, XL0, XSM0, XXd42201, 'lo.core#false', 'lo.core#false', XfCode, XD1, XL1, XV1, XSM1),
    'lo.comp.code.registers@gcPredict'(XV1, XXd42202),
    XlSize = XXd42202,
    'lo.comp.code.registers@maxReg'(XVs0, XXd42203),
    XMx = XXd42203,
    'lo.comp.code.registers@gcMap'(XVs0, XMx, XPre, X_36830),
    'lo.comp.code.clause@cond511'(XXd42206, XXd42205, XXd42204, XMx, XPre, XgCode, XlSize),
    ocall('+%1'(XXV5652),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XGp, 1, XXe5229),XXV5652,XXV5652),
    'lo.comp.code.registers@resetUsed'(XV1, XXd42207),
    'lo.comp.code.clause@compBodyParts'(XArts, XXe5229, XD1, XL1, XXd42207, XSM1, XAllo, XbCode, XDx, XLx, XVx, XSMx),
    'lo.list@<>'(XfCode, XbCode, XXd42208),
    'lo.list@<>'(XgCode, XXd42208, XXd42209),
    XCode = XXd42209.
'lo.comp.code.clause@clearLocals'(XLcls, XRg, XMax, 'lo.core#[]'):- 'lo.core@>'('lo.core$comp$lo.core*integer', XRg, XMax),
    !.
'lo.comp.code.clause@clearLocals'(XLcls, XRg, XMax, 'lo.core#,..'('lo.comp.code.instructions#iClY'(XRg), XXd42212)):- 'lo.comp.code.clause@neg352'(XXd42210, XLcls, XRg),
    !,
    ocall('+%1'(XXV5653),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XRg, 1, XXe5230),XXV5653,XXV5653),
    'lo.comp.code.clause@clearLocals'(XLcls, XXe5230, XMax, XXd42212).
'lo.comp.code.clause@clearLocals'(XLcls, XRg, XMax, XXd42214):- !,
    ocall('+%1'(XXV5654),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XRg, 1, XXe5231),XXV5654,XXV5654),
    'lo.comp.code.clause@clearLocals'(XLcls, XXe5231, XMax, XXd42214).
'lo.comp.code.clause@clearLocals'(_, _, _, _):- raise_exception('error'("lo.comp.code.clause@clearLocals", 34, 3, 40)).
'lo.comp.code.clause@compCl'('lo.comp.term#clse'(XQ, XNm, XArgs, XBody), XLts, XSM0, 'lo.comp.code.code#assem'(XNm, XCode, XLtx, XSrcMap)):- ocall('disp%1'(XXV5655),'lo.core$display$lo.comp.term*clse','lo.core$display$lo.comp.term*clse'),
    ocall('_call%2'('lo.comp.term#clse'(XQ, XNm, XArgs, XBody), XXe5232),XXV5655,XXV5655),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("compiling "), 'lo.core#,..'(XXe5232, 'lo.core#[]'))), XXd42220),
    'lo.io@logMsg'(XXd42220),
    'lo.comp.code.analyse@partitionGoals'(XBody, 'lo.core#[]', XXd42221),
    XParts = XXd42221,
    'lo.comp.code.analyse@varAnalysis'(XQ, XArgs, XParts, XXd42222),
    '()3'(XDict, XPermSize, XVs) = XXd42222,
    'lo.comp.code.clause@condExp137'(XCndV137, XParts),
    'lo.comp.code.unify@unifyHead'(XArgs, XDict, XLts, XVs, XCndV137, XVs0, XD0, XLt0, XHCode),
    'lo.comp.code.estimate@estimateArgSpace'(XArgs, 'lo.core#false', XXd42223),
    XheapSize = XXd42223,
    'lo.comp.code.registers@gcPredict'(XVs0, XXd42224),
    XhSize = XXd42224,
    'lo.comp.code.clause@condExp138'(XCndV138, XXe5233, XXV5656, XParts, XPermSize),
    Xallocating = XCndV138,
    'lo.comp.code.clause@compBodyParts'(XParts, 1, XD0, XLt0, XVs0, XSM0, Xallocating, XbCode, XDx, XLtx, XVsx, XSrcMap),
    ocall('size%1'(XXV5657),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe5234),XXV5657,XXV5657),
    XArity = XXe5234,
    'lo.comp.code.registers@maxLocals'(XVsx, XXd42225),
    'lo.core@max'('lo.core$comp$lo.core*integer', XXd42225, XPermSize, XXd42226),
    XlSize = XXd42226,
    'lo.comp.code.clause@condExp139'(XCndV139, XXd42228, XXd42227, XArity, XhSize),
    'lo.comp.code.clause@condExp140'(XCndV140, XXd42234, XXd42233, XXd42232, XXd42231, XVs0, XXd42230, XXd42229, XlSize, XArity, Xallocating),
    'lo.list@<>'(XHCode, XbCode, XXd42235),
    'lo.list@<>'(XCndV140, XXd42235, XXd42236),
    'lo.list@<>'(XCndV139, XXd42236, XXd42237),
    XCode = XXd42237,
    !.
'lo.comp.code.clause@compCl'(_, _, _, _):- raise_exception('error'("lo.comp.code.clause@compCl", 17, 3, 781)).
'lo.comp.code.clause^newLabel'('_call%2'(XV33413, XV33414), 'lo.comp.code.clause^newLabel', _):- 'lo.comp.code.clause@newLabel'(XV33413, XV33414).
'lo.comp.code.clause^addToSrcMap'('_call%5'(XV33415, XV33416, XV33417, XV33418, XV33419), 'lo.comp.code.clause^addToSrcMap', _):- 'lo.comp.code.clause@addToSrcMap'(XV33415, XV33416, XV33417, XV33418, XV33419).
'lo.comp.code.clause@or213'(XXd41986, XVs, XAlloc):- XAlloc = 'lo.core#true'.
'lo.comp.code.clause@or213'(XXd41986, XVs, XAlloc):- 'lo.comp.code.registers@maxLocals'(XVs, XXd41986),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd41986, 0).
'lo.comp.code.clause@cond494'(XXd41992, XXd41991, XXd41990, XXd41989, XXd41988, XXd41987, XCode, XXd41986, XVs, XAlloc):- 'lo.comp.code.clause@or213'(XXd41986, XVs, XAlloc),
    !,
    XCode = 'lo.core#,..'('lo.comp.code.instructions#iCut', 'lo.core#,..'('lo.comp.code.instructions#iDealloc', 'lo.core#,..'('lo.comp.code.instructions#iGcmap'(0, 0), 'lo.core#[]'))).
'lo.comp.code.clause@cond494'(XXd41992, XXd41991, XXd41990, XXd41989, XXd41988, XXd41987, XCode, XXd41986, XVs, XAlloc):- XCode = 'lo.core#,..'('lo.comp.code.instructions#iCut', 'lo.core#,..'('lo.comp.code.instructions#iSucc', 'lo.core#[]')).
'lo.comp.code.clause@or214'(XXd41995, XVs, XAlloc):- XAlloc = 'lo.core#true'.
'lo.comp.code.clause@or214'(XXd41995, XVs, XAlloc):- 'lo.comp.code.registers@maxLocals'(XVs, XXd41995),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd41995, 0).
'lo.comp.code.clause@cond495'(XXd42023, XXd42022, XXd42021, XXd42020, XXd42019, XXd42018, XXd42017, XXd42016, XXd42015, XXd42014, XXd42013, XPre, XXd42012, XXd42011, XXd42010, XXd42009, XXd42008, XXd42007, XXd42006, XXd42005, XXd42004, XXd42003, XXd42002, XXd42001, XXd42000, XELb, XXd41999, XMapIns, XXd41998, XEsc, XArity, XXd41997, XaCode, XXd41996, XSLb, XCode, XXd41995, XVs, XAlloc):- 'lo.comp.code.clause@or214'(XXd41995, XVs, XAlloc),
    !,
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iEscape'(XArity, XEsc), 'lo.core#,..'(XMapIns, 'lo.core#,..'('lo.comp.code.instructions#iDealloc', 'lo.core#,..'('lo.comp.code.instructions#iGcmap'(0, 0), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))))), XXd42006),
    XCode = XXd42006.
'lo.comp.code.clause@cond495'(XXd42023, XXd42022, XXd42021, XXd42020, XXd42019, XXd42018, XXd42017, XXd42016, XXd42015, XXd42014, XXd42013, XPre, XXd42012, XXd42011, XXd42010, XXd42009, XXd42008, XXd42007, XXd42006, XXd42005, XXd42004, XXd42003, XXd42002, XXd42001, XXd42000, XELb, XXd41999, XMapIns, XXd41998, XEsc, XArity, XXd41997, XaCode, XXd41996, XSLb, XCode, XXd41995, XVs, XAlloc):- 'lo.list@<>'(XPre, 'lo.core#,..'('lo.comp.code.instructions#iEscape'(XArity, XEsc), 'lo.core#,..'(XMapIns, 'lo.core#,..'('lo.comp.code.instructions#iDealloc', 'lo.core#,..'('lo.comp.code.instructions#iGcmap'(0, 0), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))))), XXd42021),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iAlloc'(XArity, 0), 'lo.core#,..'('lo.comp.code.instructions#iGcmap'(XArity, 0), 'lo.core#[]')), XXd42021, XXd42022),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), XXd42022, XXd42023),
    XCode = XXd42023.
'lo.comp.code.clause@cond496'(XXd42032, XXd42031, XXd42030, XXd42029, XXd42028, XXd42027, XXd42026, XXd42025, XXd42024, XXd42023, XXd42022, XXd42021, XXd42020, XXd42019, XXd42018, XXd42017, XXd42016, XXd42015, XXd42014, XXd42013, XPre, XXd42012, XXd42011, XXd42010, XXd42009, XXd42008, XXd42007, XXd42006, XXd42005, XXd42004, XXd42003, XXd42002, XXd42001, XXd42000, XELb, XXd41999, XMapIns, XXd41998, XEsc, XArity, XXd41997, XaCode, XXd41996, XSLb, XCode, XXd41995, XVs, XAlloc, XLast):- XLast = 'lo.core#true',
    !,
    'lo.comp.code.clause@cond495'(XXd42023, XXd42022, XXd42021, XXd42020, XXd42019, XXd42018, XXd42017, XXd42016, XXd42015, XXd42014, XXd42013, XPre, XXd42012, XXd42011, XXd42010, XXd42009, XXd42008, XXd42007, XXd42006, XXd42005, XXd42004, XXd42003, XXd42002, XXd42001, XXd42000, XELb, XXd41999, XMapIns, XXd41998, XEsc, XArity, XXd41997, XaCode, XXd41996, XSLb, XCode, XXd41995, XVs, XAlloc).
'lo.comp.code.clause@cond496'(XXd42032, XXd42031, XXd42030, XXd42029, XXd42028, XXd42027, XXd42026, XXd42025, XXd42024, XXd42023, XXd42022, XXd42021, XXd42020, XXd42019, XXd42018, XXd42017, XXd42016, XXd42015, XXd42014, XXd42013, XPre, XXd42012, XXd42011, XXd42010, XXd42009, XXd42008, XXd42007, XXd42006, XXd42005, XXd42004, XXd42003, XXd42002, XXd42001, XXd42000, XELb, XXd41999, XMapIns, XXd41998, XEsc, XArity, XXd41997, XaCode, XXd41996, XSLb, XCode, XXd41995, XVs, XAlloc, XLast):- 'lo.list@<>'(XPre, 'lo.core#,..'('lo.comp.code.instructions#iEscape'(XArity, XEsc), 'lo.core#,..'(XMapIns, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))), XXd42031),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), XXd42031, XXd42032),
    XCode = XXd42032.
'lo.comp.code.clause@one325'(XLtx, XLt0, XLbl, XPrd):- 'lo.comp.code.unify@accessLiteral'(XPrd, XLbl, XLt0, XLtx),
    !.
'lo.comp.code.clause@or215'(XXd42035, XVs, XAlloc):- XAlloc = 'lo.core#true'.
'lo.comp.code.clause@or215'(XXd42035, XVs, XAlloc):- 'lo.comp.code.registers@maxLocals'(XVs, XXd42035),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd42035, 0).
'lo.comp.code.clause@cond497'(XXd42051, XXd42050, XXd42049, XXd42048, XXd42047, XXd42046, XXd42045, XXd42044, XXd42043, XXd42042, XXd42041, XXd42040, XXd42039, XELb, XXd42038, XLbl, XArity, XXd42037, XaCode, XXd42036, XSLb, XCode, XXd42035, XVs, XAlloc):- 'lo.comp.code.clause@or215'(XXd42035, XVs, XAlloc),
    !,
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iDlkawl'(XArity, XLbl), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]')), XXd42042),
    XCode = XXd42042.
'lo.comp.code.clause@cond497'(XXd42051, XXd42050, XXd42049, XXd42048, XXd42047, XXd42046, XXd42045, XXd42044, XXd42043, XXd42042, XXd42041, XXd42040, XXd42039, XELb, XXd42038, XLbl, XArity, XXd42037, XaCode, XXd42036, XSLb, XCode, XXd42035, XVs, XAlloc):- 'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iLkawl'(XArity, XLbl), 'lo.core#,..'('lo.comp.code.instructions#iGcmap'(XArity, 0), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))), XXd42051),
    XCode = XXd42051.
'lo.comp.code.clause@cond498'(XXd42060, XXd42059, XXd42058, XXd42057, XXd42056, XXd42055, XMapIns, XXd42054, XPre, XXd42053, XXd42052, XXd42051, XXd42050, XXd42049, XXd42048, XXd42047, XXd42046, XXd42045, XXd42044, XXd42043, XXd42042, XXd42041, XXd42040, XXd42039, XELb, XXd42038, XLbl, XArity, XXd42037, XaCode, XXd42036, XSLb, XCode, XXd42035, XVs, XAlloc, XLast):- XLast = 'lo.core#true',
    !,
    'lo.comp.code.clause@cond497'(XXd42051, XXd42050, XXd42049, XXd42048, XXd42047, XXd42046, XXd42045, XXd42044, XXd42043, XXd42042, XXd42041, XXd42040, XXd42039, XELb, XXd42038, XLbl, XArity, XXd42037, XaCode, XXd42036, XSLb, XCode, XXd42035, XVs, XAlloc).
'lo.comp.code.clause@cond498'(XXd42060, XXd42059, XXd42058, XXd42057, XXd42056, XXd42055, XMapIns, XXd42054, XPre, XXd42053, XXd42052, XXd42051, XXd42050, XXd42049, XXd42048, XXd42047, XXd42046, XXd42045, XXd42044, XXd42043, XXd42042, XXd42041, XXd42040, XXd42039, XELb, XXd42038, XLbl, XArity, XXd42037, XaCode, XXd42036, XSLb, XCode, XXd42035, XVs, XAlloc, XLast):- 'lo.list@<>'(XPre, 'lo.core#,..'('lo.comp.code.instructions#iKawl'(XArity, XLbl), 'lo.core#,..'(XMapIns, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))), XXd42059),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), XXd42059, XXd42060),
    XCode = XXd42060.
'lo.comp.code.clause@or216'(XXd42066, XVs, XAlloc):- XAlloc = 'lo.core#true'.
'lo.comp.code.clause@or216'(XXd42066, XVs, XAlloc):- 'lo.comp.code.registers@maxLocals'(XVs, XXd42066),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd42066, 0).
'lo.comp.code.clause@cond499'(XXd42081, XXd42080, XXd42079, XXd42078, XXd42077, XMapIns, XXd42076, XXd42075, XXd42074, XXd42073, XXd42072, XXd42071, XXd42070, XELb, XXd42069, XXd42068, XaCode, XXd42067, XSLb, XCode, XXd42066, XVs, XAlloc):- 'lo.comp.code.clause@or216'(XXd42066, XVs, XAlloc),
    !,
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iDlkawlO'(3, 2), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]')), XXd42073),
    XCode = XXd42073.
'lo.comp.code.clause@cond499'(XXd42081, XXd42080, XXd42079, XXd42078, XXd42077, XMapIns, XXd42076, XXd42075, XXd42074, XXd42073, XXd42072, XXd42071, XXd42070, XELb, XXd42069, XXd42068, XaCode, XXd42067, XSLb, XCode, XXd42066, XVs, XAlloc):- 'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iLkawlO'(3, 2), 'lo.core#,..'(XMapIns, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))), XXd42081),
    XCode = XXd42081.
'lo.comp.code.clause@cond500'(XXd42090, XXd42089, XXd42088, XXd42087, XXd42086, XXd42085, XXd42084, XPre, XXd42083, XXd42082, XXd42081, XXd42080, XXd42079, XXd42078, XXd42077, XMapIns, XXd42076, XXd42075, XXd42074, XXd42073, XXd42072, XXd42071, XXd42070, XELb, XXd42069, XXd42068, XaCode, XXd42067, XSLb, XCode, XXd42066, XVs, XAlloc, XLast):- XLast = 'lo.core#true',
    !,
    'lo.comp.code.clause@cond499'(XXd42081, XXd42080, XXd42079, XXd42078, XXd42077, XMapIns, XXd42076, XXd42075, XXd42074, XXd42073, XXd42072, XXd42071, XXd42070, XELb, XXd42069, XXd42068, XaCode, XXd42067, XSLb, XCode, XXd42066, XVs, XAlloc).
'lo.comp.code.clause@cond500'(XXd42090, XXd42089, XXd42088, XXd42087, XXd42086, XXd42085, XXd42084, XPre, XXd42083, XXd42082, XXd42081, XXd42080, XXd42079, XXd42078, XXd42077, XMapIns, XXd42076, XXd42075, XXd42074, XXd42073, XXd42072, XXd42071, XXd42070, XELb, XXd42069, XXd42068, XaCode, XXd42067, XSLb, XCode, XXd42066, XVs, XAlloc, XLast):- 'lo.list@<>'(XPre, 'lo.core#,..'('lo.comp.code.instructions#iKawlO'(3, 2), 'lo.core#,..'(XMapIns, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))), XXd42089),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), XXd42089, XXd42090),
    XCode = XXd42090.
'lo.comp.code.clause@cond501'(XXe5227, XXV5650, XXd42098, XXd42097, XNm, XXd42096, XD1, XXd42095, XLast, XLtx, XLt, XVsx, XVs, XDx, XD, XAddr, XR, XaCode, XV):- 'lo.comp.code.registers@isInitialized'(XV),
    !,
    'lo.comp.code.unify@unifyTerm'(XR, 'lo.core#true', XAddr, XD, XDx, XVs, XVsx, XLt, XLtx, XLast, XXd42095),
    XaCode = XXd42095.
'lo.comp.code.clause@cond501'(XXe5227, XXV5650, XXd42098, XXd42097, XNm, XXd42096, XD1, XXd42095, XLast, XLtx, XLt, XVsx, XVs, XDx, XD, XAddr, XR, XaCode, XV):- 'lo.comp.code.unify@buildTerm'(XR, 'lo.core#true', XAddr, XD, XD1, XVs, XVsx, XLt, XLtx, XLast, XXd42096),
    XaCode = XXd42096,
    ocall('_put%1'(XXV5650),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.registers@markInited'(XV, XAddr, XXd42097),
    ocall('_call%4'(XD, XNm, XXd42097, XXe5227),XXV5650,XXV5650),
    XDx = XXe5227.
'lo.comp.code.clause@or217'(XXd42099, XVs, XAlloc):- XAlloc = 'lo.core#true'.
'lo.comp.code.clause@or217'(XXd42099, XVs, XAlloc):- 'lo.comp.code.registers@maxLocals'(XVs, XXd42099),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd42099, 0).
'lo.comp.code.clause@cond502'(XXd42113, XXd42112, XXd42111, XXd42110, XXd42109, XXd42108, XXd42107, XXd42106, XXd42105, XXd42104, XXd42103, XELb, XXd42102, XXd42101, XaCode, XXd42100, XSLb, XCode, XXd42099, XVs, XAlloc):- 'lo.comp.code.clause@or217'(XXd42099, XVs, XAlloc),
    !,
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iDealloc', 'lo.core#,..'('lo.comp.code.instructions#iGcmap'(0, 0), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))), XXd42107),
    XCode = XXd42107.
'lo.comp.code.clause@cond502'(XXd42113, XXd42112, XXd42111, XXd42110, XXd42109, XXd42108, XXd42107, XXd42106, XXd42105, XXd42104, XXd42103, XELb, XXd42102, XXd42101, XaCode, XXd42100, XSLb, XCode, XXd42099, XVs, XAlloc):- 'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iSucc', 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]')), XXd42113),
    XCode = XXd42113.
'lo.comp.code.clause@cond503'(XXd42119, XXd42118, XXd42117, XXd42116, XbCode, XXd42115, XXd42114, XXd42113, XXd42112, XXd42111, XXd42110, XXd42109, XXd42108, XXd42107, XXd42106, XXd42105, XXd42104, XXd42103, XELb, XXd42102, XXd42101, XaCode, XXd42100, XSLb, XCode, XXd42099, XVs, XAlloc, XLast):- XLast = 'lo.core#true',
    !,
    'lo.comp.code.clause@cond502'(XXd42113, XXd42112, XXd42111, XXd42110, XXd42109, XXd42108, XXd42107, XXd42106, XXd42105, XXd42104, XXd42103, XELb, XXd42102, XXd42101, XaCode, XXd42100, XSLb, XCode, XXd42099, XVs, XAlloc).
'lo.comp.code.clause@cond503'(XXd42119, XXd42118, XXd42117, XXd42116, XbCode, XXd42115, XXd42114, XXd42113, XXd42112, XXd42111, XXd42110, XXd42109, XXd42108, XXd42107, XXd42106, XXd42105, XXd42104, XXd42103, XELb, XXd42102, XXd42101, XaCode, XXd42100, XSLb, XCode, XXd42099, XVs, XAlloc, XLast):- 'lo.list@<>'(XbCode, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'), XXd42118),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), XXd42118, XXd42119),
    XCode = XXd42119.
'lo.comp.code.clause@cond504'(XXe5228, XXV5651, XXd42127, XXd42126, XNm, XXd42125, XD1, XXd42124, XLast, XLtx, XLt, XVsx, XVs, XDx, XD, XAddr, XL, XaCode, XV):- 'lo.comp.code.registers@isInitialized'(XV),
    !,
    'lo.comp.code.unify@unifyTerm'(XL, 'lo.core#true', XAddr, XD, XDx, XVs, XVsx, XLt, XLtx, XLast, XXd42124),
    XaCode = XXd42124.
'lo.comp.code.clause@cond504'(XXe5228, XXV5651, XXd42127, XXd42126, XNm, XXd42125, XD1, XXd42124, XLast, XLtx, XLt, XVsx, XVs, XDx, XD, XAddr, XL, XaCode, XV):- 'lo.comp.code.unify@buildTerm'(XL, 'lo.core#true', XAddr, XD, XD1, XVs, XVsx, XLt, XLtx, XLast, XXd42125),
    XaCode = XXd42125,
    ocall('_put%1'(XXV5651),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.registers@markInited'(XV, XAddr, XXd42126),
    ocall('_call%4'(XD, XNm, XXd42126, XXe5228),XXV5651,XXV5651),
    XDx = XXe5228.
'lo.comp.code.clause@or218'(XXd42128, XVs, XAlloc):- XAlloc = 'lo.core#true'.
'lo.comp.code.clause@or218'(XXd42128, XVs, XAlloc):- 'lo.comp.code.registers@maxLocals'(XVs, XXd42128),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd42128, 0).
'lo.comp.code.clause@cond505'(XXd42142, XXd42141, XXd42140, XXd42139, XXd42138, XXd42137, XXd42136, XXd42135, XXd42134, XXd42133, XXd42132, XELb, XXd42131, XXd42130, XaCode, XXd42129, XSLb, XCode, XXd42128, XVs, XAlloc):- 'lo.comp.code.clause@or218'(XXd42128, XVs, XAlloc),
    !,
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iDealloc', 'lo.core#,..'('lo.comp.code.instructions#iGcmap'(0, 0), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))), XXd42136),
    XCode = XXd42136.
'lo.comp.code.clause@cond505'(XXd42142, XXd42141, XXd42140, XXd42139, XXd42138, XXd42137, XXd42136, XXd42135, XXd42134, XXd42133, XXd42132, XELb, XXd42131, XXd42130, XaCode, XXd42129, XSLb, XCode, XXd42128, XVs, XAlloc):- 'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), 'lo.core#,..'('lo.comp.code.instructions#iSucc', 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]')), XXd42142),
    XCode = XXd42142.
'lo.comp.code.clause@cond506'(XXd42148, XXd42147, XXd42146, XXd42145, XbCode, XXd42144, XXd42143, XXd42142, XXd42141, XXd42140, XXd42139, XXd42138, XXd42137, XXd42136, XXd42135, XXd42134, XXd42133, XXd42132, XELb, XXd42131, XXd42130, XaCode, XXd42129, XSLb, XCode, XXd42128, XVs, XAlloc, XLast):- XLast = 'lo.core#true',
    !,
    'lo.comp.code.clause@cond505'(XXd42142, XXd42141, XXd42140, XXd42139, XXd42138, XXd42137, XXd42136, XXd42135, XXd42134, XXd42133, XXd42132, XELb, XXd42131, XXd42130, XaCode, XXd42129, XSLb, XCode, XXd42128, XVs, XAlloc).
'lo.comp.code.clause@cond506'(XXd42148, XXd42147, XXd42146, XXd42145, XbCode, XXd42144, XXd42143, XXd42142, XXd42141, XXd42140, XXd42139, XXd42138, XXd42137, XXd42136, XXd42135, XXd42134, XXd42133, XXd42132, XELb, XXd42131, XXd42130, XaCode, XXd42129, XSLb, XCode, XXd42128, XVs, XAlloc, XLast):- 'lo.list@<>'(XbCode, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'), XXd42147),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), XXd42147, XXd42148),
    XCode = XXd42148.
'lo.comp.code.clause@or219'(XXd42153, XVs, XAlloc):- XAlloc = 'lo.core#true'.
'lo.comp.code.clause@or219'(XXd42153, XVs, XAlloc):- 'lo.comp.code.registers@maxLocals'(XVs, XXd42153),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd42153, 0).
'lo.comp.code.clause@cond507'(XXd42169, XXd42168, XXd42167, XXd42166, XXd42165, XXd42164, XXd42163, XXd42162, XXd42161, XXd42160, XXd42159, XXd42158, XXd42157, XELb, XXd42156, XbCode, XXd42155, XaCode, XXd42154, XSLb, XCode, XXd42153, XVs, XAlloc):- 'lo.comp.code.clause@or219'(XXd42153, XVs, XAlloc),
    !,
    'lo.list@<>'(XbCode, 'lo.core#,..'('lo.comp.code.instructions#iDealloc', 'lo.core#,..'('lo.comp.code.instructions#iGcmap'(0, 0), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'))), XXd42161),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), XXd42161, XXd42162),
    XCode = XXd42162.
'lo.comp.code.clause@cond507'(XXd42169, XXd42168, XXd42167, XXd42166, XXd42165, XXd42164, XXd42163, XXd42162, XXd42161, XXd42160, XXd42159, XXd42158, XXd42157, XELb, XXd42156, XbCode, XXd42155, XaCode, XXd42154, XSLb, XCode, XXd42153, XVs, XAlloc):- 'lo.list@<>'(XbCode, 'lo.core#,..'('lo.comp.code.instructions#iSucc', 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]')), XXd42168),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), XXd42168, XXd42169),
    XCode = XXd42169.
'lo.comp.code.clause@cond508'(XXd42175, XXd42174, XXd42173, XXd42172, XXd42171, XXd42170, XXd42169, XXd42168, XXd42167, XXd42166, XXd42165, XXd42164, XXd42163, XXd42162, XXd42161, XXd42160, XXd42159, XXd42158, XXd42157, XELb, XXd42156, XbCode, XXd42155, XaCode, XXd42154, XSLb, XCode, XXd42153, XVs, XAlloc, XLast):- XLast = 'lo.core#true',
    !,
    'lo.comp.code.clause@cond507'(XXd42169, XXd42168, XXd42167, XXd42166, XXd42165, XXd42164, XXd42163, XXd42162, XXd42161, XXd42160, XXd42159, XXd42158, XXd42157, XELb, XXd42156, XbCode, XXd42155, XaCode, XXd42154, XSLb, XCode, XXd42153, XVs, XAlloc).
'lo.comp.code.clause@cond508'(XXd42175, XXd42174, XXd42173, XXd42172, XXd42171, XXd42170, XXd42169, XXd42168, XXd42167, XXd42166, XXd42165, XXd42164, XXd42163, XXd42162, XXd42161, XXd42160, XXd42159, XXd42158, XXd42157, XELb, XXd42156, XbCode, XXd42155, XaCode, XXd42154, XSLb, XCode, XXd42153, XVs, XAlloc, XLast):- 'lo.list@<>'(XbCode, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XELb), 'lo.core#[]'), XXd42174),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XSLb), XaCode), XXd42174, XXd42175),
    XCode = XXd42175.
'lo.comp.code.clause^compGoal'('_call%13'(XV33420, XV33421, XV33422, XV33423, XV33424, XV33425, XV33426, XV33427, XV33428, XV33429, XV33430, XV33431, XV33432), 'lo.comp.code.clause^compGoal', _):- 'lo.comp.code.clause@compGoal'(XV33420, XV33421, XV33422, XV33423, XV33424, XV33425, XV33426, XV33427, XV33428, XV33429, XV33430, XV33431, XV33432).
'lo.comp.code.clause^compPart'('_call%13'(XV33433, XV33434, XV33435, XV33436, XV33437, XV33438, XV33439, XV33440, XV33441, XV33442, XV33443, XV33444, XV33445), 'lo.comp.code.clause^compPart', _):- 'lo.comp.code.clause@compPart'(XV33433, XV33434, XV33435, XV33436, XV33437, XV33438, XV33439, XV33440, XV33441, XV33442, XV33443, XV33444, XV33445).
'lo.comp.code.clause@or220'(XXd42191, XVsx, XAllo):- XAllo = 'lo.core#true'.
'lo.comp.code.clause@or220'(XXd42191, XVsx, XAllo):- 'lo.comp.code.registers@maxLocals'(XVsx, XXd42191),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd42191, 0).
'lo.comp.code.clause@cond509'(XXd42200, XXd42199, XXd42198, XXd42197, XXd42196, XXd42195, XXd42194, XXd42193, XfCode, XXd42192, XlSize, XCode, X_36826, XPre, XMx, XVs0, XXd42191, XVsx, XAllo):- 'lo.comp.code.clause@or220'(XXd42191, XVsx, XAllo),
    !,
    'lo.comp.code.registers@gcMap'(XVs0, XMx, XPre, X_36826),
    'lo.list@<>'(XPre, 'lo.core#,..'('lo.comp.code.instructions#iGc'(XMx, XlSize), XfCode), XXd42194),
    XCode = XXd42194.
'lo.comp.code.clause@cond509'(XXd42200, XXd42199, XXd42198, XXd42197, XXd42196, XXd42195, XXd42194, XXd42193, XfCode, XXd42192, XlSize, XCode, X_36826, XPre, XMx, XVs0, XXd42191, XVsx, XAllo):- 'lo.comp.code.registers@usedRegs'(XVs0, XXd42195),
    'lo.comp.code.registers@clearAs'(1, XMx, XXd42195, XXd42196),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iGc'(XMx, XlSize), 'lo.core#[]'), XfCode, XXd42199),
    'lo.list@<>'(XXd42196, XXd42199, XXd42200),
    XCode = XXd42200.
'lo.comp.code.clause@cond510'(XXd42200, XXd42199, XXd42198, XXd42197, XXd42196, XXd42195, XXd42194, XXd42193, XfCode, XXd42192, XCode, X_36826, XPre, XMx, XVs0, XXd42191, XVsx, XAllo, XlSize):- 'lo.core@>'('lo.core$comp$lo.core*integer', XlSize, 0),
    !,
    'lo.comp.code.clause@cond509'(XXd42200, XXd42199, XXd42198, XXd42197, XXd42196, XXd42195, XXd42194, XXd42193, XfCode, XXd42192, XlSize, XCode, X_36826, XPre, XMx, XVs0, XXd42191, XVsx, XAllo).
'lo.comp.code.clause@cond510'(XXd42200, XXd42199, XXd42198, XXd42197, XXd42196, XXd42195, XXd42194, XXd42193, XfCode, XXd42192, XCode, X_36826, XPre, XMx, XVs0, XXd42191, XVsx, XAllo, XlSize):- XCode = XfCode.
'lo.comp.code.clause@cond511'(XXd42206, XXd42205, XXd42204, XMx, XPre, XgCode, XlSize):- 'lo.core@>'('lo.core$comp$lo.core*integer', XlSize, 0),
    !,
    'lo.list@<>'(XPre, 'lo.core#,..'('lo.comp.code.instructions#iGc'(XMx, XlSize), 'lo.core#[]'), XXd42206),
    XgCode = XXd42206.
'lo.comp.code.clause@cond511'(XXd42206, XXd42205, XXd42204, XMx, XPre, XgCode, XlSize):- XgCode = 'lo.core#[]'.
'lo.comp.code.clause^compBodyParts'('_call%12'(XV33446, XV33447, XV33448, XV33449, XV33450, XV33451, XV33452, XV33453, XV33454, XV33455, XV33456, XV33457), 'lo.comp.code.clause^compBodyParts', _):- 'lo.comp.code.clause@compBodyParts'(XV33446, XV33447, XV33448, XV33449, XV33450, XV33451, XV33452, XV33453, XV33454, XV33455, XV33456, XV33457).
'lo.comp.code.clause@neg352'(XXd42210, XLcls, XRg):- ocall('in%2'(XRg, XLcls),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer'),'lo.collection$membership$lo.sets*set'('lo.core$equality$lo.core*integer')),
    !,
    fail.
'lo.comp.code.clause@neg352'(XXd42210, XLcls, XRg).
'lo.comp.code.clause^clearLocals'('_call%4'(XV33458, XV33459, XV33460, XV33461), 'lo.comp.code.clause^clearLocals', _):- 'lo.comp.code.clause@clearLocals'(XV33458, XV33459, XV33460, XV33461).
'lo.comp.code.clause@condExp137'('lo.core#true', XParts):- ocall('isEmpty%1'(XParts),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    !.
'lo.comp.code.clause@condExp137'('lo.core#false', XParts).
'lo.comp.code.clause@or221'(XXe5233, XXV5656, XParts, XPermSize):- 'lo.core@>'('lo.core$comp$lo.core*integer', XPermSize, 0).
'lo.comp.code.clause@or221'(XXe5233, XXV5656, XParts, XPermSize):- ocall('size%1'(XXV5656),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XParts, XXe5233),XXV5656,XXV5656),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXe5233, 1).
'lo.comp.code.clause@condExp138'('lo.core#true', XXe5233, XXV5656, XParts, XPermSize):- 'lo.comp.code.clause@or221'(XXe5233, XXV5656, XParts, XPermSize),
    !.
'lo.comp.code.clause@condExp138'('lo.core#false', XXe5233, XXV5656, XParts, XPermSize).
'lo.comp.code.clause@condExp139'('lo.core#,..'('lo.comp.code.instructions#iGc'(XArity, XhSize), 'lo.core#[]'), XXd42228, XXd42227, XArity, XhSize):- 'lo.core@>'('lo.core$comp$lo.core*integer', XhSize, 0),
    !.
'lo.comp.code.clause@condExp139'('lo.core#[]', XXd42228, XXd42227, XArity, XhSize).
'lo.comp.code.clause@condExp140'('lo.core#,..'('lo.comp.code.instructions#iAlloc'(XArity, XlSize), 'lo.core#,..'('lo.comp.code.instructions#iGcmap'(XArity, XlSize), XXd42232)), XXd42234, XXd42233, XXd42232, XXd42231, XVs0, XXd42230, XXd42229, XlSize, XArity, Xallocating):- Xallocating = 'lo.core#true',
    !,
    'lo.comp.code.registers@usedLocals'(XVs0, XXd42231),
    'lo.comp.code.clause@clearLocals'(XXd42231, 1, XlSize, XXd42232).
'lo.comp.code.clause@condExp140'('lo.core#[]', XXd42234, XXd42233, XXd42232, XXd42231, XVs0, XXd42230, XXd42229, XlSize, XArity, Xallocating).
'lo.comp.code.clause^compCl'('_call%4'(XV33462, XV33463, XV33464, XV33465), 'lo.comp.code.clause^compCl', _):- 'lo.comp.code.clause@compCl'(XV33462, XV33463, XV33464, XV33465).
