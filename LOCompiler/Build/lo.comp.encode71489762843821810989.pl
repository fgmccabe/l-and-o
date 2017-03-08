'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.encode's'0.0.1'n7o7'()7'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.base64'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.parseutils'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.asm'e'*'s\"I5'encodeTerm'FT1t'lo.comp.term*term'Li'encodeType'FT1t'lo.comp.types*tipe'Li'encodeConstraint'FT1t'lo.comp.types*constraint'Li'packageSig'FT1t'lo.comp.package*pkgSpec't'lo.comp.term*term''encMdl'FT1t'lo.comp.code.asm*codeMdl'Li\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.encode@init'():- !.
'lo.comp.encode@encodeQuoted'('lo.core#[]', XDelim, XL, 'lo.core#,..'(XDelim, XL)):- !.
'lo.comp.encode@encodeQuoted'('lo.core#,..'(92, XM), XDelim, XL, 'lo.core#,..'(92, 'lo.core#,..'(92, XXd40868))):- !,
    'lo.comp.encode@encodeQuoted'(XM, XDelim, XL, XXd40868).
'lo.comp.encode@encodeQuoted'('lo.core#,..'(XDelim, XM), XDelim, XL, 'lo.core#,..'(92, 'lo.core#,..'(XDelim, XXd40871))):- !,
    'lo.comp.encode@encodeQuoted'(XM, XDelim, XL, XXd40871).
'lo.comp.encode@encodeQuoted'('lo.core#,..'(XCh, XM), XDelim, XL, 'lo.core#,..'(XCh, XXd40874)):- !,
    'lo.comp.encode@encodeQuoted'(XM, XDelim, XL, XXd40874).
'lo.comp.encode@encodeQuoted'(_, _, _, _):- raise_exception('error'("lo.comp.encode@encodeQuoted", 49, 3, 39)).
'lo.comp.encode@encodeChars'(XChars, XDelim, XL, 'lo.core#,..'(XDelim, XXd40876)):- !,
    'lo.comp.encode@encodeQuoted'(XChars, XDelim, XL, XXd40876).
'lo.comp.encode@encodeChars'(_, _, _, _):- raise_exception('error'("lo.comp.encode@encodeChars", 46, 3, 67)).
'lo.comp.encode@findDelim'(XChrs, XDelim):- ocall('in%2'(XDelim, 'lo.core#,..'(39, 'lo.core#,..'(34, 'lo.core#,..'(124, 'lo.core#,..'(47, 'lo.core#,..'(37, 'lo.core#[]')))))),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.encode@neg343'(XChrs, XDelim).
'lo.comp.encode@findDelim'(X_36178, 34).
'lo.comp.encode@encodeText'(XNm, XL, XXd40883):- 'explode'(XNm, XXc540),
    XChrs = XXc540,
    'lo.comp.encode@one313'(XDelim, XChrs),
    !,
    'lo.comp.encode@encodeChars'(XChrs, XDelim, XL, XXd40883).
'lo.comp.encode@encodeText'(_, _, _):- raise_exception('error'("lo.comp.encode@encodeText", 37, 3, 91)).
'lo.comp.encode@encFlot'(XDx, XL, XXd40884):- !,
    ocall('_coerce%1'(XXV5559),'lo.coerce$coercion$lo.core*float$lo.core*string','lo.coerce$coercion$lo.core*float$lo.core*string'),
    ocall('_call%2'(XDx, XXe5160),XXV5559,XXV5559),
    'lo.comp.encode@encodeText'(XXe5160, XL, XXd40884).
'lo.comp.encode@encFlot'(_, _, _):- raise_exception('error'("lo.comp.encode@encFlot", 34, 3, 41)).
'lo.comp.encode@encInt'(XN, XL, 'lo.core#,..'(45, XXd40885)):- ocall('<%2'(XN, 0),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    !,
    ocall('-%1'(XXV5561),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('zero%1'(XXV5560),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XXV5560, XN, XXe5161),XXV5561,XXV5561),
    'lo.comp.encode@encInt'(XXe5161, XL, XXd40885).
'lo.comp.encode@encInt'(XN, XL, 'lo.core#,..'(XD, XL)):- 'lo.comp.parseutils@digitVal'(XD, XN),
    ocall('>=%2'(XN, 0),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    'lo.core@=<'('lo.core$comp$lo.core*integer', XN, 9),
    !.
'lo.comp.encode@encInt'(XN, XL, XXd40889):- ocall('>=%2'(XN, 10),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    ocall('%%1'(XXV5562),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('_call%3'(XN, 10, XXe5162),XXV5562,XXV5562),
    'lo.comp.parseutils@digitVal'(XD, XXe5162),
    !,
    ocall('/%1'(XXV5563),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('_call%3'(XN, 10, XXe5163),XXV5563,XXV5563),
    'lo.comp.encode@encInt'(XXe5163, 'lo.core#,..'(XD, XL), XXd40889).
'lo.comp.encode@encInt'(_, _, _):- raise_exception('error'("lo.comp.encode@encInt", 24, 3, 42)).
'lo.comp.encode@encEls'('lo.core#[]', XL, XL):- !.
'lo.comp.encode@encEls'('lo.core#,..'(XE, XM), XL, XXd40891):- !,
    'lo.comp.encode@encEls'(XM, XL, XXd40890),
    'lo.comp.encode@encTerm'(XE, XXd40890, XXd40891).
'lo.comp.encode@encEls'(_, _, _):- raise_exception('error'("lo.comp.encode@encEls", 29, 3, 17)).
'lo.comp.encode@encTerm'('lo.comp.term#anon', XL, 'lo.core#,..'(97, XL)):- !.
'lo.comp.encode@encTerm'('lo.comp.term#intgr'(XIx), XL, 'lo.core#,..'(120, XXd40893)):- !,
    'lo.comp.encode@encInt'(XIx, XL, XXd40893).
'lo.comp.encode@encTerm'('lo.comp.term#flot'(XDx), XL, 'lo.core#,..'(100, XXd40895)):- !,
    'lo.comp.encode@encFlot'(XDx, XL, XXd40895).
'lo.comp.encode@encTerm'('lo.comp.term#enum'(XNm), XL, 'lo.core#,..'(101, XXd40897)):- !,
    'lo.comp.encode@encodeText'(XNm, XL, XXd40897).
'lo.comp.encode@encTerm'('lo.comp.term#strng'(XSx), XL, 'lo.core#,..'(115, XXd40899)):- !,
    'lo.comp.encode@encodeText'(XSx, XL, XXd40899).
'lo.comp.encode@encTerm'('lo.comp.term#strct'(XNm, XAr), XL, 'lo.core#,..'(111, XXd40902)):- !,
    'lo.comp.encode@encodeText'(XNm, XL, XXd40901),
    'lo.comp.encode@encInt'(XAr, XXd40901, XXd40902).
'lo.comp.encode@encTerm'('lo.comp.term#prg'(XNm, XAr), XL, 'lo.core#,..'(112, XXd40905)):- !,
    'lo.comp.encode@encodeText'(XNm, XL, XXd40904),
    'lo.comp.encode@encInt'(XAr, XXd40904, XXd40905).
'lo.comp.encode@encTerm'('lo.comp.term#cons'(XOp, XEls), XL, 'lo.core#,..'(110, XXd40910)):- !,
    'lo.list@length'(XEls, XXd40907),
    'lo.comp.encode@encEls'(XEls, XL, XXd40908),
    'lo.comp.encode@encTerm'(XOp, XXd40908, XXd40909),
    'lo.comp.encode@encInt'(XXd40907, XXd40909, XXd40910).
'lo.comp.encode@encTerm'(_, _, _):- raise_exception('error'("lo.comp.encode@encTerm", 14, 3, 28)).
'lo.comp.encode@encodeTerm'(XT, XXd40912):- !,
    'lo.comp.encode@encTerm'(XT, 'lo.core#[]', XXd40912).
'lo.comp.encode@encodeTerm'(_, _):- raise_exception('error'("lo.comp.encode@encodeTerm", 11, 3, 30)).
'lo.comp.encode@encTps'('lo.core#[]', XL, XL):- !.
'lo.comp.encode@encTps'('lo.core#,..'(XT, XM), XL, XXd40914):- !,
    'lo.comp.encode@encTps'(XM, XL, XXd40913),
    'lo.comp.encode@encType'(XT, XXd40913, XXd40914).
'lo.comp.encode@encTps'(_, _, _):- raise_exception('error'("lo.comp.encode@encTps", 85, 3, 17)).
'lo.comp.encode@encTypes'(XEls, XL, XXd40917):- !,
    'lo.list@length'(XEls, XXd40915),
    'lo.comp.encode@encTps'(XEls, XL, XXd40916),
    'lo.comp.encode@encInt'(XXd40915, XXd40916, XXd40917).
'lo.comp.encode@encTypes'(_, _, _):- raise_exception('error'("lo.comp.encode@encTypes", 82, 3, 52)).
'lo.comp.encode@encFieldTps'('lo.core#[]', XL, XL):- !.
'lo.comp.encode@encFieldTps'('lo.core#,..'('()2'(XFld, XTp), XM), XL, XXd40920):- !,
    'lo.comp.encode@encFieldTps'(XM, XL, XXd40918),
    'lo.comp.encode@encType'(XTp, XXd40918, XXd40919),
    'lo.comp.encode@encodeText'(XFld, XXd40919, XXd40920).
'lo.comp.encode@encFieldTps'(_, _, _):- raise_exception('error'("lo.comp.encode@encFieldTps", 92, 3, 22)).
'lo.comp.encode@encFieldTypes'(XF, XL, XXd40923):- !,
    'lo.list@length'(XF, XXd40921),
    'lo.comp.encode@encFieldTps'(XF, XL, XXd40922),
    'lo.comp.encode@encInt'(XXd40921, XXd40922, XXd40923).
'lo.comp.encode@encFieldTypes'(_, _, _):- raise_exception('error'("lo.comp.encode@encFieldTypes", 89, 3, 56)).
'lo.comp.encode@encConstraint'('lo.comp.types#univCon'(XV, XC), XL, 'lo.core#,..'(58, XXd40925)):- !,
    'lo.comp.encode@encConstraint'(XC, XL, XXd40924),
    'lo.comp.encode@encType'(XV, XXd40924, XXd40925).
'lo.comp.encode@encConstraint'('lo.comp.types#conTract'(XNm, XArgs, XDeps), XL, 'lo.core#,..'(99, XXd40931)):- !,
    'lo.comp.encode@encType'('lo.comp.types#tupleType'(XDeps), XL, XXd40929),
    'lo.comp.encode@encType'('lo.comp.types#tupleType'(XArgs), XXd40929, XXd40930),
    'lo.comp.encode@encodeText'(XNm, XXd40930, XXd40931).
'lo.comp.encode@encConstraint'('lo.comp.types#implementsFace'(XTp, XFlds), XL, 'lo.core#,..'(97, XXd40935)):- !,
    'lo.comp.encode@encType'('lo.comp.types#faceType'(XFlds), XL, XXd40934),
    'lo.comp.encode@encType'(XTp, XXd40934, XXd40935).
'lo.comp.encode@encConstraint'('lo.comp.types#conCon'(XC, XE), XL, 'lo.core#,..'(124, XXd40938)):- !,
    'lo.comp.encode@encConstraint'(XE, XL, XXd40937),
    'lo.comp.encode@encConstraint'(XC, XXd40937, XXd40938).
'lo.comp.encode@encConstraint'(_, _, _):- raise_exception('error'("lo.comp.encode@encConstraint", 99, 3, 70)).
'lo.comp.encode@encType'('lo.comp.types#anonType', XL, 'lo.core#,..'(95, XL)):- !.
'lo.comp.encode@encType'('lo.comp.types#voidType', XL, 'lo.core#,..'(118, XL)):- !.
'lo.comp.encode@encType'('lo.comp.types#thisType', XL, 'lo.core#,..'(104, XL)):- !.
'lo.comp.encode@encType'('lo.comp.types#tipe'("lo.core*logical"), XL, 'lo.core#,..'(108, XL)):- !.
'lo.comp.encode@encType'('lo.comp.types#tipe'("lo.core*integer"), XL, 'lo.core#,..'(105, XL)):- !.
'lo.comp.encode@encType'('lo.comp.types#tipe'("lo.core*float"), XL, 'lo.core#,..'(102, XL)):- !.
'lo.comp.encode@encType'('lo.comp.types#tipe'("lo.core*string"), XL, 'lo.core#,..'(83, XL)):- !.
'lo.comp.encode@encType'('lo.comp.types#tpFun'(XNm, XAr), XL, 'lo.core#,..'(122, XXd40948)):- !,
    'lo.comp.encode@encodeText'(XNm, XL, XXd40947),
    'lo.comp.encode@encInt'(XAr, XXd40947, XXd40948).
'lo.comp.encode@encType'('lo.comp.types#typeExp'(XOp, 'lo.core#,..'(XE, 'lo.core#[]')), XL, 'lo.core#,..'(76, XXd40952)):- 'lo.comp.types@deRef'(XOp, XXd40950),
    XXd40950 = 'lo.comp.types#tpFun'("lo.core*list", 1),
    !,
    'lo.comp.encode@encType'(XE, XL, XXd40952).
'lo.comp.encode@encType'('lo.comp.types#kVar'(XNm), XL, 'lo.core#,..'(107, XXd40954)):- !,
    'lo.comp.encode@encodeText'(XNm, XL, XXd40954).
'lo.comp.encode@encType'('lo.comp.types#kFun'(XNm, XAr), XL, 'lo.core#,..'(75, XXd40957)):- !,
    'lo.comp.encode@encodeText'(XNm, XL, XXd40956),
    'lo.comp.encode@encInt'(XAr, XXd40956, XXd40957).
'lo.comp.encode@encType'('lo.comp.types#tipe'(XNm), XL, 'lo.core#,..'(116, XXd40959)):- !,
    'lo.comp.encode@encodeText'(XNm, XL, XXd40959).
'lo.comp.encode@encType'('lo.comp.types#typeExp'(XOp, XEls), XL, 'lo.core#,..'(85, XXd40963)):- !,
    'lo.comp.types@deRef'(XOp, XXd40961),
    'lo.comp.encode@encTypes'(XEls, XL, XXd40962),
    'lo.comp.encode@encType'(XXd40961, XXd40962, XXd40963).
'lo.comp.encode@encType'('lo.comp.types#tupleType'(XEls), XL, 'lo.core#,..'(84, XXd40965)):- !,
    'lo.comp.encode@encTypes'(XEls, XL, XXd40965).
'lo.comp.encode@encType'('lo.comp.types#funType'(XA, XR), XL, 'lo.core#,..'(70, XXd40968)):- !,
    'lo.comp.encode@encType'(XR, XL, XXd40967),
    'lo.comp.encode@encType'(XA, XXd40967, XXd40968).
'lo.comp.encode@encType'('lo.comp.types#classType'(XA, XR), XL, 'lo.core#,..'(67, XXd40971)):- !,
    'lo.comp.encode@encType'(XR, XL, XXd40970),
    'lo.comp.encode@encType'(XA, XXd40970, XXd40971).
'lo.comp.encode@encType'('lo.comp.types#predType'(XA), XL, 'lo.core#,..'(80, XXd40973)):- !,
    'lo.comp.encode@encType'(XA, XL, XXd40973).
'lo.comp.encode@encType'('lo.comp.types#grammarType'(XA, XR), XL, 'lo.core#,..'(71, XXd40976)):- !,
    'lo.comp.encode@encType'(XR, XL, XXd40975),
    'lo.comp.encode@encType'(XA, XXd40975, XXd40976).
'lo.comp.encode@encType'('lo.comp.types#typeRule'(XA, XR), XL, 'lo.core#,..'(89, XXd40979)):- !,
    'lo.comp.encode@encType'(XR, XL, XXd40978),
    'lo.comp.encode@encType'(XA, XXd40978, XXd40979).
'lo.comp.encode@encType'('lo.comp.types#faceType'(XEls), XL, 'lo.core#,..'(73, XXd40981)):- !,
    'lo.comp.encode@encFieldTypes'(XEls, XL, XXd40981).
'lo.comp.encode@encType'('lo.comp.types#constrained'(XT, XC), XL, 'lo.core#,..'(124, XXd40984)):- !,
    'lo.comp.encode@encConstraint'(XC, XL, XXd40983),
    'lo.comp.encode@encType'(XT, XXd40983, XXd40984).
'lo.comp.encode@encType'('lo.comp.types#univType'(XV, XT), XL, 'lo.core#,..'(58, XXd40987)):- !,
    'lo.comp.encode@encType'(XT, XL, XXd40986),
    'lo.comp.encode@encType'(XV, XXd40986, XXd40987).
'lo.comp.encode@encType'(_, _, _):- raise_exception('error'("lo.comp.encode@encType", 58, 3, 32)).
'lo.comp.encode@encodeType'(XTp, XXd40989):- !,
    'lo.comp.encode@encType'(XTp, 'lo.core#[]', XXd40989).
'lo.comp.encode@encodeType'(_, _):- raise_exception('error'("lo.comp.encode@encodeType", 55, 3, 32)).
'lo.comp.encode@encodeConstraint'(XCon, XXd40990):- !,
    'lo.comp.encode@encConstraint'(XCon, 'lo.core#[]', XXd40990).
'lo.comp.encode@encodeConstraint'(_, _):- raise_exception('error'("lo.comp.encode@encodeConstraint", 96, 3, 46)).
'lo.comp.encode@fmtConstraint'(XCon, 'lo.comp.term#strng'(XXc541)):- !,
    'lo.comp.encode@encodeConstraint'(XCon, XXd40991),
    'implode'(XXd40991, XXc541).
'lo.comp.encode@fmtConstraint'(_, _):- raise_exception('error'("lo.comp.encode@fmtConstraint", 152, 3, 59)).
'lo.comp.encode@fmtImpl'('lo.comp.types#implEntry'(XNm, XCon), XXd40997):- !,
    'lo.comp.encode@fmtConstraint'(XCon, XXd40994),
    'lo.comp.term@mkTpl'('lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'(XXd40994, 'lo.core#[]')), XXd40997).
'lo.comp.encode@fmtImpl'(_, _):- raise_exception('error'("lo.comp.encode@fmtImpl", 146, 3, 67)).
'lo.comp.encode@fmtImpls'(XL, XXd40998):- !,
    ocall('//%1'(XXV5564),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%3'(XL, 'lo.comp.encode^fmtImpl', XXe5164),XXV5564,XXV5564),
    'lo.comp.term@mkTpl'(XXe5164, XXd40998).
'lo.comp.encode@fmtImpls'(_, _):- raise_exception('error'("lo.comp.encode@fmtImpls", 143, 3, 32)).
'lo.comp.encode@fmtType'(XT, 'lo.comp.term#strng'(XXc542)):- !,
    'lo.comp.encode@encodeType'(XT, XXd40999),
    'implode'(XXd40999, XXc542).
'lo.comp.encode@fmtType'(_, _):- raise_exception('error'("lo.comp.encode@fmtType", 149, 3, 43)).
'lo.comp.encode@fmtContract'('lo.comp.types#conEntry'(XNm, XConNm, XCon, XFace), XXd41009):- !,
    'lo.comp.encode@fmtConstraint'(XCon, XXd41003),
    'lo.comp.encode@fmtType'(XFace, XXd41004),
    'lo.comp.term@mkTpl'('lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#strng'(XConNm), 'lo.core#,..'(XXd41003, 'lo.core#,..'(XXd41004, 'lo.core#[]')))), XXd41009).
'lo.comp.encode@fmtContract'(_, _):- raise_exception('error'("lo.comp.encode@fmtContract", 140, 3, 108)).
'lo.comp.encode@fmtContracts'(XL, XXd41010):- !,
    ocall('//%1'(XXV5565),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%3'(XL, 'lo.comp.encode^fmtContract', XXe5165),XXV5565,XXV5565),
    'lo.comp.term@mkTpl'(XXe5165, XXd41010).
'lo.comp.encode@fmtContracts'(_, _):- raise_exception('error'("lo.comp.encode@fmtContracts", 137, 3, 40)).
'lo.comp.encode@fmtEnum'(XE, 'lo.comp.term#strng'(XE)):- !.
'lo.comp.encode@fmtEnum'(_, _):- raise_exception('error'("lo.comp.encode@fmtEnum", 134, 3, 22)).
'lo.comp.encode@fmtEnums'(XEnums, XXd41012):- !,
    ocall('//%1'(XXV5566),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%3'(XEnums, 'lo.comp.encode^fmtEnum', XXe5166),XXV5566,XXV5566),
    'lo.comp.term@mkTpl'(XXe5166, XXd41012).
'lo.comp.encode@fmtEnums'(_, _):- raise_exception('error'("lo.comp.encode@fmtEnums", 131, 3, 40)).
'lo.comp.encode@fmtTypes'(XFields, XXd41014):- !,
    'lo.comp.encode@fmtType'('lo.comp.types#faceType'(XFields), XXd41014).
'lo.comp.encode@fmtTypes'(_, _):- raise_exception('error'("lo.comp.encode@fmtTypes", 128, 3, 45)).
'lo.comp.encode@fmtVer'('lo.repo#defltVersion', 'lo.comp.term#enum'("*")):- !.
'lo.comp.encode@fmtVer'('lo.repo#vers'(XV), 'lo.comp.term#strng'(XV)):- !.
'lo.comp.encode@fmtVer'(_, _):- raise_exception('error'("lo.comp.encode@fmtVer", 114, 3, 33)).
'lo.comp.encode@fmtPkg'('lo.repo#pkg'(XNm, XVers), 'lo.comp.term#cons'('lo.comp.term#strct'("pkg", 2), 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'(XXd41019, 'lo.core#[]')))):- !,
    'lo.comp.encode@fmtVer'(XVers, XXd41019).
'lo.comp.encode@fmtPkg'(_, _):- raise_exception('error'("lo.comp.encode@fmtPkg", 111, 3, 69)).
'lo.comp.encode@fmtViz'('lo.comp.package#priVate', 'lo.comp.term#enum'("private")):- !.
'lo.comp.encode@fmtViz'('lo.comp.package#pUblic', 'lo.comp.term#enum'("public")):- !.
'lo.comp.encode@fmtViz'(_, _):- raise_exception('error'("lo.comp.encode@fmtViz", 124, 3, 34)).
'lo.comp.encode@fmtImport'('()2'(XViz, XPkg), 'lo.comp.term#cons'('lo.comp.term#strct'("import", 2), 'lo.core#,..'(XXd41026, 'lo.core#,..'(XXd41027, 'lo.core#[]')))):- !,
    'lo.comp.encode@fmtViz'(XViz, XXd41026),
    'lo.comp.encode@fmtPkg'(XPkg, XXd41027).
'lo.comp.encode@fmtImport'(_, _):- raise_exception('error'("lo.comp.encode@fmtImport", 121, 3, 73)).
'lo.comp.encode@fmtImports'(XImports, XXd41031):- !,
    ocall('//%1'(XXV5567),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%3'(XImports, 'lo.comp.encode^fmtImport', XXe5167),XXV5567,XXV5567),
    'lo.comp.term@mkTpl'(XXe5167, XXd41031).
'lo.comp.encode@fmtImports'(_, _):- raise_exception('error'("lo.comp.encode@fmtImports", 118, 3, 48)).
'lo.comp.encode@packageSig'('lo.comp.package#pkgSpec'(XPkg, XFields, XTypes, XEnums, XContracts, XImpls, XImports), 'lo.comp.term#strng'(XXc543)):- !,
    'lo.comp.encode@fmtPkg'(XPkg, XXd41032),
    'lo.comp.encode@fmtImports'(XImports, XXd41033),
    'lo.comp.encode@fmtTypes'(XFields, XXd41034),
    'lo.comp.encode@fmtTypes'(XTypes, XXd41035),
    'lo.comp.encode@fmtEnums'(XEnums, XXd41036),
    'lo.comp.encode@fmtContracts'(XContracts, XXd41037),
    'lo.comp.encode@fmtImpls'(XImpls, XXd41038),
    'lo.comp.term@mkTpl'('lo.core#,..'(XXd41032, 'lo.core#,..'(XXd41033, 'lo.core#,..'(XXd41034, 'lo.core#,..'(XXd41035, 'lo.core#,..'(XXd41036, 'lo.core#,..'(XXd41037, 'lo.core#,..'(XXd41038, 'lo.core#[]'))))))), XXd41046),
    'lo.comp.encode@encodeTerm'(XXd41046, XXd41047),
    'implode'(XXd41047, XXc543).
'lo.comp.encode@packageSig'(_, _):- raise_exception('error'("lo.comp.encode@packageSig", 105, 3, 248)).
'lo.comp.encode@fmtIns'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.encode@fmtIns'('lo.core#,..'(XI, XL), 'lo.core#,..'(XXd41050, 'lo.core#,..'(XXd41052, 'lo.core#,..'(XXd41054, 'lo.core#,..'(XXd41055, XXd41056))))):- !,
    'lo.bits@.>>.'(XI, 24, XXd41049),
    'lo.bits@.&.'(XXd41049, 255, XXd41050),
    'lo.bits@.>>.'(XI, 16, XXd41051),
    'lo.bits@.&.'(XXd41051, 255, XXd41052),
    'lo.bits@.>>.'(XI, 8, XXd41053),
    'lo.bits@.&.'(XXd41053, 255, XXd41054),
    'lo.bits@.&.'(XI, 255, XXd41055),
    'lo.comp.encode@fmtIns'(XL, XXd41056).
'lo.comp.encode@fmtIns'(_, _):- raise_exception('error'("lo.comp.encode@fmtIns", 156, 3, 16)).
'lo.comp.encode@LOSig'(16975111):- !.
'lo.comp.encode@encCodeSeg'('lo.comp.code.asm#codeSeg'(XNm, XCode, XLits, XSrcMap), XL, 'lo.core#,..'(110, XXd41073)):- !,
    'lo.comp.encode@LOSig'(XLOSig7),
    'lo.comp.encode@fmtIns'('lo.core#,..'(XLOSig7, XCode), XXd41063),
    'lo.comp.base64@encode64'(XXd41063, XXd41064),
    'lo.comp.term@mkTpl'(XLits, XXd41065),
    'lo.comp.term@mkTpl'(XSrcMap, XXd41066),
    'lo.comp.encode@encTerm'(XXd41066, XL, XXd41067),
    'lo.comp.encode@encTerm'(XXd41065, XXd41067, XXd41068),
    'lo.comp.encode@encodeChars'(XXd41064, 39, XXd41068, XXd41069),
    'lo.comp.encode@encTerm'(XNm, 'lo.core#,..'(115, XXd41069), XXd41071),
    'lo.comp.encode@encTerm'('lo.comp.term#strct'("#code", 4), XXd41071, XXd41072),
    'lo.comp.encode@encInt'(4, XXd41072, XXd41073).
'lo.comp.encode@encCodeSeg'(_, _, _):- raise_exception('error'("lo.comp.encode@encCodeSeg", 164, 3, 209)).
'lo.comp.encode@encSegs'('lo.core#[]', XL, XL):- !.
'lo.comp.encode@encSegs'('lo.core#,..'(XS, XR), XL, XXd41076):- !,
    'lo.comp.encode@encSegs'(XR, XL, XXd41075),
    'lo.comp.encode@encCodeSeg'(XS, XXd41075, XXd41076).
'lo.comp.encode@encSegs'(_, _, _):- raise_exception('error'("lo.comp.encode@encSegs", 168, 3, 18)).
'lo.comp.encode@encMdl'('lo.comp.code.asm#codeMdl'(XSpec, XSegs), XXd41079):- !,
    'lo.comp.encode@packageSig'(XSpec, XXd41077),
    'lo.comp.encode@encSegs'(XSegs, 'lo.core#[]', XXd41078),
    'lo.comp.encode@encTerm'(XXd41077, XXd41078, XXd41079).
'lo.comp.encode@encMdl'(_, _):- raise_exception('error'("lo.comp.encode@encMdl", 173, 3, 76)).
'lo.comp.encode^encodeQuoted'('_call%4'(XV32995, XV32996, XV32997, XV32998), 'lo.comp.encode^encodeQuoted', _):- 'lo.comp.encode@encodeQuoted'(XV32995, XV32996, XV32997, XV32998).
'lo.comp.encode^encodeChars'('_call%4'(XV32999, XV33000, XV33001, XV33002), 'lo.comp.encode^encodeChars', _):- 'lo.comp.encode@encodeChars'(XV32999, XV33000, XV33001, XV33002).
'lo.comp.encode@neg343'(XChrs, XDelim):- ocall('in%2'(XDelim, XChrs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.encode@neg343'(XChrs, XDelim).
'lo.comp.encode^findDelim'('_call%2'(XV33003, XV33004), 'lo.comp.encode^findDelim', _):- 'lo.comp.encode@findDelim'(XV33003, XV33004).
'lo.comp.encode@one313'(XDelim, XChrs):- 'lo.comp.encode@findDelim'(XChrs, XDelim),
    !.
'lo.comp.encode^encodeText'('_call%3'(XV33005, XV33006, XV33007), 'lo.comp.encode^encodeText', _):- 'lo.comp.encode@encodeText'(XV33005, XV33006, XV33007).
'lo.comp.encode^encFlot'('_call%3'(XV33008, XV33009, XV33010), 'lo.comp.encode^encFlot', _):- 'lo.comp.encode@encFlot'(XV33008, XV33009, XV33010).
'lo.comp.encode^encInt'('_call%3'(XV33011, XV33012, XV33013), 'lo.comp.encode^encInt', _):- 'lo.comp.encode@encInt'(XV33011, XV33012, XV33013).
'lo.comp.encode^encEls'('_call%3'(XV33014, XV33015, XV33016), 'lo.comp.encode^encEls', _):- 'lo.comp.encode@encEls'(XV33014, XV33015, XV33016).
'lo.comp.encode^encTerm'('_call%3'(XV33017, XV33018, XV33019), 'lo.comp.encode^encTerm', _):- 'lo.comp.encode@encTerm'(XV33017, XV33018, XV33019).
'lo.comp.encode^encodeTerm'('_call%2'(XV33020, XV33021), 'lo.comp.encode^encodeTerm', _):- 'lo.comp.encode@encodeTerm'(XV33020, XV33021).
'lo.comp.encode^encTps'('_call%3'(XV33022, XV33023, XV33024), 'lo.comp.encode^encTps', _):- 'lo.comp.encode@encTps'(XV33022, XV33023, XV33024).
'lo.comp.encode^encTypes'('_call%3'(XV33025, XV33026, XV33027), 'lo.comp.encode^encTypes', _):- 'lo.comp.encode@encTypes'(XV33025, XV33026, XV33027).
'lo.comp.encode^encFieldTps'('_call%3'(XV33028, XV33029, XV33030), 'lo.comp.encode^encFieldTps', _):- 'lo.comp.encode@encFieldTps'(XV33028, XV33029, XV33030).
'lo.comp.encode^encFieldTypes'('_call%3'(XV33031, XV33032, XV33033), 'lo.comp.encode^encFieldTypes', _):- 'lo.comp.encode@encFieldTypes'(XV33031, XV33032, XV33033).
'lo.comp.encode^encConstraint'('_call%3'(XV33034, XV33035, XV33036), 'lo.comp.encode^encConstraint', _):- 'lo.comp.encode@encConstraint'(XV33034, XV33035, XV33036).
'lo.comp.encode^encType'('_call%3'(XV33037, XV33038, XV33039), 'lo.comp.encode^encType', _):- 'lo.comp.encode@encType'(XV33037, XV33038, XV33039).
'lo.comp.encode^encodeType'('_call%2'(XV33040, XV33041), 'lo.comp.encode^encodeType', _):- 'lo.comp.encode@encodeType'(XV33040, XV33041).
'lo.comp.encode^encodeConstraint'('_call%2'(XV33042, XV33043), 'lo.comp.encode^encodeConstraint', _):- 'lo.comp.encode@encodeConstraint'(XV33042, XV33043).
'lo.comp.encode^fmtConstraint'('_call%2'(XV33044, XV33045), 'lo.comp.encode^fmtConstraint', _):- 'lo.comp.encode@fmtConstraint'(XV33044, XV33045).
'lo.comp.encode^fmtImpl'('_call%2'(XV33046, XV33047), 'lo.comp.encode^fmtImpl', _):- 'lo.comp.encode@fmtImpl'(XV33046, XV33047).
'lo.comp.encode^fmtImpls'('_call%2'(XV33048, XV33049), 'lo.comp.encode^fmtImpls', _):- 'lo.comp.encode@fmtImpls'(XV33048, XV33049).
'lo.comp.encode^fmtType'('_call%2'(XV33050, XV33051), 'lo.comp.encode^fmtType', _):- 'lo.comp.encode@fmtType'(XV33050, XV33051).
'lo.comp.encode^fmtContract'('_call%2'(XV33052, XV33053), 'lo.comp.encode^fmtContract', _):- 'lo.comp.encode@fmtContract'(XV33052, XV33053).
'lo.comp.encode^fmtContracts'('_call%2'(XV33054, XV33055), 'lo.comp.encode^fmtContracts', _):- 'lo.comp.encode@fmtContracts'(XV33054, XV33055).
'lo.comp.encode^fmtEnum'('_call%2'(XV33056, XV33057), 'lo.comp.encode^fmtEnum', _):- 'lo.comp.encode@fmtEnum'(XV33056, XV33057).
'lo.comp.encode^fmtEnums'('_call%2'(XV33058, XV33059), 'lo.comp.encode^fmtEnums', _):- 'lo.comp.encode@fmtEnums'(XV33058, XV33059).
'lo.comp.encode^fmtTypes'('_call%2'(XV33060, XV33061), 'lo.comp.encode^fmtTypes', _):- 'lo.comp.encode@fmtTypes'(XV33060, XV33061).
'lo.comp.encode^fmtVer'('_call%2'(XV33062, XV33063), 'lo.comp.encode^fmtVer', _):- 'lo.comp.encode@fmtVer'(XV33062, XV33063).
'lo.comp.encode^fmtPkg'('_call%2'(XV33064, XV33065), 'lo.comp.encode^fmtPkg', _):- 'lo.comp.encode@fmtPkg'(XV33064, XV33065).
'lo.comp.encode^fmtViz'('_call%2'(XV33066, XV33067), 'lo.comp.encode^fmtViz', _):- 'lo.comp.encode@fmtViz'(XV33066, XV33067).
'lo.comp.encode^fmtImport'('_call%2'(XV33068, XV33069), 'lo.comp.encode^fmtImport', _):- 'lo.comp.encode@fmtImport'(XV33068, XV33069).
'lo.comp.encode^fmtImports'('_call%2'(XV33070, XV33071), 'lo.comp.encode^fmtImports', _):- 'lo.comp.encode@fmtImports'(XV33070, XV33071).
'lo.comp.encode^packageSig'('_call%2'(XV33072, XV33073), 'lo.comp.encode^packageSig', _):- 'lo.comp.encode@packageSig'(XV33072, XV33073).
'lo.comp.encode^fmtIns'('_call%2'(XV33074, XV33075), 'lo.comp.encode^fmtIns', _):- 'lo.comp.encode@fmtIns'(XV33074, XV33075).
'lo.comp.encode^encCodeSeg'('_call%3'(XV33076, XV33077, XV33078), 'lo.comp.encode^encCodeSeg', _):- 'lo.comp.encode@encCodeSeg'(XV33076, XV33077, XV33078).
'lo.comp.encode^encSegs'('_call%3'(XV33079, XV33080, XV33081), 'lo.comp.encode^encSegs', _):- 'lo.comp.encode@encSegs'(XV33079, XV33080, XV33081).
'lo.comp.encode^encMdl'('_call%2'(XV33082, XV33083), 'lo.comp.encode^encMdl', _):- 'lo.comp.encode@encMdl'(XV33082, XV33083).
