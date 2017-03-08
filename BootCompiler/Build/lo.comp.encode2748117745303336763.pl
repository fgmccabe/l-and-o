'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.encode'e'*'n17o17'()17'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.asm'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.parseutils'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.base64'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I5'encodeTerm'FT1t'lo.comp.term*term'Li'encodeType'FT1t'lo.comp.types*tipe'Li'encodeConstraint'FT1t'lo.comp.types*constraint'Li'packageSig'FT1t'lo.comp.package*pkgSpec't'lo.comp.term*term''encMdl'FT1t'lo.comp.code.asm*codeMdl'Li\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.encode@init'() :- !.
'lo.comp.encode@encInt'(XN, XL, 'lo.core#,..'(45, XX40360)) :- ocall('<%2'(XN, 0),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    !,
    ocall('zero%1'(XXV81),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('-%3'(XXV81, XN, XX40357),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.encode@encInt'(XX40357, XL, XX40360).
'lo.comp.encode@encInt'(XN, XL, 'lo.core#,..'(XD, XL)) :- 'lo.comp.parseutils@digitVal'(XD, XN),
    ocall('>=%2'(XN, 0),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    'lo.core@=<'('lo.core$comp$lo.core*integer', XN, 9),
    !.
'lo.comp.encode@encInt'(XN, XL, XX40387) :- ocall('>=%2'(XN, 10),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    ocall('%%3'(XN, 10, XX40379),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    'lo.comp.parseutils@digitVal'(XD, XX40379),
    !,
    ocall('/%3'(XN, 10, XX40382),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    'lo.comp.encode@encInt'(XX40382, 'lo.core#,..'(XD, XL), XX40387).
'lo.comp.encode@encInt'(_, _, _) :- raise_exception('error'("encInt", 24, 3, 42)).
'lo.comp.encode@findDelim'(XChrs, XDelim) :- ocall('in%2'(XDelim, 'lo.core#,..'(39, 'lo.core#,..'(34, 'lo.core#,..'(124, 'lo.core#,..'(47, 'lo.core#,..'(37, 'lo.core#[]')))))),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.encode@neg44'(XChrs, XDelim).
'lo.comp.encode@findDelim'(X_2549, 34).
'lo.comp.encode@encodeQuoted'('lo.core#[]', XDelim, XL, 'lo.core#,..'(XDelim, XL)) :- !.
'lo.comp.encode@encodeQuoted'('lo.core#,..'(92, XM), XDelim, XL, 'lo.core#,..'(92, 'lo.core#,..'(92, XX40415))) :- !,
    'lo.comp.encode@encodeQuoted'(XM, XDelim, XL, XX40415).
'lo.comp.encode@encodeQuoted'('lo.core#,..'(XDelim, XM), XDelim, XL, 'lo.core#,..'(92, 'lo.core#,..'(XDelim, XX40427))) :- !,
    'lo.comp.encode@encodeQuoted'(XM, XDelim, XL, XX40427).
'lo.comp.encode@encodeQuoted'('lo.core#,..'(XCh, XM), XDelim, XL, 'lo.core#,..'(XCh, XX40439)) :- !,
    'lo.comp.encode@encodeQuoted'(XM, XDelim, XL, XX40439).
'lo.comp.encode@encodeQuoted'(_, _, _, _) :- raise_exception('error'("encodeQuoted", 49, 3, 39)).
'lo.comp.encode@encodeChars'(XChars, XDelim, XL, 'lo.core#,..'(XDelim, XX40448)) :- !,
    'lo.comp.encode@encodeQuoted'(XChars, XDelim, XL, XX40448).
'lo.comp.encode@encodeChars'(_, _, _, _) :- raise_exception('error'("encodeChars", 46, 3, 67)).
'lo.comp.encode@encodeText'(XNm, XL, XX40460) :- 'explode'(XNm, XX40454),
    XChrs = XX40454,
    'lo.comp.encode@one47'(XDelim, XChrs),
    !,
    'lo.comp.encode@encodeChars'(XChrs, XDelim, XL, XX40460).
'lo.comp.encode@encodeText'(_, _, _) :- raise_exception('error'("encodeText", 37, 3, 91)).
'lo.comp.encode@encFlot'(XDx, XL, XX40467) :- !,
    ocall('_coerce%2'(XDx, XX40464),'lo.coerce$coercion$lo.core*float$lo.core*string','lo.coerce$coercion$lo.core*float$lo.core*string'),
    'lo.comp.encode@encodeText'(XX40464, XL, XX40467).
'lo.comp.encode@encFlot'(_, _, _) :- raise_exception('error'("encFlot", 34, 3, 41)).
'lo.comp.encode@encEls'('lo.core#[]', XL, XL) :- !.
'lo.comp.encode@encEls'('lo.core#,..'(XE, XM), XL, XX40479) :- !,
    'lo.comp.encode@encEls'(XM, XL, XX40478),
    'lo.comp.encode@encTerm'(XE, XX40478, XX40479).
'lo.comp.encode@encEls'(_, _, _) :- raise_exception('error'("encEls", 29, 3, 17)).
'lo.comp.encode@encTerm'('lo.comp.term#anon', XL, 'lo.core#,..'(97, XL)) :- !.
'lo.comp.encode@encTerm'('lo.comp.term#intgr'(XIx), XL, 'lo.core#,..'(120, XX40489)) :- !,
    'lo.comp.encode@encInt'(XIx, XL, XX40489).
'lo.comp.encode@encTerm'('lo.comp.term#flot'(XDx), XL, 'lo.core#,..'(100, XX40496)) :- !,
    'lo.comp.encode@encFlot'(XDx, XL, XX40496).
'lo.comp.encode@encTerm'('lo.comp.term#enum'(XNm), XL, 'lo.core#,..'(101, XX40503)) :- !,
    'lo.comp.encode@encodeText'(XNm, XL, XX40503).
'lo.comp.encode@encTerm'('lo.comp.term#strng'(XSx), XL, 'lo.core#,..'(115, XX40510)) :- !,
    'lo.comp.encode@encodeText'(XSx, XL, XX40510).
'lo.comp.encode@encTerm'('lo.comp.term#strct'(XNm, XAr), XL, 'lo.core#,..'(111, XX40520)) :- !,
    'lo.comp.encode@encodeText'(XNm, XL, XX40519),
    'lo.comp.encode@encInt'(XAr, XX40519, XX40520).
'lo.comp.encode@encTerm'('lo.comp.term#prg'(XNm, XAr), XL, 'lo.core#,..'(112, XX40530)) :- !,
    'lo.comp.encode@encodeText'(XNm, XL, XX40529),
    'lo.comp.encode@encInt'(XAr, XX40529, XX40530).
'lo.comp.encode@encTerm'('lo.comp.term#cons'(XOp, XEls), XL, 'lo.core#,..'(110, XX40543)) :- !,
    'lo.list@length'(XEls, XX40537),
    'lo.comp.encode@encEls'(XEls, XL, XX40541),
    'lo.comp.encode@encTerm'(XOp, XX40541, XX40542),
    'lo.comp.encode@encInt'(XX40537, XX40542, XX40543).
'lo.comp.encode@encTerm'(_, _, _) :- raise_exception('error'("encTerm", 14, 3, 28)).
'lo.comp.encode@encodeTerm'(XT, XX40548) :- !,
    'lo.comp.encode@encTerm'(XT, 'lo.core#[]', XX40548).
'lo.comp.encode@encodeTerm'(_, _) :- raise_exception('error'("encodeTerm", 11, 3, 30)).
'lo.comp.encode@encConstraint'('lo.comp.types#univCon'(XV, XC), XL, 'lo.core#,..'(58, XX40557)) :- !,
    'lo.comp.encode@encConstraint'(XC, XL, XX40556),
    'lo.comp.encode@encType'(XV, XX40556, XX40557).
'lo.comp.encode@encConstraint'('lo.comp.types#conTract'(XNm, XArgs, XDeps), XL, 'lo.core#,..'(99, XX40572)) :- !,
    'lo.comp.encode@encType'('lo.comp.types#tupleType'(XDeps), XL, XX40570),
    'lo.comp.encode@encType'('lo.comp.types#tupleType'(XArgs), XX40570, XX40571),
    'lo.comp.encode@encodeText'(XNm, XX40571, XX40572).
'lo.comp.encode@encConstraint'('lo.comp.types#implementsFace'(XTp, XFlds), XL, 'lo.core#,..'(97, XX40583)) :- !,
    'lo.comp.encode@encType'('lo.comp.types#faceType'(XFlds), XL, XX40582),
    'lo.comp.encode@encType'(XTp, XX40582, XX40583).
'lo.comp.encode@encConstraint'('lo.comp.types#conCon'(XC, XE), XL, 'lo.core#,..'(124, XX40593)) :- !,
    'lo.comp.encode@encConstraint'(XE, XL, XX40592),
    'lo.comp.encode@encConstraint'(XC, XX40592, XX40593).
'lo.comp.encode@encConstraint'(_, _, _) :- raise_exception('error'("encConstraint", 99, 3, 70)).
'lo.comp.encode@encFieldTps'('lo.core#[]', XL, XL) :- !.
'lo.comp.encode@encFieldTps'('lo.core#,..'((XFld, XTp), XM), XL, XX40609) :- !,
    'lo.comp.encode@encFieldTps'(XM, XL, XX40607),
    'lo.comp.encode@encType'(XTp, XX40607, XX40608),
    'lo.comp.encode@encodeText'(XFld, XX40608, XX40609).
'lo.comp.encode@encFieldTps'(_, _, _) :- raise_exception('error'("encFieldTps", 92, 3, 22)).
'lo.comp.encode@encFieldTypes'(XF, XL, XX40617) :- !,
    'lo.list@length'(XF, XX40613),
    'lo.comp.encode@encFieldTps'(XF, XL, XX40616),
    'lo.comp.encode@encInt'(XX40613, XX40616, XX40617).
'lo.comp.encode@encFieldTypes'(_, _, _) :- raise_exception('error'("encFieldTypes", 89, 3, 56)).
'lo.comp.encode@encTps'('lo.core#[]', XL, XL) :- !.
'lo.comp.encode@encTps'('lo.core#,..'(XT, XM), XL, XX40629) :- !,
    'lo.comp.encode@encTps'(XM, XL, XX40628),
    'lo.comp.encode@encType'(XT, XX40628, XX40629).
'lo.comp.encode@encTps'(_, _, _) :- raise_exception('error'("encTps", 85, 3, 17)).
'lo.comp.encode@encTypes'(XEls, XL, XX40637) :- !,
    'lo.list@length'(XEls, XX40633),
    'lo.comp.encode@encTps'(XEls, XL, XX40636),
    'lo.comp.encode@encInt'(XX40633, XX40636, XX40637).
'lo.comp.encode@encTypes'(_, _, _) :- raise_exception('error'("encTypes", 82, 3, 52)).
'lo.comp.encode@encType'('lo.comp.types#anonType', XL, 'lo.core#,..'(95, XL)) :- !.
'lo.comp.encode@encType'('lo.comp.types#voidType', XL, 'lo.core#,..'(118, XL)) :- !.
'lo.comp.encode@encType'('lo.comp.types#thisType', XL, 'lo.core#,..'(104, XL)) :- !.
'lo.comp.encode@encType'('lo.comp.types#tipe'("lo.core*logical"), XL, 'lo.core#,..'(108, XL)) :- !.
'lo.comp.encode@encType'('lo.comp.types#tipe'("lo.core*integer"), XL, 'lo.core#,..'(105, XL)) :- !.
'lo.comp.encode@encType'('lo.comp.types#tipe'("lo.core*float"), XL, 'lo.core#,..'(102, XL)) :- !.
'lo.comp.encode@encType'('lo.comp.types#tipe'("lo.core*string"), XL, 'lo.core#,..'(83, XL)) :- !.
'lo.comp.encode@encType'('lo.comp.types#tpFun'(XNm, XAr), XL, 'lo.core#,..'(122, XX40674)) :- !,
    'lo.comp.encode@encodeText'(XNm, XL, XX40673),
    'lo.comp.encode@encInt'(XAr, XX40673, XX40674).
'lo.comp.encode@encType'('lo.comp.types#typeExp'(XOp, 'lo.core#,..'(XE, 'lo.core#[]')), XL, 'lo.core#,..'(76, XX40687)) :- 'lo.comp.types@deRef'(XOp, XX40683),
    XX40683 = 'lo.comp.types#tpFun'("lo.core*list", 1),
    !,
    'lo.comp.encode@encType'(XE, XL, XX40687).
'lo.comp.encode@encType'('lo.comp.types#kVar'(XNm), XL, 'lo.core#,..'(107, XX40694)) :- !,
    'lo.comp.encode@encodeText'(XNm, XL, XX40694).
'lo.comp.encode@encType'('lo.comp.types#kFun'(XNm, XAr), XL, 'lo.core#,..'(75, XX40704)) :- !,
    'lo.comp.encode@encodeText'(XNm, XL, XX40703),
    'lo.comp.encode@encInt'(XAr, XX40703, XX40704).
'lo.comp.encode@encType'('lo.comp.types#tipe'(XNm), XL, 'lo.core#,..'(116, XX40711)) :- !,
    'lo.comp.encode@encodeText'(XNm, XL, XX40711).
'lo.comp.encode@encType'('lo.comp.types#typeExp'(XOp, XEls), XL, 'lo.core#,..'(85, XX40722)) :- !,
    'lo.comp.types@deRef'(XOp, XX40718),
    'lo.comp.encode@encTypes'(XEls, XL, XX40721),
    'lo.comp.encode@encType'(XX40718, XX40721, XX40722).
'lo.comp.encode@encType'('lo.comp.types#tupleType'(XEls), XL, 'lo.core#,..'(84, XX40729)) :- !,
    'lo.comp.encode@encTypes'(XEls, XL, XX40729).
'lo.comp.encode@encType'('lo.comp.types#funType'(XA, XR), XL, 'lo.core#,..'(70, XX40739)) :- !,
    'lo.comp.encode@encType'(XR, XL, XX40738),
    'lo.comp.encode@encType'(XA, XX40738, XX40739).
'lo.comp.encode@encType'('lo.comp.types#classType'(XA, XR), XL, 'lo.core#,..'(67, XX40749)) :- !,
    'lo.comp.encode@encType'(XR, XL, XX40748),
    'lo.comp.encode@encType'(XA, XX40748, XX40749).
'lo.comp.encode@encType'('lo.comp.types#predType'(XA), XL, 'lo.core#,..'(80, XX40756)) :- !,
    'lo.comp.encode@encType'(XA, XL, XX40756).
'lo.comp.encode@encType'('lo.comp.types#grammarType'(XA, XR), XL, 'lo.core#,..'(71, XX40766)) :- !,
    'lo.comp.encode@encType'(XR, XL, XX40765),
    'lo.comp.encode@encType'(XA, XX40765, XX40766).
'lo.comp.encode@encType'('lo.comp.types#typeRule'(XA, XR), XL, 'lo.core#,..'(89, XX40776)) :- !,
    'lo.comp.encode@encType'(XR, XL, XX40775),
    'lo.comp.encode@encType'(XA, XX40775, XX40776).
'lo.comp.encode@encType'('lo.comp.types#faceType'(XEls), XL, 'lo.core#,..'(73, XX40783)) :- !,
    'lo.comp.encode@encFieldTypes'(XEls, XL, XX40783).
'lo.comp.encode@encType'('lo.comp.types#constrained'(XT, XC), XL, 'lo.core#,..'(124, XX40793)) :- !,
    'lo.comp.encode@encConstraint'(XC, XL, XX40792),
    'lo.comp.encode@encType'(XT, XX40792, XX40793).
'lo.comp.encode@encType'('lo.comp.types#univType'(XV, XT), XL, 'lo.core#,..'(58, XX40803)) :- !,
    'lo.comp.encode@encType'(XT, XL, XX40802),
    'lo.comp.encode@encType'(XV, XX40802, XX40803).
'lo.comp.encode@encType'(_, _, _) :- raise_exception('error'("encType", 58, 3, 32)).
'lo.comp.encode@encodeType'(XTp, XX40808) :- !,
    'lo.comp.encode@encType'(XTp, 'lo.core#[]', XX40808).
'lo.comp.encode@encodeType'(_, _) :- raise_exception('error'("encodeType", 55, 3, 32)).
'lo.comp.encode@encodeConstraint'(XCon, XX40812) :- !,
    'lo.comp.encode@encConstraint'(XCon, 'lo.core#[]', XX40812).
'lo.comp.encode@encodeConstraint'(_, _) :- raise_exception('error'("encodeConstraint", 96, 3, 46)).
'lo.comp.encode@fmtVer'('lo.repo#defltVersion', 'lo.comp.term#enum'("*")) :- !.
'lo.comp.encode@fmtVer'('lo.repo#vers'(XV), 'lo.comp.term#strng'(XV)) :- !.
'lo.comp.encode@fmtVer'(_, _) :- raise_exception('error'("fmtVer", 114, 3, 33)).
'lo.comp.encode@fmtPkg'('lo.repo#pkg'(XNm, XVers), 'lo.comp.term#cons'('lo.comp.term#strct'("pkg", 2), 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'(XX40826, 'lo.core#[]')))) :- !,
    'lo.comp.encode@fmtVer'(XVers, XX40826).
'lo.comp.encode@fmtPkg'(_, _) :- raise_exception('error'("fmtPkg", 111, 3, 69)).
'lo.comp.encode@fmtViz'('lo.comp.package#priVate', 'lo.comp.term#enum'("private")) :- !.
'lo.comp.encode@fmtViz'('lo.comp.package#pUblic', 'lo.comp.term#enum'("public")) :- !.
'lo.comp.encode@fmtViz'(_, _) :- raise_exception('error'("fmtViz", 124, 3, 34)).
'lo.comp.encode@fmtImport'((XViz, XPkg), 'lo.comp.term#cons'('lo.comp.term#strct'("import", 2), 'lo.core#,..'(XX40839, 'lo.core#,..'(XX40841, 'lo.core#[]')))) :- !,
    'lo.comp.encode@fmtViz'(XViz, XX40839),
    'lo.comp.encode@fmtPkg'(XPkg, XX40841).
'lo.comp.encode@fmtImport'(_, _) :- raise_exception('error'("fmtImport", 121, 3, 73)).
'lo.comp.encode@fmtImports'(XImports, XX40851) :- !,
    ocall('//%3'(XImports, 'lo.comp.encode^fmtImport', XX40849),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo.comp.term@mkTpl'(XX40849, XX40851).
'lo.comp.encode@fmtImports'(_, _) :- raise_exception('error'("fmtImports", 118, 3, 48)).
'lo.comp.encode@fmtType'(XT, 'lo.comp.term#strng'(XX40855)) :- !,
    'lo.comp.encode@encodeType'(XT, XX40854),
    'implode'(XX40854, XX40855).
'lo.comp.encode@fmtType'(_, _) :- raise_exception('error'("fmtType", 149, 3, 43)).
'lo.comp.encode@fmtTypes'(XFields, XX40860) :- !,
    'lo.comp.encode@fmtType'('lo.comp.types#faceType'(XFields), XX40860).
'lo.comp.encode@fmtTypes'(_, _) :- raise_exception('error'("fmtTypes", 128, 3, 45)).
'lo.comp.encode@fmtEnum'(XE, 'lo.comp.term#strng'(XE)) :- !.
'lo.comp.encode@fmtEnum'(_, _) :- raise_exception('error'("fmtEnum", 134, 3, 22)).
'lo.comp.encode@fmtEnums'(XEnums, XX40869) :- !,
    ocall('//%3'(XEnums, 'lo.comp.encode^fmtEnum', XX40867),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo.comp.term@mkTpl'(XX40867, XX40869).
'lo.comp.encode@fmtEnums'(_, _) :- raise_exception('error'("fmtEnums", 131, 3, 40)).
'lo.comp.encode@fmtConstraint'(XCon, 'lo.comp.term#strng'(XX40873)) :- !,
    'lo.comp.encode@encodeConstraint'(XCon, XX40872),
    'implode'(XX40872, XX40873).
'lo.comp.encode@fmtConstraint'(_, _) :- raise_exception('error'("fmtConstraint", 152, 3, 59)).
'lo.comp.encode@fmtContract'('lo.comp.types#conEntry'(XNm, XConNm, XCon, XFace), XX40893) :- !,
    'lo.comp.encode@fmtConstraint'(XCon, XX40885),
    'lo.comp.encode@fmtType'(XFace, XX40887),
    'lo.comp.term@mkTpl'('lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#strng'(XConNm), 'lo.core#,..'(XX40885, 'lo.core#,..'(XX40887, 'lo.core#[]')))), XX40893).
'lo.comp.encode@fmtContract'(_, _) :- raise_exception('error'("fmtContract", 140, 3, 108)).
'lo.comp.encode@fmtContracts'(XL, XX40899) :- !,
    ocall('//%3'(XL, 'lo.comp.encode^fmtContract', XX40897),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo.comp.term@mkTpl'(XX40897, XX40899).
'lo.comp.encode@fmtContracts'(_, _) :- raise_exception('error'("fmtContracts", 137, 3, 40)).
'lo.comp.encode@fmtImpl'('lo.comp.types#implEntry'(XNm, XCon), XX40910) :- !,
    'lo.comp.encode@fmtConstraint'(XCon, XX40906),
    'lo.comp.term@mkTpl'('lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'(XX40906, 'lo.core#[]')), XX40910).
'lo.comp.encode@fmtImpl'(_, _) :- raise_exception('error'("fmtImpl", 146, 3, 67)).
'lo.comp.encode@fmtImpls'(XL, XX40916) :- !,
    ocall('//%3'(XL, 'lo.comp.encode^fmtImpl', XX40914),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo.comp.term@mkTpl'(XX40914, XX40916).
'lo.comp.encode@fmtImpls'(_, _) :- raise_exception('error'("fmtImpls", 143, 3, 32)).
'lo.comp.encode@packageSig'('lo.comp.package#pkgSpec'(XPkg, XFields, XTypes, XEnums, XContracts, XImpls, XImports), 'lo.comp.term#strng'(XX40949)) :- !,
    'lo.comp.encode@fmtPkg'(XPkg, XX40926),
    'lo.comp.encode@fmtImports'(XImports, XX40928),
    'lo.comp.encode@fmtTypes'(XFields, XX40930),
    'lo.comp.encode@fmtTypes'(XTypes, XX40932),
    'lo.comp.encode@fmtEnums'(XEnums, XX40934),
    'lo.comp.encode@fmtContracts'(XContracts, XX40936),
    'lo.comp.encode@fmtImpls'(XImpls, XX40938),
    'lo.comp.term@mkTpl'('lo.core#,..'(XX40926, 'lo.core#,..'(XX40928, 'lo.core#,..'(XX40930, 'lo.core#,..'(XX40932, 'lo.core#,..'(XX40934, 'lo.core#,..'(XX40936, 'lo.core#,..'(XX40938, 'lo.core#[]'))))))), XX40947),
    'lo.comp.encode@encodeTerm'(XX40947, XX40948),
    'implode'(XX40948, XX40949).
'lo.comp.encode@packageSig'(_, _) :- raise_exception('error'("packageSig", 105, 3, 248)).
'lo.comp.encode@fmtIns'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.encode@fmtIns'('lo.core#,..'(XI, XL), 'lo.core#,..'(XX40958, 'lo.core#,..'(XX40961, 'lo.core#,..'(XX40964, 'lo.core#,..'(XX40966, XX40968))))) :- !,
    'lo.bits@.>>.'(XI, 24, XX40957),
    'lo.bits@.&.'(XX40957, 255, XX40958),
    'lo.bits@.>>.'(XI, 16, XX40960),
    'lo.bits@.&.'(XX40960, 255, XX40961),
    'lo.bits@.>>.'(XI, 8, XX40963),
    'lo.bits@.&.'(XX40963, 255, XX40964),
    'lo.bits@.&.'(XI, 255, XX40966),
    'lo.comp.encode@fmtIns'(XL, XX40968).
'lo.comp.encode@fmtIns'(_, _) :- raise_exception('error'("fmtIns", 156, 3, 16)).
'lo.comp.encode@LOSig'(16975111) :- !.
'lo.comp.encode@encCodeSeg'('lo.comp.code.asm#codeSeg'(XNm, XCode, XLits, XSrcMap), XL, 'lo.core#,..'(110, XX40997)) :- !,
    'lo.comp.encode@LOSig'(XX40981),
    'lo.comp.encode@fmtIns'('lo.core#,..'(XX40981, XCode), XX40984),
    'lo.comp.base64@encode64'(XX40984, XX40985),
    'lo.comp.term@mkTpl'(XLits, XX40987),
    'lo.comp.term@mkTpl'(XSrcMap, XX40989),
    'lo.comp.encode@encTerm'(XX40989, XL, XX40991),
    'lo.comp.encode@encTerm'(XX40987, XX40991, XX40992),
    'lo.comp.encode@encodeChars'(XX40985, 39, XX40992, XX40993),
    'lo.comp.encode@encTerm'(XNm, 'lo.core#,..'(115, XX40993), XX40995),
    'lo.comp.encode@encTerm'('lo.comp.term#strct'("#code", 4), XX40995, XX40996),
    'lo.comp.encode@encInt'(3, XX40996, XX40997).
'lo.comp.encode@encCodeSeg'(_, _, _) :- raise_exception('error'("encCodeSeg", 164, 3, 209)).
'lo.comp.encode@encSegs'('lo.core#[]', XL, XL) :- !.
'lo.comp.encode@encSegs'('lo.core#,..'(XS, XR), XL, XX41010) :- !,
    'lo.comp.encode@encSegs'(XR, XL, XX41009),
    'lo.comp.encode@encCodeSeg'(XS, XX41009, XX41010).
'lo.comp.encode@encSegs'(_, _, _) :- raise_exception('error'("encSegs", 168, 3, 18)).
'lo.comp.encode@encMdl'('lo.comp.code.asm#codeMdl'(XSpec, XSegs), XX41019) :- !,
    'lo.comp.encode@packageSig'(XSpec, XX41015),
    'lo.comp.encode@encSegs'(XSegs, 'lo.core#[]', XX41018),
    'lo.comp.encode@encTerm'(XX41015, XX41018, XX41019).
'lo.comp.encode@encMdl'(_, _) :- raise_exception('error'("encMdl", 173, 3, 76)).
'lo.comp.encode^encInt'('_call%3'(XV4839, XV4840, XV4841), 'lo.comp.encode^encInt', _) :- 'lo.comp.encode@encInt'(XV4839, XV4840, XV4841).
'lo.comp.encode@neg44'(XChrs, XDelim) :- ocall('in%2'(XDelim, XChrs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.encode@neg44'(XChrs, XDelim).
'lo.comp.encode^findDelim'('_call%2'(XV4842, XV4843), 'lo.comp.encode^findDelim', _) :- 'lo.comp.encode@findDelim'(XV4842, XV4843).
'lo.comp.encode^encodeQuoted'('_call%4'(XV4844, XV4845, XV4846, XV4847), 'lo.comp.encode^encodeQuoted', _) :- 'lo.comp.encode@encodeQuoted'(XV4844, XV4845, XV4846, XV4847).
'lo.comp.encode^encodeChars'('_call%4'(XV4848, XV4849, XV4850, XV4851), 'lo.comp.encode^encodeChars', _) :- 'lo.comp.encode@encodeChars'(XV4848, XV4849, XV4850, XV4851).
'lo.comp.encode@one47'(XDelim, XChrs) :- 'lo.comp.encode@findDelim'(XChrs, XDelim),
    !.
'lo.comp.encode^encodeText'('_call%3'(XV4852, XV4853, XV4854), 'lo.comp.encode^encodeText', _) :- 'lo.comp.encode@encodeText'(XV4852, XV4853, XV4854).
'lo.comp.encode^encFlot'('_call%3'(XV4855, XV4856, XV4857), 'lo.comp.encode^encFlot', _) :- 'lo.comp.encode@encFlot'(XV4855, XV4856, XV4857).
'lo.comp.encode^encEls'('_call%3'(XV4858, XV4859, XV4860), 'lo.comp.encode^encEls', _) :- 'lo.comp.encode@encEls'(XV4858, XV4859, XV4860).
'lo.comp.encode^encTerm'('_call%3'(XV4861, XV4862, XV4863), 'lo.comp.encode^encTerm', _) :- 'lo.comp.encode@encTerm'(XV4861, XV4862, XV4863).
'lo.comp.encode^encodeTerm'('_call%2'(XV4864, XV4865), 'lo.comp.encode^encodeTerm', _) :- 'lo.comp.encode@encodeTerm'(XV4864, XV4865).
'lo.comp.encode^encConstraint'('_call%3'(XV4866, XV4867, XV4868), 'lo.comp.encode^encConstraint', _) :- 'lo.comp.encode@encConstraint'(XV4866, XV4867, XV4868).
'lo.comp.encode^encFieldTps'('_call%3'(XV4869, XV4870, XV4871), 'lo.comp.encode^encFieldTps', _) :- 'lo.comp.encode@encFieldTps'(XV4869, XV4870, XV4871).
'lo.comp.encode^encFieldTypes'('_call%3'(XV4872, XV4873, XV4874), 'lo.comp.encode^encFieldTypes', _) :- 'lo.comp.encode@encFieldTypes'(XV4872, XV4873, XV4874).
'lo.comp.encode^encTps'('_call%3'(XV4875, XV4876, XV4877), 'lo.comp.encode^encTps', _) :- 'lo.comp.encode@encTps'(XV4875, XV4876, XV4877).
'lo.comp.encode^encTypes'('_call%3'(XV4878, XV4879, XV4880), 'lo.comp.encode^encTypes', _) :- 'lo.comp.encode@encTypes'(XV4878, XV4879, XV4880).
'lo.comp.encode^encType'('_call%3'(XV4881, XV4882, XV4883), 'lo.comp.encode^encType', _) :- 'lo.comp.encode@encType'(XV4881, XV4882, XV4883).
'lo.comp.encode^encodeType'('_call%2'(XV4884, XV4885), 'lo.comp.encode^encodeType', _) :- 'lo.comp.encode@encodeType'(XV4884, XV4885).
'lo.comp.encode^encodeConstraint'('_call%2'(XV4886, XV4887), 'lo.comp.encode^encodeConstraint', _) :- 'lo.comp.encode@encodeConstraint'(XV4886, XV4887).
'lo.comp.encode^fmtVer'('_call%2'(XV4888, XV4889), 'lo.comp.encode^fmtVer', _) :- 'lo.comp.encode@fmtVer'(XV4888, XV4889).
'lo.comp.encode^fmtPkg'('_call%2'(XV4890, XV4891), 'lo.comp.encode^fmtPkg', _) :- 'lo.comp.encode@fmtPkg'(XV4890, XV4891).
'lo.comp.encode^fmtViz'('_call%2'(XV4892, XV4893), 'lo.comp.encode^fmtViz', _) :- 'lo.comp.encode@fmtViz'(XV4892, XV4893).
'lo.comp.encode^fmtImport'('_call%2'(XV4894, XV4895), 'lo.comp.encode^fmtImport', _) :- 'lo.comp.encode@fmtImport'(XV4894, XV4895).
'lo.comp.encode^fmtImports'('_call%2'(XV4896, XV4897), 'lo.comp.encode^fmtImports', _) :- 'lo.comp.encode@fmtImports'(XV4896, XV4897).
'lo.comp.encode^fmtType'('_call%2'(XV4898, XV4899), 'lo.comp.encode^fmtType', _) :- 'lo.comp.encode@fmtType'(XV4898, XV4899).
'lo.comp.encode^fmtTypes'('_call%2'(XV4900, XV4901), 'lo.comp.encode^fmtTypes', _) :- 'lo.comp.encode@fmtTypes'(XV4900, XV4901).
'lo.comp.encode^fmtEnum'('_call%2'(XV4902, XV4903), 'lo.comp.encode^fmtEnum', _) :- 'lo.comp.encode@fmtEnum'(XV4902, XV4903).
'lo.comp.encode^fmtEnums'('_call%2'(XV4904, XV4905), 'lo.comp.encode^fmtEnums', _) :- 'lo.comp.encode@fmtEnums'(XV4904, XV4905).
'lo.comp.encode^fmtConstraint'('_call%2'(XV4906, XV4907), 'lo.comp.encode^fmtConstraint', _) :- 'lo.comp.encode@fmtConstraint'(XV4906, XV4907).
'lo.comp.encode^fmtContract'('_call%2'(XV4908, XV4909), 'lo.comp.encode^fmtContract', _) :- 'lo.comp.encode@fmtContract'(XV4908, XV4909).
'lo.comp.encode^fmtContracts'('_call%2'(XV4910, XV4911), 'lo.comp.encode^fmtContracts', _) :- 'lo.comp.encode@fmtContracts'(XV4910, XV4911).
'lo.comp.encode^fmtImpl'('_call%2'(XV4912, XV4913), 'lo.comp.encode^fmtImpl', _) :- 'lo.comp.encode@fmtImpl'(XV4912, XV4913).
'lo.comp.encode^fmtImpls'('_call%2'(XV4914, XV4915), 'lo.comp.encode^fmtImpls', _) :- 'lo.comp.encode@fmtImpls'(XV4914, XV4915).
'lo.comp.encode^packageSig'('_call%2'(XV4916, XV4917), 'lo.comp.encode^packageSig', _) :- 'lo.comp.encode@packageSig'(XV4916, XV4917).
'lo.comp.encode^fmtIns'('_call%2'(XV4918, XV4919), 'lo.comp.encode^fmtIns', _) :- 'lo.comp.encode@fmtIns'(XV4918, XV4919).
'lo.comp.encode^encCodeSeg'('_call%3'(XV4920, XV4921, XV4922), 'lo.comp.encode^encCodeSeg', _) :- 'lo.comp.encode@encCodeSeg'(XV4920, XV4921, XV4922).
'lo.comp.encode^encSegs'('_call%3'(XV4923, XV4924, XV4925), 'lo.comp.encode^encSegs', _) :- 'lo.comp.encode@encSegs'(XV4923, XV4924, XV4925).
'lo.comp.encode^encMdl'('_call%2'(XV4926, XV4927), 'lo.comp.encode^encMdl', _) :- 'lo.comp.encode@encMdl'(XV4926, XV4927).
