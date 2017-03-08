'#pkg'("n7o7'()7'n2o2'pkg's'lo.repo.file's'1.0.0'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo.manifest'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'s\"I10'repo'CT2t'lo.uri*uri't'lo.repo.manifest*manifest't'lo.repo.file*fileRepo''openRepository'FT1t'lo.uri*uri't'lo.repo.file*fileRepo''addToRepo'FT4t'lo.repo.file*fileRepo't'lo.repo*pkg'SSt'lo.repo.file*fileRepo''locateCode'PT4t'lo.repo.file*fileRepo't'lo.repo*pkg'SS'locateProlog'PT4t'lo.repo.file*fileRepo't'lo.repo*pkg'SS'packageCodeOk'PT2t'lo.repo.file*fileRepo't'lo.repo*pkg''packagePrologOk'PT2t'lo.repo.file*fileRepo't'lo.repo*pkg''addPackage'FT3t'lo.repo.file*fileRepo't'lo.repo*pkg'St'lo.repo.file*fileRepo''addPrologPackage'FT3t'lo.repo.file*fileRepo't'lo.repo*pkg'St'lo.repo.file*fileRepo''addSource'FT3t'lo.repo.file*fileRepo't'lo.repo*pkg'St'lo.repo.file*fileRepo'\"s\"I1'fileRepo'Yt'lo.repo.file*fileRepo'I0\"n1o1'()1's'repo'n0o0'()0'n1o1'()1'n2o2'()2's'lo.repo$repository$lo.repo.file*fileRepo's\"c'lo.repo$repository'T1t'lo.repo.file*fileRepo'T0\"").
'lo.repo.file@init'():- !.
'lo.repo.file#repo'('repo%1'('lo.repo.file@repo'())):- !.
'lo.repo.file@openRepository'(XRoot, 'lo.repo.file#repo'(XRoot, XXd9072)):- 'lo.uri@parseUri'("manifest", XXd9070),
    'lo.uri@resolveUri'(XRoot, XXd9070, XXd9071),
    XRepoUri = XXd9071,
    'lo.resources@resourcePresent'(XRepoUri),
    !,
    'lo.repo.manifest@readManifest'(XRepoUri, XXd9072).
'lo.repo.file@openRepository'(XRoot, 'lo.repo.file#repo'(XRoot, 'lo.repo.manifest#manifest'(XXV1974))):- !,
    ocall('_empty%1'(XXV1974),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.file@openRepository'(_, _):- raise_exception('error'("lo.repo.file@openRepository", 11, 3, 141)).
'lo.repo$repository$lo.repo.file*fileRepo'('lo.repo$repository$lo.repo.file*fileRepo%1'('lo.repo$repository$lo.repo.file*fileRepo')):- !.
'lo.repo$repository$lo.repo.file*fileRepo'('packagePresent%3'(XV18762, XV18763, XV18764), XLbl3860, XThis3860):- !,
    'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'(XV18762, XV18763, XV18764, XLbl3860, XThis3860).
'lo.repo$repository$lo.repo.file*fileRepo'('packagePresent%1'('lo.repo$repository$lo.repo.file*fileRepo^packagePresent'(XLbl3861, XThis3861)), XLbl3861, XThis3861).
'lo.repo$repository$lo.repo.file*fileRepo'('loadFromRepo%4'(XV18771, XV18772, XV18773, XV18774), XLbl3862, XThis3862):- !,
    'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'(XV18771, XV18772, XV18773, XV18774, XLbl3862, XThis3862).
'lo.repo$repository$lo.repo.file*fileRepo'('loadFromRepo%1'('lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'(XLbl3863, XThis3863)), XLbl3863, XThis3863).
'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'('lo.repo.file#repo'(XRoot, XMan), XPkg, XKind, XLbV1695, XThV1695):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, XS, XU),
    'lo.uri@parseUri'(XU, XXd9077),
    'lo.uri@resolveUri'(XRoot, XXd9077, XXd9078),
    XCodeFile = XXd9078,
    'lo.uri@parseUri'(XS, XXd9079),
    'lo.uri@resolveUri'(XRoot, XXd9079, XXd9080),
    XSrcFile = XXd9080,
    'lo.resources@resourcePresent'(XCodeFile),
    'lo.repo$repository$lo.repo.file*fileRepo@cond93'(XXd9082, XXd9081, XCodeFile, XSrcFile).
'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'('lo.repo.file#repo'(XRoot, XMan), XPkg, XKind, XXb4205, XLbV1695, XThV1695):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, XKind, XU),
    'lo.uri@parseUri'(XU, XXb4203),
    'lo.uri@resolveUri'(XRoot, XXb4203, XXb4204),
    'lo.resources@getResource'(XXb4204, XXb4205).
'lo.repo.file@extensionMapping'("source", ".lo").
'lo.repo.file@extensionMapping'("prolog", ".pl").
'lo.repo.file@extensionMapping'("code", "").
'lo.repo.file@packageHash'(XPkg, 'lo.repo#defltVersion', XXe1949):- !,
    ocall('hash%1'(XXV1975),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'(XPkg, XXe1949),XXV1975,XXV1975).
'lo.repo.file@packageHash'(XPkg, 'lo.repo#vers'(XV), XXe1953):- !,
    ocall('hash%1'(XXV1976),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('*%1'(XXV1977),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('hash%1'(XXV1978),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('+%1'(XXV1979),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XPkg, XXe1950),XXV1976,XXV1976),
    ocall('_call%3'(37, XXe1950, XXe1951),XXV1977,XXV1977),
    ocall('_call%2'(XV, XXe1952),XXV1978,XXV1978),
    ocall('_call%3'(XXe1951, XXe1952, XXe1953),XXV1979,XXV1979).
'lo.repo.file@packageHash'(_, _, _):- raise_exception('error'("lo.repo.file@packageHash", 72, 3, 42)).
'lo.repo.file@addToRepo'('lo.repo.file#repo'(XRoot, XMan), 'lo.repo#pkg'(XPkg, XVers), XKind, XText, 'lo.repo.file#repo'(XRoot, XNM)):- 'lo.repo.file@extensionMapping'(XKind, XExt),
    ocall('_coerce%1'(XXV1980),'lo.coerce$coercion$lo.core*integer$lo.core*string','lo.coerce$coercion$lo.core*integer$lo.core*string'),
    ocall('+%1'(XXV1981),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%1'(XXV1982),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    'lo.repo.file@packageHash'(XPkg, XVers, XXd9083),
    ocall('_call%2'(XXd9083, XXe1954),XXV1980,XXV1980),
    ocall('_call%3'(XPkg, XXe1954, XXe1955),XXV1981,XXV1981),
    ocall('_call%3'(XXe1955, XExt, XXe1956),XXV1982,XXV1982),
    XFn = XXe1956,
    'lo.uri@parseUri'(XFn, XXd9084),
    'lo.uri@resolveUri'(XRoot, XXd9084, XXd9085),
    'lo.resources@putResource'(XXd9085, XText),
    'lo.repo.manifest@addToManifest'(XMan, 'lo.repo#pkg'(XPkg, XVers), XKind, XFn, XXd9087),
    XNM = XXd9087,
    'lo.uri@parseUri'("manifest", XXd9088),
    'lo.uri@resolveUri'(XRoot, XXd9088, XXd9089),
    XRepoUri = XXd9089,
    'lo.repo.manifest@flushManifest'(XRepoUri, XNM),
    !.
'lo.repo.file@addToRepo'(_, _, _, _, _):- raise_exception('error'("lo.repo.file@addToRepo", 30, 3, 338)).
'lo.repo.file@locateCode'('lo.repo.file#repo'(XRoot, XMan), XPkg, XU, XXb4212):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, "code", XU),
    'lo.uri@parseUri'(XU, XXb4210),
    'lo.uri@resolveUri'(XRoot, XXb4210, XXb4211),
    'lo.resources@getResource'(XXb4211, XXb4212).
'lo.repo.file@locateProlog'('lo.repo.file#repo'(XRoot, XMan), XPkg, XU, XXb4216):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, "prolog", XU),
    'lo.uri@parseUri'(XU, XXb4214),
    'lo.uri@resolveUri'(XRoot, XXb4214, XXb4215),
    'lo.resources@getResource'(XXb4215, XXb4216).
'lo.repo.file@packageOk'('lo.repo.file#repo'(XRoot, XMan), XPkg, XKind):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, XKind, XU),
    'lo.repo.manifest@locateInManifest'(XMan, XPkg, "source", XS),
    'lo.uri@parseUri'(XU, XXd9091),
    'lo.uri@resolveUri'(XRoot, XXd9091, XXd9092),
    XCodeFile = XXd9092,
    'lo.uri@parseUri'(XS, XXd9093),
    'lo.uri@resolveUri'(XRoot, XXd9093, XXd9094),
    XSrcFile = XXd9094,
    'lo.resources@resourcePresent'(XCodeFile),
    'lo.repo.file@cond94'(XXd9096, XXd9095, XCodeFile, XSrcFile).
'lo.repo.file@packageCodeOk'(XRepo, XPkg):- 'lo.repo.file@packageOk'(XRepo, XPkg, "code").
'lo.repo.file@packagePrologOk'(XRepo, XPkg):- 'lo.repo.file@packageOk'(XRepo, XPkg, "prolog").
'lo.repo.file@addPackage'(XRepo, XPkg, XText, XXd9097):- !,
    'lo.repo.file@addToRepo'(XRepo, XPkg, "code", XText, XXd9097).
'lo.repo.file@addPackage'(_, _, _, _):- raise_exception('error'("lo.repo.file@addPackage", 63, 3, 60)).
'lo.repo.file@addPrologPackage'(XRepo, XPkg, XText, XXd9098):- !,
    'lo.repo.file@addToRepo'(XRepo, XPkg, "prolog", XText, XXd9098).
'lo.repo.file@addPrologPackage'(_, _, _, _):- raise_exception('error'("lo.repo.file@addPrologPackage", 66, 3, 68)).
'lo.repo.file@addSource'('lo.repo.file#repo'(XRoot, XMan), XPkg, XNm, 'lo.repo.file#repo'(XRoot, XXd9099)):- !,
    'lo.repo.manifest@addToManifest'(XMan, XPkg, "source", XNm, XXd9099).
'lo.repo.file@addSource'(_, _, _, _):- raise_exception('error'("lo.repo.file@addSource", 69, 3, 81)).
'lo.repo.file^openRepository'('_call%2'(XV18755, XV18756), 'lo.repo.file^openRepository', _):- 'lo.repo.file@openRepository'(XV18755, XV18756).
'lo.repo$repository$lo.repo.file*fileRepo@cond93'(XXd9082, XXd9081, XCodeFile, XSrcFile):- 'lo.resources@resourcePresent'(XSrcFile),
    !,
    'lo.uri@getUriPath'(XCodeFile, XXd9081),
    'lo.uri@getUriPath'(XSrcFile, XXd9082),
    'lo.io@newerFile'(XXd9081, XXd9082).
'lo.repo$repository$lo.repo.file*fileRepo@cond93'(XXd9082, XXd9081, XCodeFile, XSrcFile).
'lo.repo$repository$lo.repo.file*fileRepo^packagePresent'('_call%5'(XV18757, XV18758, XV18759, XV18760, XV18761), 'lo.repo$repository$lo.repo.file*fileRepo^packagePresent'(XLbV1695, XThV1695), _):- 'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'(XV18757, XV18758, XV18759, XV18760, XV18761, XLbV1695, XThV1695).
'lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'('_call%6'(XV18765, XV18766, XV18767, XV18768, XV18769, XV18770), 'lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'(XLbV1695, XThV1695), _):- 'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'(XV18765, XV18766, XV18767, XV18768, XV18769, XV18770, XLbV1695, XThV1695).
'lo.repo.file^extensionMapping'('_call%2'(XV18775, XV18776), 'lo.repo.file^extensionMapping', _):- 'lo.repo.file@extensionMapping'(XV18775, XV18776).
'lo.repo.file^packageHash'('_call%3'(XV18777, XV18778, XV18779), 'lo.repo.file^packageHash', _):- 'lo.repo.file@packageHash'(XV18777, XV18778, XV18779).
'lo.repo.file^addToRepo'('_call%5'(XV18780, XV18781, XV18782, XV18783, XV18784), 'lo.repo.file^addToRepo', _):- 'lo.repo.file@addToRepo'(XV18780, XV18781, XV18782, XV18783, XV18784).
'lo.repo.file^locateCode'('_call%4'(XV18785, XV18786, XV18787, XV18788), 'lo.repo.file^locateCode', _):- 'lo.repo.file@locateCode'(XV18785, XV18786, XV18787, XV18788).
'lo.repo.file^locateProlog'('_call%4'(XV18789, XV18790, XV18791, XV18792), 'lo.repo.file^locateProlog', _):- 'lo.repo.file@locateProlog'(XV18789, XV18790, XV18791, XV18792).
'lo.repo.file@cond94'(XXd9096, XXd9095, XCodeFile, XSrcFile):- 'lo.resources@resourcePresent'(XSrcFile),
    !,
    'lo.uri@getUriPath'(XCodeFile, XXd9095),
    'lo.uri@getUriPath'(XSrcFile, XXd9096),
    'lo.io@newerFile'(XXd9095, XXd9096).
'lo.repo.file@cond94'(XXd9096, XXd9095, XCodeFile, XSrcFile).
'lo.repo.file^packageOk'('_call%3'(XV18793, XV18794, XV18795), 'lo.repo.file^packageOk', _):- 'lo.repo.file@packageOk'(XV18793, XV18794, XV18795).
'lo.repo.file^packageCodeOk'('_call%2'(XV18796, XV18797), 'lo.repo.file^packageCodeOk', _):- 'lo.repo.file@packageCodeOk'(XV18796, XV18797).
'lo.repo.file^packagePrologOk'('_call%2'(XV18798, XV18799), 'lo.repo.file^packagePrologOk', _):- 'lo.repo.file@packagePrologOk'(XV18798, XV18799).
'lo.repo.file^addPackage'('_call%4'(XV18800, XV18801, XV18802, XV18803), 'lo.repo.file^addPackage', _):- 'lo.repo.file@addPackage'(XV18800, XV18801, XV18802, XV18803).
'lo.repo.file^addPrologPackage'('_call%4'(XV18804, XV18805, XV18806, XV18807), 'lo.repo.file^addPrologPackage', _):- 'lo.repo.file@addPrologPackage'(XV18804, XV18805, XV18806, XV18807).
'lo.repo.file^addSource'('_call%4'(XV18808, XV18809, XV18810, XV18811), 'lo.repo.file^addSource', _):- 'lo.repo.file@addSource'(XV18808, XV18809, XV18810, XV18811).
