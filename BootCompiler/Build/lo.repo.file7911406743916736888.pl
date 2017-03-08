'#pkg'("n7o7'()7'n2o2'pkg's'lo.repo.file'e'*'n14o14'()14'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo.manifest'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I10'repo'CT2t'lo.uri*uri't'lo.repo.manifest*manifest't'lo.repo.file*fileRepo''openRepository'FT1t'lo.uri*uri't'lo.repo.file*fileRepo''addToRepo'FT4t'lo.repo.file*fileRepo't'lo.repo*pkg'SSt'lo.repo.file*fileRepo''locateCode'PT4t'lo.repo.file*fileRepo't'lo.repo*pkg'SS'locateProlog'PT4t'lo.repo.file*fileRepo't'lo.repo*pkg'SS'packageCodeOk'PT2t'lo.repo.file*fileRepo't'lo.repo*pkg''packagePrologOk'PT2t'lo.repo.file*fileRepo't'lo.repo*pkg''addPackage'FT3t'lo.repo.file*fileRepo't'lo.repo*pkg'St'lo.repo.file*fileRepo''addPrologPackage'FT3t'lo.repo.file*fileRepo't'lo.repo*pkg'St'lo.repo.file*fileRepo''addSource'FT3t'lo.repo.file*fileRepo't'lo.repo*pkg'St'lo.repo.file*fileRepo'\"s\"I1'fileRepo'Yt'lo.repo.file*fileRepo'I0\"n0o0'()0'n0o0'()0'n1o1'()1'n2o2'()2's'lo.repo$repository$lo.repo.file*fileRepo's\"c'lo.repo$repository'T1t'lo.repo.file*fileRepo'T0\"").
'lo.repo.file@init'() :- !.
'lo.repo.file#repo'('repo%1'('lo.repo.file@repo'())) :- !.
'lo.repo.file@openRepository'(XRoot, 'lo.repo.file#repo'(XRoot, XX22383)) :- 'lo.uri@parseUri'("manifest", XX22378),
    'lo.uri@resolveUri'(XRoot, XX22378, XX22379),
    XRepoUri = XX22379,
    'lo.resources@resourcePresent'(XRepoUri),
    !,
    'lo.repo.manifest@readManifest'(XRepoUri, XX22383).
'lo.repo.file@openRepository'(XRoot, 'lo.repo.file#repo'(XRoot, 'lo.repo.manifest#manifest'(XXV41))) :- !,
    ocall('_empty%1'(XXV41),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.file@openRepository'(_, _) :- raise_exception('error'("openRepository", 11, 3, 141)).
'lo.repo$repository$lo.repo.file*fileRepo'('lo.repo$repository$lo.repo.file*fileRepo%1'('lo.repo$repository$lo.repo.file*fileRepo')) :- !.
'lo.repo$repository$lo.repo.file*fileRepo'('packagePresent%3'(XV3146, XV3147, XV3148), XLbl281, XThis281) :- !,
    'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'(XV3146, XV3147, XV3148, XLbl281, XThis281).
'lo.repo$repository$lo.repo.file*fileRepo'('packagePresent%1'('lo.repo$repository$lo.repo.file*fileRepo^packagePresent'(XLbl282, XThis282)), XLbl282, XThis282).
'lo.repo$repository$lo.repo.file*fileRepo'('loadFromRepo%4'(XV3156, XV3157, XV3158, XV3159), XLbl283, XThis283) :- !,
    'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'(XV3156, XV3157, XV3158, XV3159, XLbl283, XThis283).
'lo.repo$repository$lo.repo.file*fileRepo'('loadFromRepo%1'('lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'(XLbl284, XThis284)), XLbl284, XThis284).
'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'('lo.repo.file#repo'(XRoot, XMan), XPkg, XKind, XLbV292, XThV292) :- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, XS, XU),
    'lo.uri@parseUri'(XU, XX22403),
    'lo.uri@resolveUri'(XRoot, XX22403, XX22404),
    XCodeFile = XX22404,
    'lo.uri@parseUri'(XS, XX22408),
    'lo.uri@resolveUri'(XRoot, XX22408, XX22409),
    XSrcFile = XX22409,
    'lo.resources@resourcePresent'(XCodeFile),
    'lo.repo$repository$lo.repo.file*fileRepo@cond14'(XX22415, XX22413, XCodeFile, XSrcFile).
'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'('lo.repo.file#repo'(XRoot, XMan), XPkg, XKind, XX22425, XLbV292, XThV292) :- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, XKind, XU),
    'lo.uri@parseUri'(XU, XX22423),
    'lo.uri@resolveUri'(XRoot, XX22423, XX22424),
    'lo.resources@getResource'(XX22424, XX22425).
'lo.repo.file@extensionMapping'("source", ".lo").
'lo.repo.file@extensionMapping'("prolog", ".pl").
'lo.repo.file@extensionMapping'("code", "").
'lo.repo.file@packageHash'(XPkg, 'lo.repo#defltVersion', XX22433) :- !,
    ocall('hash%2'(XPkg, XX22433),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.repo.file@packageHash'(XPkg, 'lo.repo#vers'(XV), XX22446) :- !,
    ocall('hash%2'(XPkg, XX22439),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('*%3'(37, XX22439, XX22441),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('hash%2'(XV, XX22444),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('+%3'(XX22441, XX22444, XX22446),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.repo.file@packageHash'(_, _, _) :- raise_exception('error'("packageHash", 72, 3, 42)).
'lo.repo.file@addToRepo'('lo.repo.file#repo'(XRoot, XMan), 'lo.repo#pkg'(XPkg, XVers), XKind, XText, 'lo.repo.file#repo'(XRoot, XNM)) :- 'lo.repo.file@extensionMapping'(XKind, XExt),
    'lo.repo.file@packageHash'(XPkg, XVers, XX22462),
    ocall('_coerce%2'(XX22462, XX22463),'lo.coerce$coercion$lo.core*integer$lo.core*string','lo.coerce$coercion$lo.core*integer$lo.core*string'),
    ocall('+%3'(XPkg, XX22463, XX22465),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%3'(XX22465, XExt, XX22468),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    XFn = XX22468,
    'lo.uri@parseUri'(XFn, XX22472),
    'lo.uri@resolveUri'(XRoot, XX22472, XX22473),
    'lo.resources@putResource'(XX22473, XText),
    'lo.repo.manifest@addToManifest'(XMan, 'lo.repo#pkg'(XPkg, XVers), XKind, XFn, XX22482),
    XNM = XX22482,
    'lo.uri@parseUri'("manifest", XX22485),
    'lo.uri@resolveUri'(XRoot, XX22485, XX22486),
    XRepoUri = XX22486,
    'lo.repo.manifest@flushManifest'(XRepoUri, XNM),
    !.
'lo.repo.file@addToRepo'(_, _, _, _, _) :- raise_exception('error'("addToRepo", 30, 3, 338)).
'lo.repo.file@locateCode'('lo.repo.file#repo'(XRoot, XMan), XPkg, XU, XX22501) :- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, "code", XU),
    'lo.uri@parseUri'(XU, XX22499),
    'lo.uri@resolveUri'(XRoot, XX22499, XX22500),
    'lo.resources@getResource'(XX22500, XX22501).
'lo.repo.file@locateProlog'('lo.repo.file#repo'(XRoot, XMan), XPkg, XU, XX22514) :- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, "prolog", XU),
    'lo.uri@parseUri'(XU, XX22512),
    'lo.uri@resolveUri'(XRoot, XX22512, XX22513),
    'lo.resources@getResource'(XX22513, XX22514).
'lo.repo.file@packageOk'('lo.repo.file#repo'(XRoot, XMan), XPkg, XKind) :- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, XKind, XU),
    'lo.repo.manifest@locateInManifest'(XMan, XPkg, "source", XS),
    'lo.uri@parseUri'(XU, XX22533),
    'lo.uri@resolveUri'(XRoot, XX22533, XX22534),
    XCodeFile = XX22534,
    'lo.uri@parseUri'(XS, XX22538),
    'lo.uri@resolveUri'(XRoot, XX22538, XX22539),
    XSrcFile = XX22539,
    'lo.resources@resourcePresent'(XCodeFile),
    'lo.repo.file@cond15'(XX22545, XX22543, XCodeFile, XSrcFile).
'lo.repo.file@packageCodeOk'(XRepo, XPkg) :- 'lo.repo.file@packageOk'(XRepo, XPkg, "code").
'lo.repo.file@packagePrologOk'(XRepo, XPkg) :- 'lo.repo.file@packageOk'(XRepo, XPkg, "prolog").
'lo.repo.file@addPackage'(XRepo, XPkg, XText, XX22560) :- !,
    'lo.repo.file@addToRepo'(XRepo, XPkg, "code", XText, XX22560).
'lo.repo.file@addPackage'(_, _, _, _) :- raise_exception('error'("addPackage", 63, 3, 60)).
'lo.repo.file@addPrologPackage'(XRepo, XPkg, XText, XX22567) :- !,
    'lo.repo.file@addToRepo'(XRepo, XPkg, "prolog", XText, XX22567).
'lo.repo.file@addPrologPackage'(_, _, _, _) :- raise_exception('error'("addPrologPackage", 66, 3, 68)).
'lo.repo.file@addSource'('lo.repo.file#repo'(XRoot, XMan), XPkg, XNm, 'lo.repo.file#repo'(XRoot, XX22577)) :- !,
    'lo.repo.manifest@addToManifest'(XMan, XPkg, "source", XNm, XX22577).
'lo.repo.file@addSource'(_, _, _, _) :- raise_exception('error'("addSource", 69, 3, 81)).
'lo.repo.file^openRepository'('_call%2'(XV3141, XV3142), 'lo.repo.file^openRepository', _) :- 'lo.repo.file@openRepository'(XV3141, XV3142).
'lo.repo$repository$lo.repo.file*fileRepo@cond14'(XX22415, XX22413, XCodeFile, XSrcFile) :- 'lo.resources@resourcePresent'(XSrcFile),
    !,
    'lo.uri@getUriPath'(XCodeFile, XX22413),
    'lo.uri@getUriPath'(XSrcFile, XX22415),
    'lo.io@newerFile'(XX22413, XX22415).
'lo.repo$repository$lo.repo.file*fileRepo@cond14'(XX22415, XX22413, XCodeFile, XSrcFile).
'lo.repo$repository$lo.repo.file*fileRepo^packagePresent'('_call%3'(XV3143, XV3144, XV3145), 'lo.repo$repository$lo.repo.file*fileRepo^packagePresent'(XLbV292, XThV292), _) :- 'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'(XV3143, XV3144, XV3145, XLbV292, XThV292).
'lo.repo$repository$lo.repo.file*fileRepo^packagePresent'('_call%3'(XV3149, XV3150, XV3151), 'lo.repo$repository$lo.repo.file*fileRepo^packagePresent'(XLbV292, XThV292), _) :- 'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'(XV3149, XV3150, XV3151, XLbV292, XThV292).
'lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'('_call%4'(XV3152, XV3153, XV3154, XV3155), 'lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'(XLbV292, XThV292), _) :- 'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'(XV3152, XV3153, XV3154, XV3155, XLbV292, XThV292).
'lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'('_call%4'(XV3160, XV3161, XV3162, XV3163), 'lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'(XLbV292, XThV292), _) :- 'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'(XV3160, XV3161, XV3162, XV3163, XLbV292, XThV292).
'lo.repo.file^extensionMapping'('_call%2'(XV3164, XV3165), 'lo.repo.file^extensionMapping', _) :- 'lo.repo.file@extensionMapping'(XV3164, XV3165).
'lo.repo.file^packageHash'('_call%3'(XV3166, XV3167, XV3168), 'lo.repo.file^packageHash', _) :- 'lo.repo.file@packageHash'(XV3166, XV3167, XV3168).
'lo.repo.file^addToRepo'('_call%5'(XV3169, XV3170, XV3171, XV3172, XV3173), 'lo.repo.file^addToRepo', _) :- 'lo.repo.file@addToRepo'(XV3169, XV3170, XV3171, XV3172, XV3173).
'lo.repo.file^locateCode'('_call%4'(XV3174, XV3175, XV3176, XV3177), 'lo.repo.file^locateCode', _) :- 'lo.repo.file@locateCode'(XV3174, XV3175, XV3176, XV3177).
'lo.repo.file^locateProlog'('_call%4'(XV3178, XV3179, XV3180, XV3181), 'lo.repo.file^locateProlog', _) :- 'lo.repo.file@locateProlog'(XV3178, XV3179, XV3180, XV3181).
'lo.repo.file@cond15'(XX22545, XX22543, XCodeFile, XSrcFile) :- 'lo.resources@resourcePresent'(XSrcFile),
    !,
    'lo.uri@getUriPath'(XCodeFile, XX22543),
    'lo.uri@getUriPath'(XSrcFile, XX22545),
    'lo.io@newerFile'(XX22543, XX22545).
'lo.repo.file@cond15'(XX22545, XX22543, XCodeFile, XSrcFile).
'lo.repo.file^packageOk'('_call%3'(XV3182, XV3183, XV3184), 'lo.repo.file^packageOk', _) :- 'lo.repo.file@packageOk'(XV3182, XV3183, XV3184).
'lo.repo.file^packageCodeOk'('_call%2'(XV3185, XV3186), 'lo.repo.file^packageCodeOk', _) :- 'lo.repo.file@packageCodeOk'(XV3185, XV3186).
'lo.repo.file^packagePrologOk'('_call%2'(XV3187, XV3188), 'lo.repo.file^packagePrologOk', _) :- 'lo.repo.file@packagePrologOk'(XV3187, XV3188).
'lo.repo.file^addPackage'('_call%4'(XV3189, XV3190, XV3191, XV3192), 'lo.repo.file^addPackage', _) :- 'lo.repo.file@addPackage'(XV3189, XV3190, XV3191, XV3192).
'lo.repo.file^addPrologPackage'('_call%4'(XV3193, XV3194, XV3195, XV3196), 'lo.repo.file^addPrologPackage', _) :- 'lo.repo.file@addPrologPackage'(XV3193, XV3194, XV3195, XV3196).
'lo.repo.file^addSource'('_call%4'(XV3197, XV3198, XV3199, XV3200), 'lo.repo.file^addSource', _) :- 'lo.repo.file@addSource'(XV3197, XV3198, XV3199, XV3200).
