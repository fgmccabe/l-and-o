'#pkg'("n7o7'()7'n2o2'pkg's'lo.repo.file's'1.0.0'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo.manifest'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'s\"I10'repo'CT2t'lo.uri*uri't'lo.repo.manifest*manifest't'lo.repo.file*fileRepo''openRepository'FT1t'lo.uri*uri't'lo.repo.file*fileRepo''addToRepo'FT4t'lo.repo.file*fileRepo't'lo.repo*pkg'SSt'lo.repo.file*fileRepo''locateCode'PT4t'lo.repo.file*fileRepo't'lo.repo*pkg'SS'locateProlog'PT4t'lo.repo.file*fileRepo't'lo.repo*pkg'SS'packageCodeOk'PT2t'lo.repo.file*fileRepo't'lo.repo*pkg''packagePrologOk'PT2t'lo.repo.file*fileRepo't'lo.repo*pkg''addPackage'FT3t'lo.repo.file*fileRepo't'lo.repo*pkg'St'lo.repo.file*fileRepo''addPrologPackage'FT3t'lo.repo.file*fileRepo't'lo.repo*pkg'St'lo.repo.file*fileRepo''addSource'FT3t'lo.repo.file*fileRepo't'lo.repo*pkg'St'lo.repo.file*fileRepo'\"s\"I1'fileRepo'Yt'lo.repo.file*fileRepo'I0\"n1o1'()1's'repo'n0o0'()0'n1o1'()1'n2o2'()2's'lo.repo$repository$lo.repo.file*fileRepo's\"c'lo.repo$repository'T1t'lo.repo.file*fileRepo'T0\"").
'lo.repo.file@init'():- !.
'lo.repo.file#repo'('repo%1'('lo.repo.file@repo'())):- !.
'lo.repo.file@openRepository'(XRoot, 'lo.repo.file#repo'(XRoot, XXd39748)):- 'lo.uri@parseUri'("manifest", XXd39746),
    'lo.uri@resolveUri'(XRoot, XXd39746, XXd39747),
    XRepoUri = XXd39747,
    'lo.resources@resourcePresent'(XRepoUri),
    !,
    'lo.repo.manifest@readManifest'(XRepoUri, XXd39748).
'lo.repo.file@openRepository'(XRoot, 'lo.repo.file#repo'(XRoot, 'lo.repo.manifest#manifest'(XXV5426))):- !,
    ocall('_empty%1'(XXV5426),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.file@openRepository'(_, _):- raise_exception('error'("lo.repo.file@openRepository", 11, 3, 141)).
'lo.repo$repository$lo.repo.file*fileRepo'('lo.repo$repository$lo.repo.file*fileRepo%1'('lo.repo$repository$lo.repo.file*fileRepo')):- !.
'lo.repo$repository$lo.repo.file*fileRepo'('packagePresent%3'(XV31580, XV31581, XV31582), XLbl2215, XThis2215):- !,
    'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'(XV31580, XV31581, XV31582, XLbl2215, XThis2215).
'lo.repo$repository$lo.repo.file*fileRepo'('packagePresent%1'('lo.repo$repository$lo.repo.file*fileRepo^packagePresent'(XLbl2216, XThis2216)), XLbl2216, XThis2216).
'lo.repo$repository$lo.repo.file*fileRepo'('loadFromRepo%4'(XV31589, XV31590, XV31591, XV31592), XLbl2217, XThis2217):- !,
    'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'(XV31589, XV31590, XV31591, XV31592, XLbl2217, XThis2217).
'lo.repo$repository$lo.repo.file*fileRepo'('loadFromRepo%1'('lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'(XLbl2218, XThis2218)), XLbl2218, XThis2218).
'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'('lo.repo.file#repo'(XRoot, XMan), XPkg, XKind, XLbV2541, XThV2541):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, XS, XU),
    'lo.uri@parseUri'(XU, XXd39753),
    'lo.uri@resolveUri'(XRoot, XXd39753, XXd39754),
    XCodeFile = XXd39754,
    'lo.uri@parseUri'(XS, XXd39755),
    'lo.uri@resolveUri'(XRoot, XXd39755, XXd39756),
    XSrcFile = XXd39756,
    'lo.resources@resourcePresent'(XCodeFile),
    'lo.repo$repository$lo.repo.file*fileRepo@cond412'(XXd39758, XXd39757, XCodeFile, XSrcFile).
'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'('lo.repo.file#repo'(XRoot, XMan), XPkg, XKind, XXb20052, XLbV2541, XThV2541):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, XKind, XU),
    'lo.uri@parseUri'(XU, XXb20050),
    'lo.uri@resolveUri'(XRoot, XXb20050, XXb20051),
    'lo.resources@getResource'(XXb20051, XXb20052).
'lo.repo.file@extensionMapping'("source", ".lo").
'lo.repo.file@extensionMapping'("prolog", ".pl").
'lo.repo.file@extensionMapping'("code", "").
'lo.repo.file@packageHash'(XPkg, 'lo.repo#defltVersion', XXe5062):- !,
    ocall('hash%1'(XXV5427),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'(XPkg, XXe5062),XXV5427,XXV5427).
'lo.repo.file@packageHash'(XPkg, 'lo.repo#vers'(XV), XXe5066):- !,
    ocall('hash%1'(XXV5428),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('*%1'(XXV5429),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('hash%1'(XXV5430),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('+%1'(XXV5431),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XPkg, XXe5063),XXV5428,XXV5428),
    ocall('_call%3'(37, XXe5063, XXe5064),XXV5429,XXV5429),
    ocall('_call%2'(XV, XXe5065),XXV5430,XXV5430),
    ocall('_call%3'(XXe5064, XXe5065, XXe5066),XXV5431,XXV5431).
'lo.repo.file@packageHash'(_, _, _):- raise_exception('error'("lo.repo.file@packageHash", 72, 3, 42)).
'lo.repo.file@addToRepo'('lo.repo.file#repo'(XRoot, XMan), 'lo.repo#pkg'(XPkg, XVers), XKind, XText, 'lo.repo.file#repo'(XRoot, XNM)):- 'lo.repo.file@extensionMapping'(XKind, XExt),
    ocall('_coerce%1'(XXV5432),'lo.coerce$coercion$lo.core*integer$lo.core*string','lo.coerce$coercion$lo.core*integer$lo.core*string'),
    ocall('+%1'(XXV5433),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%1'(XXV5434),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    'lo.repo.file@packageHash'(XPkg, XVers, XXd39759),
    ocall('_call%2'(XXd39759, XXe5067),XXV5432,XXV5432),
    ocall('_call%3'(XPkg, XXe5067, XXe5068),XXV5433,XXV5433),
    ocall('_call%3'(XXe5068, XExt, XXe5069),XXV5434,XXV5434),
    XFn = XXe5069,
    'lo.uri@parseUri'(XFn, XXd39760),
    'lo.uri@resolveUri'(XRoot, XXd39760, XXd39761),
    'lo.resources@putResource'(XXd39761, XText),
    'lo.repo.manifest@addToManifest'(XMan, 'lo.repo#pkg'(XPkg, XVers), XKind, XFn, XXd39763),
    XNM = XXd39763,
    'lo.uri@parseUri'("manifest", XXd39764),
    'lo.uri@resolveUri'(XRoot, XXd39764, XXd39765),
    XRepoUri = XXd39765,
    'lo.repo.manifest@flushManifest'(XRepoUri, XNM),
    !.
'lo.repo.file@addToRepo'(_, _, _, _, _):- raise_exception('error'("lo.repo.file@addToRepo", 30, 3, 338)).
'lo.repo.file@locateCode'('lo.repo.file#repo'(XRoot, XMan), XPkg, XU, XXb20059):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, "code", XU),
    'lo.uri@parseUri'(XU, XXb20057),
    'lo.uri@resolveUri'(XRoot, XXb20057, XXb20058),
    'lo.resources@getResource'(XXb20058, XXb20059).
'lo.repo.file@locateProlog'('lo.repo.file#repo'(XRoot, XMan), XPkg, XU, XXb20063):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, "prolog", XU),
    'lo.uri@parseUri'(XU, XXb20061),
    'lo.uri@resolveUri'(XRoot, XXb20061, XXb20062),
    'lo.resources@getResource'(XXb20062, XXb20063).
'lo.repo.file@packageOk'('lo.repo.file#repo'(XRoot, XMan), XPkg, XKind):- 'lo.repo.manifest@locateInManifest'(XMan, XPkg, XKind, XU),
    'lo.repo.manifest@locateInManifest'(XMan, XPkg, "source", XS),
    'lo.uri@parseUri'(XU, XXd39767),
    'lo.uri@resolveUri'(XRoot, XXd39767, XXd39768),
    XCodeFile = XXd39768,
    'lo.uri@parseUri'(XS, XXd39769),
    'lo.uri@resolveUri'(XRoot, XXd39769, XXd39770),
    XSrcFile = XXd39770,
    'lo.resources@resourcePresent'(XCodeFile),
    'lo.repo.file@cond413'(XXd39772, XXd39771, XCodeFile, XSrcFile).
'lo.repo.file@packageCodeOk'(XRepo, XPkg):- 'lo.repo.file@packageOk'(XRepo, XPkg, "code").
'lo.repo.file@packagePrologOk'(XRepo, XPkg):- 'lo.repo.file@packageOk'(XRepo, XPkg, "prolog").
'lo.repo.file@addPackage'(XRepo, XPkg, XText, XXd39773):- !,
    'lo.repo.file@addToRepo'(XRepo, XPkg, "code", XText, XXd39773).
'lo.repo.file@addPackage'(_, _, _, _):- raise_exception('error'("lo.repo.file@addPackage", 63, 3, 60)).
'lo.repo.file@addPrologPackage'(XRepo, XPkg, XText, XXd39774):- !,
    'lo.repo.file@addToRepo'(XRepo, XPkg, "prolog", XText, XXd39774).
'lo.repo.file@addPrologPackage'(_, _, _, _):- raise_exception('error'("lo.repo.file@addPrologPackage", 66, 3, 68)).
'lo.repo.file@addSource'('lo.repo.file#repo'(XRoot, XMan), XPkg, XNm, 'lo.repo.file#repo'(XRoot, XXd39775)):- !,
    'lo.repo.manifest@addToManifest'(XMan, XPkg, "source", XNm, XXd39775).
'lo.repo.file@addSource'(_, _, _, _):- raise_exception('error'("lo.repo.file@addSource", 69, 3, 81)).
'lo.repo.file^openRepository'('_call%2'(XV31573, XV31574), 'lo.repo.file^openRepository', _):- 'lo.repo.file@openRepository'(XV31573, XV31574).
'lo.repo$repository$lo.repo.file*fileRepo@cond412'(XXd39758, XXd39757, XCodeFile, XSrcFile):- 'lo.resources@resourcePresent'(XSrcFile),
    !,
    'lo.uri@getUriPath'(XCodeFile, XXd39757),
    'lo.uri@getUriPath'(XSrcFile, XXd39758),
    'lo.io@newerFile'(XXd39757, XXd39758).
'lo.repo$repository$lo.repo.file*fileRepo@cond412'(XXd39758, XXd39757, XCodeFile, XSrcFile).
'lo.repo$repository$lo.repo.file*fileRepo^packagePresent'('_call%5'(XV31575, XV31576, XV31577, XV31578, XV31579), 'lo.repo$repository$lo.repo.file*fileRepo^packagePresent'(XLbV2541, XThV2541), _):- 'lo.repo$repository$lo.repo.file*fileRepo@packagePresent'(XV31575, XV31576, XV31577, XV31578, XV31579, XLbV2541, XThV2541).
'lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'('_call%6'(XV31583, XV31584, XV31585, XV31586, XV31587, XV31588), 'lo.repo$repository$lo.repo.file*fileRepo^loadFromRepo'(XLbV2541, XThV2541), _):- 'lo.repo$repository$lo.repo.file*fileRepo@loadFromRepo'(XV31583, XV31584, XV31585, XV31586, XV31587, XV31588, XLbV2541, XThV2541).
'lo.repo.file^extensionMapping'('_call%2'(XV31593, XV31594), 'lo.repo.file^extensionMapping', _):- 'lo.repo.file@extensionMapping'(XV31593, XV31594).
'lo.repo.file^packageHash'('_call%3'(XV31595, XV31596, XV31597), 'lo.repo.file^packageHash', _):- 'lo.repo.file@packageHash'(XV31595, XV31596, XV31597).
'lo.repo.file^addToRepo'('_call%5'(XV31598, XV31599, XV31600, XV31601, XV31602), 'lo.repo.file^addToRepo', _):- 'lo.repo.file@addToRepo'(XV31598, XV31599, XV31600, XV31601, XV31602).
'lo.repo.file^locateCode'('_call%4'(XV31603, XV31604, XV31605, XV31606), 'lo.repo.file^locateCode', _):- 'lo.repo.file@locateCode'(XV31603, XV31604, XV31605, XV31606).
'lo.repo.file^locateProlog'('_call%4'(XV31607, XV31608, XV31609, XV31610), 'lo.repo.file^locateProlog', _):- 'lo.repo.file@locateProlog'(XV31607, XV31608, XV31609, XV31610).
'lo.repo.file@cond413'(XXd39772, XXd39771, XCodeFile, XSrcFile):- 'lo.resources@resourcePresent'(XSrcFile),
    !,
    'lo.uri@getUriPath'(XCodeFile, XXd39771),
    'lo.uri@getUriPath'(XSrcFile, XXd39772),
    'lo.io@newerFile'(XXd39771, XXd39772).
'lo.repo.file@cond413'(XXd39772, XXd39771, XCodeFile, XSrcFile).
'lo.repo.file^packageOk'('_call%3'(XV31611, XV31612, XV31613), 'lo.repo.file^packageOk', _):- 'lo.repo.file@packageOk'(XV31611, XV31612, XV31613).
'lo.repo.file^packageCodeOk'('_call%2'(XV31614, XV31615), 'lo.repo.file^packageCodeOk', _):- 'lo.repo.file@packageCodeOk'(XV31614, XV31615).
'lo.repo.file^packagePrologOk'('_call%2'(XV31616, XV31617), 'lo.repo.file^packagePrologOk', _):- 'lo.repo.file@packagePrologOk'(XV31616, XV31617).
'lo.repo.file^addPackage'('_call%4'(XV31618, XV31619, XV31620, XV31621), 'lo.repo.file^addPackage', _):- 'lo.repo.file@addPackage'(XV31618, XV31619, XV31620, XV31621).
'lo.repo.file^addPrologPackage'('_call%4'(XV31622, XV31623, XV31624, XV31625), 'lo.repo.file^addPrologPackage', _):- 'lo.repo.file@addPrologPackage'(XV31622, XV31623, XV31624, XV31625).
'lo.repo.file^addSource'('_call%4'(XV31626, XV31627, XV31628, XV31629), 'lo.repo.file^addSource', _):- 'lo.repo.file@addSource'(XV31626, XV31627, XV31628, XV31629).
