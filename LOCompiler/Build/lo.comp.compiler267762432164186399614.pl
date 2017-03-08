'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.compiler's'0.0.1'n21o21'()21'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.either'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo.file'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.encode'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.grammar'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.args'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.catalog'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.genprolog'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.imports'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.typecheck'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.grapher'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.transform'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.pkg'e'*'s'I0's'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.compiler@init'():- !.
'lo.comp.compiler@compilePkg'(XPkg, XRepo, XRepox, XCat, XOpts, XRp, XRpx):- XPkg = 'lo.repo#pkg'(XP, XV),
    'lo.comp.compiler@cond516'(XXd42344, XXd42343, XXd42342, XXd42341, XXd42340, XXd42339, XXe5244, XXV5669, XXd42338, XP, XXd42337, XXd42336, XXd42335, XXd42334, XXd42333, XXd42332, XXd42331, XXd42330, XXd42329, XemptyArray, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XXd42324, XXe5243, XXV5667, XXd42323, XXe5242, XXV5666, XRepo1, XRp2, XTrPrg, XOpts, XXd42322, XRp1, XRepo, XV, XPrg, XXd42321, XRp0, XRp, XTerm, XXc556, XXd42320, XSrc, X_36879, XU, XPkg, XCat).
'lo.comp.compiler@compilePkgs'('lo.core#[]', X_36883, X_36884, X_36885, XRp, XRp).
'lo.comp.compiler@compilePkgs'('lo.core#,..'(XPk, XL), XRepo, XCat, XOpt, XRp, XRpx):- 'lo.comp.errors@errorFree'(XRp),
    'lo.comp.compiler@compilePkg'(XPk, XRepo, XRepo0, XCat, XOpt, XRp, XRp0),
    'lo.comp.compiler@compilePkgs'(XL, XRepo0, XCat, XOpts, XRp0, XRpx).
'lo.comp.compiler@compilePkgs'(X_36887, X_36888, X_36889, X_36890, XRp, XRp):- 'lo.comp.compiler@neg353'(XRp).
'lo.comp.compiler@importsOk'('lo.core#[]', X_36891).
'lo.comp.compiler@importsOk'('lo.core#,..'(XPk, XL), XCP):- 'lo.comp.compiler@neg354'(XPk, XCP, XP),
    'lo.comp.compiler@importsOk'(XL, XCP).
'lo.comp.compiler@processPkg'(XP, XImps, XRepo, XRepo, X_36893, XOpts, XCP, XCP, XRp, XRp):- 'lo.comp.compiler@importsOk'(XImps, XCP),
    'lo.comp.compiler@cond517'(XP, XRepo, XXV5670, XOpts),
    ocall('disp%1'(XXV5671),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    ocall('_call%2'(XP, XXe5245),XXV5671,XXV5671),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("skipping package "), 'lo.core#,..'(XXe5245, 'lo.core#[]'))), XXd42349),
    '_logmsg'(XXd42349).
'lo.comp.compiler@processPkg'(XP, X_36896, XRepo, XRepoX, XCat, XOpts, XCP, 'lo.core#,..'(XP, XCP), XRp, XRpx):- ocall('disp%1'(XXV5672),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    ocall('_call%2'(XP, XXe5246),XXV5672,XXV5672),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("compiling package "), 'lo.core#,..'(XXe5246, 'lo.core#[]'))), XXd42354),
    '_logmsg'(XXd42354),
    'lo.comp.compiler@one326'(XRpx, XRp, XOpts, XCat, XRepoX, XRepo, XP).
'lo.comp.compiler@processGroup'('lo.core#[]', XRepo, XRepo, X_36900, X_36901, XCP, XCP, XRp, XRp).
'lo.comp.compiler@processGroup'('lo.core#,..'('()2'(XP, XImps), XL), XRepo, XRepoX, XCat, XOpts, XCP, XCPx, XRp, XRpx):- 'lo.comp.compiler@one327'(XRp0, XRp, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XImps, XP),
    'lo.comp.compiler@processGroup'(XL, XRepo0, XRepoX, XCat, XOpts, XCP0, XCPx, XRp0, XRpx).
'lo.comp.compiler@projectPkgs'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.compiler@projectPkgs'('lo.core#,..'('()2'(XPkg, X_36904), XL), 'lo.core#,..'(XPkg, XXd42355)):- !,
    'lo.comp.compiler@projectPkgs'(XL, XXd42355).
'lo.comp.compiler@projectPkgs'(_, _):- raise_exception('error'("lo.comp.compiler@projectPkgs", 54, 3, 21)).
'lo.comp.compiler@processGroups'(X_36906, X_36907, X_36908, X_36909, X_36910, XRp, XRpx):- 'lo.comp.errors@countErrors'(XRp, XXd42357),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd42357, 0),
    'lo.comp.errors@reportMsg'("aborting compilation", 'lo.comp.location#std', XRp, XRpx).
'lo.comp.compiler@processGroups'('lo.core#[]', X_36911, X_36912, X_36913, X_36914, XRp, XRp).
'lo.comp.compiler@processGroups'('lo.core#,..'(XGp, XL), XRepo, XCat, XOpts, XCP, XRp, XRpx):- 'lo.comp.compiler@cond518'(XL, XRp0, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XRpx, XRp, XXd42365, XXd42364, XXd42363, XXd42362, XXe5247, XXV5673, XXd42361, XXd42360, XXd42359, XXd42358, XGp).
'lo.comp.compiler@processArgs'('lo.either#either'(XOpts)):- ocall('repoDir%1'(XXV5674),XOpts,XOpts),
    'lo.repo.file@openRepository'(XXV5674, XXd42366),
    XRepo = XXd42366,
    ocall('wDir%1'(XXV5675),XOpts,XOpts),
    'lo.comp.catalog@locateCatalog'(XXV5675, XXd42367),
    XCat = XXd42367,
    'lo.comp.compiler@cond519'(XreportBase18, XXV5678, XRpx, XXd42368, XRp0, XreportBase17, XCat, XRepo, XXV5677, XXV5676, XOpts),
    'lo.comp.errors@fullReport'(XRpx, XXd42369),
    'lo@formatSS'(XXd42369, XXd42370),
    '_logmsg'(XXd42370).
'lo.comp.compiler@processArgs'('lo.either#alternate'(XM)):- '_logmsg'(XM).
'lo.comp.compiler@main'(XArgs):- 'lo.comp.args@parseFlags'(XArgs, XXd42371),
    'lo.comp.compiler@processArgs'(XXd42371).
'lo.comp.compiler@cond512'(XXd42327, XXc557, XXd42326, XPkg, XXd42325, XTrPrg, XCode, XRepo1, XRepox, XXV5668, XOpts):- ocall('prologOnly%1'(XXV5668),XOpts,XOpts),
    XXV5668 = 'lo.core#true',
    !,
    XRepox = XRepo1.
'lo.comp.compiler@cond512'(XXd42327, XXc557, XXd42326, XPkg, XXd42325, XTrPrg, XCode, XRepo1, XRepox, XXV5668, XOpts):- 'lo.comp.code.pkg@compileMdl'(XTrPrg, XXd42325),
    XCode = XXd42325,
    'lo.comp.encode@encMdl'(XCode, XXd42326),
    'implode'(XXd42326, XXc557),
    'lo.repo.file@addPackage'(XRepo1, XPkg, XXc557, XXd42327),
    XRepox = XXd42327.
'lo.comp.compiler@cond513'(XXd42330, XXd42329, XemptyArray, XSrc, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XOpts, XXd42324, XXe5243, XXV5667, XTrPrg, XXd42323, XXe5242, XXV5666, XU, XPkg, XRepo, XRepo1, XRp2):- 'lo.comp.errors@errorFree'(XRp2),
    !,
    ocall('_coerce%1'(XXV5666),'lo.coerce$coercion$lo.uri*uri$lo.core*string','lo.coerce$coercion$lo.uri*uri$lo.core*string'),
    ocall('_coerce%1'(XXV5667),'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string','lo.coerce$coercion$lo.comp.term*prProg$lo.core*string'),
    ocall('_call%2'(XU, XXe5242),XXV5666,XXV5666),
    'lo.repo.file@addSource'(XRepo, XPkg, XXe5242, XXd42323),
    ocall('_call%2'(XTrPrg, XXe5243),XXV5667,XXV5667),
    'lo.repo.file@addPrologPackage'(XXd42323, XPkg, XXe5243, XXd42324),
    XRepo1 = XXd42324,
    'lo.comp.compiler@cond512'(XXd42327, XXc557, XXd42326, XPkg, XXd42325, XTrPrg, XCode, XRepo1, XRepox, XXV5668, XOpts).
'lo.comp.compiler@cond513'(XXd42330, XXd42329, XemptyArray, XSrc, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XOpts, XXd42324, XXe5243, XXV5667, XTrPrg, XXd42323, XXe5242, XXV5666, XU, XPkg, XRepo, XRepo1, XRp2):- 'lo.uri@getUriPath'(XU, XXd42328),
    'lo.comp.location@collectSrc'(XSrc, 1, XemptyArray, XXd42329),
    'lo.comp.errors@populateContext'(XRp2, XXd42328, XXd42329, XXd42330),
    XRpx = XXd42330.
'lo.comp.compiler@cond514'(XXd42333, XXd42332, XXd42331, XXd42330, XXd42329, XemptyArray, XSrc, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XXd42324, XXe5243, XXV5667, XXd42323, XXe5242, XXV5666, XU, XPkg, XRepo, XRepo1, XRp2, XTrPrg, XOpts, XPrg, XRp1):- 'lo.comp.errors@errorFree'(XRp1),
    !,
    'lo.comp.transform@transformProg'(XPrg, XOpts, XTrPrg, XRp1, XRp2),
    'lo.comp.compiler@cond513'(XXd42330, XXd42329, XemptyArray, XSrc, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XOpts, XXd42324, XXe5243, XXV5667, XTrPrg, XXd42323, XXe5242, XXV5666, XU, XPkg, XRepo, XRepo1, XRp2).
'lo.comp.compiler@cond514'(XXd42333, XXd42332, XXd42331, XXd42330, XXd42329, XemptyArray, XSrc, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XXd42324, XXe5243, XXV5667, XXd42323, XXe5242, XXV5666, XU, XPkg, XRepo, XRepo1, XRp2, XTrPrg, XOpts, XPrg, XRp1):- 'lo.uri@getUriPath'(XU, XXd42331),
    'lo.comp.location@collectSrc'(XSrc, 1, XemptyArray, XXd42332),
    'lo.comp.errors@populateContext'(XRp1, XXd42331, XXd42332, XXd42333),
    XRpx = XXd42333.
'lo.comp.compiler@cond515'(XXd42336, XXd42335, XXd42334, XXd42333, XXd42332, XXd42331, XXd42330, XXd42329, XemptyArray, XSrc, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XXd42324, XXe5243, XXV5667, XXd42323, XXe5242, XXV5666, XU, XPkg, XRepo1, XRp2, XTrPrg, XOpts, XXd42322, XRp1, XRepo, XV, XTerm, XPrg, XRp0):- 'lo.comp.errors@errorFree'(XRp0),
    !,
    'lo.comp.typecheck@checkProgram'('lo.repo$repository$lo.repo.file*fileRepo', XTerm, XV, XRepo, XRp0, XRp1, XXd42322),
    XPrg = XXd42322,
    'lo.comp.compiler@cond514'(XXd42333, XXd42332, XXd42331, XXd42330, XXd42329, XemptyArray, XSrc, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XXd42324, XXe5243, XXV5667, XXd42323, XXe5242, XXV5666, XU, XPkg, XRepo, XRepo1, XRp2, XTrPrg, XOpts, XPrg, XRp1).
'lo.comp.compiler@cond515'(XXd42336, XXd42335, XXd42334, XXd42333, XXd42332, XXd42331, XXd42330, XXd42329, XemptyArray, XSrc, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XXd42324, XXe5243, XXV5667, XXd42323, XXe5242, XXV5666, XU, XPkg, XRepo1, XRp2, XTrPrg, XOpts, XXd42322, XRp1, XRepo, XV, XTerm, XPrg, XRp0):- 'lo.uri@getUriPath'(XU, XXd42334),
    'lo.comp.location@collectSrc'(XSrc, 1, XemptyArray, XXd42335),
    'lo.comp.errors@populateContext'(XRp0, XXd42334, XXd42335, XXd42336),
    XRpx = XXd42336.
'lo.comp.compiler@cond516'(XXd42344, XXd42343, XXd42342, XXd42341, XXd42340, XXd42339, XXe5244, XXV5669, XXd42338, XP, XXd42337, XXd42336, XXd42335, XXd42334, XXd42333, XXd42332, XXd42331, XXd42330, XXd42329, XemptyArray, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XXd42324, XXe5243, XXV5667, XXd42323, XXe5242, XXV5666, XRepo1, XRp2, XTrPrg, XOpts, XXd42322, XRp1, XRepo, XV, XPrg, XXd42321, XRp0, XRp, XTerm, XXc556, XXd42320, XSrc, X_36879, XU, XPkg, XCat):- 'lo.comp.catalog@resolveCatalog'(XCat, XPkg, XU, X_36879),
    !,
    'lo.resources@getResource'(XU, XXd42320),
    'explode'(XXd42320, XXc556),
    XSrc = XXc556,
    'lo.comp.grammar@parseSrc'(XU, XSrc, XRp, XRp0, XXd42321),
    XTerm = XXd42321,
    'lo.comp.compiler@cond515'(XXd42336, XXd42335, XXd42334, XXd42333, XXd42332, XXd42331, XXd42330, XXd42329, XemptyArray, XSrc, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XXd42324, XXe5243, XXV5667, XXd42323, XXe5242, XXV5666, XU, XPkg, XRepo1, XRp2, XTrPrg, XOpts, XXd42322, XRp1, XRepo, XV, XTerm, XPrg, XRp0).
'lo.comp.compiler@cond516'(XXd42344, XXd42343, XXd42342, XXd42341, XXd42340, XXd42339, XXe5244, XXV5669, XXd42338, XP, XXd42337, XXd42336, XXd42335, XXd42334, XXd42333, XXd42332, XXd42331, XXd42330, XXd42329, XemptyArray, XXd42328, XRpx, XXd42327, XXc557, XXd42326, XXd42325, XCode, XRepox, XXV5668, XXd42324, XXe5243, XXV5667, XXd42323, XXe5242, XXV5666, XRepo1, XRp2, XTrPrg, XOpts, XXd42322, XRp1, XRepo, XV, XPrg, XXd42321, XRp0, XRp, XTerm, XXc556, XXd42320, XSrc, X_36879, XU, XPkg, XCat):- ocall('disp%1'(XXV5669),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    ocall('_call%2'('lo.repo#pkg'(XP, XV), XXe5244),XXV5669,XXV5669),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot locate "), 'lo.core#,..'(XXe5244, 'lo.core#,..'('lo.core#ss'(" in catalog"), 'lo.core#[]')))), XXd42344),
    'lo.comp.errors@reportError'(XXd42344, 'lo.comp.location#std', XRp, XRpx),
    XRepox = XRepo.
'lo.comp.compiler^compilePkg'('_call%7'(XV33486, XV33487, XV33488, XV33489, XV33490, XV33491, XV33492), 'lo.comp.compiler^compilePkg', _):- 'lo.comp.compiler@compilePkg'(XV33486, XV33487, XV33488, XV33489, XV33490, XV33491, XV33492).
'lo.comp.compiler@neg353'(XRp):- 'lo.comp.errors@errorFree'(XRp),
    !,
    fail.
'lo.comp.compiler@neg353'(XRp).
'lo.comp.compiler^compilePkgs'('_call%6'(XV33493, XV33494, XV33495, XV33496, XV33497, XV33498), 'lo.comp.compiler^compilePkgs', _):- 'lo.comp.compiler@compilePkgs'(XV33493, XV33494, XV33495, XV33496, XV33497, XV33498).
'lo.comp.compiler@neg354'(XPk, XCP, XP):- ocall('in%2'(XP, XCP),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.imports@consistentPkg'(XPk, XP),
    !,
    fail.
'lo.comp.compiler@neg354'(XPk, XCP, XP).
'lo.comp.compiler^importsOk'('_call%2'(XV33499, XV33500), 'lo.comp.compiler^importsOk', _):- 'lo.comp.compiler@importsOk'(XV33499, XV33500).
'lo.comp.compiler@cond517'(XP, XRepo, XXV5670, XOpts):- ocall('prologOnly%1'(XXV5670),XOpts,XOpts),
    XXV5670 = 'lo.core#true',
    !,
    'lo.repo.file@packagePrologOk'(XRepo, XP).
'lo.comp.compiler@cond517'(XP, XRepo, XXV5670, XOpts):- 'lo.repo.file@packageCodeOk'(XRepo, XP).
'lo.comp.compiler@one326'(XRpx, XRp, XOpts, XCat, XRepoX, XRepo, XP):- 'lo.comp.compiler@compilePkg'(XP, XRepo, XRepoX, XCat, XOpts, XRp, XRpx),
    !.
'lo.comp.compiler^processPkg'('_call%10'(XV33501, XV33502, XV33503, XV33504, XV33505, XV33506, XV33507, XV33508, XV33509, XV33510), 'lo.comp.compiler^processPkg', _):- 'lo.comp.compiler@processPkg'(XV33501, XV33502, XV33503, XV33504, XV33505, XV33506, XV33507, XV33508, XV33509, XV33510).
'lo.comp.compiler@one327'(XRp0, XRp, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XImps, XP):- 'lo.comp.compiler@processPkg'(XP, XImps, XRepo, XRepo0, XCat, XOpts, XCP, XCP0, XRp, XRp0),
    !.
'lo.comp.compiler^processGroup'('_call%9'(XV33511, XV33512, XV33513, XV33514, XV33515, XV33516, XV33517, XV33518, XV33519), 'lo.comp.compiler^processGroup', _):- 'lo.comp.compiler@processGroup'(XV33511, XV33512, XV33513, XV33514, XV33515, XV33516, XV33517, XV33518, XV33519).
'lo.comp.compiler^projectPkgs'('_call%2'(XV33520, XV33521), 'lo.comp.compiler^projectPkgs', _):- 'lo.comp.compiler@projectPkgs'(XV33520, XV33521).
'lo.comp.compiler@cond518'(XL, XRp0, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XRpx, XRp, XXd42365, XXd42364, XXd42363, XXd42362, XXe5247, XXV5673, XXd42361, XXd42360, XXd42359, XXd42358, XGp):- 'lo.list@length'(XGp, XXd42358),
    'lo.core@>'('lo.core$comp$lo.core*integer', XXd42358, 1),
    !,
    ocall('disp%1'(XXV5673),'lo.core$display$lo.core*list'('lo.core$display$lo.repo*pkg'),'lo.core$display$lo.core*list'('lo.core$display$lo.repo*pkg')),
    'lo.comp.compiler@projectPkgs'(XGp, XXd42360),
    ocall('_call%2'(XXd42360, XXe5247),XXV5673,XXV5673),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("circular dependency in packages "), 'lo.core#,..'(XXe5247, 'lo.core#[]'))), XXd42365),
    'lo.comp.errors@reportError'(XXd42365, 'lo.comp.location#std', XRp, XRpx).
'lo.comp.compiler@cond518'(XL, XRp0, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XRpx, XRp, XXd42365, XXd42364, XXd42363, XXd42362, XXe5247, XXV5673, XXd42361, XXd42360, XXd42359, XXd42358, XGp):- 'lo.comp.compiler@processGroup'(XGp, XRepo, XRepo0, XCat, XOpts, XCP, XCP0, XRp, XRp0),
    'lo.comp.compiler@processGroups'(XL, XRepo0, XCat, XOpts, XCP0, XRp0, XRpx).
'lo.comp.compiler^processGroups'('_call%7'(XV33522, XV33523, XV33524, XV33525, XV33526, XV33527, XV33528), 'lo.comp.compiler^processGroups', _):- 'lo.comp.compiler@processGroups'(XV33522, XV33523, XV33524, XV33525, XV33526, XV33527, XV33528).
'lo.comp.compiler@one328'(XRpx, XXd42368, XRp0, XreportBase17, XCat, XRepo, XXV5677, XOpts):- 'lo.comp.errors@reportBase'(XreportBase17),
    ocall('pkgs%1'(XXV5677),XOpts,XOpts),
    'lo.comp.grapher@makeGraph'(XXV5677, XRepo, XCat, XreportBase17, XRp0, XXd42368),
    'lo.comp.compiler@processGroups'(XXd42368, XRepo, XCat, XOpts, 'lo.core#[]', XRp0, XRpx),
    !.
'lo.comp.compiler@cond519'(XreportBase18, XXV5678, XRpx, XXd42368, XRp0, XreportBase17, XCat, XRepo, XXV5677, XXV5676, XOpts):- ocall('dependency%1'(XXV5676),XOpts,XOpts),
    XXV5676 = 'lo.core#true',
    !,
    'lo.comp.compiler@one328'(XRpx, XXd42368, XRp0, XreportBase17, XCat, XRepo, XXV5677, XOpts).
'lo.comp.compiler@cond519'(XreportBase18, XXV5678, XRpx, XXd42368, XRp0, XreportBase17, XCat, XRepo, XXV5677, XXV5676, XOpts):- 'lo.comp.errors@reportBase'(XreportBase18),
    ocall('pkgs%1'(XXV5678),XOpts,XOpts),
    'lo.comp.compiler@compilePkgs'(XXV5678, XRepo, XCat, XOpts, XreportBase18, XRpx).
'lo.comp.compiler^processArgs'('_call%1'(XV33529), 'lo.comp.compiler^processArgs', _):- 'lo.comp.compiler@processArgs'(XV33529).
'lo.comp.compiler^main'('_call%1'(XV33530), 'lo.comp.compiler^main', _):- 'lo.comp.compiler@main'(XV33530).
