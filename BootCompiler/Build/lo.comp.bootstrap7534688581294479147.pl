'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.bootstrap'e'*'n29o29'()29'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.transform'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.grapher'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.typecheck'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.imports'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.genprolog'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.catalog'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.args'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.grammar'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo.file'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.either'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s'I0's'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.bootstrap@init'() :- !.
'lo.comp.bootstrap@projectPkgs'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.bootstrap@projectPkgs'('lo.core#,..'((XPkg, X_2559), XL), 'lo.core#,..'(XPkg, XX41344)) :- !,
    'lo.comp.bootstrap@projectPkgs'(XL, XX41344).
'lo.comp.bootstrap@projectPkgs'(_, _) :- raise_exception('error'("projectPkgs", 51, 3, 21)).
'lo.comp.bootstrap@importsOk'('lo.core#[]', X_2560).
'lo.comp.bootstrap@importsOk'('lo.core#,..'(XPk, XL), XCP) :- 'lo.comp.bootstrap@neg45'(XPk, XCP, XP),
    'lo.comp.bootstrap@importsOk'(XL, XCP).
'lo.comp.bootstrap@compilePkg'(XPkg, XRepo, XRepox, XCat, XOpts, XRp, XRpx) :- XPkg = 'lo.repo#pkg'(XP, XV),
    'lo.comp.bootstrap@cond38'(XX41439, XX41431, XP, XPrg, XV, XRepo, XRp1, XX41392, XX41418, XX41417, XemptyArray, XX41414, XX41410, XX41408, XX41405, XX41403, XRepox, XRpx, XTrPrg, XOpts, XX41422, XX41425, XX41426, XX41383, XRp0, XRp, XTerm, XX41377, XX41376, XSrc, X_2561, XU, XPkg, XCat).
'lo.comp.bootstrap@processPkg'(XP, XImps, XRepo, XRepo, X_2562, X_2563, XCP, XCP, XRp, XRp) :- 'lo.comp.bootstrap@importsOk'(XImps, XCP),
    'lo.repo.file@packagePrologOk'(XRepo, XP),
    ocall('disp%2'(XP, XX41461),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("skipping package "), 'lo.core#,..'(XX41461, 'lo.core#[]'))), XX41467),
    '_logmsg'(XX41467).
'lo.comp.bootstrap@processPkg'(XP, X_2564, XRepo, XRepoX, XCat, XOpts, XCP, 'lo.core#,..'(XP, XCP), XRp, XRpx) :- ocall('disp%2'(XP, XX41482),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("compiling package "), 'lo.core#,..'(XX41482, 'lo.core#[]'))), XX41488),
    '_logmsg'(XX41488),
    'lo.comp.bootstrap@one48'(XRpx, XRp, XOpts, XCat, XRepoX, XRepo, XP).
'lo.comp.bootstrap@processGroup'('lo.core#[]', XRepo, XRepo, X_2565, X_2566, XCP, XCP, XRp, XRp).
'lo.comp.bootstrap@processGroup'('lo.core#,..'((XP, XImps), XL), XRepo, XRepoX, XCat, XOpts, XCP, XCPx, XRp, XRpx) :- 'lo.comp.bootstrap@one49'(XRp0, XRp, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XImps, XP),
    'lo.comp.bootstrap@processGroup'(XL, XRepo0, XRepoX, XCat, XOpts, XCP0, XCPx, XRp0, XRpx).
'lo.comp.bootstrap@processGroups'(X_2567, X_2568, X_2569, X_2570, X_2571, XRp, XRpx) :- 'lo.comp.errors@countErrors'(XRp, XX41545),
    'lo.core@>'('lo.core$comp$lo.core*integer', XX41545, 0),
    'lo.comp.errors@reportMsg'("aborting compilation", 'lo.comp.location#std', XRp, XRpx).
'lo.comp.bootstrap@processGroups'('lo.core#[]', X_2572, X_2573, X_2574, X_2575, XRp, XRp).
'lo.comp.bootstrap@processGroups'('lo.core#,..'(XGp, XL), XRepo, XCat, XOpts, XCP, XRp, XRpx) :- 'lo.comp.bootstrap@cond39'(XL, XRp0, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XRpx, XRp, XX41578, XX41571, XX41570, XX41567, XGp).
'lo.comp.bootstrap@compilePkgs'('lo.core#[]', X_2576, X_2577, X_2578, XRp, XRp).
'lo.comp.bootstrap@compilePkgs'('lo.core#,..'(XPk, XL), XRepo, XCat, XOpt, XRp, XRpx) :- 'lo.comp.errors@errorFree'(XRp),
    'lo.comp.bootstrap@compilePkg'(XPk, XRepo, XRepo0, XCat, XOpt, XRp, XRp0),
    'lo.comp.bootstrap@compilePkgs'(XL, XRepo0, XCat, XOpts, XRp0, XRpx).
'lo.comp.bootstrap@compilePkgs'(X_2579, X_2580, X_2581, X_2582, XRp, XRp) :- 'lo.comp.bootstrap@neg46'(XRp).
'lo.comp.bootstrap@processArgs'('lo.either#either'(XOpts)) :- ocall('repoDir%1'(XXV82),XOpts,XOpts),
    'lo.repo.file@openRepository'(XXV82, XX41637),
    XRepo = XX41637,
    ocall('wDir%1'(XXV83),XOpts,XOpts),
    'lo.comp.catalog@locateCatalog'(XXV83, XX41640),
    XCat = XX41640,
    'lo.comp.bootstrap@cond40'(XX41658, XCar, XXV86, XXV85, XRepo, XCat, XX41645, XRp0, XX41647, XRpx, XXV84, XOpts),
    'lo.comp.errors@fullReport'(XRpx, XX41661),
    'lo@formatSS'(XX41661, XX41662),
    '_logmsg'(XX41662).
'lo.comp.bootstrap@processArgs'('lo.either#alternate'(XM)) :- '_logmsg'(XM).
'lo.comp.bootstrap@main'(XArgs) :- 'lo.comp.args@parseFlags'(XArgs, XX41668),
    'lo.comp.bootstrap@processArgs'(XX41668).
'lo.comp.bootstrap^projectPkgs'('_call%2'(XV4978, XV4979), 'lo.comp.bootstrap^projectPkgs', _) :- 'lo.comp.bootstrap@projectPkgs'(XV4978, XV4979).
'lo.comp.bootstrap@neg45'(XPk, XCP, XP) :- ocall('in%2'(XP, XCP),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.imports@consistentPkg'(XPk, XP),
    !,
    fail.
'lo.comp.bootstrap@neg45'(XPk, XCP, XP).
'lo.comp.bootstrap^importsOk'('_call%2'(XV4980, XV4981), 'lo.comp.bootstrap^importsOk', _) :- 'lo.comp.bootstrap@importsOk'(XV4980, XV4981).
'lo.comp.bootstrap@cond36'(XX41418, XX41417, XemptyArray, XSrc, XX41414, XX41410, XX41408, XX41405, XX41403, XU, XPkg, XRepo, XRepox, XRpx, XTrPrg, XOpts, XPrg, XRp1) :- 'lo.comp.errors@errorFree'(XRp1),
    !,
    'lo.comp.transform@transformProg'(XPrg, XOpts, XTrPrg, XRp1, XRpx),
    ocall('_coerce%2'(XU, XX41403),'lo.coerce$coercion$lo.uri*uri$lo.core*string','lo.coerce$coercion$lo.uri*uri$lo.core*string'),
    'lo.repo.file@addSource'(XRepo, XPkg, XX41403, XX41405),
    ocall('_coerce%2'(XTrPrg, XX41408),'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string','lo.coerce$coercion$lo.comp.term*prProg$lo.core*string'),
    'lo.repo.file@addPrologPackage'(XX41405, XPkg, XX41408, XX41410),
    XRepox = XX41410.
'lo.comp.bootstrap@cond36'(XX41418, XX41417, XemptyArray, XSrc, XX41414, XX41410, XX41408, XX41405, XX41403, XU, XPkg, XRepo, XRepox, XRpx, XTrPrg, XOpts, XPrg, XRp1) :- 'lo.uri@getUriPath'(XU, XX41414),
    'lo.comp.location@collectSrc'(XSrc, 1, XemptyArray, XX41417),
    'lo.comp.errors@populateContext'(XRp1, XX41414, XX41417, XX41418),
    XRpx = XX41418.
'lo.comp.bootstrap@cond37'(XX41426, XX41425, XX41422, XOpts, XTrPrg, XRpx, XRepox, XPkg, XU, XX41403, XX41405, XX41408, XX41410, XX41414, XSrc, XemptyArray, XX41417, XX41418, XX41392, XRp1, XRepo, XV, XTerm, XPrg, XRp0) :- 'lo.comp.errors@errorFree'(XRp0),
    !,
    'lo.comp.typecheck@checkProgram'('lo.repo$repository$lo.repo.file*fileRepo', XTerm, XV, XRepo, XRp0, XRp1, XX41392),
    XPrg = XX41392,
    'lo.comp.bootstrap@cond36'(XX41418, XX41417, XemptyArray, XSrc, XX41414, XX41410, XX41408, XX41405, XX41403, XU, XPkg, XRepo, XRepox, XRpx, XTrPrg, XOpts, XPrg, XRp1).
'lo.comp.bootstrap@cond37'(XX41426, XX41425, XX41422, XOpts, XTrPrg, XRpx, XRepox, XPkg, XU, XX41403, XX41405, XX41408, XX41410, XX41414, XSrc, XemptyArray, XX41417, XX41418, XX41392, XRp1, XRepo, XV, XTerm, XPrg, XRp0) :- 'lo.uri@getUriPath'(XU, XX41422),
    'lo.comp.location@collectSrc'(XSrc, 1, XemptyArray, XX41425),
    'lo.comp.errors@populateContext'(XRp0, XX41422, XX41425, XX41426),
    XRpx = XX41426.
'lo.comp.bootstrap@cond38'(XX41439, XX41431, XP, XPrg, XV, XRepo, XRp1, XX41392, XX41418, XX41417, XemptyArray, XX41414, XX41410, XX41408, XX41405, XX41403, XRepox, XRpx, XTrPrg, XOpts, XX41422, XX41425, XX41426, XX41383, XRp0, XRp, XTerm, XX41377, XX41376, XSrc, X_2561, XU, XPkg, XCat) :- 'lo.comp.catalog@resolveCatalog'(XCat, XPkg, XU, X_2561),
    !,
    'lo.resources@getResource'(XU, XX41376),
    'explode'(XX41376, XX41377),
    XSrc = XX41377,
    'lo.comp.grammar@parseSrc'(XU, XSrc, XRp, XRp0, XX41383),
    XTerm = XX41383,
    'lo.comp.bootstrap@cond37'(XX41426, XX41425, XX41422, XOpts, XTrPrg, XRpx, XRepox, XPkg, XU, XX41403, XX41405, XX41408, XX41410, XX41414, XSrc, XemptyArray, XX41417, XX41418, XX41392, XRp1, XRepo, XV, XTerm, XPrg, XRp0).
'lo.comp.bootstrap@cond38'(XX41439, XX41431, XP, XPrg, XV, XRepo, XRp1, XX41392, XX41418, XX41417, XemptyArray, XX41414, XX41410, XX41408, XX41405, XX41403, XRepox, XRpx, XTrPrg, XOpts, XX41422, XX41425, XX41426, XX41383, XRp0, XRp, XTerm, XX41377, XX41376, XSrc, X_2561, XU, XPkg, XCat) :- ocall('disp%2'('lo.repo#pkg'(XP, XV), XX41431),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot locate "), 'lo.core#,..'(XX41431, 'lo.core#,..'('lo.core#ss'(" in catalog"), 'lo.core#[]')))), XX41439),
    'lo.comp.errors@reportError'(XX41439, 'lo.comp.location#std', XRp, XRpx),
    XRepox = XRepo.
'lo.comp.bootstrap^compilePkg'('_call%7'(XV4982, XV4983, XV4984, XV4985, XV4986, XV4987, XV4988), 'lo.comp.bootstrap^compilePkg', _) :- 'lo.comp.bootstrap@compilePkg'(XV4982, XV4983, XV4984, XV4985, XV4986, XV4987, XV4988).
'lo.comp.bootstrap@one48'(XRpx, XRp, XOpts, XCat, XRepoX, XRepo, XP) :- 'lo.comp.bootstrap@compilePkg'(XP, XRepo, XRepoX, XCat, XOpts, XRp, XRpx),
    !.
'lo.comp.bootstrap^processPkg'('_call%10'(XV4989, XV4990, XV4991, XV4992, XV4993, XV4994, XV4995, XV4996, XV4997, XV4998), 'lo.comp.bootstrap^processPkg', _) :- 'lo.comp.bootstrap@processPkg'(XV4989, XV4990, XV4991, XV4992, XV4993, XV4994, XV4995, XV4996, XV4997, XV4998).
'lo.comp.bootstrap@one49'(XRp0, XRp, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XImps, XP) :- 'lo.comp.bootstrap@processPkg'(XP, XImps, XRepo, XRepo0, XCat, XOpts, XCP, XCP0, XRp, XRp0),
    !.
'lo.comp.bootstrap^processGroup'('_call%9'(XV4999, XV5000, XV5001, XV5002, XV5003, XV5004, XV5005, XV5006, XV5007), 'lo.comp.bootstrap^processGroup', _) :- 'lo.comp.bootstrap@processGroup'(XV4999, XV5000, XV5001, XV5002, XV5003, XV5004, XV5005, XV5006, XV5007).
'lo.comp.bootstrap@cond39'(XL, XRp0, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XRpx, XRp, XX41578, XX41571, XX41570, XX41567, XGp) :- 'lo.list@length'(XGp, XX41567),
    'lo.core@>'('lo.core$comp$lo.core*integer', XX41567, 1),
    !,
    'lo.comp.bootstrap@projectPkgs'(XGp, XX41570),
    ocall('disp%2'(XX41570, XX41571),'lo.core$display$lo.core*list'('lo.core$display$lo.repo*pkg'),'lo.core$display$lo.core*list'('lo.core$display$lo.repo*pkg')),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("circular dependency in packages "), 'lo.core#,..'(XX41571, 'lo.core#[]'))), XX41578),
    'lo.comp.errors@reportError'(XX41578, 'lo.comp.location#std', XRp, XRpx).
'lo.comp.bootstrap@cond39'(XL, XRp0, XCP0, XCP, XOpts, XCat, XRepo0, XRepo, XRpx, XRp, XX41578, XX41571, XX41570, XX41567, XGp) :- 'lo.comp.bootstrap@processGroup'(XGp, XRepo, XRepo0, XCat, XOpts, XCP, XCP0, XRp, XRp0),
    'lo.comp.bootstrap@processGroups'(XL, XRepo0, XCat, XOpts, XCP0, XRp0, XRpx).
'lo.comp.bootstrap^processGroups'('_call%7'(XV5008, XV5009, XV5010, XV5011, XV5012, XV5013, XV5014), 'lo.comp.bootstrap^processGroups', _) :- 'lo.comp.bootstrap@processGroups'(XV5008, XV5009, XV5010, XV5011, XV5012, XV5013, XV5014).
'lo.comp.bootstrap@neg46'(XRp) :- 'lo.comp.errors@errorFree'(XRp),
    !,
    fail.
'lo.comp.bootstrap@neg46'(XRp).
'lo.comp.bootstrap^compilePkgs'('_call%6'(XV5015, XV5016, XV5017, XV5018, XV5019, XV5020), 'lo.comp.bootstrap^compilePkgs', _) :- 'lo.comp.bootstrap@compilePkgs'(XV5015, XV5016, XV5017, XV5018, XV5019, XV5020).
'lo.comp.bootstrap@one50'(XRpx, XX41647, XRp0, XX41645, XCat, XRepo, XXV85, XOpts) :- 'lo.comp.errors@reportBase'(XX41645),
    ocall('pkgs%1'(XXV85),XOpts,XOpts),
    'lo.comp.grapher@makeGraph'(XXV85, XRepo, XCat, XX41645, XRp0, XX41647),
    'lo.comp.bootstrap@processGroups'(XX41647, XRepo, XCat, XOpts, 'lo.core#[]', XRp0, XRpx),
    !.
'lo.comp.bootstrap@cond40'(XX41658, XCar, XXV86, XXV85, XRepo, XCat, XX41645, XRp0, XX41647, XRpx, XXV84, XOpts) :- ocall('dependency%1'(XXV84),XOpts,XOpts),
    'lo.core@true'(XXV84),
    !,
    'lo.comp.bootstrap@one50'(XRpx, XX41647, XRp0, XX41645, XCat, XRepo, XXV85, XOpts).
'lo.comp.bootstrap@cond40'(XX41658, XCar, XXV86, XXV85, XRepo, XCat, XX41645, XRp0, XX41647, XRpx, XXV84, XOpts) :- 'lo.comp.errors@reportBase'(XX41658),
    ocall('pkgs%1'(XXV86),XOpts,XOpts),
    'lo.comp.bootstrap@compilePkgs'(XXV86, XRepo, XCar, XOpts, XX41658, XRpx).
'lo.comp.bootstrap^processArgs'('_call%1'(XV5021), 'lo.comp.bootstrap^processArgs', _) :- 'lo.comp.bootstrap@processArgs'(XV5021).
'lo.comp.bootstrap^main'('_call%1'(XV5022), 'lo.comp.bootstrap^main', _) :- 'lo.comp.bootstrap@main'(XV5022).
