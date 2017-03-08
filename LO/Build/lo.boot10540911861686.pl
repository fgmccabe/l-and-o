'#pkg'("n7o7'()7'n2o2'pkg's'lo.boot's'1.0.0'n6o6'()6'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.options'e'*'n2o2'import'e'private'n2o2'pkg's'lo.either'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo.file'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'s\"I1'__boot'PT0\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.boot@init'():- !.
'lo.boot#bootOptions'('bootOptions%1'('lo.boot@bootOptions'())):- !.
'lo.boot#bootOptions'('repo%1'(XV18812), XLbl3864, XThis3864):- !,
    'lo.boot#bootOptions@repo'(XV18812, XLbl3864, XThis3864).
'lo.boot#bootOptions'('wd%1'(XV18813), XLbl3865, XThis3865):- !,
    'lo.boot#bootOptions@wd'(XV18813, XLbl3865, XThis3865).
'lo.boot#bootOptions@repo'(XR, XLbV1696, XThV1696):- XLbV1696 = 'lo.boot#bootOptions'(XR, XW),
    !.
'lo.boot#bootOptions@wd'(XW, XLbV1696, XThV1696):- XLbV1696 = 'lo.boot#bootOptions'(XR, XW),
    !.
'lo.boot#repoOption'('repoOption%1'('lo.boot@repoOption')):- !.
'lo.boot#repoOption'('shortForm%1'(XV18814), XLbl3866, XThis3866):- !,
    'lo.boot#repoOption@shortForm'(XV18814, XLbl3866, XThis3866).
'lo.boot#repoOption'('alternatives%1'(XV18815), XLbl3867, XThis3867):- !,
    'lo.boot#repoOption@alternatives'(XV18815, XLbl3867, XThis3867).
'lo.boot#repoOption'('usage%1'(XV18816), XLbl3868, XThis3868):- !,
    'lo.boot#repoOption@usage'(XV18816, XLbl3868, XThis3868).
'lo.boot#repoOption'('validator%1'(XV18817), XLbl3869, XThis3869):- !,
    'lo.boot#repoOption@validator'(XV18817, XLbl3869, XThis3869).
'lo.boot#repoOption'('setOption%3'(XV18821, XV18822, XV18823), XLbl3870, XThis3870):- !,
    'lo.boot#repoOption@setOption'(XV18821, XV18822, XV18823, XLbl3870, XThis3870).
'lo.boot#repoOption'('setOption%1'('lo.boot#repoOption^setOption'(XLbl3871, XThis3871)), XLbl3871, XThis3871).
'lo.boot#repoOption@shortForm'("-r", XLbV1697, XThV1697):- !.
'lo.boot#repoOption@alternatives'('lo.core#[]', XLbV1697, XThV1697):- !.
'lo.boot#repoOption@usage'("-r dir -- directory of code repository", XLbV1697, XThV1697):- !.
'lo.boot#repoOption@validator'('lo.core#some'('lo.io^isDir'), XLbV1697, XThV1697):- !.
'lo.boot#repoOption@setOption'(XR, 'lo.boot#bootOptions'(X_5621, XW), 'lo.boot#bootOptions'(XR, XW), XLbV1697, XThV1697):- !.
'lo.boot#repoOption@setOption'(_, _, _):- raise_exception('error'("lo.boot#repoOption@setOption", 39, 5, 49)).
'lo.boot#wdOption'('wdOption%1'('lo.boot@wdOption')):- !.
'lo.boot#wdOption'('shortForm%1'(XV18824), XLbl3872, XThis3872):- !,
    'lo.boot#wdOption@shortForm'(XV18824, XLbl3872, XThis3872).
'lo.boot#wdOption'('alternatives%1'(XV18825), XLbl3873, XThis3873):- !,
    'lo.boot#wdOption@alternatives'(XV18825, XLbl3873, XThis3873).
'lo.boot#wdOption'('usage%1'(XV18826), XLbl3874, XThis3874):- !,
    'lo.boot#wdOption@usage'(XV18826, XLbl3874, XThis3874).
'lo.boot#wdOption'('validator%1'(XV18827), XLbl3875, XThis3875):- !,
    'lo.boot#wdOption@validator'(XV18827, XLbl3875, XThis3875).
'lo.boot#wdOption'('setOption%3'(XV18831, XV18832, XV18833), XLbl3876, XThis3876):- !,
    'lo.boot#wdOption@setOption'(XV18831, XV18832, XV18833, XLbl3876, XThis3876).
'lo.boot#wdOption'('setOption%1'('lo.boot#wdOption^setOption'(XLbl3877, XThis3877)), XLbl3877, XThis3877).
'lo.boot#wdOption@shortForm'("-w", XLbV1698, XThV1698):- !.
'lo.boot#wdOption@alternatives'('lo.core#[]', XLbV1698, XThV1698):- !.
'lo.boot#wdOption@usage'("-w dir -- override working directory", XLbV1698, XThV1698):- !.
'lo.boot#wdOption@validator'('lo.core#some'('lo.io^isDir'), XLbV1698, XThV1698):- !.
'lo.boot#wdOption@setOption'(XW, 'lo.boot#bootOptions'(XR, X_5622), 'lo.boot#bootOptions'(XR, XW), XLbV1698, XThV1698):- !.
'lo.boot#wdOption@setOption'(_, _, _):- raise_exception('error'("lo.boot#wdOption@setOption", 48, 5, 49)).
'lo.boot@pkgUp'(XP, "*", 'lo.repo#pkg'(XP, 'lo.repo#defltVersion')):- !.
'lo.boot@pkgUp'(XP, XV, 'lo.repo#pkg'(XP, 'lo.repo#vers'(XV))):- !.
'lo.boot@pkgUp'(_, _, _):- raise_exception('error'("lo.boot@pkgUp", 78, 3, 35)).
'lo.boot@importPkg'(Xrepository81, XP, X_5623, XLd, 'lo.core#[]'):- ocall('in%2'(XP, XLd),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.boot@importPkg'(Xrepository81, XP, XR, XLd, XXe1957):- ocall('loadFromRepo%4'(XR, XP, "code", XCode),Xrepository81,Xrepository81),
    '_install_pkg'(XCode, XXc834),
    XImps = XXc834,
    ocall('//%1'(XXV1983),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%3'(XImps, 'lo.boot@fun88', XXe1957),XXV1983,XXV1983).
'lo.boot@initialize'('lo.repo#pkg'(XP, X_5624)):- ocall('+%1'(XXV1984),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('_call%3'(XP, "@init", XXe1958),XXV1984,XXV1984),
    XPred = XXe1958,
    'lo.boot@cond95'(XXd9113, XXd9112, XXd9111, XXd9110, XXe1959, XXV1985, XP, XXd9109, XPred).
'lo.boot@importPkgs'(Xrepository82, 'lo.core#[]', XLd, XLd, X_5627).
'lo.boot@importPkgs'(Xrepository82, 'lo.core#,..'(XP, XL), XLd, XLdx, XR):- 'lo.boot@importPkg'(Xrepository82, XP, XR, XLd, XImps),
    'lo.boot@importPkgs'(Xrepository82, XImps, 'lo.core#,..'(XP, XLd), XLdx, XR),
    'lo.boot@initialize'(XP).
'lo.boot@invokeMain'(XTop, XArgs):- ocall('+%1'(XXV1986),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('_call%3'(XTop, "@_main", XXe1960),XXV1986,XXV1986),
    XPred = XXe1960,
    'lo.boot@cond96'(XXd9119, XXd9118, XXd9117, XXd9116, XXe1962, XXV1988, XTop, XXd9115, XXe1961, XXV1987, XArgs, XPred).
'lo.boot@handleCmdLineOpts'('lo.either#either'('()2'(XOpts, 'lo.core#,..'(XTop, XArgs)))):- 'lo.io@cwd'(XXd9120),
    'lo.uri@parseUri'(XXd9120, XXd9121),
    ocall('repo%1'(XXV1989),XOpts,XOpts),
    'lo.uri@parseUri'(XXV1989, XXd9122),
    'lo.uri@resolveUri'(XXd9121, XXd9122, XXd9123),
    XRD = XXd9123,
    ocall('disp%1'(XXV1990),'lo.core$display$lo.uri*uri','lo.core$display$lo.uri*uri'),
    ocall('_call%2'(XRD, XXe1963),XXV1990,XXV1990),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("repo dir: "), 'lo.core#,..'(XXe1963, 'lo.core#[]'))), XXd9128),
    '_logmsg'(XXd9128),
    'lo.repo.file@openRepository'(XRD, XXd9129),
    XRepo = '()2'(XXd9129, 'lo.repo#coreRepo'),
    'lo.repo@parsePkgName'(XTop, XXd9131),
    'lo.boot@importPkgs'('lo.repo$repository$()2'('lo.repo$repository$lo.repo*coreRepo', 'lo.repo$repository$lo.repo.file*fileRepo'), 'lo.core#,..'(XXd9131, 'lo.core#[]'), 'lo.core#[]', X_5636, XRepo),
    'lo.boot@invokeMain'(XTop, XArgs).
'lo.boot@__boot'():- 'lo.io@cwd'(XXd9133),
    XWD = XXd9133,
    '_command_line'(XXc835),
    'lo.options@processOptions'(XXc835, 'lo.core#,..'('lo.boot#repoOption', 'lo.core#,..'('lo.boot#wdOption', 'lo.core#[]')), 'lo.boot#bootOptions'(XWD, XWD), XXd9137),
    'lo.boot@handleCmdLineOpts'(XXd9137).
'lo.boot@delayHandler'('lo.core#[]').
'lo.boot@delayHandler'('lo.core#,..'(XH, XL)):- 'lo.boot@one67'(XH),
    'lo.boot@delayHandler'(XL).
'lo.boot#repoOption^setOption'('_call%3'(XV18818, XV18819, XV18820), 'lo.boot#repoOption^setOption'(XLbV1697, XThV1697), _):- 'lo.boot#repoOption@setOption'(XV18818, XV18819, XV18820, XLbV1697, XThV1697).
'lo.boot#wdOption^setOption'('_call%3'(XV18828, XV18829, XV18830), 'lo.boot#wdOption^setOption'(XLbV1698, XThV1698), _):- 'lo.boot#wdOption@setOption'(XV18828, XV18829, XV18830, XLbV1698, XThV1698).
'lo.boot^pkgUp'('_call%3'(XV18834, XV18835, XV18836), 'lo.boot^pkgUp', _):- 'lo.boot@pkgUp'(XV18834, XV18835, XV18836).
'lo.boot@fun88'('_call%2'('()2'(XPk, XV), XXd9108), 'lo.boot@fun88', _):- !,
    'lo.boot@pkgUp'(XPk, XV, XXd9108).
'lo.boot@fun88'(_, _, _):- raise_exception('error'("lo.boot@fun88", 73, 27, 21)).
'lo.boot^importPkg'('_call%5'(XV18837, XV18838, XV18839, XV18840, XV18841), 'lo.boot^importPkg', _):- 'lo.boot@importPkg'(XV18837, XV18838, XV18839, XV18840, XV18841).
'lo.boot@cond95'(XXd9113, XXd9112, XXd9111, XXd9110, XXe1959, XXV1985, XP, XXd9109, XPred):- '_defined'(XPred, 0),
    !,
    '_call'(XPred, 0, 'lo.core#[]').
'lo.boot@cond95'(XXd9113, XXd9112, XXd9111, XXd9110, XXe1959, XXV1985, XP, XXd9109, XPred):- ocall('disp%1'(XXV1985),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XP, XXe1959),XXV1985,XXV1985),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("No init for "), 'lo.core#,..'(XXe1959, 'lo.core#[]'))), XXd9113),
    'lo.io@logMsg'(XXd9113).
'lo.boot^initialize'('_call%1'(XV18842), 'lo.boot^initialize', _):- 'lo.boot@initialize'(XV18842).
'lo.boot^importPkgs'('_call%5'(XV18843, XV18844, XV18845, XV18846, XV18847), 'lo.boot^importPkgs', _):- 'lo.boot@importPkgs'(XV18843, XV18844, XV18845, XV18846, XV18847).
'lo.boot@cond96'(XXd9119, XXd9118, XXd9117, XXd9116, XXe1962, XXV1988, XTop, XXd9115, XXe1961, XXV1987, XArgs, XPred):- ocall('size%1'(XXV1987),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe1961),XXV1987,XXV1987),
    '_defined'(XPred, XXe1961),
    !,
    'lo.io@logMsg'("Starting ..."),
    '_call'(XPred, 1, XArgs).
'lo.boot@cond96'(XXd9119, XXd9118, XXd9117, XXd9116, XXe1962, XXV1988, XTop, XXd9115, XXe1961, XXV1987, XArgs, XPred):- ocall('disp%1'(XXV1988),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XTop, XXe1962),XXV1988,XXV1988),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("No main program: "), 'lo.core#,..'(XXe1962, 'lo.core#[]'))), XXd9119),
    'lo.io@logMsg'(XXd9119).
'lo.boot^invokeMain'('_call%2'(XV18848, XV18849), 'lo.boot^invokeMain', _):- 'lo.boot@invokeMain'(XV18848, XV18849).
'lo.boot^handleCmdLineOpts'('_call%1'(XV18850), 'lo.boot^handleCmdLineOpts', _):- 'lo.boot@handleCmdLineOpts'(XV18850).
'lo.boot^__boot'('_call%0'(), 'lo.boot^__boot', _):- 'lo.boot@__boot'().
'lo.boot@one67'(XH):- ocall('_call%0'(),XH,XH),
    !.
'lo.boot^delayHandler'('_call%1'(XV18851), 'lo.boot^delayHandler', _):- 'lo.boot@delayHandler'(XV18851).
