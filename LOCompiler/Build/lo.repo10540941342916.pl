'#pkg'("n7o7'()7'n2o2'pkg's'lo.repo's'1.0.0'n2o2'()2'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.json'e'*'s\"I5'pkg'CT2St'lo.repo*version't'lo.repo*pkg''defltVersion't'lo.repo*version''vers'CT1St'lo.repo*version''coreRepo't'lo.repo*coreRepo''parsePkgName'FT1St'lo.repo*pkg'\"s\"I3'version'Yt'lo.repo*version'I0'pkg'Yt'lo.repo*pkg'I0'coreRepo'Yt'lo.repo*coreRepo'I0\"n4o4'()4's'pkg's'defltVersion's'vers's'coreRepo'n1o1'()1'n4o4'()4's'repository's'lo.repo$repository's\":k'r'c'lo.repo$repository'T1k'r'T0\"s\":k'r'I2'packagePresent'PT3k'r't'lo.repo*pkg'S'loadFromRepo'PT4k'r't'lo.repo*pkg'SS\"n6o6'()6'n2o2'()2's'lo.repo$repository$()2's\":k'a':k'b'||c'lo.repo$repository'T1T2k'a'k'b'T0c'lo.repo$repository'T1k'a'T0c'lo.repo$repository'T1k'b'T0\"n2o2'()2's'lo.repo$repository$lo.repo*coreRepo's\"c'lo.repo$repository'T1t'lo.repo*coreRepo'T0\"n2o2'()2's'lo.core$equality$lo.repo*pkg's\"c'lo.core$equality'T1t'lo.repo*pkg'T0\"n2o2'()2's'lo.core$display$lo.repo*pkg's\"c'lo.core$display'T1t'lo.repo*pkg'T0\"n2o2'()2's'lo.core$display$lo.repo*version's\"c'lo.core$display'T1t'lo.repo*version'T0\"n2o2'()2's'lo.coerce$coercion$lo.repo*pkg$lo.core*string's\"c'lo.coerce$coercion'T2t'lo.repo*pkg'ST0\"").
'lo.repo@init'():- !.
'lo.repo#pkg'('pkg%1'('lo.repo@pkg'())):- !.
'lo.repo#defltVersion'('defltVersion%1'('lo.repo@defltVersion')):- !.
'lo.repo#vers'('vers%1'('lo.repo@vers'())):- !.
'lo.repo@comboLoad'(Xrepository55, Xrepository56, '()2'(XA, XB), XP, XK, XTxt):- 'lo.repo@cond379'(Xrepository56, XB, XTxt, Xrepository55, XK, XP, XA).
'lo.repo@comboPkgPrsnt'(Xrepository57, Xrepository58, '()2'(XA, X_31078), XP, XK):- ocall('packagePresent%3'(XA, XP, XK),Xrepository57,Xrepository57).
'lo.repo@comboPkgPrsnt'(Xrepository57, Xrepository58, '()2'(X_31079, XB), XP, XK):- ocall('packagePresent%3'(XB, XP, XK),Xrepository58,Xrepository58).
'lo.repo$repository$()2'('lo.repo$repository$()2%1'('lo.repo$repository$()2')):- !.
'lo.repo$repository$()2'('packagePresent%3'(XV28835, XV28836, XV28837), XLbl2071, XThis2071):- !,
    'lo.repo$repository$()2@packagePresent'(XV28835, XV28836, XV28837, XLbl2071, XThis2071).
'lo.repo$repository$()2'('packagePresent%1'('lo.repo$repository$()2^packagePresent'(XLbl2072, XThis2072)), XLbl2072, XThis2072).
'lo.repo$repository$()2'('loadFromRepo%4'(XV28844, XV28845, XV28846, XV28847), XLbl2073, XThis2073):- !,
    'lo.repo$repository$()2@loadFromRepo'(XV28844, XV28845, XV28846, XV28847, XLbl2073, XThis2073).
'lo.repo$repository$()2'('loadFromRepo%1'('lo.repo$repository$()2^loadFromRepo'(XLbl2074, XThis2074)), XLbl2074, XThis2074).
'lo.repo$repository$()2@packagePresent'(XC, XP, XK, XLbV2243, XThV2243):- XLbV2243 = 'lo.repo$repository$()2'(Xrepository59, Xrepository60),
    'lo.repo@comboPkgPrsnt'(Xrepository60, Xrepository59, XC, XP, XK).
'lo.repo$repository$()2@loadFromRepo'(XC, XP, XK, XTxt, XLbV2243, XThV2243):- XLbV2243 = 'lo.repo$repository$()2'(Xrepository59, Xrepository60),
    'lo.repo@comboLoad'(Xrepository60, Xrepository59, XC, XP, XK, XTxt).
'lo.repo#coreRepo'('coreRepo%1'('lo.repo@coreRepo')):- !.
'lo.repo$repository$lo.repo*coreRepo'('lo.repo$repository$lo.repo*coreRepo%1'('lo.repo$repository$lo.repo*coreRepo')):- !.
'lo.repo$repository$lo.repo*coreRepo'('packagePresent%3'(XV28853, XV28854, XV28855), XLbl2075, XThis2075):- !,
    'lo.repo$repository$lo.repo*coreRepo@packagePresent'(XV28853, XV28854, XV28855, XLbl2075, XThis2075).
'lo.repo$repository$lo.repo*coreRepo'('packagePresent%1'('lo.repo$repository$lo.repo*coreRepo^packagePresent'(XLbl2076, XThis2076)), XLbl2076, XThis2076).
'lo.repo$repository$lo.repo*coreRepo'('loadFromRepo%4'(XV28862, XV28863, XV28864, XV28865), XLbl2077, XThis2077):- !,
    'lo.repo$repository$lo.repo*coreRepo@loadFromRepo'(XV28862, XV28863, XV28864, XV28865, XLbl2077, XThis2077).
'lo.repo$repository$lo.repo*coreRepo'('loadFromRepo%1'('lo.repo$repository$lo.repo*coreRepo^loadFromRepo'(XLbl2078, XThis2078)), XLbl2078, XThis2078).
'lo.repo$repository$lo.repo*coreRepo@packagePresent'(X_31080, 'lo.repo#pkg'(XP, 'lo.repo#defltVersion'), XK, XLbV2245, XThV2245):- '_pkg_is_present'(XP, "*", XK, X_31081).
'lo.repo$repository$lo.repo*coreRepo@packagePresent'(X_31082, 'lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), XK, XLbV2245, XThV2245):- '_pkg_is_present'(XP, XV, XK, X_31083).
'lo.repo$repository$lo.repo*coreRepo@loadFromRepo'(X_31084, 'lo.repo#pkg'(XP, 'lo.repo#defltVersion'), XK, XXa93, XLbV2245, XThV2245):- '_pkg_is_present'(XP, "*", XK, XFn),
    '_get_file'(XFn, XXa93).
'lo.repo$repository$lo.repo*coreRepo@loadFromRepo'(X_31085, 'lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), XK, XXa94, XLbV2245, XThV2245):- '_pkg_is_present'(XP, XV, XK, XFn),
    '_get_file'(XFn, XXa94).
'lo.repo@packageHash'('lo.repo#pkg'(XP, 'lo.repo#defltVersion'), XXe4438):- !,
    ocall('hash%1'(XXV4780),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('_call%2'(XP, XXe4438),XXV4780,XXV4780).
'lo.repo@packageHash'('lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), XXe4442):- !,
    ocall('hash%1'(XXV4781),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('*%1'(XXV4782),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('hash%1'(XXV4783),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('+%1'(XXV4784),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XP, XXe4439),XXV4781,XXV4781),
    ocall('_call%3'(XXe4439, 47, XXe4440),XXV4782,XXV4782),
    ocall('_call%2'(XV, XXe4441),XXV4783,XXV4783),
    ocall('_call%3'(XXe4440, XXe4441, XXe4442),XXV4784,XXV4784).
'lo.repo@packageHash'(_, _):- raise_exception('error'("lo.repo@packageHash", 73, 3, 43)).
'lo.repo@samePackage'('lo.repo#pkg'(XP1, XV1), 'lo.repo#pkg'(XP2, XV2)):- XP1 = XP2,
    XV1 = XV2.
'lo.core$equality$lo.repo*pkg'('lo.core$equality$lo.repo*pkg%1'('lo.core$equality$lo.repo*pkg')):- !.
'lo.core$equality$lo.repo*pkg'('==%2'(XV28874, XV28875), XLbl2079, XThis2079):- !,
    'lo.core$equality$lo.repo*pkg@=='(XV28874, XV28875, XLbl2079, XThis2079).
'lo.core$equality$lo.repo*pkg'('==%1'('lo.core$equality$lo.repo*pkg^=='(XLbl2080, XThis2080)), XLbl2080, XThis2080).
'lo.core$equality$lo.repo*pkg'('hash%2'(XV28878, XV28879), XLbl2081, XThis2081):- !,
    'lo.core$equality$lo.repo*pkg@hash'(XV28878, XV28879, XLbl2081, XThis2081).
'lo.core$equality$lo.repo*pkg'('hash%1'('lo.core$equality$lo.repo*pkg^hash'(XLbl2082, XThis2082)), XLbl2082, XThis2082).
'lo.core$equality$lo.repo*pkg@=='(XP1, XP2, XLbV2246, XThV2246):- 'lo.repo@samePackage'(XP1, XP2).
'lo.core$equality$lo.repo*pkg@hash'(XP, XXd35960, XLbV2246, XThV2246):- !,
    'lo.repo@packageHash'(XP, XXd35960).
'lo.core$equality$lo.repo*pkg@hash'(_, _):- raise_exception('error'("lo.core$equality$lo.repo*pkg@hash", 51, 5, 25)).
'lo.core$display$lo.repo*pkg'('lo.core$display$lo.repo*pkg%1'('lo.core$display$lo.repo*pkg')):- !.
'lo.core$display$lo.repo*pkg'('disp%2'(XV28882, XV28883), XLbl2083, XThis2083):- !,
    'lo.core$display$lo.repo*pkg@disp'(XV28882, XV28883, XLbl2083, XThis2083).
'lo.core$display$lo.repo*pkg'('disp%1'('lo.core$display$lo.repo*pkg^disp'(XLbl2084, XThis2084)), XLbl2084, XThis2084).
'lo.core$display$lo.repo*pkg@disp'('lo.repo#pkg'(XP, 'lo.repo#defltVersion'), 'lo.core#ss'(XP), XLbV2247, XThV2247):- !.
'lo.core$display$lo.repo*pkg@disp'('lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XP), 'lo.core#,..'('lo.core#ss'("#"), 'lo.core#,..'('lo.core#ss'(XV), 'lo.core#[]')))), XLbV2247, XThV2247):- !.
'lo.core$display$lo.repo*pkg@disp'(_, _):- raise_exception('error'("lo.core$display$lo.repo*pkg@disp", 55, 5, 34)).
'lo.core$display$lo.repo*version'('lo.core$display$lo.repo*version%1'('lo.core$display$lo.repo*version')):- !.
'lo.core$display$lo.repo*version'('disp%2'(XV28886, XV28887), XLbl2085, XThis2085):- !,
    'lo.core$display$lo.repo*version@disp'(XV28886, XV28887, XLbl2085, XThis2085).
'lo.core$display$lo.repo*version'('disp%1'('lo.core$display$lo.repo*version^disp'(XLbl2086, XThis2086)), XLbl2086, XThis2086).
'lo.core$display$lo.repo*version@disp'('lo.repo#defltVersion', 'lo.core#ss'("*"), XLbV2248, XThV2248):- !.
'lo.core$display$lo.repo*version@disp'('lo.repo#vers'(XV), 'lo.core#ss'(XV), XLbV2248, XThV2248):- !.
'lo.core$display$lo.repo*version@disp'(_, _):- raise_exception('error'("lo.core$display$lo.repo*version@disp", 60, 5, 29)).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string'('lo.coerce$coercion$lo.repo*pkg$lo.core*string%1'('lo.coerce$coercion$lo.repo*pkg$lo.core*string')):- !.
'lo.coerce$coercion$lo.repo*pkg$lo.core*string'('_coerce%2'(XV28890, XV28891), XLbl2087, XThis2087):- !,
    'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'(XV28890, XV28891, XLbl2087, XThis2087).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string'('_coerce%1'('lo.coerce$coercion$lo.repo*pkg$lo.core*string^_coerce'(XLbl2088, XThis2088)), XLbl2088, XThis2088).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'('lo.repo#pkg'(XP, 'lo.repo#defltVersion'), XP, XLbV2249, XThV2249):- !.
'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'('lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), XXe4444, XLbV2249, XThV2249):- !,
    ocall('+%1'(XXV4785),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%1'(XXV4786),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('_call%3'(XP, "#", XXe4443),XXV4785,XXV4785),
    ocall('_call%3'(XXe4443, XV, XXe4444),XXV4786,XXV4786).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce", 65, 5, 33)).
'lo.repo@parsePkgName'(XP, 'lo.repo#pkg'(XNm, 'lo.repo#vers'(XV))):- '_str_find'(XP, "#", 0, XPos),
    '_str_split'(XP, XPos, XNm, X_31089),
    ocall('+%1'(XXV4787),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XPos, 1, XXe4445),XXV4787,XXV4787),
    '_str_split'(XP, XXe4445, X_31090, XV),
    !.
'lo.repo@parsePkgName'(XP, 'lo.repo#pkg'(XP, 'lo.repo#defltVersion')):- !.
'lo.repo@parsePkgName'(_, _):- raise_exception('error'("lo.repo@parsePkgName", 77, 3, 121)).
'lo.repo@cond379'(Xrepository56, XB, XTxt, Xrepository55, XK, XP, XA):- ocall('packagePresent%3'(XA, XP, XK),Xrepository55,Xrepository55),
    !,
    ocall('loadFromRepo%4'(XA, XP, XK, XTxt),Xrepository55,Xrepository55).
'lo.repo@cond379'(Xrepository56, XB, XTxt, Xrepository55, XK, XP, XA):- ocall('loadFromRepo%4'(XB, XP, XK, XTxt),Xrepository56,Xrepository56).
'lo.repo^comboLoad'('_call%6'(XV28819, XV28820, XV28821, XV28822, XV28823, XV28824), 'lo.repo^comboLoad', _):- 'lo.repo@comboLoad'(XV28819, XV28820, XV28821, XV28822, XV28823, XV28824).
'lo.repo^comboPkgPrsnt'('_call%5'(XV28825, XV28826, XV28827, XV28828, XV28829), 'lo.repo^comboPkgPrsnt', _):- 'lo.repo@comboPkgPrsnt'(XV28825, XV28826, XV28827, XV28828, XV28829).
'lo.repo$repository$()2^packagePresent'('_call%5'(XV28830, XV28831, XV28832, XV28833, XV28834), 'lo.repo$repository$()2^packagePresent'(XLbV2243, XThV2243), _):- 'lo.repo$repository$()2@packagePresent'(XV28830, XV28831, XV28832, XV28833, XV28834, XLbV2243, XThV2243).
'lo.repo$repository$()2^loadFromRepo'('_call%6'(XV28838, XV28839, XV28840, XV28841, XV28842, XV28843), 'lo.repo$repository$()2^loadFromRepo'(XLbV2243, XThV2243), _):- 'lo.repo$repository$()2@loadFromRepo'(XV28838, XV28839, XV28840, XV28841, XV28842, XV28843, XLbV2243, XThV2243).
'lo.repo$repository$lo.repo*coreRepo^packagePresent'('_call%5'(XV28848, XV28849, XV28850, XV28851, XV28852), 'lo.repo$repository$lo.repo*coreRepo^packagePresent'(XLbV2245, XThV2245), _):- 'lo.repo$repository$lo.repo*coreRepo@packagePresent'(XV28848, XV28849, XV28850, XV28851, XV28852, XLbV2245, XThV2245).
'lo.repo$repository$lo.repo*coreRepo^loadFromRepo'('_call%6'(XV28856, XV28857, XV28858, XV28859, XV28860, XV28861), 'lo.repo$repository$lo.repo*coreRepo^loadFromRepo'(XLbV2245, XThV2245), _):- 'lo.repo$repository$lo.repo*coreRepo@loadFromRepo'(XV28856, XV28857, XV28858, XV28859, XV28860, XV28861, XLbV2245, XThV2245).
'lo.repo^packageHash'('_call%2'(XV28866, XV28867), 'lo.repo^packageHash', _):- 'lo.repo@packageHash'(XV28866, XV28867).
'lo.repo^samePackage'('_call%2'(XV28868, XV28869), 'lo.repo^samePackage', _):- 'lo.repo@samePackage'(XV28868, XV28869).
'lo.core$equality$lo.repo*pkg^=='('_call%4'(XV28870, XV28871, XV28872, XV28873), 'lo.core$equality$lo.repo*pkg^=='(XLbV2246, XThV2246), _):- 'lo.core$equality$lo.repo*pkg@=='(XV28870, XV28871, XV28872, XV28873, XLbV2246, XThV2246).
'lo.core$equality$lo.repo*pkg^hash'('_call%2'(XV28876, XV28877), 'lo.core$equality$lo.repo*pkg^hash'(XLbV2246, XThV2246), _):- 'lo.core$equality$lo.repo*pkg@hash'(XV28876, XV28877, XLbV2246, XThV2246).
'lo.core$display$lo.repo*pkg^disp'('_call%2'(XV28880, XV28881), 'lo.core$display$lo.repo*pkg^disp'(XLbV2247, XThV2247), _):- 'lo.core$display$lo.repo*pkg@disp'(XV28880, XV28881, XLbV2247, XThV2247).
'lo.core$display$lo.repo*version^disp'('_call%2'(XV28884, XV28885), 'lo.core$display$lo.repo*version^disp'(XLbV2248, XThV2248), _):- 'lo.core$display$lo.repo*version@disp'(XV28884, XV28885, XLbV2248, XThV2248).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string^_coerce'('_call%2'(XV28888, XV28889), 'lo.coerce$coercion$lo.repo*pkg$lo.core*string^_coerce'(XLbV2249, XThV2249), _):- 'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'(XV28888, XV28889, XLbV2249, XThV2249).
'lo.repo^parsePkgName'('_call%2'(XV28892, XV28893), 'lo.repo^parsePkgName', _):- 'lo.repo@parsePkgName'(XV28892, XV28893).
