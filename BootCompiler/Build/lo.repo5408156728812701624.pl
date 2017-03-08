'#pkg'("n7o7'()7'n2o2'pkg's'lo.repo'e'*'n11o11'()11'n2o2'import'e'private'n2o2'pkg's'lo.json'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I5'pkg'CT2St'lo.repo*version't'lo.repo*pkg''defltVersion't'lo.repo*version''vers'CT1St'lo.repo*version''coreRepo't'lo.repo*coreRepo''parsePkgName'FT1St'lo.repo*pkg'\"s\"I3'pkg'Yt'lo.repo*pkg'I0'version'Yt'lo.repo*version'I0'coreRepo'Yt'lo.repo*coreRepo'I0\"n2o2'()2's'defltVersion's'coreRepo'n1o1'()1'n4o4'()4's'repository's'lo.repo$repository's\":k'r'c'lo.repo$repository'T1k'r'T0\"s\":k'r'I2'loadFromRepo'PT4k'r't'lo.repo*pkg'SS'packagePresent'PT3k'r't'lo.repo*pkg'S\"n6o6'()6'n2o2'()2's'lo.repo$repository$()2's\":k'a':k'b'||c'lo.repo$repository'T1T2k'a'k'b'T0c'lo.repo$repository'T1k'b'T0c'lo.repo$repository'T1k'a'T0\"n2o2'()2's'lo.repo$repository$lo.repo*coreRepo's\"c'lo.repo$repository'T1t'lo.repo*coreRepo'T0\"n2o2'()2's'lo.core$equality$lo.repo*pkg's\"c'lo.core$equality'T1t'lo.repo*pkg'T0\"n2o2'()2's'lo.core$display$lo.repo*pkg's\"c'lo.core$display'T1t'lo.repo*pkg'T0\"n2o2'()2's'lo.core$display$lo.repo*version's\"c'lo.core$display'T1t'lo.repo*version'T0\"n2o2'()2's'lo.coerce$coercion$lo.repo*pkg$lo.core*string's\"c'lo.coerce$coercion'T2t'lo.repo*pkg'ST0\"").
'lo.repo@init'() :- !.
'lo.repo#pkg'('pkg%1'('lo.repo@pkg'())) :- !.
'lo.repo#vers'('vers%1'('lo.repo@vers'())) :- !.
'lo.repo@comboPkgPrsnt'(Xlo_repo_repository_a1, Xlo_repo_repository_b1, (XA, X_173), XP, XK) :- ocall('packagePresent%3'(XA, XP, XK),Xlo_repo_repository_a1,Xlo_repo_repository_a1).
'lo.repo@comboPkgPrsnt'(Xlo_repo_repository_a1, Xlo_repo_repository_b1, (X_174, XB), XP, XK) :- ocall('packagePresent%3'(XB, XP, XK),Xlo_repo_repository_b1,Xlo_repo_repository_b1).
'lo.repo@comboLoad'(Xlo_repo_repository_a2, Xlo_repo_repository_b2, (XA, XB), XP, XK, XTxt) :- 'lo.repo@cond4'(Xlo_repo_repository_b2, XB, XTxt, Xlo_repo_repository_a2, XK, XP, XA).
'lo.repo$repository$()2'('lo.repo$repository$()2%1'('lo.repo$repository$()2')) :- !.
'lo.repo$repository$()2'('packagePresent%3'(XV721, XV722, XV723), XLbl136, XThis136) :- !,
    'lo.repo$repository$()2@packagePresent'(XV721, XV722, XV723, XLbl136, XThis136).
'lo.repo$repository$()2'('packagePresent%1'('lo.repo$repository$()2^packagePresent'(XLbl137, XThis137)), XLbl137, XThis137).
'lo.repo$repository$()2'('loadFromRepo%4'(XV731, XV732, XV733, XV734), XLbl138, XThis138) :- !,
    'lo.repo$repository$()2@loadFromRepo'(XV731, XV732, XV733, XV734, XLbl138, XThis138).
'lo.repo$repository$()2'('loadFromRepo%1'('lo.repo$repository$()2^loadFromRepo'(XLbl139, XThis139)), XLbl139, XThis139).
'lo.repo$repository$()2@packagePresent'(XC, XP, XK, XLbV94, XThV94) :- XLbV94 = 'lo.repo$repository$()2'(Xlo_repo_repository_a3, Xlo_repo_repository_b3),
    'lo.repo@comboPkgPrsnt'(Xlo_repo_repository_a3, Xlo_repo_repository_b3, XC, XP, XK).
'lo.repo$repository$()2@loadFromRepo'(XC, XP, XK, XTxt, XLbV94, XThV94) :- XLbV94 = 'lo.repo$repository$()2'(Xlo_repo_repository_a3, Xlo_repo_repository_b3),
    'lo.repo@comboLoad'(Xlo_repo_repository_a3, Xlo_repo_repository_b3, XC, XP, XK, XTxt).
'lo.repo$repository$lo.repo*coreRepo'('lo.repo$repository$lo.repo*coreRepo%1'('lo.repo$repository$lo.repo*coreRepo')) :- !.
'lo.repo$repository$lo.repo*coreRepo'('packagePresent%3'(XV742, XV743, XV744), XLbl140, XThis140) :- !,
    'lo.repo$repository$lo.repo*coreRepo@packagePresent'(XV742, XV743, XV744, XLbl140, XThis140).
'lo.repo$repository$lo.repo*coreRepo'('packagePresent%1'('lo.repo$repository$lo.repo*coreRepo^packagePresent'(XLbl141, XThis141)), XLbl141, XThis141).
'lo.repo$repository$lo.repo*coreRepo'('loadFromRepo%4'(XV752, XV753, XV754, XV755), XLbl142, XThis142) :- !,
    'lo.repo$repository$lo.repo*coreRepo@loadFromRepo'(XV752, XV753, XV754, XV755, XLbl142, XThis142).
'lo.repo$repository$lo.repo*coreRepo'('loadFromRepo%1'('lo.repo$repository$lo.repo*coreRepo^loadFromRepo'(XLbl143, XThis143)), XLbl143, XThis143).
'lo.repo$repository$lo.repo*coreRepo@packagePresent'(X_175, 'lo.repo#pkg'(XP, 'lo.repo#defltVersion'), XK, XLbV96, XThV96) :- '_pkg_is_present'(XP, "*", XK, X_176).
'lo.repo$repository$lo.repo*coreRepo@packagePresent'(X_177, 'lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), XK, XLbV96, XThV96) :- '_pkg_is_present'(XP, XV, XK, X_178).
'lo.repo$repository$lo.repo*coreRepo@loadFromRepo'(X_179, 'lo.repo#pkg'(XP, 'lo.repo#defltVersion'), XK, XX3233, XLbV96, XThV96) :- '_pkg_is_present'(XP, "*", XK, XFn),
    '_get_file'(XFn, XX3233).
'lo.repo$repository$lo.repo*coreRepo@loadFromRepo'(X_180, 'lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), XK, XX3244, XLbV96, XThV96) :- '_pkg_is_present'(XP, XV, XK, XFn),
    '_get_file'(XFn, XX3244).
'lo.repo@samePackage'('lo.repo#pkg'(XP1, XV1), 'lo.repo#pkg'(XP2, XV2)) :- XP1 = XP2,
    XV1 = XV2.
'lo.repo@packageHash'('lo.repo#pkg'(XP, 'lo.repo#defltVersion'), XX3263) :- !,
    ocall('hash%2'(XP, XX3263),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.repo@packageHash'('lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), XX3277) :- !,
    ocall('hash%2'(XP, XX3270),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('*%3'(XX3270, 47, XX3272),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('hash%2'(XV, XX3275),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string'),
    ocall('+%3'(XX3272, XX3275, XX3277),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.repo@packageHash'(_, _) :- raise_exception('error'("packageHash", 73, 3, 43)).
'lo.core$equality$lo.repo*pkg'('lo.core$equality$lo.repo*pkg%1'('lo.core$equality$lo.repo*pkg')) :- !.
'lo.core$equality$lo.repo*pkg'('==%2'(XV766, XV767), XLbl144, XThis144) :- !,
    'lo.core$equality$lo.repo*pkg@=='(XV766, XV767, XLbl144, XThis144).
'lo.core$equality$lo.repo*pkg'('==%1'('lo.core$equality$lo.repo*pkg^=='(XLbl145, XThis145)), XLbl145, XThis145).
'lo.core$equality$lo.repo*pkg'('hash%2'(XV772, XV773), XLbl146, XThis146) :- !,
    'lo.core$equality$lo.repo*pkg@hash'(XV772, XV773, XLbl146, XThis146).
'lo.core$equality$lo.repo*pkg'('hash%1'('lo.core$equality$lo.repo*pkg^hash'(XLbl147, XThis147)), XLbl147, XThis147).
'lo.core$equality$lo.repo*pkg@=='(XP1, XP2, XLbV97, XThV97) :- 'lo.repo@samePackage'(XP1, XP2).
'lo.core$equality$lo.repo*pkg@hash'(XP, XX3285, XLbV97, XThV97) :- !,
    'lo.repo@packageHash'(XP, XX3285).
'lo.core$equality$lo.repo*pkg@hash'(_, _, _, _) :- raise_exception('error'("hash", 51, 5, 25)).
'lo.core$display$lo.repo*pkg'('lo.core$display$lo.repo*pkg%1'('lo.core$display$lo.repo*pkg')) :- !.
'lo.core$display$lo.repo*pkg'('disp%2'(XV778, XV779), XLbl148, XThis148) :- !,
    'lo.core$display$lo.repo*pkg@disp'(XV778, XV779, XLbl148, XThis148).
'lo.core$display$lo.repo*pkg'('disp%1'('lo.core$display$lo.repo*pkg^disp'(XLbl149, XThis149)), XLbl149, XThis149).
'lo.core$display$lo.repo*pkg@disp'('lo.repo#pkg'(XP, 'lo.repo#defltVersion'), 'lo.core#ss'(XP), XLbV98, XThV98) :- !.
'lo.core$display$lo.repo*pkg@disp'('lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XP), 'lo.core#,..'('lo.core#ss'("#"), 'lo.core#,..'('lo.core#ss'(XV), 'lo.core#[]')))), XLbV98, XThV98) :- !.
'lo.core$display$lo.repo*pkg@disp'(_, _, _, _) :- raise_exception('error'("disp", 55, 5, 34)).
'lo.core$display$lo.repo*version'('lo.core$display$lo.repo*version%1'('lo.core$display$lo.repo*version')) :- !.
'lo.core$display$lo.repo*version'('disp%2'(XV784, XV785), XLbl150, XThis150) :- !,
    'lo.core$display$lo.repo*version@disp'(XV784, XV785, XLbl150, XThis150).
'lo.core$display$lo.repo*version'('disp%1'('lo.core$display$lo.repo*version^disp'(XLbl151, XThis151)), XLbl151, XThis151).
'lo.core$display$lo.repo*version@disp'('lo.repo#defltVersion', 'lo.core#ss'("*"), XLbV99, XThV99) :- !.
'lo.core$display$lo.repo*version@disp'('lo.repo#vers'(XV), 'lo.core#ss'(XV), XLbV99, XThV99) :- !.
'lo.core$display$lo.repo*version@disp'(_, _, _, _) :- raise_exception('error'("disp", 60, 5, 29)).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string'('lo.coerce$coercion$lo.repo*pkg$lo.core*string%1'('lo.coerce$coercion$lo.repo*pkg$lo.core*string')) :- !.
'lo.coerce$coercion$lo.repo*pkg$lo.core*string'('_coerce%2'(XV790, XV791), XLbl152, XThis152) :- !,
    'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'(XV790, XV791, XLbl152, XThis152).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string'('_coerce%1'('lo.coerce$coercion$lo.repo*pkg$lo.core*string^_coerce'(XLbl153, XThis153)), XLbl153, XThis153).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'('lo.repo#pkg'(XP, 'lo.repo#defltVersion'), XP, XLbV100, XThV100) :- !.
'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'('lo.repo#pkg'(XP, 'lo.repo#vers'(XV)), XX3323, XLbV100, XThV100) :- !,
    ocall('+%3'(XP, "#", XX3320),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%3'(XX3320, XV, XX3323),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string').
'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 65, 5, 33)).
'lo.repo@parsePkgName'(XP, 'lo.repo#pkg'(XNm, 'lo.repo#vers'(XV))) :- '_str_find'(XP, "#", 0, XPos),
    '_str_split'(XP, XPos, XNm, X_181),
    ocall('+%3'(XPos, 1, XX3334),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    '_str_split'(XP, XX3334, X_182, XV),
    !.
'lo.repo@parsePkgName'(XP, 'lo.repo#pkg'(XP, 'lo.repo#defltVersion')) :- !.
'lo.repo@parsePkgName'(_, _) :- raise_exception('error'("parsePkgName", 77, 3, 121)).
'lo.repo@defltVersion'('lo.repo#defltVersion') :- !.
'lo.repo^comboPkgPrsnt'('_call%3'(XV711, XV712, XV713), 'lo.repo^comboPkgPrsnt', _) :- 'lo.repo@comboPkgPrsnt'(XV711, XV712, XV713).
'lo.repo@cond4'(Xlo_repo_repository_b2, XB, XTxt, Xlo_repo_repository_a2, XK, XP, XA) :- ocall('packagePresent%3'(XA, XP, XK),Xlo_repo_repository_a2,Xlo_repo_repository_a2),
    !,
    ocall('loadFromRepo%4'(XA, XP, XK, XTxt),Xlo_repo_repository_a2,Xlo_repo_repository_a2).
'lo.repo@cond4'(Xlo_repo_repository_b2, XB, XTxt, Xlo_repo_repository_a2, XK, XP, XA) :- ocall('loadFromRepo%4'(XB, XP, XK, XTxt),Xlo_repo_repository_b2,Xlo_repo_repository_b2).
'lo.repo^comboLoad'('_call%4'(XV714, XV715, XV716, XV717), 'lo.repo^comboLoad', _) :- 'lo.repo@comboLoad'(XV714, XV715, XV716, XV717).
'lo.repo$repository$()2^packagePresent'('_call%3'(XV718, XV719, XV720), 'lo.repo$repository$()2^packagePresent'(XLbV94, XThV94), _) :- 'lo.repo$repository$()2@packagePresent'(XV718, XV719, XV720, XLbV94, XThV94).
'lo.repo$repository$()2^packagePresent'('_call%3'(XV724, XV725, XV726), 'lo.repo$repository$()2^packagePresent'(XLbV94, XThV94), _) :- 'lo.repo$repository$()2@packagePresent'(XV724, XV725, XV726, XLbV94, XThV94).
'lo.repo$repository$()2^loadFromRepo'('_call%4'(XV727, XV728, XV729, XV730), 'lo.repo$repository$()2^loadFromRepo'(XLbV94, XThV94), _) :- 'lo.repo$repository$()2@loadFromRepo'(XV727, XV728, XV729, XV730, XLbV94, XThV94).
'lo.repo$repository$()2^loadFromRepo'('_call%4'(XV735, XV736, XV737, XV738), 'lo.repo$repository$()2^loadFromRepo'(XLbV94, XThV94), _) :- 'lo.repo$repository$()2@loadFromRepo'(XV735, XV736, XV737, XV738, XLbV94, XThV94).
'lo.repo@coreRepo'('lo.repo#coreRepo') :- !.
'lo.repo$repository$lo.repo*coreRepo^packagePresent'('_call%3'(XV739, XV740, XV741), 'lo.repo$repository$lo.repo*coreRepo^packagePresent'(XLbV96, XThV96), _) :- 'lo.repo$repository$lo.repo*coreRepo@packagePresent'(XV739, XV740, XV741, XLbV96, XThV96).
'lo.repo$repository$lo.repo*coreRepo^packagePresent'('_call%3'(XV745, XV746, XV747), 'lo.repo$repository$lo.repo*coreRepo^packagePresent'(XLbV96, XThV96), _) :- 'lo.repo$repository$lo.repo*coreRepo@packagePresent'(XV745, XV746, XV747, XLbV96, XThV96).
'lo.repo$repository$lo.repo*coreRepo^loadFromRepo'('_call%4'(XV748, XV749, XV750, XV751), 'lo.repo$repository$lo.repo*coreRepo^loadFromRepo'(XLbV96, XThV96), _) :- 'lo.repo$repository$lo.repo*coreRepo@loadFromRepo'(XV748, XV749, XV750, XV751, XLbV96, XThV96).
'lo.repo$repository$lo.repo*coreRepo^loadFromRepo'('_call%4'(XV756, XV757, XV758, XV759), 'lo.repo$repository$lo.repo*coreRepo^loadFromRepo'(XLbV96, XThV96), _) :- 'lo.repo$repository$lo.repo*coreRepo@loadFromRepo'(XV756, XV757, XV758, XV759, XLbV96, XThV96).
'lo.repo^samePackage'('_call%2'(XV760, XV761), 'lo.repo^samePackage', _) :- 'lo.repo@samePackage'(XV760, XV761).
'lo.repo^packageHash'('_call%2'(XV762, XV763), 'lo.repo^packageHash', _) :- 'lo.repo@packageHash'(XV762, XV763).
'lo.core$equality$lo.repo*pkg^=='('_call%2'(XV764, XV765), 'lo.core$equality$lo.repo*pkg^=='(XLbV97, XThV97), _) :- 'lo.core$equality$lo.repo*pkg@=='(XV764, XV765, XLbV97, XThV97).
'lo.core$equality$lo.repo*pkg^=='('_call%2'(XV768, XV769), 'lo.core$equality$lo.repo*pkg^=='(XLbV97, XThV97), _) :- 'lo.core$equality$lo.repo*pkg@=='(XV768, XV769, XLbV97, XThV97).
'lo.core$equality$lo.repo*pkg^hash'('_call%2'(XV770, XV771), 'lo.core$equality$lo.repo*pkg^hash'(XLbV97, XThV97), _) :- 'lo.core$equality$lo.repo*pkg@hash'(XV770, XV771, XLbV97, XThV97).
'lo.core$equality$lo.repo*pkg^hash'('_call%2'(XV774, XV775), 'lo.core$equality$lo.repo*pkg^hash'(XLbV97, XThV97), _) :- 'lo.core$equality$lo.repo*pkg@hash'(XV774, XV775, XLbV97, XThV97).
'lo.core$display$lo.repo*pkg^disp'('_call%2'(XV776, XV777), 'lo.core$display$lo.repo*pkg^disp'(XLbV98, XThV98), _) :- 'lo.core$display$lo.repo*pkg@disp'(XV776, XV777, XLbV98, XThV98).
'lo.core$display$lo.repo*pkg^disp'('_call%2'(XV780, XV781), 'lo.core$display$lo.repo*pkg^disp'(XLbV98, XThV98), _) :- 'lo.core$display$lo.repo*pkg@disp'(XV780, XV781, XLbV98, XThV98).
'lo.core$display$lo.repo*version^disp'('_call%2'(XV782, XV783), 'lo.core$display$lo.repo*version^disp'(XLbV99, XThV99), _) :- 'lo.core$display$lo.repo*version@disp'(XV782, XV783, XLbV99, XThV99).
'lo.core$display$lo.repo*version^disp'('_call%2'(XV786, XV787), 'lo.core$display$lo.repo*version^disp'(XLbV99, XThV99), _) :- 'lo.core$display$lo.repo*version@disp'(XV786, XV787, XLbV99, XThV99).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string^_coerce'('_call%2'(XV788, XV789), 'lo.coerce$coercion$lo.repo*pkg$lo.core*string^_coerce'(XLbV100, XThV100), _) :- 'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'(XV788, XV789, XLbV100, XThV100).
'lo.coerce$coercion$lo.repo*pkg$lo.core*string^_coerce'('_call%2'(XV792, XV793), 'lo.coerce$coercion$lo.repo*pkg$lo.core*string^_coerce'(XLbV100, XThV100), _) :- 'lo.coerce$coercion$lo.repo*pkg$lo.core*string@_coerce'(XV792, XV793, XLbV100, XThV100).
'lo.repo^parsePkgName'('_call%2'(XV794, XV795), 'lo.repo^parsePkgName', _) :- 'lo.repo@parsePkgName'(XV794, XV795).
