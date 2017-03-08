'#pkg'("n7o7'()7'n2o2'pkg's'lo.sets's'1.0.0'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.index'e'*'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'s\"I3'set':k'e'CT1Uz2'lo.index*map'2k'e'T0Uz1'lo.sets*set'1k'e''nullSet':k'e'|Uz1'lo.sets*set'1k'e'c'lo.core$equality'T1k'e'T0'setMap':k'e':k'f'||FT2Uz1'lo.sets*set'1k'e'FT1k'e'k'f'Uz1'lo.sets*set'1k'f'c'lo.core$equality'T1k'f'T0c'lo.core$equality'T1k'e'T0\"s\"I1'set':k'e':k'e'YUz1'lo.sets*set'1k'e'I0\"n1o1'()1's'set'n0o0'()0'n8o8'()8'n2o2'()2's'lo.collection$membership$lo.sets*set's\":k'e'|c'lo.collection$membership'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.collection$setops$lo.sets*set's\":k'e'|c'lo.collection$setops'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.coerce$coercion$lo.sets*set$lo.core*list's\":k'e'|c'lo.coerce$coercion'T2Uz1'lo.sets*set'1k'e'Lk'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.sets*set's\":k'e'|c'lo.coerce$coercion'T2Lk'e'Uz1'lo.sets*set'1k'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.core$additive$lo.sets*set's\":k'e'|c'lo.core$additive'T1Uz1'lo.sets*set'1k'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.collection$mapping$lo.sets*set's\":k'e':k'f'||c'lo.collection$mapping'T1z1'lo.sets*set'T2k'e'k'f'c'lo.core$equality'T1k'e'T0c'lo.core$equality'T1k'f'T0\"n2o2'()2's'lo.collection$folding$lo.sets*set's\":k'e'|c'lo.collection$folding'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.core$display$lo.sets*set's\":k'e'||c'lo.core$display'T1Uz1'lo.sets*set'1k'e'T0c'lo.core$display'T1k'e'T0c'lo.core$equality'T1k'e'T0\"").
'lo.sets@init'():- !.
'lo.sets#set'('set%1'('lo.sets@set'())):- !.
'lo.sets@nullSet'('lo.sets@nullSet23'):- !.
'lo.collection$membership$lo.sets*set'('lo.collection$membership$lo.sets*set%1'('lo.collection$membership$lo.sets*set')):- !.
'lo.collection$membership$lo.sets*set'('empty%1'(XV18024), XLbl3747, XThis3747):- !,
    'lo.collection$membership$lo.sets*set@empty'(XV18024, XLbl3747, XThis3747).
'lo.collection$membership$lo.sets*set'('addMem%3'(XV18028, XV18029, XV18030), XLbl3748, XThis3748):- !,
    'lo.collection$membership$lo.sets*set@addMem'(XV18028, XV18029, XV18030, XLbl3748, XThis3748).
'lo.collection$membership$lo.sets*set'('addMem%1'('lo.collection$membership$lo.sets*set^addMem'(XLbl3749, XThis3749)), XLbl3749, XThis3749).
'lo.collection$membership$lo.sets*set'('delMem%3'(XV18034, XV18035, XV18036), XLbl3750, XThis3750):- !,
    'lo.collection$membership$lo.sets*set@delMem'(XV18034, XV18035, XV18036, XLbl3750, XThis3750).
'lo.collection$membership$lo.sets*set'('delMem%1'('lo.collection$membership$lo.sets*set^delMem'(XLbl3751, XThis3751)), XLbl3751, XThis3751).
'lo.collection$membership$lo.sets*set'('in%2'(XV18041, XV18042), XLbl3752, XThis3752):- !,
    'lo.collection$membership$lo.sets*set@in'(XV18041, XV18042, XLbl3752, XThis3752).
'lo.collection$membership$lo.sets*set'('in%1'('lo.collection$membership$lo.sets*set^in'(XLbl3753, XThis3753)), XLbl3753, XThis3753).
'lo.collection$membership$lo.sets*set@empty'(XXe1857, XLbV1620, XThV1620):- XLbV1620 = 'lo.collection$membership$lo.sets*set'(Xequality777),
    !,
    'lo.sets@nullSet'(XnullSet24),
    ocall('_call%2'(Xequality777, XXe1857),XnullSet24,XnullSet24).
'lo.collection$membership$lo.sets*set@addMem'('lo.sets#set'(XS), XE, 'lo.sets#set'(XXe1858), XLbV1620, XThV1620):- XLbV1620 = 'lo.collection$membership$lo.sets*set'(Xequality777),
    !,
    ocall('_put%1'(XXV1874),'lo.collection$map$lo.index*map'(Xequality777),'lo.collection$map$lo.index*map'(Xequality777)),
    ocall('_call%4'(XS, XE, '()0'(), XXe1858),XXV1874,XXV1874).
'lo.collection$membership$lo.sets*set@addMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.sets*set@addMem", 15, 5, 33)).
'lo.collection$membership$lo.sets*set@delMem'('lo.sets#set'(XS), XE, 'lo.sets#set'(XXe1859), XLbV1620, XThV1620):- XLbV1620 = 'lo.collection$membership$lo.sets*set'(Xequality777),
    !,
    ocall('_remove%1'(XXV1875),'lo.collection$map$lo.index*map'(Xequality777),'lo.collection$map$lo.index*map'(Xequality777)),
    ocall('_call%3'(XS, XE, XXe1859),XXV1875,XXV1875).
'lo.collection$membership$lo.sets*set@delMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.sets*set@delMem", 16, 5, 31)).
'lo.collection$membership$lo.sets*set@in'(Xk, 'lo.sets#set'(XM1), XLbV1620, XThV1620):- XLbV1620 = 'lo.collection$membership$lo.sets*set'(Xequality777),
    'lo.collection$membership$lo.sets*set@cond91'(XXd8680, X_5380, XXe1860, XXV1876, XXd8679, Xequality777, XLbV1620, XThV1620, XM1, Xk).
'lo.sets@setDifference'(Xequality778, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XXe1861)):- !,
    ocall('-%1'(XXV1877),'lo.core$additive$lo.index*map'(Xequality778),'lo.core$additive$lo.index*map'(Xequality778)),
    ocall('_call%3'(XM1, XM2, XXe1861),XXV1877,XXV1877).
'lo.sets@setDifference'(_, _, _, _):- raise_exception('error'("lo.sets@setDifference", 43, 3, 44)).
'lo.sets@binApp'(Xequality779, Xk, X_5381, XM, XM):- ocall('present%3'(XM, Xk, X_5382),'lo.collection$map$lo.index*map'(Xequality779),'lo.collection$map$lo.index*map'(Xequality779)),
    !.
'lo.sets@binApp'(Xequality779, Xk, X_5383, XM, XXe1862):- !,
    ocall('_put%1'(XXV1878),'lo.collection$map$lo.index*map'(Xequality779),'lo.collection$map$lo.index*map'(Xequality779)),
    ocall('_call%4'(XM, Xk, '()0'(), XXe1862),XXV1878,XXV1878).
'lo.sets@binApp'(_, _, _, _, _):- raise_exception('error'("lo.sets@binApp", 49, 3, 36)).
'lo.sets@setIntersection'(Xequality780, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XXd8686)):- !,
    'lo.sets@binApp'(Xequality780, XXd8685),
    'lo.index@foldMap'(XXd8685, XM2, XM1, XXd8686).
'lo.sets@setIntersection'(_, _, _, _):- raise_exception('error'("lo.sets@setIntersection", 46, 3, 63)).
'lo.sets@setUnion'(Xequality781, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XXe1863)):- !,
    ocall('+%1'(XXV1879),'lo.core$additive$lo.index*map'(Xequality781),'lo.core$additive$lo.index*map'(Xequality781)),
    ocall('_call%3'(XM1, XM2, XXe1863),XXV1879,XXV1879).
'lo.sets@setUnion'(_, _, _, _):- raise_exception('error'("lo.sets@setUnion", 40, 3, 39)).
'lo.collection$setops$lo.sets*set'('lo.collection$setops$lo.sets*set%1'('lo.collection$setops$lo.sets*set')):- !.
'lo.collection$setops$lo.sets*set'('union%3'(XV18063, XV18064, XV18065), XLbl3754, XThis3754):- !,
    'lo.collection$setops$lo.sets*set@union'(XV18063, XV18064, XV18065, XLbl3754, XThis3754).
'lo.collection$setops$lo.sets*set'('union%1'('lo.collection$setops$lo.sets*set^union'(XLbl3755, XThis3755)), XLbl3755, XThis3755).
'lo.collection$setops$lo.sets*set'('intersect%3'(XV18069, XV18070, XV18071), XLbl3756, XThis3756):- !,
    'lo.collection$setops$lo.sets*set@intersect'(XV18069, XV18070, XV18071, XLbl3756, XThis3756).
'lo.collection$setops$lo.sets*set'('intersect%1'('lo.collection$setops$lo.sets*set^intersect'(XLbl3757, XThis3757)), XLbl3757, XThis3757).
'lo.collection$setops$lo.sets*set'('difference%3'(XV18075, XV18076, XV18077), XLbl3758, XThis3758):- !,
    'lo.collection$setops$lo.sets*set@difference'(XV18075, XV18076, XV18077, XLbl3758, XThis3758).
'lo.collection$setops$lo.sets*set'('difference%1'('lo.collection$setops$lo.sets*set^difference'(XLbl3759, XThis3759)), XLbl3759, XThis3759).
'lo.collection$setops$lo.sets*set@union'(XS1, XS2, XXd8690, XLbV1621, XThV1621):- XLbV1621 = 'lo.collection$setops$lo.sets*set'(Xequality782),
    !,
    'lo.sets@setUnion'(Xequality782, XS1, XS2, XXd8690).
'lo.collection$setops$lo.sets*set@union'(_, _, _):- raise_exception('error'("lo.collection$setops$lo.sets*set@union", 21, 5, 31)).
'lo.collection$setops$lo.sets*set@intersect'(XS1, XS2, XXd8691, XLbV1621, XThV1621):- XLbV1621 = 'lo.collection$setops$lo.sets*set'(Xequality782),
    !,
    'lo.sets@setIntersection'(Xequality782, XS1, XS2, XXd8691).
'lo.collection$setops$lo.sets*set@intersect'(_, _, _):- raise_exception('error'("lo.collection$setops$lo.sets*set@intersect", 22, 5, 42)).
'lo.collection$setops$lo.sets*set@difference'(XS1, XS2, XXd8692, XLbV1621, XThV1621):- XLbV1621 = 'lo.collection$setops$lo.sets*set'(Xequality782),
    !,
    'lo.sets@setDifference'(Xequality782, XS1, XS2, XXd8692).
'lo.collection$setops$lo.sets*set@difference'(_, _, _):- raise_exception('error'("lo.collection$setops$lo.sets*set@difference", 23, 5, 41)).
'lo.coerce$coercion$lo.sets*set$lo.core*list'('lo.coerce$coercion$lo.sets*set$lo.core*list%1'('lo.coerce$coercion$lo.sets*set$lo.core*list')):- !.
'lo.coerce$coercion$lo.sets*set$lo.core*list'('_coerce%2'(XV18080, XV18081), XLbl3760, XThis3760):- !,
    'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(XV18080, XV18081, XLbl3760, XThis3760).
'lo.coerce$coercion$lo.sets*set$lo.core*list'('_coerce%1'('lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'(XLbl3761, XThis3761)), XLbl3761, XThis3761).
'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'('lo.sets#set'(XEls), XXe1864, XLbV1622, XThV1622):- XLbV1622 = 'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality783),
    !,
    ocall('keys%1'(XXV1880),'lo.collection$map$lo.index*map'(Xequality783),'lo.collection$map$lo.index*map'(Xequality783)),
    ocall('_call%2'(XEls, XXe1864),XXV1880,XXV1880).
'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce", 27, 5, 30)).
'lo.coerce$coercion$lo.core*list$lo.sets*set'('lo.coerce$coercion$lo.core*list$lo.sets*set%1'('lo.coerce$coercion$lo.core*list$lo.sets*set')):- !.
'lo.coerce$coercion$lo.core*list$lo.sets*set'('_coerce%2'(XV18084, XV18085), XLbl3762, XThis3762):- !,
    'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XV18084, XV18085, XLbl3762, XThis3762).
'lo.coerce$coercion$lo.core*list$lo.sets*set'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'(XLbl3763, XThis3763)), XLbl3763, XThis3763).
'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XEls, 'lo.sets#set'(XXe1866), XLbV1623, XThV1623):- XLbV1623 = 'lo.coerce$coercion$lo.core*list$lo.sets*set'(Xequality784),
    !,
    ocall('foldLeft%1'(XXV1883),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_empty%1'(XXV1882),'lo.collection$map$lo.index*map'(Xequality784),'lo.collection$map$lo.index*map'(Xequality784)),
    ocall('_call%4'('lo.coerce$coercion$lo.core*list$lo.sets*set@fun81'(Xequality784), XXV1882, XEls, XXe1866),XXV1883,XXV1883).
'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce", 31, 5, 53)).
'lo.core$additive$lo.sets*set'('lo.core$additive$lo.sets*set%1'('lo.core$additive$lo.sets*set')):- !.
'lo.core$additive$lo.sets*set'('+%3'(XV18089, XV18090, XV18091), XLbl3764, XThis3764):- !,
    'lo.core$additive$lo.sets*set@+'(XV18089, XV18090, XV18091, XLbl3764, XThis3764).
'lo.core$additive$lo.sets*set'('+%1'('lo.core$additive$lo.sets*set^+'(XLbl3765, XThis3765)), XLbl3765, XThis3765).
'lo.core$additive$lo.sets*set'('-%3'(XV18095, XV18096, XV18097), XLbl3766, XThis3766):- !,
    'lo.core$additive$lo.sets*set@-'(XV18095, XV18096, XV18097, XLbl3766, XThis3766).
'lo.core$additive$lo.sets*set'('-%1'('lo.core$additive$lo.sets*set^-'(XLbl3767, XThis3767)), XLbl3767, XThis3767).
'lo.core$additive$lo.sets*set@+'(XS1, XS2, XXd8697, XLbV1624, XThV1624):- XLbV1624 = 'lo.core$additive$lo.sets*set'(Xequality785),
    !,
    'lo.sets@setUnion'(Xequality785, XS1, XS2, XXd8697).
'lo.core$additive$lo.sets*set@+'(_, _, _):- raise_exception('error'("lo.core$additive$lo.sets*set@+", 35, 5, 24)).
'lo.core$additive$lo.sets*set@-'(XS1, XS2, XXd8698, XLbV1624, XThV1624):- XLbV1624 = 'lo.core$additive$lo.sets*set'(Xequality785),
    !,
    'lo.sets@setDifference'(Xequality785, XS1, XS2, XXd8698).
'lo.core$additive$lo.sets*set@-'(_, _, _):- raise_exception('error'("lo.core$additive$lo.sets*set@-", 36, 5, 29)).
'lo.sets@setMap'(Xequality786, Xequality787, 'lo.sets#set'(XEls), XF, 'lo.sets#set'(XXe1870)):- !,
    ocall('keys%1'(XXV1886),'lo.collection$map$lo.index*map'(Xequality786),'lo.collection$map$lo.index*map'(Xequality786)),
    ocall('foldLeft%1'(XXV1887),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_empty%1'(XXV1885),'lo.collection$map$lo.index*map'(Xequality787),'lo.collection$map$lo.index*map'(Xequality787)),
    ocall('_call%2'(XEls, XXe1869),XXV1886,XXV1886),
    ocall('_call%4'('lo.sets@fun82'(Xequality787, XF), XXV1885, XXe1869, XXe1870),XXV1887,XXV1887).
'lo.sets@setMap'(_, _, _, _, _):- raise_exception('error'("lo.sets@setMap", 57, 3, 68)).
'lo.collection$mapping$lo.sets*set'('lo.collection$mapping$lo.sets*set%1'('lo.collection$mapping$lo.sets*set')):- !.
'lo.collection$mapping$lo.sets*set'('//%3'(XV18106, XV18107, XV18108), XLbl3768, XThis3768):- !,
    'lo.collection$mapping$lo.sets*set@//'(XV18106, XV18107, XV18108, XLbl3768, XThis3768).
'lo.collection$mapping$lo.sets*set'('//%1'('lo.collection$mapping$lo.sets*set^//'(XLbl3769, XThis3769)), XLbl3769, XThis3769).
'lo.collection$mapping$lo.sets*set@//'(XS, XF, XXd8704, XLbV1625, XThV1625):- XLbV1625 = 'lo.collection$mapping$lo.sets*set'(Xequality788, Xequality789),
    !,
    'lo.sets@setMap'(Xequality789, Xequality788, XS, XF, XXd8704).
'lo.collection$mapping$lo.sets*set@//'(_, _, _):- raise_exception('error'("lo.collection$mapping$lo.sets*set@//", 53, 5, 19)).
'lo.sets@foldLeftEls'(XF, XX, XS, XXe1871):- !,
    ocall('foldLeft%1'(XXV1888),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'(XF, XX, XS, XXe1871),XXV1888,XXV1888).
'lo.sets@foldLeftEls'(_, _, _, _):- raise_exception('error'("lo.sets@foldLeftEls", 65, 3, 37)).
'lo.sets@foldRightEls'(XF, XX, XS, XXe1872):- !,
    ocall('foldRight%1'(XXV1889),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'(XF, XX, XS, XXe1872),XXV1889,XXV1889).
'lo.sets@foldRightEls'(_, _, _, _):- raise_exception('error'("lo.sets@foldRightEls", 68, 3, 39)).
'lo.collection$folding$lo.sets*set'('lo.collection$folding$lo.sets*set%1'('lo.collection$folding$lo.sets*set')):- !.
'lo.collection$folding$lo.sets*set'('foldRight%4'(XV18121, XV18122, XV18123, XV18124), XLbl3770, XThis3770):- !,
    'lo.collection$folding$lo.sets*set@foldRight'(XV18121, XV18122, XV18123, XV18124, XLbl3770, XThis3770).
'lo.collection$folding$lo.sets*set'('foldRight%1'('lo.collection$folding$lo.sets*set^foldRight'(XLbl3771, XThis3771)), XLbl3771, XThis3771).
'lo.collection$folding$lo.sets*set'('foldLeft%4'(XV18129, XV18130, XV18131, XV18132), XLbl3772, XThis3772):- !,
    'lo.collection$folding$lo.sets*set@foldLeft'(XV18129, XV18130, XV18131, XV18132, XLbl3772, XThis3772).
'lo.collection$folding$lo.sets*set'('foldLeft%1'('lo.collection$folding$lo.sets*set^foldLeft'(XLbl3773, XThis3773)), XLbl3773, XThis3773).
'lo.collection$folding$lo.sets*set@foldRight'(XF, XX, XS, XXd8706, XLbV1626, XThV1626):- XLbV1626 = 'lo.collection$folding$lo.sets*set'(Xequality790),
    !,
    ocall('_coerce%1'(XXV1890),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality790),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality790)),
    ocall('_call%2'(XS, XXe1873),XXV1890,XXV1890),
    'lo.sets@foldRightEls'(XF, XX, XXe1873, XXd8706).
'lo.collection$folding$lo.sets*set@foldRight'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.sets*set@foldRight", 60, 5, 48)).
'lo.collection$folding$lo.sets*set@foldLeft'(XF, XX, XS, XXd8708, XLbV1626, XThV1626):- XLbV1626 = 'lo.collection$folding$lo.sets*set'(Xequality790),
    !,
    ocall('_coerce%1'(XXV1891),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality790),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality790)),
    ocall('_call%2'(XS, XXe1874),XXV1891,XXV1891),
    'lo.sets@foldLeftEls'(XF, XX, XXe1874, XXd8708).
'lo.collection$folding$lo.sets*set@foldLeft'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.sets*set@foldLeft", 61, 5, 46)).
'lo.sets@dispEls'(Xdisplay249, 'lo.core#[]', X_5384, 'lo.core#[]'):- !.
'lo.sets@dispEls'(Xdisplay249, 'lo.core#,..'(XE, XL), XSep, 'lo.core#,..'('lo.core#ss'(XSep), 'lo.core#,..'(XXe1875, XXd8710))):- !,
    ocall('disp%1'(XXV1892),Xdisplay249,Xdisplay249),
    ocall('_call%2'(XE, XXe1875),XXV1892,XXV1892),
    'lo.sets@dispEls'(Xdisplay249, XL, ", ", XXd8710).
'lo.sets@dispEls'(_, _, _, _):- raise_exception('error'("lo.sets@dispEls", 75, 3, 19)).
'lo.core$display$lo.sets*set'('lo.core$display$lo.sets*set%1'('lo.core$display$lo.sets*set')):- !.
'lo.core$display$lo.sets*set'('disp%2'(XV18139, XV18140), XLbl3774, XThis3774):- !,
    'lo.core$display$lo.sets*set@disp'(XV18139, XV18140, XLbl3774, XThis3774).
'lo.core$display$lo.sets*set'('disp%1'('lo.core$display$lo.sets*set^disp'(XLbl3775, XThis3775)), XLbl3775, XThis3775).
'lo.core$display$lo.sets*set@disp'('lo.sets#set'(XEls), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("{"), 'lo.core#,..'('lo.core#ssSeq'(XXd8715), 'lo.core#,..'('lo.core#ss'("}"), 'lo.core#[]')))), XLbV1627, XThV1627):- XLbV1627 = 'lo.core$display$lo.sets*set'(Xequality791, Xdisplay250),
    !,
    ocall('keys%1'(XXV1893),'lo.collection$map$lo.index*map'(Xequality791),'lo.collection$map$lo.index*map'(Xequality791)),
    ocall('_call%2'(XEls, XXe1876),XXV1893,XXV1893),
    'lo.sets@dispEls'(Xdisplay250, XXe1876, "", XXd8715).
'lo.core$display$lo.sets*set@disp'(_, _):- raise_exception('error'("lo.core$display$lo.sets*set@disp", 71, 5, 81)).
'lo.sets@nullSet23'('_call%2'(Xequality776, 'lo.sets#set'(XXV1873)), 'lo.sets@nullSet23', _):- !,
    ocall('_empty%1'(XXV1873),'lo.collection$map$lo.index*map'(Xequality776),'lo.collection$map$lo.index*map'(Xequality776)).
'lo.sets@nullSet23'(_, _, _):- raise_exception('error'("lo.sets@nullSet23", 11, 3, 17)).
'lo.collection$membership$lo.sets*set^addMem'('_call%3'(XV18025, XV18026, XV18027), 'lo.collection$membership$lo.sets*set^addMem'(XLbV1620, XThV1620), _):- 'lo.collection$membership$lo.sets*set@addMem'(XV18025, XV18026, XV18027, XLbV1620, XThV1620).
'lo.collection$membership$lo.sets*set^delMem'('_call%3'(XV18031, XV18032, XV18033), 'lo.collection$membership$lo.sets*set^delMem'(XLbV1620, XThV1620), _):- 'lo.collection$membership$lo.sets*set@delMem'(XV18031, XV18032, XV18033, XLbV1620, XThV1620).
'lo.collection$membership$lo.sets*set@cond91'(XXd8680, X_5380, XXe1860, XXV1876, XXd8679, Xequality777, XLbV1620, XThV1620, XM1, Xk):- 'ground'(Xk),
    !,
    ocall('keys%1'(XXV1876),'lo.collection$map$lo.index*map'(Xequality777),'lo.collection$map$lo.index*map'(Xequality777)),
    ocall('_call%2'(XM1, XXe1860),XXV1876,XXV1876),
    'lo.list@listEl'(Xk, XXe1860).
'lo.collection$membership$lo.sets*set@cond91'(XXd8680, X_5380, XXe1860, XXV1876, XXd8679, Xequality777, XLbV1620, XThV1620, XM1, Xk):- ocall('present%3'(XM1, Xk, X_5380),'lo.collection$map$lo.index*map'(Xequality777),'lo.collection$map$lo.index*map'(Xequality777)).
'lo.collection$membership$lo.sets*set^in'('_call%4'(XV18037, XV18038, XV18039, XV18040), 'lo.collection$membership$lo.sets*set^in'(XLbV1620, XThV1620), _):- 'lo.collection$membership$lo.sets*set@in'(XV18037, XV18038, XV18039, XV18040, XLbV1620, XThV1620).
'lo.sets^setDifference'('_call%4'(XV18043, XV18044, XV18045, XV18046), 'lo.sets^setDifference', _):- 'lo.sets@setDifference'(XV18043, XV18044, XV18045, XV18046).
'lo.sets^binApp'('_call%5'(XV18047, XV18048, XV18049, XV18050, XV18051), 'lo.sets^binApp', _):- 'lo.sets@binApp'(XV18047, XV18048, XV18049, XV18050, XV18051).
'lo.sets^setIntersection'('_call%4'(XV18052, XV18053, XV18054, XV18055), 'lo.sets^setIntersection', _):- 'lo.sets@setIntersection'(XV18052, XV18053, XV18054, XV18055).
'lo.sets^setUnion'('_call%4'(XV18056, XV18057, XV18058, XV18059), 'lo.sets^setUnion', _):- 'lo.sets@setUnion'(XV18056, XV18057, XV18058, XV18059).
'lo.collection$setops$lo.sets*set^union'('_call%3'(XV18060, XV18061, XV18062), 'lo.collection$setops$lo.sets*set^union'(XLbV1621, XThV1621), _):- 'lo.collection$setops$lo.sets*set@union'(XV18060, XV18061, XV18062, XLbV1621, XThV1621).
'lo.collection$setops$lo.sets*set^intersect'('_call%3'(XV18066, XV18067, XV18068), 'lo.collection$setops$lo.sets*set^intersect'(XLbV1621, XThV1621), _):- 'lo.collection$setops$lo.sets*set@intersect'(XV18066, XV18067, XV18068, XLbV1621, XThV1621).
'lo.collection$setops$lo.sets*set^difference'('_call%3'(XV18072, XV18073, XV18074), 'lo.collection$setops$lo.sets*set^difference'(XLbV1621, XThV1621), _):- 'lo.collection$setops$lo.sets*set@difference'(XV18072, XV18073, XV18074, XLbV1621, XThV1621).
'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'('_call%2'(XV18078, XV18079), 'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'(XLbV1622, XThV1622), _):- 'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(XV18078, XV18079, XLbV1622, XThV1622).
'lo.coerce$coercion$lo.core*list$lo.sets*set@fun81'('_call%3'(XM, XE, XXe1865), 'lo.coerce$coercion$lo.core*list$lo.sets*set@fun81'(Xequality784), _):- !,
    ocall('_put%1'(XXV1881),'lo.collection$map$lo.index*map'(Xequality784),'lo.collection$map$lo.index*map'(Xequality784)),
    ocall('_call%4'(XM, XE, '()0'(), XXe1865),XXV1881,XXV1881).
'lo.coerce$coercion$lo.core*list$lo.sets*set@fun81'(_, _, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.sets*set@fun81", 31, 34, 15)).
'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'('_call%2'(XV18082, XV18083), 'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'(XLbV1623, XThV1623), _):- 'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XV18082, XV18083, XLbV1623, XThV1623).
'lo.core$additive$lo.sets*set^+'('_call%3'(XV18086, XV18087, XV18088), 'lo.core$additive$lo.sets*set^+'(XLbV1624, XThV1624), _):- 'lo.core$additive$lo.sets*set@+'(XV18086, XV18087, XV18088, XLbV1624, XThV1624).
'lo.core$additive$lo.sets*set^-'('_call%3'(XV18092, XV18093, XV18094), 'lo.core$additive$lo.sets*set^-'(XLbV1624, XThV1624), _):- 'lo.core$additive$lo.sets*set@-'(XV18092, XV18093, XV18094, XLbV1624, XThV1624).
'lo.sets@fun82'('_call%3'(XM, Xk, XXe1868), 'lo.sets@fun82'(Xequality787, XF), _):- !,
    ocall('_put%1'(XXV1884),'lo.collection$map$lo.index*map'(Xequality787),'lo.collection$map$lo.index*map'(Xequality787)),
    ocall('_call%2'(Xk, XXe1867),XF,XF),
    ocall('_call%4'(XM, XXe1867, '()0'(), XXe1868),XXV1884,XXV1884).
'lo.sets@fun82'(_, _, _):- raise_exception('error'("lo.sets@fun82", 57, 38, 18)).
'lo.sets^setMap'('_call%5'(XV18098, XV18099, XV18100, XV18101, XV18102), 'lo.sets^setMap', _):- 'lo.sets@setMap'(XV18098, XV18099, XV18100, XV18101, XV18102).
'lo.collection$mapping$lo.sets*set^//'('_call%3'(XV18103, XV18104, XV18105), 'lo.collection$mapping$lo.sets*set^//'(XLbV1625, XThV1625), _):- 'lo.collection$mapping$lo.sets*set@//'(XV18103, XV18104, XV18105, XLbV1625, XThV1625).
'lo.sets^foldLeftEls'('_call%4'(XV18109, XV18110, XV18111, XV18112), 'lo.sets^foldLeftEls', _):- 'lo.sets@foldLeftEls'(XV18109, XV18110, XV18111, XV18112).
'lo.sets^foldRightEls'('_call%4'(XV18113, XV18114, XV18115, XV18116), 'lo.sets^foldRightEls', _):- 'lo.sets@foldRightEls'(XV18113, XV18114, XV18115, XV18116).
'lo.collection$folding$lo.sets*set^foldRight'('_call%4'(XV18117, XV18118, XV18119, XV18120), 'lo.collection$folding$lo.sets*set^foldRight'(XLbV1626, XThV1626), _):- 'lo.collection$folding$lo.sets*set@foldRight'(XV18117, XV18118, XV18119, XV18120, XLbV1626, XThV1626).
'lo.collection$folding$lo.sets*set^foldLeft'('_call%4'(XV18125, XV18126, XV18127, XV18128), 'lo.collection$folding$lo.sets*set^foldLeft'(XLbV1626, XThV1626), _):- 'lo.collection$folding$lo.sets*set@foldLeft'(XV18125, XV18126, XV18127, XV18128, XLbV1626, XThV1626).
'lo.sets^dispEls'('_call%4'(XV18133, XV18134, XV18135, XV18136), 'lo.sets^dispEls', _):- 'lo.sets@dispEls'(XV18133, XV18134, XV18135, XV18136).
'lo.core$display$lo.sets*set^disp'('_call%2'(XV18137, XV18138), 'lo.core$display$lo.sets*set^disp'(XLbV1627, XThV1627), _):- 'lo.core$display$lo.sets*set@disp'(XV18137, XV18138, XLbV1627, XThV1627).
