'#pkg'("n7o7'()7'n2o2'pkg's'lo.sets's'1.0.0'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.index'e'*'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'s\"I3'set':k'e'CT1Uz2'lo.index*map'2k'e'T0Uz1'lo.sets*set'1k'e''nullSet':k'e'|Uz1'lo.sets*set'1k'e'c'lo.core$equality'T1k'e'T0'setMap':k'e':k'f'||FT2Uz1'lo.sets*set'1k'e'FT1k'e'k'f'Uz1'lo.sets*set'1k'f'c'lo.core$equality'T1k'f'T0c'lo.core$equality'T1k'e'T0\"s\"I1'set':k'e':k'e'YUz1'lo.sets*set'1k'e'I0\"n1o1'()1's'set'n0o0'()0'n8o8'()8'n2o2'()2's'lo.collection$membership$lo.sets*set's\":k'e'|c'lo.collection$membership'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.collection$setops$lo.sets*set's\":k'e'|c'lo.collection$setops'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.coerce$coercion$lo.sets*set$lo.core*list's\":k'e'|c'lo.coerce$coercion'T2Uz1'lo.sets*set'1k'e'Lk'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.sets*set's\":k'e'|c'lo.coerce$coercion'T2Lk'e'Uz1'lo.sets*set'1k'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.core$additive$lo.sets*set's\":k'e'|c'lo.core$additive'T1Uz1'lo.sets*set'1k'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.collection$mapping$lo.sets*set's\":k'e':k'f'||c'lo.collection$mapping'T1z1'lo.sets*set'T2k'e'k'f'c'lo.core$equality'T1k'e'T0c'lo.core$equality'T1k'f'T0\"n2o2'()2's'lo.collection$folding$lo.sets*set's\":k'e'|c'lo.collection$folding'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.core$display$lo.sets*set's\":k'e'||c'lo.core$display'T1Uz1'lo.sets*set'1k'e'T0c'lo.core$display'T1k'e'T0c'lo.core$equality'T1k'e'T0\"").
'lo.sets@init'():- !.
'lo.sets#set'('set%1'('lo.sets@set'())):- !.
'lo.sets@nullSet'('lo.sets@nullSet25'):- !.
'lo.collection$membership$lo.sets*set'('lo.collection$membership$lo.sets*set%1'('lo.collection$membership$lo.sets*set')):- !.
'lo.collection$membership$lo.sets*set'('empty%1'(XV19708), XLbl4089, XThis4089):- !,
    'lo.collection$membership$lo.sets*set@empty'(XV19708, XLbl4089, XThis4089).
'lo.collection$membership$lo.sets*set'('addMem%3'(XV19712, XV19713, XV19714), XLbl4090, XThis4090):- !,
    'lo.collection$membership$lo.sets*set@addMem'(XV19712, XV19713, XV19714, XLbl4090, XThis4090).
'lo.collection$membership$lo.sets*set'('addMem%1'('lo.collection$membership$lo.sets*set^addMem'(XLbl4091, XThis4091)), XLbl4091, XThis4091).
'lo.collection$membership$lo.sets*set'('delMem%3'(XV19718, XV19719, XV19720), XLbl4092, XThis4092):- !,
    'lo.collection$membership$lo.sets*set@delMem'(XV19718, XV19719, XV19720, XLbl4092, XThis4092).
'lo.collection$membership$lo.sets*set'('delMem%1'('lo.collection$membership$lo.sets*set^delMem'(XLbl4093, XThis4093)), XLbl4093, XThis4093).
'lo.collection$membership$lo.sets*set'('in%2'(XV19725, XV19726), XLbl4094, XThis4094):- !,
    'lo.collection$membership$lo.sets*set@in'(XV19725, XV19726, XLbl4094, XThis4094).
'lo.collection$membership$lo.sets*set'('in%1'('lo.collection$membership$lo.sets*set^in'(XLbl4095, XThis4095)), XLbl4095, XThis4095).
'lo.collection$membership$lo.sets*set@empty'(XXe2024, XLbV1782, XThV1782):- XLbV1782 = 'lo.collection$membership$lo.sets*set'(Xequality833),
    !,
    'lo.sets@nullSet'(XnullSet26),
    ocall('_call%2'(Xequality833, XXe2024),XnullSet26,XnullSet26).
'lo.collection$membership$lo.sets*set@addMem'('lo.sets#set'(XS), XE, 'lo.sets#set'(XXe2025), XLbV1782, XThV1782):- XLbV1782 = 'lo.collection$membership$lo.sets*set'(Xequality833),
    !,
    ocall('_put%1'(XXV2044),'lo.collection$map$lo.index*map'(Xequality833),'lo.collection$map$lo.index*map'(Xequality833)),
    ocall('_call%4'(XS, XE, '()0'(), XXe2025),XXV2044,XXV2044).
'lo.collection$membership$lo.sets*set@addMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.sets*set@addMem", 15, 5, 33)).
'lo.collection$membership$lo.sets*set@delMem'('lo.sets#set'(XS), XE, 'lo.sets#set'(XXe2026), XLbV1782, XThV1782):- XLbV1782 = 'lo.collection$membership$lo.sets*set'(Xequality833),
    !,
    ocall('_remove%1'(XXV2045),'lo.collection$map$lo.index*map'(Xequality833),'lo.collection$map$lo.index*map'(Xequality833)),
    ocall('_call%3'(XS, XE, XXe2026),XXV2045,XXV2045).
'lo.collection$membership$lo.sets*set@delMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.sets*set@delMem", 16, 5, 31)).
'lo.collection$membership$lo.sets*set@in'(Xk, 'lo.sets#set'(XM1), XLbV1782, XThV1782):- XLbV1782 = 'lo.collection$membership$lo.sets*set'(Xequality833),
    'lo.collection$membership$lo.sets*set@cond99'(XXd9514, X_5886, XXe2027, XXV2046, XXd9513, Xequality833, XLbV1782, XThV1782, XM1, Xk).
'lo.sets@setDifference'(Xequality834, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XXe2028)):- !,
    ocall('-%1'(XXV2047),'lo.core$additive$lo.index*map'(Xequality834),'lo.core$additive$lo.index*map'(Xequality834)),
    ocall('_call%3'(XM1, XM2, XXe2028),XXV2047,XXV2047).
'lo.sets@setDifference'(_, _, _, _):- raise_exception('error'("lo.sets@setDifference", 43, 3, 44)).
'lo.sets@binApp'(Xequality835, Xk, X_5887, XM, XM):- ocall('present%3'(XM, Xk, X_5888),'lo.collection$map$lo.index*map'(Xequality835),'lo.collection$map$lo.index*map'(Xequality835)),
    !.
'lo.sets@binApp'(Xequality835, Xk, X_5889, XM, XXe2029):- !,
    ocall('_put%1'(XXV2048),'lo.collection$map$lo.index*map'(Xequality835),'lo.collection$map$lo.index*map'(Xequality835)),
    ocall('_call%4'(XM, Xk, '()0'(), XXe2029),XXV2048,XXV2048).
'lo.sets@binApp'(_, _, _, _, _):- raise_exception('error'("lo.sets@binApp", 49, 3, 36)).
'lo.sets@setIntersection'(Xequality836, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XXd9520)):- !,
    'lo.sets@binApp'(Xequality836, XXd9519),
    'lo.index@foldMap'(XXd9519, XM2, XM1, XXd9520).
'lo.sets@setIntersection'(_, _, _, _):- raise_exception('error'("lo.sets@setIntersection", 46, 3, 63)).
'lo.sets@setUnion'(Xequality837, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XXe2030)):- !,
    ocall('+%1'(XXV2049),'lo.core$additive$lo.index*map'(Xequality837),'lo.core$additive$lo.index*map'(Xequality837)),
    ocall('_call%3'(XM1, XM2, XXe2030),XXV2049,XXV2049).
'lo.sets@setUnion'(_, _, _, _):- raise_exception('error'("lo.sets@setUnion", 40, 3, 39)).
'lo.collection$setops$lo.sets*set'('lo.collection$setops$lo.sets*set%1'('lo.collection$setops$lo.sets*set')):- !.
'lo.collection$setops$lo.sets*set'('union%3'(XV19747, XV19748, XV19749), XLbl4096, XThis4096):- !,
    'lo.collection$setops$lo.sets*set@union'(XV19747, XV19748, XV19749, XLbl4096, XThis4096).
'lo.collection$setops$lo.sets*set'('union%1'('lo.collection$setops$lo.sets*set^union'(XLbl4097, XThis4097)), XLbl4097, XThis4097).
'lo.collection$setops$lo.sets*set'('intersect%3'(XV19753, XV19754, XV19755), XLbl4098, XThis4098):- !,
    'lo.collection$setops$lo.sets*set@intersect'(XV19753, XV19754, XV19755, XLbl4098, XThis4098).
'lo.collection$setops$lo.sets*set'('intersect%1'('lo.collection$setops$lo.sets*set^intersect'(XLbl4099, XThis4099)), XLbl4099, XThis4099).
'lo.collection$setops$lo.sets*set'('difference%3'(XV19759, XV19760, XV19761), XLbl4100, XThis4100):- !,
    'lo.collection$setops$lo.sets*set@difference'(XV19759, XV19760, XV19761, XLbl4100, XThis4100).
'lo.collection$setops$lo.sets*set'('difference%1'('lo.collection$setops$lo.sets*set^difference'(XLbl4101, XThis4101)), XLbl4101, XThis4101).
'lo.collection$setops$lo.sets*set@union'(XS1, XS2, XXd9524, XLbV1783, XThV1783):- XLbV1783 = 'lo.collection$setops$lo.sets*set'(Xequality838),
    !,
    'lo.sets@setUnion'(Xequality838, XS1, XS2, XXd9524).
'lo.collection$setops$lo.sets*set@union'(_, _, _):- raise_exception('error'("lo.collection$setops$lo.sets*set@union", 21, 5, 31)).
'lo.collection$setops$lo.sets*set@intersect'(XS1, XS2, XXd9525, XLbV1783, XThV1783):- XLbV1783 = 'lo.collection$setops$lo.sets*set'(Xequality838),
    !,
    'lo.sets@setIntersection'(Xequality838, XS1, XS2, XXd9525).
'lo.collection$setops$lo.sets*set@intersect'(_, _, _):- raise_exception('error'("lo.collection$setops$lo.sets*set@intersect", 22, 5, 42)).
'lo.collection$setops$lo.sets*set@difference'(XS1, XS2, XXd9526, XLbV1783, XThV1783):- XLbV1783 = 'lo.collection$setops$lo.sets*set'(Xequality838),
    !,
    'lo.sets@setDifference'(Xequality838, XS1, XS2, XXd9526).
'lo.collection$setops$lo.sets*set@difference'(_, _, _):- raise_exception('error'("lo.collection$setops$lo.sets*set@difference", 23, 5, 41)).
'lo.coerce$coercion$lo.sets*set$lo.core*list'('lo.coerce$coercion$lo.sets*set$lo.core*list%1'('lo.coerce$coercion$lo.sets*set$lo.core*list')):- !.
'lo.coerce$coercion$lo.sets*set$lo.core*list'('_coerce%2'(XV19764, XV19765), XLbl4102, XThis4102):- !,
    'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(XV19764, XV19765, XLbl4102, XThis4102).
'lo.coerce$coercion$lo.sets*set$lo.core*list'('_coerce%1'('lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'(XLbl4103, XThis4103)), XLbl4103, XThis4103).
'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'('lo.sets#set'(XEls), XXe2031, XLbV1784, XThV1784):- XLbV1784 = 'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality839),
    !,
    ocall('keys%1'(XXV2050),'lo.collection$map$lo.index*map'(Xequality839),'lo.collection$map$lo.index*map'(Xequality839)),
    ocall('_call%2'(XEls, XXe2031),XXV2050,XXV2050).
'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce", 27, 5, 30)).
'lo.coerce$coercion$lo.core*list$lo.sets*set'('lo.coerce$coercion$lo.core*list$lo.sets*set%1'('lo.coerce$coercion$lo.core*list$lo.sets*set')):- !.
'lo.coerce$coercion$lo.core*list$lo.sets*set'('_coerce%2'(XV19768, XV19769), XLbl4104, XThis4104):- !,
    'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XV19768, XV19769, XLbl4104, XThis4104).
'lo.coerce$coercion$lo.core*list$lo.sets*set'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'(XLbl4105, XThis4105)), XLbl4105, XThis4105).
'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XEls, 'lo.sets#set'(XXe2033), XLbV1785, XThV1785):- XLbV1785 = 'lo.coerce$coercion$lo.core*list$lo.sets*set'(Xequality840),
    !,
    ocall('foldLeft%1'(XXV2053),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_empty%1'(XXV2052),'lo.collection$map$lo.index*map'(Xequality840),'lo.collection$map$lo.index*map'(Xequality840)),
    ocall('_call%4'('lo.coerce$coercion$lo.core*list$lo.sets*set@fun90'(Xequality840), XXV2052, XEls, XXe2033),XXV2053,XXV2053).
'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce", 31, 5, 53)).
'lo.core$additive$lo.sets*set'('lo.core$additive$lo.sets*set%1'('lo.core$additive$lo.sets*set')):- !.
'lo.core$additive$lo.sets*set'('+%3'(XV19773, XV19774, XV19775), XLbl4106, XThis4106):- !,
    'lo.core$additive$lo.sets*set@+'(XV19773, XV19774, XV19775, XLbl4106, XThis4106).
'lo.core$additive$lo.sets*set'('+%1'('lo.core$additive$lo.sets*set^+'(XLbl4107, XThis4107)), XLbl4107, XThis4107).
'lo.core$additive$lo.sets*set'('-%3'(XV19779, XV19780, XV19781), XLbl4108, XThis4108):- !,
    'lo.core$additive$lo.sets*set@-'(XV19779, XV19780, XV19781, XLbl4108, XThis4108).
'lo.core$additive$lo.sets*set'('-%1'('lo.core$additive$lo.sets*set^-'(XLbl4109, XThis4109)), XLbl4109, XThis4109).
'lo.core$additive$lo.sets*set@+'(XS1, XS2, XXd9531, XLbV1786, XThV1786):- XLbV1786 = 'lo.core$additive$lo.sets*set'(Xequality841),
    !,
    'lo.sets@setUnion'(Xequality841, XS1, XS2, XXd9531).
'lo.core$additive$lo.sets*set@+'(_, _, _):- raise_exception('error'("lo.core$additive$lo.sets*set@+", 35, 5, 24)).
'lo.core$additive$lo.sets*set@-'(XS1, XS2, XXd9532, XLbV1786, XThV1786):- XLbV1786 = 'lo.core$additive$lo.sets*set'(Xequality841),
    !,
    'lo.sets@setDifference'(Xequality841, XS1, XS2, XXd9532).
'lo.core$additive$lo.sets*set@-'(_, _, _):- raise_exception('error'("lo.core$additive$lo.sets*set@-", 36, 5, 29)).
'lo.sets@setMap'(Xequality842, Xequality843, 'lo.sets#set'(XEls), XF, 'lo.sets#set'(XXe2037)):- !,
    ocall('keys%1'(XXV2056),'lo.collection$map$lo.index*map'(Xequality842),'lo.collection$map$lo.index*map'(Xequality842)),
    ocall('foldLeft%1'(XXV2057),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_empty%1'(XXV2055),'lo.collection$map$lo.index*map'(Xequality843),'lo.collection$map$lo.index*map'(Xequality843)),
    ocall('_call%2'(XEls, XXe2036),XXV2056,XXV2056),
    ocall('_call%4'('lo.sets@fun91'(Xequality843, XF), XXV2055, XXe2036, XXe2037),XXV2057,XXV2057).
'lo.sets@setMap'(_, _, _, _, _):- raise_exception('error'("lo.sets@setMap", 57, 3, 68)).
'lo.collection$mapping$lo.sets*set'('lo.collection$mapping$lo.sets*set%1'('lo.collection$mapping$lo.sets*set')):- !.
'lo.collection$mapping$lo.sets*set'('//%3'(XV19790, XV19791, XV19792), XLbl4110, XThis4110):- !,
    'lo.collection$mapping$lo.sets*set@//'(XV19790, XV19791, XV19792, XLbl4110, XThis4110).
'lo.collection$mapping$lo.sets*set'('//%1'('lo.collection$mapping$lo.sets*set^//'(XLbl4111, XThis4111)), XLbl4111, XThis4111).
'lo.collection$mapping$lo.sets*set@//'(XS, XF, XXd9538, XLbV1787, XThV1787):- XLbV1787 = 'lo.collection$mapping$lo.sets*set'(Xequality844, Xequality845),
    !,
    'lo.sets@setMap'(Xequality845, Xequality844, XS, XF, XXd9538).
'lo.collection$mapping$lo.sets*set@//'(_, _, _):- raise_exception('error'("lo.collection$mapping$lo.sets*set@//", 53, 5, 19)).
'lo.sets@foldLeftEls'(XF, XX, XS, XXe2038):- !,
    ocall('foldLeft%1'(XXV2058),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'(XF, XX, XS, XXe2038),XXV2058,XXV2058).
'lo.sets@foldLeftEls'(_, _, _, _):- raise_exception('error'("lo.sets@foldLeftEls", 65, 3, 37)).
'lo.sets@foldRightEls'(XF, XX, XS, XXe2039):- !,
    ocall('foldRight%1'(XXV2059),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'(XF, XX, XS, XXe2039),XXV2059,XXV2059).
'lo.sets@foldRightEls'(_, _, _, _):- raise_exception('error'("lo.sets@foldRightEls", 68, 3, 39)).
'lo.collection$folding$lo.sets*set'('lo.collection$folding$lo.sets*set%1'('lo.collection$folding$lo.sets*set')):- !.
'lo.collection$folding$lo.sets*set'('foldRight%4'(XV19805, XV19806, XV19807, XV19808), XLbl4112, XThis4112):- !,
    'lo.collection$folding$lo.sets*set@foldRight'(XV19805, XV19806, XV19807, XV19808, XLbl4112, XThis4112).
'lo.collection$folding$lo.sets*set'('foldRight%1'('lo.collection$folding$lo.sets*set^foldRight'(XLbl4113, XThis4113)), XLbl4113, XThis4113).
'lo.collection$folding$lo.sets*set'('foldLeft%4'(XV19813, XV19814, XV19815, XV19816), XLbl4114, XThis4114):- !,
    'lo.collection$folding$lo.sets*set@foldLeft'(XV19813, XV19814, XV19815, XV19816, XLbl4114, XThis4114).
'lo.collection$folding$lo.sets*set'('foldLeft%1'('lo.collection$folding$lo.sets*set^foldLeft'(XLbl4115, XThis4115)), XLbl4115, XThis4115).
'lo.collection$folding$lo.sets*set@foldRight'(XF, XX, XS, XXd9540, XLbV1788, XThV1788):- XLbV1788 = 'lo.collection$folding$lo.sets*set'(Xequality846),
    !,
    ocall('_coerce%1'(XXV2060),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality846),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality846)),
    ocall('_call%2'(XS, XXe2040),XXV2060,XXV2060),
    'lo.sets@foldRightEls'(XF, XX, XXe2040, XXd9540).
'lo.collection$folding$lo.sets*set@foldRight'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.sets*set@foldRight", 60, 5, 48)).
'lo.collection$folding$lo.sets*set@foldLeft'(XF, XX, XS, XXd9542, XLbV1788, XThV1788):- XLbV1788 = 'lo.collection$folding$lo.sets*set'(Xequality846),
    !,
    ocall('_coerce%1'(XXV2061),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality846),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality846)),
    ocall('_call%2'(XS, XXe2041),XXV2061,XXV2061),
    'lo.sets@foldLeftEls'(XF, XX, XXe2041, XXd9542).
'lo.collection$folding$lo.sets*set@foldLeft'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.sets*set@foldLeft", 61, 5, 46)).
'lo.sets@dispEls'(Xdisplay265, 'lo.core#[]', X_5890, 'lo.core#[]'):- !.
'lo.sets@dispEls'(Xdisplay265, 'lo.core#,..'(XE, XL), XSep, 'lo.core#,..'('lo.core#ss'(XSep), 'lo.core#,..'(XXe2042, XXd9544))):- !,
    ocall('disp%1'(XXV2062),Xdisplay265,Xdisplay265),
    ocall('_call%2'(XE, XXe2042),XXV2062,XXV2062),
    'lo.sets@dispEls'(Xdisplay265, XL, ", ", XXd9544).
'lo.sets@dispEls'(_, _, _, _):- raise_exception('error'("lo.sets@dispEls", 75, 3, 19)).
'lo.core$display$lo.sets*set'('lo.core$display$lo.sets*set%1'('lo.core$display$lo.sets*set')):- !.
'lo.core$display$lo.sets*set'('disp%2'(XV19823, XV19824), XLbl4116, XThis4116):- !,
    'lo.core$display$lo.sets*set@disp'(XV19823, XV19824, XLbl4116, XThis4116).
'lo.core$display$lo.sets*set'('disp%1'('lo.core$display$lo.sets*set^disp'(XLbl4117, XThis4117)), XLbl4117, XThis4117).
'lo.core$display$lo.sets*set@disp'('lo.sets#set'(XEls), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("{"), 'lo.core#,..'('lo.core#ssSeq'(XXd9549), 'lo.core#,..'('lo.core#ss'("}"), 'lo.core#[]')))), XLbV1789, XThV1789):- XLbV1789 = 'lo.core$display$lo.sets*set'(Xequality847, Xdisplay266),
    !,
    ocall('keys%1'(XXV2063),'lo.collection$map$lo.index*map'(Xequality847),'lo.collection$map$lo.index*map'(Xequality847)),
    ocall('_call%2'(XEls, XXe2043),XXV2063,XXV2063),
    'lo.sets@dispEls'(Xdisplay266, XXe2043, "", XXd9549).
'lo.core$display$lo.sets*set@disp'(_, _):- raise_exception('error'("lo.core$display$lo.sets*set@disp", 71, 5, 81)).
'lo.sets@nullSet25'('_call%2'(Xequality832, 'lo.sets#set'(XXV2043)), 'lo.sets@nullSet25', _):- !,
    ocall('_empty%1'(XXV2043),'lo.collection$map$lo.index*map'(Xequality832),'lo.collection$map$lo.index*map'(Xequality832)).
'lo.sets@nullSet25'(_, _, _):- raise_exception('error'("lo.sets@nullSet25", 11, 3, 17)).
'lo.collection$membership$lo.sets*set^addMem'('_call%3'(XV19709, XV19710, XV19711), 'lo.collection$membership$lo.sets*set^addMem'(XLbV1782, XThV1782), _):- 'lo.collection$membership$lo.sets*set@addMem'(XV19709, XV19710, XV19711, XLbV1782, XThV1782).
'lo.collection$membership$lo.sets*set^delMem'('_call%3'(XV19715, XV19716, XV19717), 'lo.collection$membership$lo.sets*set^delMem'(XLbV1782, XThV1782), _):- 'lo.collection$membership$lo.sets*set@delMem'(XV19715, XV19716, XV19717, XLbV1782, XThV1782).
'lo.collection$membership$lo.sets*set@cond99'(XXd9514, X_5886, XXe2027, XXV2046, XXd9513, Xequality833, XLbV1782, XThV1782, XM1, Xk):- 'ground'(Xk),
    !,
    ocall('keys%1'(XXV2046),'lo.collection$map$lo.index*map'(Xequality833),'lo.collection$map$lo.index*map'(Xequality833)),
    ocall('_call%2'(XM1, XXe2027),XXV2046,XXV2046),
    'lo.list@listEl'(Xk, XXe2027).
'lo.collection$membership$lo.sets*set@cond99'(XXd9514, X_5886, XXe2027, XXV2046, XXd9513, Xequality833, XLbV1782, XThV1782, XM1, Xk):- ocall('present%3'(XM1, Xk, X_5886),'lo.collection$map$lo.index*map'(Xequality833),'lo.collection$map$lo.index*map'(Xequality833)).
'lo.collection$membership$lo.sets*set^in'('_call%4'(XV19721, XV19722, XV19723, XV19724), 'lo.collection$membership$lo.sets*set^in'(XLbV1782, XThV1782), _):- 'lo.collection$membership$lo.sets*set@in'(XV19721, XV19722, XV19723, XV19724, XLbV1782, XThV1782).
'lo.sets^setDifference'('_call%4'(XV19727, XV19728, XV19729, XV19730), 'lo.sets^setDifference', _):- 'lo.sets@setDifference'(XV19727, XV19728, XV19729, XV19730).
'lo.sets^binApp'('_call%5'(XV19731, XV19732, XV19733, XV19734, XV19735), 'lo.sets^binApp', _):- 'lo.sets@binApp'(XV19731, XV19732, XV19733, XV19734, XV19735).
'lo.sets^setIntersection'('_call%4'(XV19736, XV19737, XV19738, XV19739), 'lo.sets^setIntersection', _):- 'lo.sets@setIntersection'(XV19736, XV19737, XV19738, XV19739).
'lo.sets^setUnion'('_call%4'(XV19740, XV19741, XV19742, XV19743), 'lo.sets^setUnion', _):- 'lo.sets@setUnion'(XV19740, XV19741, XV19742, XV19743).
'lo.collection$setops$lo.sets*set^union'('_call%3'(XV19744, XV19745, XV19746), 'lo.collection$setops$lo.sets*set^union'(XLbV1783, XThV1783), _):- 'lo.collection$setops$lo.sets*set@union'(XV19744, XV19745, XV19746, XLbV1783, XThV1783).
'lo.collection$setops$lo.sets*set^intersect'('_call%3'(XV19750, XV19751, XV19752), 'lo.collection$setops$lo.sets*set^intersect'(XLbV1783, XThV1783), _):- 'lo.collection$setops$lo.sets*set@intersect'(XV19750, XV19751, XV19752, XLbV1783, XThV1783).
'lo.collection$setops$lo.sets*set^difference'('_call%3'(XV19756, XV19757, XV19758), 'lo.collection$setops$lo.sets*set^difference'(XLbV1783, XThV1783), _):- 'lo.collection$setops$lo.sets*set@difference'(XV19756, XV19757, XV19758, XLbV1783, XThV1783).
'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'('_call%2'(XV19762, XV19763), 'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'(XLbV1784, XThV1784), _):- 'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(XV19762, XV19763, XLbV1784, XThV1784).
'lo.coerce$coercion$lo.core*list$lo.sets*set@fun90'('_call%3'(XM, XE, XXe2032), 'lo.coerce$coercion$lo.core*list$lo.sets*set@fun90'(Xequality840), _):- !,
    ocall('_put%1'(XXV2051),'lo.collection$map$lo.index*map'(Xequality840),'lo.collection$map$lo.index*map'(Xequality840)),
    ocall('_call%4'(XM, XE, '()0'(), XXe2032),XXV2051,XXV2051).
'lo.coerce$coercion$lo.core*list$lo.sets*set@fun90'(_, _, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.sets*set@fun90", 31, 34, 15)).
'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'('_call%2'(XV19766, XV19767), 'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'(XLbV1785, XThV1785), _):- 'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XV19766, XV19767, XLbV1785, XThV1785).
'lo.core$additive$lo.sets*set^+'('_call%3'(XV19770, XV19771, XV19772), 'lo.core$additive$lo.sets*set^+'(XLbV1786, XThV1786), _):- 'lo.core$additive$lo.sets*set@+'(XV19770, XV19771, XV19772, XLbV1786, XThV1786).
'lo.core$additive$lo.sets*set^-'('_call%3'(XV19776, XV19777, XV19778), 'lo.core$additive$lo.sets*set^-'(XLbV1786, XThV1786), _):- 'lo.core$additive$lo.sets*set@-'(XV19776, XV19777, XV19778, XLbV1786, XThV1786).
'lo.sets@fun91'('_call%3'(XM, Xk, XXe2035), 'lo.sets@fun91'(Xequality843, XF), _):- !,
    ocall('_put%1'(XXV2054),'lo.collection$map$lo.index*map'(Xequality843),'lo.collection$map$lo.index*map'(Xequality843)),
    ocall('_call%2'(Xk, XXe2034),XF,XF),
    ocall('_call%4'(XM, XXe2034, '()0'(), XXe2035),XXV2054,XXV2054).
'lo.sets@fun91'(_, _, _):- raise_exception('error'("lo.sets@fun91", 57, 38, 18)).
'lo.sets^setMap'('_call%5'(XV19782, XV19783, XV19784, XV19785, XV19786), 'lo.sets^setMap', _):- 'lo.sets@setMap'(XV19782, XV19783, XV19784, XV19785, XV19786).
'lo.collection$mapping$lo.sets*set^//'('_call%3'(XV19787, XV19788, XV19789), 'lo.collection$mapping$lo.sets*set^//'(XLbV1787, XThV1787), _):- 'lo.collection$mapping$lo.sets*set@//'(XV19787, XV19788, XV19789, XLbV1787, XThV1787).
'lo.sets^foldLeftEls'('_call%4'(XV19793, XV19794, XV19795, XV19796), 'lo.sets^foldLeftEls', _):- 'lo.sets@foldLeftEls'(XV19793, XV19794, XV19795, XV19796).
'lo.sets^foldRightEls'('_call%4'(XV19797, XV19798, XV19799, XV19800), 'lo.sets^foldRightEls', _):- 'lo.sets@foldRightEls'(XV19797, XV19798, XV19799, XV19800).
'lo.collection$folding$lo.sets*set^foldRight'('_call%4'(XV19801, XV19802, XV19803, XV19804), 'lo.collection$folding$lo.sets*set^foldRight'(XLbV1788, XThV1788), _):- 'lo.collection$folding$lo.sets*set@foldRight'(XV19801, XV19802, XV19803, XV19804, XLbV1788, XThV1788).
'lo.collection$folding$lo.sets*set^foldLeft'('_call%4'(XV19809, XV19810, XV19811, XV19812), 'lo.collection$folding$lo.sets*set^foldLeft'(XLbV1788, XThV1788), _):- 'lo.collection$folding$lo.sets*set@foldLeft'(XV19809, XV19810, XV19811, XV19812, XLbV1788, XThV1788).
'lo.sets^dispEls'('_call%4'(XV19817, XV19818, XV19819, XV19820), 'lo.sets^dispEls', _):- 'lo.sets@dispEls'(XV19817, XV19818, XV19819, XV19820).
'lo.core$display$lo.sets*set^disp'('_call%2'(XV19821, XV19822), 'lo.core$display$lo.sets*set^disp'(XLbV1789, XThV1789), _):- 'lo.core$display$lo.sets*set@disp'(XV19821, XV19822, XLbV1789, XThV1789).
