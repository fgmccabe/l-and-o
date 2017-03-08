'#pkg'("n7o7'()7'n2o2'pkg's'lo.sets's'1.0.0'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.index'e'*'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'s\"I3'set':k'e'CT1Uz2'lo.index*map'2k'e'T0Uz1'lo.sets*set'1k'e''nullSet':k'e'|Uz1'lo.sets*set'1k'e'c'lo.core$equality'T1k'e'T0'setMap':k'e':k'f'||FT2Uz1'lo.sets*set'1k'e'FT1k'e'k'f'Uz1'lo.sets*set'1k'f'c'lo.core$equality'T1k'f'T0c'lo.core$equality'T1k'e'T0\"s\"I1'set':k'e':k'e'YUz1'lo.sets*set'1k'e'I0\"n1o1'()1's'set'n0o0'()0'n8o8'()8'n2o2'()2's'lo.collection$membership$lo.sets*set's\":k'e'|c'lo.collection$membership'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.collection$setops$lo.sets*set's\":k'e'|c'lo.collection$setops'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.coerce$coercion$lo.sets*set$lo.core*list's\":k'e'|c'lo.coerce$coercion'T2Uz1'lo.sets*set'1k'e'Lk'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.sets*set's\":k'e'|c'lo.coerce$coercion'T2Lk'e'Uz1'lo.sets*set'1k'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.core$additive$lo.sets*set's\":k'e'|c'lo.core$additive'T1Uz1'lo.sets*set'1k'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.collection$mapping$lo.sets*set's\":k'e':k'f'||c'lo.collection$mapping'T1z1'lo.sets*set'T2k'e'k'f'c'lo.core$equality'T1k'e'T0c'lo.core$equality'T1k'f'T0\"n2o2'()2's'lo.collection$folding$lo.sets*set's\":k'e'|c'lo.collection$folding'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.core$display$lo.sets*set's\":k'e'||c'lo.core$display'T1Uz1'lo.sets*set'1k'e'T0c'lo.core$display'T1k'e'T0c'lo.core$equality'T1k'e'T0\"").
'lo.sets@init'():- !.
'lo.sets#set'('set%1'('lo.sets@set'())):- !.
'lo.sets@nullSet'('lo.sets@nullSet23'):- !.
'lo.collection$membership$lo.sets*set'('lo.collection$membership$lo.sets*set%1'('lo.collection$membership$lo.sets*set')):- !.
'lo.collection$membership$lo.sets*set'('empty%1'(XV19313), XLbl1649, XThis1649):- !,
    'lo.collection$membership$lo.sets*set@empty'(XV19313, XLbl1649, XThis1649).
'lo.collection$membership$lo.sets*set'('addMem%3'(XV19317, XV19318, XV19319), XLbl1650, XThis1650):- !,
    'lo.collection$membership$lo.sets*set@addMem'(XV19317, XV19318, XV19319, XLbl1650, XThis1650).
'lo.collection$membership$lo.sets*set'('addMem%1'('lo.collection$membership$lo.sets*set^addMem'(XLbl1651, XThis1651)), XLbl1651, XThis1651).
'lo.collection$membership$lo.sets*set'('delMem%3'(XV19323, XV19324, XV19325), XLbl1652, XThis1652):- !,
    'lo.collection$membership$lo.sets*set@delMem'(XV19323, XV19324, XV19325, XLbl1652, XThis1652).
'lo.collection$membership$lo.sets*set'('delMem%1'('lo.collection$membership$lo.sets*set^delMem'(XLbl1653, XThis1653)), XLbl1653, XThis1653).
'lo.collection$membership$lo.sets*set'('in%2'(XV19330, XV19331), XLbl1654, XThis1654):- !,
    'lo.collection$membership$lo.sets*set@in'(XV19330, XV19331, XLbl1654, XThis1654).
'lo.collection$membership$lo.sets*set'('in%1'('lo.collection$membership$lo.sets*set^in'(XLbl1655, XThis1655)), XLbl1655, XThis1655).
'lo.collection$membership$lo.sets*set@empty'(XXe2777, XLbV1531, XThV1531):- XLbV1531 = 'lo.collection$membership$lo.sets*set'(Xequality250),
    !,
    'lo.sets@nullSet'(XnullSet24),
    ocall('_call%2'(Xequality250, XXe2777),XnullSet24,XnullSet24).
'lo.collection$membership$lo.sets*set@addMem'('lo.sets#set'(XS), XE, 'lo.sets#set'(XXe2778), XLbV1531, XThV1531):- XLbV1531 = 'lo.collection$membership$lo.sets*set'(Xequality250),
    !,
    ocall('_put%1'(XXV2983),'lo.collection$map$lo.index*map'(Xequality250),'lo.collection$map$lo.index*map'(Xequality250)),
    ocall('_call%4'(XS, XE, '()0'(), XXe2778),XXV2983,XXV2983).
'lo.collection$membership$lo.sets*set@addMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.sets*set@addMem", 15, 5, 33)).
'lo.collection$membership$lo.sets*set@delMem'('lo.sets#set'(XS), XE, 'lo.sets#set'(XXe2779), XLbV1531, XThV1531):- XLbV1531 = 'lo.collection$membership$lo.sets*set'(Xequality250),
    !,
    ocall('_remove%1'(XXV2984),'lo.collection$map$lo.index*map'(Xequality250),'lo.collection$map$lo.index*map'(Xequality250)),
    ocall('_call%3'(XS, XE, XXe2779),XXV2984,XXV2984).
'lo.collection$membership$lo.sets*set@delMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.sets*set@delMem", 16, 5, 31)).
'lo.collection$membership$lo.sets*set@in'(Xk, 'lo.sets#set'(XM1), XLbV1531, XThV1531):- XLbV1531 = 'lo.collection$membership$lo.sets*set'(Xequality250),
    'lo.collection$membership$lo.sets*set@cond228'(XXd23091, X_19718, XXe2780, XXV2985, XXd23090, Xequality250, XLbV1531, XThV1531, XM1, Xk).
'lo.sets@setDifference'(Xequality251, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XXe2781)):- !,
    ocall('-%1'(XXV2986),'lo.core$additive$lo.index*map'(Xequality251),'lo.core$additive$lo.index*map'(Xequality251)),
    ocall('_call%3'(XM1, XM2, XXe2781),XXV2986,XXV2986).
'lo.sets@setDifference'(_, _, _, _):- raise_exception('error'("lo.sets@setDifference", 43, 3, 44)).
'lo.sets@binApp'(Xequality252, Xk, X_19719, XM, XM):- ocall('present%3'(XM, Xk, X_19720),'lo.collection$map$lo.index*map'(Xequality252),'lo.collection$map$lo.index*map'(Xequality252)),
    !.
'lo.sets@binApp'(Xequality252, Xk, X_19721, XM, XXe2782):- !,
    ocall('_put%1'(XXV2987),'lo.collection$map$lo.index*map'(Xequality252),'lo.collection$map$lo.index*map'(Xequality252)),
    ocall('_call%4'(XM, Xk, '()0'(), XXe2782),XXV2987,XXV2987).
'lo.sets@binApp'(_, _, _, _, _):- raise_exception('error'("lo.sets@binApp", 49, 3, 36)).
'lo.sets@setIntersection'(Xequality253, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XXd23097)):- !,
    'lo.sets@binApp'(Xequality253, XXd23096),
    'lo.index@foldMap'(XXd23096, XM2, XM1, XXd23097).
'lo.sets@setIntersection'(_, _, _, _):- raise_exception('error'("lo.sets@setIntersection", 46, 3, 63)).
'lo.sets@setUnion'(Xequality254, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XXe2783)):- !,
    ocall('+%1'(XXV2988),'lo.core$additive$lo.index*map'(Xequality254),'lo.core$additive$lo.index*map'(Xequality254)),
    ocall('_call%3'(XM1, XM2, XXe2783),XXV2988,XXV2988).
'lo.sets@setUnion'(_, _, _, _):- raise_exception('error'("lo.sets@setUnion", 40, 3, 39)).
'lo.collection$setops$lo.sets*set'('lo.collection$setops$lo.sets*set%1'('lo.collection$setops$lo.sets*set')):- !.
'lo.collection$setops$lo.sets*set'('union%3'(XV19352, XV19353, XV19354), XLbl1656, XThis1656):- !,
    'lo.collection$setops$lo.sets*set@union'(XV19352, XV19353, XV19354, XLbl1656, XThis1656).
'lo.collection$setops$lo.sets*set'('union%1'('lo.collection$setops$lo.sets*set^union'(XLbl1657, XThis1657)), XLbl1657, XThis1657).
'lo.collection$setops$lo.sets*set'('intersect%3'(XV19358, XV19359, XV19360), XLbl1658, XThis1658):- !,
    'lo.collection$setops$lo.sets*set@intersect'(XV19358, XV19359, XV19360, XLbl1658, XThis1658).
'lo.collection$setops$lo.sets*set'('intersect%1'('lo.collection$setops$lo.sets*set^intersect'(XLbl1659, XThis1659)), XLbl1659, XThis1659).
'lo.collection$setops$lo.sets*set'('difference%3'(XV19364, XV19365, XV19366), XLbl1660, XThis1660):- !,
    'lo.collection$setops$lo.sets*set@difference'(XV19364, XV19365, XV19366, XLbl1660, XThis1660).
'lo.collection$setops$lo.sets*set'('difference%1'('lo.collection$setops$lo.sets*set^difference'(XLbl1661, XThis1661)), XLbl1661, XThis1661).
'lo.collection$setops$lo.sets*set@union'(XS1, XS2, XXd23101, XLbV1532, XThV1532):- XLbV1532 = 'lo.collection$setops$lo.sets*set'(Xequality255),
    !,
    'lo.sets@setUnion'(Xequality255, XS1, XS2, XXd23101).
'lo.collection$setops$lo.sets*set@union'(_, _, _):- raise_exception('error'("lo.collection$setops$lo.sets*set@union", 21, 5, 31)).
'lo.collection$setops$lo.sets*set@intersect'(XS1, XS2, XXd23102, XLbV1532, XThV1532):- XLbV1532 = 'lo.collection$setops$lo.sets*set'(Xequality255),
    !,
    'lo.sets@setIntersection'(Xequality255, XS1, XS2, XXd23102).
'lo.collection$setops$lo.sets*set@intersect'(_, _, _):- raise_exception('error'("lo.collection$setops$lo.sets*set@intersect", 22, 5, 42)).
'lo.collection$setops$lo.sets*set@difference'(XS1, XS2, XXd23103, XLbV1532, XThV1532):- XLbV1532 = 'lo.collection$setops$lo.sets*set'(Xequality255),
    !,
    'lo.sets@setDifference'(Xequality255, XS1, XS2, XXd23103).
'lo.collection$setops$lo.sets*set@difference'(_, _, _):- raise_exception('error'("lo.collection$setops$lo.sets*set@difference", 23, 5, 41)).
'lo.coerce$coercion$lo.sets*set$lo.core*list'('lo.coerce$coercion$lo.sets*set$lo.core*list%1'('lo.coerce$coercion$lo.sets*set$lo.core*list')):- !.
'lo.coerce$coercion$lo.sets*set$lo.core*list'('_coerce%2'(XV19369, XV19370), XLbl1662, XThis1662):- !,
    'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(XV19369, XV19370, XLbl1662, XThis1662).
'lo.coerce$coercion$lo.sets*set$lo.core*list'('_coerce%1'('lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'(XLbl1663, XThis1663)), XLbl1663, XThis1663).
'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'('lo.sets#set'(XEls), XXe2784, XLbV1533, XThV1533):- XLbV1533 = 'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality256),
    !,
    ocall('keys%1'(XXV2989),'lo.collection$map$lo.index*map'(Xequality256),'lo.collection$map$lo.index*map'(Xequality256)),
    ocall('_call%2'(XEls, XXe2784),XXV2989,XXV2989).
'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce", 27, 5, 30)).
'lo.coerce$coercion$lo.core*list$lo.sets*set'('lo.coerce$coercion$lo.core*list$lo.sets*set%1'('lo.coerce$coercion$lo.core*list$lo.sets*set')):- !.
'lo.coerce$coercion$lo.core*list$lo.sets*set'('_coerce%2'(XV19373, XV19374), XLbl1664, XThis1664):- !,
    'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XV19373, XV19374, XLbl1664, XThis1664).
'lo.coerce$coercion$lo.core*list$lo.sets*set'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'(XLbl1665, XThis1665)), XLbl1665, XThis1665).
'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XEls, 'lo.sets#set'(XXe2786), XLbV1534, XThV1534):- XLbV1534 = 'lo.coerce$coercion$lo.core*list$lo.sets*set'(Xequality257),
    !,
    ocall('foldLeft%1'(XXV2992),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_empty%1'(XXV2991),'lo.collection$map$lo.index*map'(Xequality257),'lo.collection$map$lo.index*map'(Xequality257)),
    ocall('_call%4'('lo.coerce$coercion$lo.core*list$lo.sets*set@fun60'(Xequality257), XXV2991, XEls, XXe2786),XXV2992,XXV2992).
'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce", 31, 5, 53)).
'lo.core$additive$lo.sets*set'('lo.core$additive$lo.sets*set%1'('lo.core$additive$lo.sets*set')):- !.
'lo.core$additive$lo.sets*set'('+%3'(XV19378, XV19379, XV19380), XLbl1666, XThis1666):- !,
    'lo.core$additive$lo.sets*set@+'(XV19378, XV19379, XV19380, XLbl1666, XThis1666).
'lo.core$additive$lo.sets*set'('+%1'('lo.core$additive$lo.sets*set^+'(XLbl1667, XThis1667)), XLbl1667, XThis1667).
'lo.core$additive$lo.sets*set'('-%3'(XV19384, XV19385, XV19386), XLbl1668, XThis1668):- !,
    'lo.core$additive$lo.sets*set@-'(XV19384, XV19385, XV19386, XLbl1668, XThis1668).
'lo.core$additive$lo.sets*set'('-%1'('lo.core$additive$lo.sets*set^-'(XLbl1669, XThis1669)), XLbl1669, XThis1669).
'lo.core$additive$lo.sets*set@+'(XS1, XS2, XXd23108, XLbV1535, XThV1535):- XLbV1535 = 'lo.core$additive$lo.sets*set'(Xequality258),
    !,
    'lo.sets@setUnion'(Xequality258, XS1, XS2, XXd23108).
'lo.core$additive$lo.sets*set@+'(_, _, _):- raise_exception('error'("lo.core$additive$lo.sets*set@+", 35, 5, 24)).
'lo.core$additive$lo.sets*set@-'(XS1, XS2, XXd23109, XLbV1535, XThV1535):- XLbV1535 = 'lo.core$additive$lo.sets*set'(Xequality258),
    !,
    'lo.sets@setDifference'(Xequality258, XS1, XS2, XXd23109).
'lo.core$additive$lo.sets*set@-'(_, _, _):- raise_exception('error'("lo.core$additive$lo.sets*set@-", 36, 5, 29)).
'lo.sets@setMap'(Xequality259, Xequality260, 'lo.sets#set'(XEls), XF, 'lo.sets#set'(XXe2790)):- !,
    ocall('keys%1'(XXV2995),'lo.collection$map$lo.index*map'(Xequality259),'lo.collection$map$lo.index*map'(Xequality259)),
    ocall('foldLeft%1'(XXV2996),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_empty%1'(XXV2994),'lo.collection$map$lo.index*map'(Xequality260),'lo.collection$map$lo.index*map'(Xequality260)),
    ocall('_call%2'(XEls, XXe2789),XXV2995,XXV2995),
    ocall('_call%4'('lo.sets@fun61'(Xequality260, XF), XXV2994, XXe2789, XXe2790),XXV2996,XXV2996).
'lo.sets@setMap'(_, _, _, _, _):- raise_exception('error'("lo.sets@setMap", 57, 3, 68)).
'lo.collection$mapping$lo.sets*set'('lo.collection$mapping$lo.sets*set%1'('lo.collection$mapping$lo.sets*set')):- !.
'lo.collection$mapping$lo.sets*set'('//%3'(XV19395, XV19396, XV19397), XLbl1670, XThis1670):- !,
    'lo.collection$mapping$lo.sets*set@//'(XV19395, XV19396, XV19397, XLbl1670, XThis1670).
'lo.collection$mapping$lo.sets*set'('//%1'('lo.collection$mapping$lo.sets*set^//'(XLbl1671, XThis1671)), XLbl1671, XThis1671).
'lo.collection$mapping$lo.sets*set@//'(XS, XF, XXd23115, XLbV1536, XThV1536):- XLbV1536 = 'lo.collection$mapping$lo.sets*set'(Xequality261, Xequality262),
    !,
    'lo.sets@setMap'(Xequality262, Xequality261, XS, XF, XXd23115).
'lo.collection$mapping$lo.sets*set@//'(_, _, _):- raise_exception('error'("lo.collection$mapping$lo.sets*set@//", 53, 5, 19)).
'lo.sets@foldLeftEls'(XF, XX, XS, XXe2791):- !,
    ocall('foldLeft%1'(XXV2997),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'(XF, XX, XS, XXe2791),XXV2997,XXV2997).
'lo.sets@foldLeftEls'(_, _, _, _):- raise_exception('error'("lo.sets@foldLeftEls", 65, 3, 37)).
'lo.sets@foldRightEls'(XF, XX, XS, XXe2792):- !,
    ocall('foldRight%1'(XXV2998),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'(XF, XX, XS, XXe2792),XXV2998,XXV2998).
'lo.sets@foldRightEls'(_, _, _, _):- raise_exception('error'("lo.sets@foldRightEls", 68, 3, 39)).
'lo.collection$folding$lo.sets*set'('lo.collection$folding$lo.sets*set%1'('lo.collection$folding$lo.sets*set')):- !.
'lo.collection$folding$lo.sets*set'('foldRight%4'(XV19410, XV19411, XV19412, XV19413), XLbl1672, XThis1672):- !,
    'lo.collection$folding$lo.sets*set@foldRight'(XV19410, XV19411, XV19412, XV19413, XLbl1672, XThis1672).
'lo.collection$folding$lo.sets*set'('foldRight%1'('lo.collection$folding$lo.sets*set^foldRight'(XLbl1673, XThis1673)), XLbl1673, XThis1673).
'lo.collection$folding$lo.sets*set'('foldLeft%4'(XV19418, XV19419, XV19420, XV19421), XLbl1674, XThis1674):- !,
    'lo.collection$folding$lo.sets*set@foldLeft'(XV19418, XV19419, XV19420, XV19421, XLbl1674, XThis1674).
'lo.collection$folding$lo.sets*set'('foldLeft%1'('lo.collection$folding$lo.sets*set^foldLeft'(XLbl1675, XThis1675)), XLbl1675, XThis1675).
'lo.collection$folding$lo.sets*set@foldRight'(XF, XX, XS, XXd23117, XLbV1537, XThV1537):- XLbV1537 = 'lo.collection$folding$lo.sets*set'(Xequality263),
    !,
    ocall('_coerce%1'(XXV2999),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality263),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality263)),
    ocall('_call%2'(XS, XXe2793),XXV2999,XXV2999),
    'lo.sets@foldRightEls'(XF, XX, XXe2793, XXd23117).
'lo.collection$folding$lo.sets*set@foldRight'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.sets*set@foldRight", 60, 5, 48)).
'lo.collection$folding$lo.sets*set@foldLeft'(XF, XX, XS, XXd23119, XLbV1537, XThV1537):- XLbV1537 = 'lo.collection$folding$lo.sets*set'(Xequality263),
    !,
    ocall('_coerce%1'(XXV3000),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality263),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xequality263)),
    ocall('_call%2'(XS, XXe2794),XXV3000,XXV3000),
    'lo.sets@foldLeftEls'(XF, XX, XXe2794, XXd23119).
'lo.collection$folding$lo.sets*set@foldLeft'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.sets*set@foldLeft", 61, 5, 46)).
'lo.sets@dispEls'(Xdisplay96, 'lo.core#[]', X_19722, 'lo.core#[]'):- !.
'lo.sets@dispEls'(Xdisplay96, 'lo.core#,..'(XE, XL), XSep, 'lo.core#,..'('lo.core#ss'(XSep), 'lo.core#,..'(XXe2795, XXd23121))):- !,
    ocall('disp%1'(XXV3001),Xdisplay96,Xdisplay96),
    ocall('_call%2'(XE, XXe2795),XXV3001,XXV3001),
    'lo.sets@dispEls'(Xdisplay96, XL, ", ", XXd23121).
'lo.sets@dispEls'(_, _, _, _):- raise_exception('error'("lo.sets@dispEls", 75, 3, 19)).
'lo.core$display$lo.sets*set'('lo.core$display$lo.sets*set%1'('lo.core$display$lo.sets*set')):- !.
'lo.core$display$lo.sets*set'('disp%2'(XV19428, XV19429), XLbl1676, XThis1676):- !,
    'lo.core$display$lo.sets*set@disp'(XV19428, XV19429, XLbl1676, XThis1676).
'lo.core$display$lo.sets*set'('disp%1'('lo.core$display$lo.sets*set^disp'(XLbl1677, XThis1677)), XLbl1677, XThis1677).
'lo.core$display$lo.sets*set@disp'('lo.sets#set'(XEls), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("{"), 'lo.core#,..'('lo.core#ssSeq'(XXd23126), 'lo.core#,..'('lo.core#ss'("}"), 'lo.core#[]')))), XLbV1538, XThV1538):- XLbV1538 = 'lo.core$display$lo.sets*set'(Xequality264, Xdisplay97),
    !,
    ocall('keys%1'(XXV3002),'lo.collection$map$lo.index*map'(Xequality264),'lo.collection$map$lo.index*map'(Xequality264)),
    ocall('_call%2'(XEls, XXe2796),XXV3002,XXV3002),
    'lo.sets@dispEls'(Xdisplay97, XXe2796, "", XXd23126).
'lo.core$display$lo.sets*set@disp'(_, _):- raise_exception('error'("lo.core$display$lo.sets*set@disp", 71, 5, 81)).
'lo.sets@nullSet23'('_call%2'(Xequality249, 'lo.sets#set'(XXV2982)), 'lo.sets@nullSet23', _):- !,
    ocall('_empty%1'(XXV2982),'lo.collection$map$lo.index*map'(Xequality249),'lo.collection$map$lo.index*map'(Xequality249)).
'lo.sets@nullSet23'(_, _, _):- raise_exception('error'("lo.sets@nullSet23", 11, 3, 17)).
'lo.collection$membership$lo.sets*set^addMem'('_call%3'(XV19314, XV19315, XV19316), 'lo.collection$membership$lo.sets*set^addMem'(XLbV1531, XThV1531), _):- 'lo.collection$membership$lo.sets*set@addMem'(XV19314, XV19315, XV19316, XLbV1531, XThV1531).
'lo.collection$membership$lo.sets*set^delMem'('_call%3'(XV19320, XV19321, XV19322), 'lo.collection$membership$lo.sets*set^delMem'(XLbV1531, XThV1531), _):- 'lo.collection$membership$lo.sets*set@delMem'(XV19320, XV19321, XV19322, XLbV1531, XThV1531).
'lo.collection$membership$lo.sets*set@cond228'(XXd23091, X_19718, XXe2780, XXV2985, XXd23090, Xequality250, XLbV1531, XThV1531, XM1, Xk):- 'ground'(Xk),
    !,
    ocall('keys%1'(XXV2985),'lo.collection$map$lo.index*map'(Xequality250),'lo.collection$map$lo.index*map'(Xequality250)),
    ocall('_call%2'(XM1, XXe2780),XXV2985,XXV2985),
    'lo.list@listEl'(Xk, XXe2780).
'lo.collection$membership$lo.sets*set@cond228'(XXd23091, X_19718, XXe2780, XXV2985, XXd23090, Xequality250, XLbV1531, XThV1531, XM1, Xk):- ocall('present%3'(XM1, Xk, X_19718),'lo.collection$map$lo.index*map'(Xequality250),'lo.collection$map$lo.index*map'(Xequality250)).
'lo.collection$membership$lo.sets*set^in'('_call%4'(XV19326, XV19327, XV19328, XV19329), 'lo.collection$membership$lo.sets*set^in'(XLbV1531, XThV1531), _):- 'lo.collection$membership$lo.sets*set@in'(XV19326, XV19327, XV19328, XV19329, XLbV1531, XThV1531).
'lo.sets^setDifference'('_call%4'(XV19332, XV19333, XV19334, XV19335), 'lo.sets^setDifference', _):- 'lo.sets@setDifference'(XV19332, XV19333, XV19334, XV19335).
'lo.sets^binApp'('_call%5'(XV19336, XV19337, XV19338, XV19339, XV19340), 'lo.sets^binApp', _):- 'lo.sets@binApp'(XV19336, XV19337, XV19338, XV19339, XV19340).
'lo.sets^setIntersection'('_call%4'(XV19341, XV19342, XV19343, XV19344), 'lo.sets^setIntersection', _):- 'lo.sets@setIntersection'(XV19341, XV19342, XV19343, XV19344).
'lo.sets^setUnion'('_call%4'(XV19345, XV19346, XV19347, XV19348), 'lo.sets^setUnion', _):- 'lo.sets@setUnion'(XV19345, XV19346, XV19347, XV19348).
'lo.collection$setops$lo.sets*set^union'('_call%3'(XV19349, XV19350, XV19351), 'lo.collection$setops$lo.sets*set^union'(XLbV1532, XThV1532), _):- 'lo.collection$setops$lo.sets*set@union'(XV19349, XV19350, XV19351, XLbV1532, XThV1532).
'lo.collection$setops$lo.sets*set^intersect'('_call%3'(XV19355, XV19356, XV19357), 'lo.collection$setops$lo.sets*set^intersect'(XLbV1532, XThV1532), _):- 'lo.collection$setops$lo.sets*set@intersect'(XV19355, XV19356, XV19357, XLbV1532, XThV1532).
'lo.collection$setops$lo.sets*set^difference'('_call%3'(XV19361, XV19362, XV19363), 'lo.collection$setops$lo.sets*set^difference'(XLbV1532, XThV1532), _):- 'lo.collection$setops$lo.sets*set@difference'(XV19361, XV19362, XV19363, XLbV1532, XThV1532).
'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'('_call%2'(XV19367, XV19368), 'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'(XLbV1533, XThV1533), _):- 'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(XV19367, XV19368, XLbV1533, XThV1533).
'lo.coerce$coercion$lo.core*list$lo.sets*set@fun60'('_call%3'(XM, XE, XXe2785), 'lo.coerce$coercion$lo.core*list$lo.sets*set@fun60'(Xequality257), _):- !,
    ocall('_put%1'(XXV2990),'lo.collection$map$lo.index*map'(Xequality257),'lo.collection$map$lo.index*map'(Xequality257)),
    ocall('_call%4'(XM, XE, '()0'(), XXe2785),XXV2990,XXV2990).
'lo.coerce$coercion$lo.core*list$lo.sets*set@fun60'(_, _, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.sets*set@fun60", 31, 34, 15)).
'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'('_call%2'(XV19371, XV19372), 'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'(XLbV1534, XThV1534), _):- 'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XV19371, XV19372, XLbV1534, XThV1534).
'lo.core$additive$lo.sets*set^+'('_call%3'(XV19375, XV19376, XV19377), 'lo.core$additive$lo.sets*set^+'(XLbV1535, XThV1535), _):- 'lo.core$additive$lo.sets*set@+'(XV19375, XV19376, XV19377, XLbV1535, XThV1535).
'lo.core$additive$lo.sets*set^-'('_call%3'(XV19381, XV19382, XV19383), 'lo.core$additive$lo.sets*set^-'(XLbV1535, XThV1535), _):- 'lo.core$additive$lo.sets*set@-'(XV19381, XV19382, XV19383, XLbV1535, XThV1535).
'lo.sets@fun61'('_call%3'(XM, Xk, XXe2788), 'lo.sets@fun61'(Xequality260, XF), _):- !,
    ocall('_put%1'(XXV2993),'lo.collection$map$lo.index*map'(Xequality260),'lo.collection$map$lo.index*map'(Xequality260)),
    ocall('_call%2'(Xk, XXe2787),XF,XF),
    ocall('_call%4'(XM, XXe2787, '()0'(), XXe2788),XXV2993,XXV2993).
'lo.sets@fun61'(_, _, _):- raise_exception('error'("lo.sets@fun61", 57, 38, 18)).
'lo.sets^setMap'('_call%5'(XV19387, XV19388, XV19389, XV19390, XV19391), 'lo.sets^setMap', _):- 'lo.sets@setMap'(XV19387, XV19388, XV19389, XV19390, XV19391).
'lo.collection$mapping$lo.sets*set^//'('_call%3'(XV19392, XV19393, XV19394), 'lo.collection$mapping$lo.sets*set^//'(XLbV1536, XThV1536), _):- 'lo.collection$mapping$lo.sets*set@//'(XV19392, XV19393, XV19394, XLbV1536, XThV1536).
'lo.sets^foldLeftEls'('_call%4'(XV19398, XV19399, XV19400, XV19401), 'lo.sets^foldLeftEls', _):- 'lo.sets@foldLeftEls'(XV19398, XV19399, XV19400, XV19401).
'lo.sets^foldRightEls'('_call%4'(XV19402, XV19403, XV19404, XV19405), 'lo.sets^foldRightEls', _):- 'lo.sets@foldRightEls'(XV19402, XV19403, XV19404, XV19405).
'lo.collection$folding$lo.sets*set^foldRight'('_call%4'(XV19406, XV19407, XV19408, XV19409), 'lo.collection$folding$lo.sets*set^foldRight'(XLbV1537, XThV1537), _):- 'lo.collection$folding$lo.sets*set@foldRight'(XV19406, XV19407, XV19408, XV19409, XLbV1537, XThV1537).
'lo.collection$folding$lo.sets*set^foldLeft'('_call%4'(XV19414, XV19415, XV19416, XV19417), 'lo.collection$folding$lo.sets*set^foldLeft'(XLbV1537, XThV1537), _):- 'lo.collection$folding$lo.sets*set@foldLeft'(XV19414, XV19415, XV19416, XV19417, XLbV1537, XThV1537).
'lo.sets^dispEls'('_call%4'(XV19422, XV19423, XV19424, XV19425), 'lo.sets^dispEls', _):- 'lo.sets@dispEls'(XV19422, XV19423, XV19424, XV19425).
'lo.core$display$lo.sets*set^disp'('_call%2'(XV19426, XV19427), 'lo.core$display$lo.sets*set^disp'(XLbV1538, XThV1538), _):- 'lo.core$display$lo.sets*set@disp'(XV19426, XV19427, XLbV1538, XThV1538).
