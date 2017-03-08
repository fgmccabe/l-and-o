'#pkg'("n7o7'()7'n2o2'pkg's'lo.sets'e'*'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'n2o2'import'e'private'n2o2'pkg's'lo.index'e'*'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I3'set':k'e'CT1Uz2'lo.index*map'2k'e'T0Uz1'lo.sets*set'1k'e''nullSet':k'e'|Uz1'lo.sets*set'1k'e'c'lo.core$equality'T1k'e'T0'setMap':k'e':k'f'||FT2Uz1'lo.sets*set'1k'e'FT1k'e'k'f'Uz1'lo.sets*set'1k'f'c'lo.core$equality'T1k'f'T0c'lo.core$equality'T1k'e'T0\"s\"I1'set':k'e'YUz1'lo.sets*set'1k'e'I0\"n0o0'()0'n0o0'()0'n8o8'()8'n2o2'()2's'lo.collection$membership$lo.sets*set's\":k'e'|c'lo.collection$membership'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.collection$setops$lo.sets*set's\":k'e'|c'lo.collection$setops'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.coerce$coercion$lo.sets*set$lo.core*list's\":k'e'|c'lo.coerce$coercion'T2Uz1'lo.sets*set'1k'e'Lk'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.sets*set's\":k'e'|c'lo.coerce$coercion'T2Lk'e'Uz1'lo.sets*set'1k'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.core$additive$lo.sets*set's\":k'e'|c'lo.core$additive'T1Uz1'lo.sets*set'1k'e'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.collection$mapping$lo.sets*set's\":k'e':k'f'||c'lo.collection$mapping'T1z1'lo.sets*set'T2k'e'k'f'c'lo.core$equality'T1k'f'T0c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.collection$folding$lo.sets*set's\":k'e'|c'lo.collection$folding'T1Uz1'lo.sets*set'1k'e'T1k'e'c'lo.core$equality'T1k'e'T0\"n2o2'()2's'lo.core$display$lo.sets*set's\":k'e'||c'lo.core$display'T1Uz1'lo.sets*set'1k'e'T0c'lo.core$equality'T1k'e'T0c'lo.core$display'T1k'e'T0\"").
'lo.sets@init'() :- !.
'lo.sets#set'('set%1'('lo.sets@set'())) :- !.
'lo.sets@nullSet'(Xlo_core_equality_e1, 'lo.sets#set'(XXV1)) :- !,
    ocall('_empty%1'(XXV1),'lo.collection$map$lo.index*map'(Xlo_core_equality_e1),'lo.collection$map$lo.index*map'(Xlo_core_equality_e1)).
'lo.sets@nullSet'(_) :- raise_exception('error'("nullSet", 11, 3, 17)).
'lo.collection$membership$lo.sets*set'('lo.collection$membership$lo.sets*set%1'('lo.collection$membership$lo.sets*set')) :- !.
'lo.collection$membership$lo.sets*set'('empty%1'(XV109), XLbl45, XThis45) :- !,
    'lo.collection$membership$lo.sets*set@empty'(XV109, XLbl45, XThis45).
'lo.collection$membership$lo.sets*set'('addMem%3'(XV113, XV114, XV115), XLbl46, XThis46) :- !,
    'lo.collection$membership$lo.sets*set@addMem'(XV113, XV114, XV115, XLbl46, XThis46).
'lo.collection$membership$lo.sets*set'('addMem%1'('lo.collection$membership$lo.sets*set^addMem'(XLbl47, XThis47)), XLbl47, XThis47).
'lo.collection$membership$lo.sets*set'('delMem%3'(XV122, XV123, XV124), XLbl48, XThis48) :- !,
    'lo.collection$membership$lo.sets*set@delMem'(XV122, XV123, XV124, XLbl48, XThis48).
'lo.collection$membership$lo.sets*set'('delMem%1'('lo.collection$membership$lo.sets*set^delMem'(XLbl49, XThis49)), XLbl49, XThis49).
'lo.collection$membership$lo.sets*set'('in%2'(XV130, XV131), XLbl50, XThis50) :- !,
    'lo.collection$membership$lo.sets*set@in'(XV130, XV131, XLbl50, XThis50).
'lo.collection$membership$lo.sets*set'('in%1'('lo.collection$membership$lo.sets*set^in'(XLbl51, XThis51)), XLbl51, XThis51).
'lo.collection$membership$lo.sets*set@empty'(XX393, XLbV28, XThV28) :- XLbV28 = 'lo.collection$membership$lo.sets*set'(Xlo_core_equality_e2),
    !,
    'lo.sets@nullSet'(Xlo_core_equality_e2, XX393).
'lo.collection$membership$lo.sets*set@addMem'('lo.sets#set'(XS), XE, 'lo.sets#set'(XX399), XLbV28, XThV28) :- XLbV28 = 'lo.collection$membership$lo.sets*set'(Xlo_core_equality_e2),
    !,
    ocall('_put%4'(XS, XE, '()', XX399),'lo.collection$map$lo.index*map'(Xlo_core_equality_e2),'lo.collection$map$lo.index*map'(Xlo_core_equality_e2)).
'lo.collection$membership$lo.sets*set@addMem'(_, _, _, _, _) :- raise_exception('error'("addMem", 15, 5, 33)).
'lo.collection$membership$lo.sets*set@delMem'('lo.sets#set'(XS), XE, 'lo.sets#set'(XX408), XLbV28, XThV28) :- XLbV28 = 'lo.collection$membership$lo.sets*set'(Xlo_core_equality_e2),
    !,
    ocall('_remove%3'(XS, XE, XX408),'lo.collection$map$lo.index*map'(Xlo_core_equality_e2),'lo.collection$map$lo.index*map'(Xlo_core_equality_e2)).
'lo.collection$membership$lo.sets*set@delMem'(_, _, _, _, _) :- raise_exception('error'("delMem", 16, 5, 31)).
'lo.collection$membership$lo.sets*set@in'(Xk, 'lo.sets#set'(XM1), XLbV28, XThV28) :- XLbV28 = 'lo.collection$membership$lo.sets*set'(Xlo_core_equality_e2),
    'lo.collection$membership$lo.sets*set@cond1'(X_2, XThV28, XLbV28, Xlo_core_equality_e2, XX418, XM1, Xk).
'lo.sets@setUnion'(Xlo_core_equality_e3, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XX433)) :- !,
    ocall('+%3'(XM1, XM2, XX433),'lo.core$additive$lo.index*map'(Xlo_core_equality_e3),'lo.core$additive$lo.index*map'(Xlo_core_equality_e3)).
'lo.sets@setUnion'(_, _, _) :- raise_exception('error'("setUnion", 40, 3, 39)).
'lo.sets@binApp'(Xlo_core_equality_e4, Xk, X_3, XM, XM) :- ocall('present%3'(XM, Xk, X_4),'lo.collection$map$lo.index*map'(Xlo_core_equality_e4),'lo.collection$map$lo.index*map'(Xlo_core_equality_e4)),
    !.
'lo.sets@binApp'(Xlo_core_equality_e4, Xk, X_5, XM, XX453) :- !,
    ocall('_put%4'(XM, Xk, '()', XX453),'lo.collection$map$lo.index*map'(Xlo_core_equality_e4),'lo.collection$map$lo.index*map'(Xlo_core_equality_e4)).
'lo.sets@binApp'(_, _, _, _) :- raise_exception('error'("binApp", 49, 3, 36)).
'lo.sets@setIntersection'(Xlo_core_equality_e5, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XX465)) :- !,
    'lo.sets@binApp'(Xlo_core_equality_e5, XX462),
    'lo.index@foldMap'(XX462, XM2, XM1, XX465).
'lo.sets@setIntersection'(_, _, _) :- raise_exception('error'("setIntersection", 46, 3, 63)).
'lo.sets@setDifference'(Xlo_core_equality_e6, 'lo.sets#set'(XM1), 'lo.sets#set'(XM2), 'lo.sets#set'(XX474)) :- !,
    ocall('-%3'(XM1, XM2, XX474),'lo.core$additive$lo.index*map'(Xlo_core_equality_e6),'lo.core$additive$lo.index*map'(Xlo_core_equality_e6)).
'lo.sets@setDifference'(_, _, _) :- raise_exception('error'("setDifference", 43, 3, 44)).
'lo.collection$setops$lo.sets*set'('lo.collection$setops$lo.sets*set%1'('lo.collection$setops$lo.sets*set')) :- !.
'lo.collection$setops$lo.sets*set'('union%3'(XV150, XV151, XV152), XLbl52, XThis52) :- !,
    'lo.collection$setops$lo.sets*set@union'(XV150, XV151, XV152, XLbl52, XThis52).
'lo.collection$setops$lo.sets*set'('union%1'('lo.collection$setops$lo.sets*set^union'(XLbl53, XThis53)), XLbl53, XThis53).
'lo.collection$setops$lo.sets*set'('intersect%3'(XV159, XV160, XV161), XLbl54, XThis54) :- !,
    'lo.collection$setops$lo.sets*set@intersect'(XV159, XV160, XV161, XLbl54, XThis54).
'lo.collection$setops$lo.sets*set'('intersect%1'('lo.collection$setops$lo.sets*set^intersect'(XLbl55, XThis55)), XLbl55, XThis55).
'lo.collection$setops$lo.sets*set'('difference%3'(XV168, XV169, XV170), XLbl56, XThis56) :- !,
    'lo.collection$setops$lo.sets*set@difference'(XV168, XV169, XV170, XLbl56, XThis56).
'lo.collection$setops$lo.sets*set'('difference%1'('lo.collection$setops$lo.sets*set^difference'(XLbl57, XThis57)), XLbl57, XThis57).
'lo.collection$setops$lo.sets*set@union'(XS1, XS2, XX485, XLbV29, XThV29) :- XLbV29 = 'lo.collection$setops$lo.sets*set'(Xlo_core_equality_e7),
    !,
    'lo.sets@setUnion'(Xlo_core_equality_e7, XS1, XS2, XX485).
'lo.collection$setops$lo.sets*set@union'(_, _, _, _, _) :- raise_exception('error'("union", 21, 5, 31)).
'lo.collection$setops$lo.sets*set@intersect'(XS1, XS2, XX491, XLbV29, XThV29) :- XLbV29 = 'lo.collection$setops$lo.sets*set'(Xlo_core_equality_e7),
    !,
    'lo.sets@setIntersection'(Xlo_core_equality_e7, XS1, XS2, XX491).
'lo.collection$setops$lo.sets*set@intersect'(_, _, _, _, _) :- raise_exception('error'("intersect", 22, 5, 42)).
'lo.collection$setops$lo.sets*set@difference'(XS1, XS2, XX497, XLbV29, XThV29) :- XLbV29 = 'lo.collection$setops$lo.sets*set'(Xlo_core_equality_e7),
    !,
    'lo.sets@setDifference'(Xlo_core_equality_e7, XS1, XS2, XX497).
'lo.collection$setops$lo.sets*set@difference'(_, _, _, _, _) :- raise_exception('error'("difference", 23, 5, 41)).
'lo.coerce$coercion$lo.sets*set$lo.core*list'('lo.coerce$coercion$lo.sets*set$lo.core*list%1'('lo.coerce$coercion$lo.sets*set$lo.core*list')) :- !.
'lo.coerce$coercion$lo.sets*set$lo.core*list'('_coerce%2'(XV176, XV177), XLbl58, XThis58) :- !,
    'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(XV176, XV177, XLbl58, XThis58).
'lo.coerce$coercion$lo.sets*set$lo.core*list'('_coerce%1'('lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'(XLbl59, XThis59)), XLbl59, XThis59).
'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'('lo.sets#set'(XEls), XX503, XLbV30, XThV30) :- XLbV30 = 'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xlo_core_equality_e8),
    !,
    ocall('keys%2'(XEls, XX503),'lo.collection$map$lo.index*map'(Xlo_core_equality_e8),'lo.collection$map$lo.index*map'(Xlo_core_equality_e8)).
'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 27, 5, 30)).
'lo.coerce$coercion$lo.core*list$lo.sets*set'('lo.coerce$coercion$lo.core*list$lo.sets*set%1'('lo.coerce$coercion$lo.core*list$lo.sets*set')) :- !.
'lo.coerce$coercion$lo.core*list$lo.sets*set'('_coerce%2'(XV182, XV183), XLbl60, XThis60) :- !,
    'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XV182, XV183, XLbl60, XThis60).
'lo.coerce$coercion$lo.core*list$lo.sets*set'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'(XLbl61, XThis61)), XLbl61, XThis61).
'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XEls, 'lo.sets#set'(XX519), XLbV31, XThV31) :- XLbV31 = 'lo.coerce$coercion$lo.core*list$lo.sets*set'(Xlo_core_equality_e9),
    !,
    ocall('_empty%1'(XXV2),'lo.collection$map$lo.index*map'(Xlo_core_equality_e9),'lo.collection$map$lo.index*map'(Xlo_core_equality_e9)),
    ocall('foldLeft%4'('lo.coerce$coercion$lo.core*list$lo.sets*set@$2', XXV2, XEls, XX519),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list').
'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 31, 5, 53)).
'lo.core$additive$lo.sets*set'('lo.core$additive$lo.sets*set%1'('lo.core$additive$lo.sets*set')) :- !.
'lo.core$additive$lo.sets*set'('+%3'(XV189, XV190, XV191), XLbl62, XThis62) :- !,
    'lo.core$additive$lo.sets*set@+'(XV189, XV190, XV191, XLbl62, XThis62).
'lo.core$additive$lo.sets*set'('+%1'('lo.core$additive$lo.sets*set^+'(XLbl63, XThis63)), XLbl63, XThis63).
'lo.core$additive$lo.sets*set'('-%3'(XV198, XV199, XV200), XLbl64, XThis64) :- !,
    'lo.core$additive$lo.sets*set@-'(XV198, XV199, XV200, XLbl64, XThis64).
'lo.core$additive$lo.sets*set'('-%1'('lo.core$additive$lo.sets*set^-'(XLbl65, XThis65)), XLbl65, XThis65).
'lo.core$additive$lo.sets*set@+'(XS1, XS2, XX529, XLbV32, XThV32) :- XLbV32 = 'lo.core$additive$lo.sets*set'(Xlo_core_equality_e10),
    !,
    'lo.sets@setUnion'(Xlo_core_equality_e10, XS1, XS2, XX529).
'lo.core$additive$lo.sets*set@+'(_, _, _, _, _) :- raise_exception('error'("+", 35, 5, 24)).
'lo.core$additive$lo.sets*set@-'(XS1, XS2, XX535, XLbV32, XThV32) :- XLbV32 = 'lo.core$additive$lo.sets*set'(Xlo_core_equality_e10),
    !,
    'lo.sets@setDifference'(Xlo_core_equality_e10, XS1, XS2, XX535).
'lo.core$additive$lo.sets*set@-'(_, _, _, _, _) :- raise_exception('error'("-", 36, 5, 29)).
'lo.sets@setMap'(Xlo_core_equality_e11, Xlo_core_equality_f1, 'lo.sets#set'(XEls), XF, 'lo.sets#set'(XX556)) :- !,
    ocall('_empty%1'(XXV3),'lo.collection$map$lo.index*map'(Xlo_core_equality_f1),'lo.collection$map$lo.index*map'(Xlo_core_equality_f1)),
    ocall('keys%2'(XEls, XX553),'lo.collection$map$lo.index*map'(Xlo_core_equality_e11),'lo.collection$map$lo.index*map'(Xlo_core_equality_e11)),
    ocall('foldLeft%4'('lo.sets@$3'(XF, Xlo_core_equality_f1), XXV3, XX553, XX556),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list').
'lo.sets@setMap'(_, _, _) :- raise_exception('error'("setMap", 57, 3, 68)).
'lo.collection$mapping$lo.sets*set'('lo.collection$mapping$lo.sets*set%1'('lo.collection$mapping$lo.sets*set')) :- !.
'lo.collection$mapping$lo.sets*set'('//%3'(XV210, XV211, XV212), XLbl66, XThis66) :- !,
    'lo.collection$mapping$lo.sets*set@//'(XV210, XV211, XV212, XLbl66, XThis66).
'lo.collection$mapping$lo.sets*set'('//%1'('lo.collection$mapping$lo.sets*set^//'(XLbl67, XThis67)), XLbl67, XThis67).
'lo.collection$mapping$lo.sets*set@//'(XS, XF, XX568, XLbV33, XThV33) :- XLbV33 = 'lo.collection$mapping$lo.sets*set'(Xlo_core_equality_e12, Xlo_core_equality_f2),
    !,
    'lo.sets@setMap'(Xlo_core_equality_e12, Xlo_core_equality_f2, XS, XF, XX568).
'lo.collection$mapping$lo.sets*set@//'(_, _, _, _, _) :- raise_exception('error'("//", 53, 5, 19)).
'lo.sets@foldRightEls'(XF, XX, XS, XX575) :- !,
    ocall('foldRight%4'(XF, XX, XS, XX575),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list').
'lo.sets@foldRightEls'(_, _, _, _) :- raise_exception('error'("foldRightEls", 68, 3, 39)).
'lo.sets@foldLeftEls'(XF, XX, XS, XX583) :- !,
    ocall('foldLeft%4'(XF, XX, XS, XX583),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list').
'lo.sets@foldLeftEls'(_, _, _, _) :- raise_exception('error'("foldLeftEls", 65, 3, 37)).
'lo.collection$folding$lo.sets*set'('lo.collection$folding$lo.sets*set%1'('lo.collection$folding$lo.sets*set')) :- !.
'lo.collection$folding$lo.sets*set'('foldRight%4'(XV228, XV229, XV230, XV231), XLbl68, XThis68) :- !,
    'lo.collection$folding$lo.sets*set@foldRight'(XV228, XV229, XV230, XV231, XLbl68, XThis68).
'lo.collection$folding$lo.sets*set'('foldRight%1'('lo.collection$folding$lo.sets*set^foldRight'(XLbl69, XThis69)), XLbl69, XThis69).
'lo.collection$folding$lo.sets*set'('foldLeft%4'(XV240, XV241, XV242, XV243), XLbl70, XThis70) :- !,
    'lo.collection$folding$lo.sets*set@foldLeft'(XV240, XV241, XV242, XV243, XLbl70, XThis70).
'lo.collection$folding$lo.sets*set'('foldLeft%1'('lo.collection$folding$lo.sets*set^foldLeft'(XLbl71, XThis71)), XLbl71, XThis71).
'lo.collection$folding$lo.sets*set@foldRight'(XF, XX, XS, XX596, XLbV34, XThV34) :- XLbV34 = 'lo.collection$folding$lo.sets*set'(Xlo_core_equality_e13),
    !,
    ocall('_coerce%2'(XS, XX593),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xlo_core_equality_e13),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xlo_core_equality_e13)),
    'lo.sets@foldRightEls'(XF, XX, XX593, XX596).
'lo.collection$folding$lo.sets*set@foldRight'(_, _, _, _, _, _) :- raise_exception('error'("foldRight", 60, 5, 48)).
'lo.collection$folding$lo.sets*set@foldLeft'(XF, XX, XS, XX606, XLbV34, XThV34) :- XLbV34 = 'lo.collection$folding$lo.sets*set'(Xlo_core_equality_e13),
    !,
    ocall('_coerce%2'(XS, XX603),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xlo_core_equality_e13),'lo.coerce$coercion$lo.sets*set$lo.core*list'(Xlo_core_equality_e13)),
    'lo.sets@foldLeftEls'(XF, XX, XX603, XX606).
'lo.collection$folding$lo.sets*set@foldLeft'(_, _, _, _, _, _) :- raise_exception('error'("foldLeft", 61, 5, 46)).
'lo.sets@dispEls'(Xlo_core_display_e1, 'lo.core#[]', X_6, 'lo.core#[]') :- !.
'lo.sets@dispEls'(Xlo_core_display_e1, 'lo.core#,..'(XE, XL), XSep, 'lo.core#,..'('lo.core#ss'(XSep), 'lo.core#,..'(XX619, XX623))) :- !,
    ocall('disp%2'(XE, XX619),Xlo_core_display_e1,Xlo_core_display_e1),
    'lo.sets@dispEls'(Xlo_core_display_e1, XL, ", ", XX623).
'lo.sets@dispEls'(_, _, _) :- raise_exception('error'("dispEls", 75, 3, 19)).
'lo.core$display$lo.sets*set'('lo.core$display$lo.sets*set%1'('lo.core$display$lo.sets*set')) :- !.
'lo.core$display$lo.sets*set'('disp%2'(XV253, XV254), XLbl72, XThis72) :- !,
    'lo.core$display$lo.sets*set@disp'(XV253, XV254, XLbl72, XThis72).
'lo.core$display$lo.sets*set'('disp%1'('lo.core$display$lo.sets*set^disp'(XLbl73, XThis73)), XLbl73, XThis73).
'lo.core$display$lo.sets*set@disp'('lo.sets#set'(XEls), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("{"), 'lo.core#,..'('lo.core#ssSeq'(XX637), 'lo.core#,..'('lo.core#ss'("}"), 'lo.core#[]')))), XLbV35, XThV35) :- XLbV35 = 'lo.core$display$lo.sets*set'(Xlo_core_display_e2, Xlo_core_equality_e14),
    !,
    ocall('keys%2'(XEls, XX634),'lo.collection$map$lo.index*map'(Xlo_core_equality_e14),'lo.collection$map$lo.index*map'(Xlo_core_equality_e14)),
    'lo.sets@dispEls'(Xlo_core_display_e2, XX634, "", XX637).
'lo.core$display$lo.sets*set@disp'(_, _, _, _) :- raise_exception('error'("disp", 71, 5, 81)).
'lo.sets^nullSet'('_call%1'(XV108), 'lo.sets^nullSet', _) :- 'lo.sets@nullSet'(XV108).
'lo.collection$membership$lo.sets*set^addMem'('_call%3'(XV110, XV111, XV112), 'lo.collection$membership$lo.sets*set^addMem'(XLbV28, XThV28), _) :- 'lo.collection$membership$lo.sets*set@addMem'(XV110, XV111, XV112, XLbV28, XThV28).
'lo.collection$membership$lo.sets*set^addMem'('_call%3'(XV116, XV117, XV118), 'lo.collection$membership$lo.sets*set^addMem'(XLbV28, XThV28), _) :- 'lo.collection$membership$lo.sets*set@addMem'(XV116, XV117, XV118, XLbV28, XThV28).
'lo.collection$membership$lo.sets*set^delMem'('_call%3'(XV119, XV120, XV121), 'lo.collection$membership$lo.sets*set^delMem'(XLbV28, XThV28), _) :- 'lo.collection$membership$lo.sets*set@delMem'(XV119, XV120, XV121, XLbV28, XThV28).
'lo.collection$membership$lo.sets*set^delMem'('_call%3'(XV125, XV126, XV127), 'lo.collection$membership$lo.sets*set^delMem'(XLbV28, XThV28), _) :- 'lo.collection$membership$lo.sets*set@delMem'(XV125, XV126, XV127, XLbV28, XThV28).
'lo.collection$membership$lo.sets*set@cond1'(X_2, XThV28, XLbV28, Xlo_core_equality_e2, XX418, XM1, Xk) :- 'ground'(Xk),
    !,
    ocall('keys%2'(XM1, XX418),'lo.collection$map$lo.index*map'(Xlo_core_equality_e2),'lo.collection$map$lo.index*map'(Xlo_core_equality_e2)),
    'lo.list@listEl'(Xk, XX418).
'lo.collection$membership$lo.sets*set@cond1'(X_2, XThV28, XLbV28, Xlo_core_equality_e2, XX418, XM1, Xk) :- ocall('present%3'(XM1, Xk, X_2),'lo.collection$map$lo.index*map'(Xlo_core_equality_e2),'lo.collection$map$lo.index*map'(Xlo_core_equality_e2)).
'lo.collection$membership$lo.sets*set^in'('_call%2'(XV128, XV129), 'lo.collection$membership$lo.sets*set^in'(XLbV28, XThV28), _) :- 'lo.collection$membership$lo.sets*set@in'(XV128, XV129, XLbV28, XThV28).
'lo.collection$membership$lo.sets*set^in'('_call%2'(XV132, XV133), 'lo.collection$membership$lo.sets*set^in'(XLbV28, XThV28), _) :- 'lo.collection$membership$lo.sets*set@in'(XV132, XV133, XLbV28, XThV28).
'lo.sets^setUnion'('_call%3'(XV134, XV135, XV136), 'lo.sets^setUnion', _) :- 'lo.sets@setUnion'(XV134, XV135, XV136).
'lo.sets^binApp'('_call%4'(XV137, XV138, XV139, XV140), 'lo.sets^binApp', _) :- 'lo.sets@binApp'(XV137, XV138, XV139, XV140).
'lo.sets^setIntersection'('_call%3'(XV141, XV142, XV143), 'lo.sets^setIntersection', _) :- 'lo.sets@setIntersection'(XV141, XV142, XV143).
'lo.sets^setDifference'('_call%3'(XV144, XV145, XV146), 'lo.sets^setDifference', _) :- 'lo.sets@setDifference'(XV144, XV145, XV146).
'lo.collection$setops$lo.sets*set^union'('_call%3'(XV147, XV148, XV149), 'lo.collection$setops$lo.sets*set^union'(XLbV29, XThV29), _) :- 'lo.collection$setops$lo.sets*set@union'(XV147, XV148, XV149, XLbV29, XThV29).
'lo.collection$setops$lo.sets*set^union'('_call%3'(XV153, XV154, XV155), 'lo.collection$setops$lo.sets*set^union'(XLbV29, XThV29), _) :- 'lo.collection$setops$lo.sets*set@union'(XV153, XV154, XV155, XLbV29, XThV29).
'lo.collection$setops$lo.sets*set^intersect'('_call%3'(XV156, XV157, XV158), 'lo.collection$setops$lo.sets*set^intersect'(XLbV29, XThV29), _) :- 'lo.collection$setops$lo.sets*set@intersect'(XV156, XV157, XV158, XLbV29, XThV29).
'lo.collection$setops$lo.sets*set^intersect'('_call%3'(XV162, XV163, XV164), 'lo.collection$setops$lo.sets*set^intersect'(XLbV29, XThV29), _) :- 'lo.collection$setops$lo.sets*set@intersect'(XV162, XV163, XV164, XLbV29, XThV29).
'lo.collection$setops$lo.sets*set^difference'('_call%3'(XV165, XV166, XV167), 'lo.collection$setops$lo.sets*set^difference'(XLbV29, XThV29), _) :- 'lo.collection$setops$lo.sets*set@difference'(XV165, XV166, XV167, XLbV29, XThV29).
'lo.collection$setops$lo.sets*set^difference'('_call%3'(XV171, XV172, XV173), 'lo.collection$setops$lo.sets*set^difference'(XLbV29, XThV29), _) :- 'lo.collection$setops$lo.sets*set@difference'(XV171, XV172, XV173, XLbV29, XThV29).
'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'('_call%2'(XV174, XV175), 'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'(XLbV30, XThV30), _) :- 'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(XV174, XV175, XLbV30, XThV30).
'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'('_call%2'(XV178, XV179), 'lo.coerce$coercion$lo.sets*set$lo.core*list^_coerce'(XLbV30, XThV30), _) :- 'lo.coerce$coercion$lo.sets*set$lo.core*list@_coerce'(XV178, XV179, XLbV30, XThV30).
'lo.coerce$coercion$lo.core*list$lo.sets*set@$2'('_call%3'(XM, XE, XX513), 'lo.coerce$coercion$lo.core*list$lo.sets*set@$2', _) :- !,
    ocall('_put%4'(XM, XE, '()', XX513),'lo.collection$map$lo.index*map'(Xlo_core_equality_e9),'lo.collection$map$lo.index*map'(Xlo_core_equality_e9)).
'lo.coerce$coercion$lo.core*list$lo.sets*set@$2'(_, _, _) :- raise_exception('error'("lambda", 31, 34, 15)).
'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'('_call%2'(XV180, XV181), 'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'(XLbV31, XThV31), _) :- 'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XV180, XV181, XLbV31, XThV31).
'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'('_call%2'(XV184, XV185), 'lo.coerce$coercion$lo.core*list$lo.sets*set^_coerce'(XLbV31, XThV31), _) :- 'lo.coerce$coercion$lo.core*list$lo.sets*set@_coerce'(XV184, XV185, XLbV31, XThV31).
'lo.core$additive$lo.sets*set^+'('_call%3'(XV186, XV187, XV188), 'lo.core$additive$lo.sets*set^+'(XLbV32, XThV32), _) :- 'lo.core$additive$lo.sets*set@+'(XV186, XV187, XV188, XLbV32, XThV32).
'lo.core$additive$lo.sets*set^+'('_call%3'(XV192, XV193, XV194), 'lo.core$additive$lo.sets*set^+'(XLbV32, XThV32), _) :- 'lo.core$additive$lo.sets*set@+'(XV192, XV193, XV194, XLbV32, XThV32).
'lo.core$additive$lo.sets*set^-'('_call%3'(XV195, XV196, XV197), 'lo.core$additive$lo.sets*set^-'(XLbV32, XThV32), _) :- 'lo.core$additive$lo.sets*set@-'(XV195, XV196, XV197, XLbV32, XThV32).
'lo.core$additive$lo.sets*set^-'('_call%3'(XV201, XV202, XV203), 'lo.core$additive$lo.sets*set^-'(XLbV32, XThV32), _) :- 'lo.core$additive$lo.sets*set@-'(XV201, XV202, XV203, XLbV32, XThV32).
'lo.sets@$3'('_call%3'(XM, Xk, XX547), 'lo.sets@$3'(XF, Xlo_core_equality_f1), _) :- !,
    ocall('_call%2'(Xk, XX545),XF,XF),
    ocall('_put%4'(XM, XX545, '()', XX547),'lo.collection$map$lo.index*map'(Xlo_core_equality_f1),'lo.collection$map$lo.index*map'(Xlo_core_equality_f1)).
'lo.sets@$3'(_, _, _) :- raise_exception('error'("lambda", 57, 38, 18)).
'lo.sets^setMap'('_call%3'(XV204, XV205, XV206), 'lo.sets^setMap', _) :- 'lo.sets@setMap'(XV204, XV205, XV206).
'lo.collection$mapping$lo.sets*set^//'('_call%3'(XV207, XV208, XV209), 'lo.collection$mapping$lo.sets*set^//'(XLbV33, XThV33), _) :- 'lo.collection$mapping$lo.sets*set@//'(XV207, XV208, XV209, XLbV33, XThV33).
'lo.collection$mapping$lo.sets*set^//'('_call%3'(XV213, XV214, XV215), 'lo.collection$mapping$lo.sets*set^//'(XLbV33, XThV33), _) :- 'lo.collection$mapping$lo.sets*set@//'(XV213, XV214, XV215, XLbV33, XThV33).
'lo.sets^foldRightEls'('_call%4'(XV216, XV217, XV218, XV219), 'lo.sets^foldRightEls', _) :- 'lo.sets@foldRightEls'(XV216, XV217, XV218, XV219).
'lo.sets^foldLeftEls'('_call%4'(XV220, XV221, XV222, XV223), 'lo.sets^foldLeftEls', _) :- 'lo.sets@foldLeftEls'(XV220, XV221, XV222, XV223).
'lo.collection$folding$lo.sets*set^foldRight'('_call%4'(XV224, XV225, XV226, XV227), 'lo.collection$folding$lo.sets*set^foldRight'(XLbV34, XThV34), _) :- 'lo.collection$folding$lo.sets*set@foldRight'(XV224, XV225, XV226, XV227, XLbV34, XThV34).
'lo.collection$folding$lo.sets*set^foldRight'('_call%4'(XV232, XV233, XV234, XV235), 'lo.collection$folding$lo.sets*set^foldRight'(XLbV34, XThV34), _) :- 'lo.collection$folding$lo.sets*set@foldRight'(XV232, XV233, XV234, XV235, XLbV34, XThV34).
'lo.collection$folding$lo.sets*set^foldLeft'('_call%4'(XV236, XV237, XV238, XV239), 'lo.collection$folding$lo.sets*set^foldLeft'(XLbV34, XThV34), _) :- 'lo.collection$folding$lo.sets*set@foldLeft'(XV236, XV237, XV238, XV239, XLbV34, XThV34).
'lo.collection$folding$lo.sets*set^foldLeft'('_call%4'(XV244, XV245, XV246, XV247), 'lo.collection$folding$lo.sets*set^foldLeft'(XLbV34, XThV34), _) :- 'lo.collection$folding$lo.sets*set@foldLeft'(XV244, XV245, XV246, XV247, XLbV34, XThV34).
'lo.sets^dispEls'('_call%3'(XV248, XV249, XV250), 'lo.sets^dispEls', _) :- 'lo.sets@dispEls'(XV248, XV249, XV250).
'lo.core$display$lo.sets*set^disp'('_call%2'(XV251, XV252), 'lo.core$display$lo.sets*set^disp'(XLbV35, XThV35), _) :- 'lo.core$display$lo.sets*set@disp'(XV251, XV252, XLbV35, XThV35).
'lo.core$display$lo.sets*set^disp'('_call%2'(XV255, XV256), 'lo.core$display$lo.sets*set^disp'(XLbV35, XThV35), _) :- 'lo.core$display$lo.sets*set@disp'(XV255, XV256, XLbV35, XThV35).
