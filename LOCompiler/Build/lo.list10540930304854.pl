'#pkg'("n7o7'()7'n2o2'pkg's'lo.list's'1.0.0'n3o3'()3'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'s\"I17'<>':k't'FT2Lk't'Lk't'Lk't''listEl':k't'PT2k't'Lk't''merge':k't'FT2Lk't'Lk't'Lk't''multiCat':k'x'FT1LLk'x'Lk'x''reverse':k't'FT1Lk't'Lk't''drop':k't'FT1Lk't'Lk't''subtract':k't'FT2k't'Lk't'Lk't''length':k't'FT1Lk't'i'mapList':k'v':k'w'FT2Lk'v'FT1k'v'k'w'Lk'w''nthEl':k'e'PT3Lk'e'ik'e''lastEl':k'e'PT2Lk'e'k'e''interleave':k'x'FT2Lk'x'k'x'Lk'x''zip':k'e':k'f'FT2Lk'e'Lk'f'LT2k'e'k'f''unzip':k'e':k'f'PT3LT2k'e'k'f'Lk'e'Lk'f''iota'FT2iiLi'head':k'e'FT1Lk'e'k'e''tail':k'e'FT1Lk'e'Lk'e'\"s'I0'n0o0'()0'n0o0'()0'n7o7'()7'n2o2'()2's'lo.core$sizeable$lo.core*list's\":k'c'c'lo.core$sizeable'T1Lk'c'T0\"n2o2'()2's'lo.collection$folding$lo.core*list's\":k'e'c'lo.collection$folding'T1Lk'e'T1k'e'\"n2o2'()2's'lo.collection$mapping$lo.core*list's\":k'e':k'f'c'lo.collection$mapping'T1z1'lo.core*list'T2k'e'k'f'\"n2o2'()2's'lo.collection$filter$lo.core*list's\":k'e'c'lo.collection$filter'T1Lk'e'T1k'e'\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.core*list's\":k'e':k'f'|c'lo.coerce$coercion'T2Lk'e'Lk'f'T0c'lo.coerce$coercion'T2k'e'k'f'T0\"n2o2'()2's'lo.collection$map$lo.core*list's\":k'e'c'lo.collection$map'T1Lk'e'T2ik'e'\"n2o2'()2's'lo.collection$membership$lo.core*list's\":k'e'c'lo.collection$membership'T1Lk'e'T1k'e'\"").
'lo.list@init'():- !.
'lo.list@<>'('lo.core#[]', XX, XX):- !.
'lo.list@<>'('lo.core#,..'(XE, XX), XY, 'lo.core#,..'(XE, XXd22851)):- !,
    'lo.list@<>'(XX, XY, XXd22851).
'lo.list@<>'(_, _, _):- raise_exception('error'("lo.list@<>", 8, 3, 12)).
'lo.list@listEl'(XX, 'lo.core#,..'(XX, X_19536)).
'lo.list@listEl'(XX, 'lo.core#,..'(X_19538, XY)):- 'lo.list@listEl'(XX, XY).
'lo.list@merge'('lo.core#[]', XX, XX):- !.
'lo.list@merge'('lo.core#,..'(Xe, Xl), Xx, XXd22853):- 'lo.list@listEl'(Xe, Xx),
    !,
    'lo.list@merge'(Xl, Xx, XXd22853).
'lo.list@merge'('lo.core#,..'(Xe, Xl), Xx, 'lo.core#,..'(Xe, XXd22854)):- !,
    'lo.list@merge'(Xl, Xx, XXd22854).
'lo.list@merge'(_, _, _):- raise_exception('error'("lo.list@merge", 16, 3, 16)).
'lo.list@cat'('lo.core#[]', XM, XXd22856):- !,
    'lo.list@multiCat'(XM, XXd22856).
'lo.list@cat'('lo.core#,..'(XE, XL), XM, 'lo.core#,..'(XE, XXd22857)):- !,
    'lo.list@cat'(XL, XM, XXd22857).
'lo.list@cat'(_, _, _):- raise_exception('error'("lo.list@cat", 25, 3, 24)).
'lo.list@multiCat'('lo.core#[]', 'lo.core#[]'):- !.
'lo.list@multiCat'('lo.core#,..'(XE, XL), XXd22859):- !,
    'lo.list@cat'(XE, XL, XXd22859).
'lo.list@multiCat'(_, _):- raise_exception('error'("lo.list@multiCat", 21, 3, 18)).
'lo.list@rev'('lo.core#[]', XR, XR):- !.
'lo.list@rev'('lo.core#,..'(XE, XL), XR, XXd22861):- !,
    'lo.list@rev'(XL, 'lo.core#,..'(XE, XR), XXd22861).
'lo.list@rev'(_, _, _):- raise_exception('error'("lo.list@rev", 33, 3, 14)).
'lo.list@reverse'(XX, XXd22862):- !,
    'lo.list@rev'(XX, 'lo.core#[]', XXd22862).
'lo.list@reverse'(_, _):- raise_exception('error'("lo.list@reverse", 29, 3, 23)).
'lo.list@drop'('lo.core#,..'(X_19548, XL), XL):- !.
'lo.list@drop'(_, _):- raise_exception('error'("lo.list@drop", 37, 3, 18)).
'lo.list@subtract'(X_19549, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.list@subtract'(XE, 'lo.core#,..'(XE, XR), XR):- !.
'lo.list@subtract'(XE, 'lo.core#,..'(XX, XR), 'lo.core#,..'(XX, XXd22863)):- 'lo.list@neg184'(XX, XE),
    !,
    'lo.list@subtract'(XE, XR, XXd22863).
'lo.list@subtract'(_, _, _):- raise_exception('error'("lo.list@subtract", 40, 3, 20)).
'lo.list@length'('lo.core#[]', 0):- !.
'lo.list@length'('lo.core#,..'(X_19554, XL), XXe2739):- !,
    ocall('+%1'(XXV2951),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XL, XXd22865),
    ocall('_call%3'(XXd22865, 1, XXe2739),XXV2951,XXV2951).
'lo.list@length'(_, _):- raise_exception('error'("lo.list@length", 46, 3, 15)).
'lo.core$sizeable$lo.core*list'('lo.core$sizeable$lo.core*list%1'('lo.core$sizeable$lo.core*list')):- !.
'lo.core$sizeable$lo.core*list'('size%2'(XV18900, XV18901), XLbl1573, XThis1573):- !,
    'lo.core$sizeable$lo.core*list@size'(XV18900, XV18901, XLbl1573, XThis1573).
'lo.core$sizeable$lo.core*list'('size%1'('lo.core$sizeable$lo.core*list^size'(XLbl1574, XThis1574)), XLbl1574, XThis1574).
'lo.core$sizeable$lo.core*list'('isEmpty%1'(XV18905), XLbl1575, XThis1575):- !,
    'lo.core$sizeable$lo.core*list@isEmpty'(XV18905, XLbl1575, XThis1575).
'lo.core$sizeable$lo.core*list'('isEmpty%1'('lo.core$sizeable$lo.core*list^isEmpty'(XLbl1576, XThis1576)), XLbl1576, XThis1576).
'lo.core$sizeable$lo.core*list@size'(XL, XXd22866, XLbV1511, XThV1511):- !,
    'lo.list@length'(XL, XXd22866).
'lo.core$sizeable$lo.core*list@size'(_, _):- raise_exception('error'("lo.core$sizeable$lo.core*list@size", 50, 5, 20)).
'lo.core$sizeable$lo.core*list@isEmpty'('lo.core#[]', XLbV1511, XThV1511).
'lo.collection$folding$lo.core*list'('lo.collection$folding$lo.core*list%1'('lo.collection$folding$lo.core*list')):- !.
'lo.collection$folding$lo.core*list'('foldLeft%4'(XV18910, XV18911, XV18912, XV18913), XLbl1577, XThis1577):- !,
    'lo.collection$folding$lo.core*list@foldLeft'(XV18910, XV18911, XV18912, XV18913, XLbl1577, XThis1577).
'lo.collection$folding$lo.core*list'('foldLeft%1'('lo.collection$folding$lo.core*list^foldLeft'(XLbl1578, XThis1578)), XLbl1578, XThis1578).
'lo.collection$folding$lo.core*list'('foldRight%4'(XV18918, XV18919, XV18920, XV18921), XLbl1579, XThis1579):- !,
    'lo.collection$folding$lo.core*list@foldRight'(XV18918, XV18919, XV18920, XV18921, XLbl1579, XThis1579).
'lo.collection$folding$lo.core*list'('foldRight%1'('lo.collection$folding$lo.core*list^foldRight'(XLbl1580, XThis1580)), XLbl1580, XThis1580).
'lo.collection$folding$lo.core*list@foldLeft'(Xf, Xx, 'lo.core#[]', Xx, XLbV1512, XThV1512):- !.
'lo.collection$folding$lo.core*list@foldLeft'(Xf, Xx, 'lo.core#,..'(Xe, Xl), XXd22868, XLbV1512, XThV1512):- !,
    ocall('_call%3'(Xx, Xe, XXe2740),Xf,Xf),
    'lo.collection$folding$lo.core*list@foldLeft'(Xf, XXe2740, Xl, XXd22868, XLbV1512, XThV1512).
'lo.collection$folding$lo.core*list@foldLeft'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.core*list@foldLeft", 55, 5, 21)).
'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, 'lo.core#[]', Xx, XLbV1512, XThV1512):- !.
'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, 'lo.core#,..'(Xe, Xl), XXe2741, XLbV1512, XThV1512):- !,
    'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, Xl, XXd22869, XLbV1512, XThV1512),
    ocall('_call%3'(Xe, XXd22869, XXe2741),Xf,Xf).
'lo.collection$folding$lo.core*list@foldRight'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.core*list@foldRight", 58, 5, 22)).
'lo.list@mapList'('lo.core#[]', X_19557, 'lo.core#[]'):- !.
'lo.list@mapList'('lo.core#,..'(Xe, Xl), Xf, 'lo.core#,..'(XXe2742, XXd22872)):- !,
    ocall('_call%2'(Xe, XXe2742),Xf,Xf),
    'lo.list@mapList'(Xl, Xf, XXd22872).
'lo.list@mapList'(_, _, _):- raise_exception('error'("lo.list@mapList", 67, 3, 19)).
'lo.collection$mapping$lo.core*list'('lo.collection$mapping$lo.core*list%1'('lo.collection$mapping$lo.core*list')):- !.
'lo.collection$mapping$lo.core*list'('//%3'(XV18928, XV18929, XV18930), XLbl1581, XThis1581):- !,
    'lo.collection$mapping$lo.core*list@//'(XV18928, XV18929, XV18930, XLbl1581, XThis1581).
'lo.collection$mapping$lo.core*list'('//%1'('lo.collection$mapping$lo.core*list^//'(XLbl1582, XThis1582)), XLbl1582, XThis1582).
'lo.collection$mapping$lo.core*list@//'(XL, XF, XXd22874, XLbV1513, XThV1513):- !,
    'lo.list@mapList'(XL, XF, XXd22874).
'lo.collection$mapping$lo.core*list@//'(_, _, _):- raise_exception('error'("lo.collection$mapping$lo.core*list@//", 63, 5, 20)).
'lo.list@filterList'('lo.core#[]', X_19560, 'lo.core#[]'):- !.
'lo.list@filterList'('lo.core#,..'(Xe, Xl), Xp, 'lo.core#,..'(Xe, XXd22875)):- ocall('_call%1'(Xe),Xp,Xp),
    !,
    'lo.list@filterList'(Xl, Xp, XXd22875).
'lo.list@filterList'('lo.core#,..'(X_19564, Xl), Xp, XXd22877):- !,
    'lo.list@filterList'(Xl, Xp, XXd22877).
'lo.list@filterList'(_, _, _):- raise_exception('error'("lo.list@filterList", 75, 3, 22)).
'lo.collection$filter$lo.core*list'('lo.collection$filter$lo.core*list%1'('lo.collection$filter$lo.core*list')):- !.
'lo.collection$filter$lo.core*list'('^/%3'(XV18937, XV18938, XV18939), XLbl1583, XThis1583):- !,
    'lo.collection$filter$lo.core*list@^/'(XV18937, XV18938, XV18939, XLbl1583, XThis1583).
'lo.collection$filter$lo.core*list'('^/%1'('lo.collection$filter$lo.core*list^^/'(XLbl1584, XThis1584)), XLbl1584, XThis1584).
'lo.collection$filter$lo.core*list@^/'(XL, XP, XXd22878, XLbV1514, XThV1514):- !,
    'lo.list@filterList'(XL, XP, XXd22878).
'lo.collection$filter$lo.core*list@^/'(_, _, _):- raise_exception('error'("lo.collection$filter$lo.core*list@^/", 71, 5, 23)).
'lo.list@coerceList'(Xcoercion22, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.list@coerceList'(Xcoercion22, 'lo.core#,..'(Xe, Xl), 'lo.core#,..'(XXe2743, XXd22879)):- !,
    ocall('_coerce%1'(XXV2952),Xcoercion22,Xcoercion22),
    ocall('_call%2'(Xe, XXe2743),XXV2952,XXV2952),
    'lo.list@coerceList'(Xcoercion22, Xl, XXd22879).
'lo.list@coerceList'(_, _, _):- raise_exception('error'("lo.list@coerceList", 84, 3, 20)).
'lo.coerce$coercion$lo.core*list$lo.core*list'('lo.coerce$coercion$lo.core*list$lo.core*list%1'('lo.coerce$coercion$lo.core*list$lo.core*list')):- !.
'lo.coerce$coercion$lo.core*list$lo.core*list'('_coerce%2'(XV18945, XV18946), XLbl1585, XThis1585):- !,
    'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XV18945, XV18946, XLbl1585, XThis1585).
'lo.coerce$coercion$lo.core*list$lo.core*list'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'(XLbl1586, XThis1586)), XLbl1586, XThis1586).
'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XSx, XXd22881, XLbV1515, XThV1515):- XLbV1515 = 'lo.coerce$coercion$lo.core*list$lo.core*list'(Xcoercion23),
    !,
    'lo.list@coerceList'(Xcoercion23, XSx, XXd22881).
'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.core*list@_coerce", 80, 5, 29)).
'lo.list@listPairs'('lo.core#[]', X_19567, 'lo.core#[]'):- !.
'lo.list@listPairs'('lo.core#,..'(XE, XL), XIx, 'lo.core#,..'('()2'(XIx, XE), XXd22882)):- !,
    ocall('+%1'(XXV2953),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe2744),XXV2953,XXV2953),
    'lo.list@listPairs'(XL, XXe2744, XXd22882).
'lo.list@listPairs'(_, _, _):- raise_exception('error'("lo.list@listPairs", 127, 3, 21)).
'lo.list@indexes'('lo.core#[]', X_19570, 'lo.core#[]'):- !.
'lo.list@indexes'('lo.core#,..'(X_19572, XL), XIx, 'lo.core#,..'(XIx, XXd22884)):- !,
    ocall('+%1'(XXV2954),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe2745),XXV2954,XXV2954),
    'lo.list@indexes'(XL, XXe2745, XXd22884).
'lo.list@indexes'(_, _, _):- raise_exception('error'("lo.list@indexes", 123, 3, 19)).
'lo.list@replaceNth'('lo.core#,..'(X_19575, XL), 0, Xe, 'lo.core#,..'(Xe, XL)):- !.
'lo.list@replaceNth'('lo.core#[]', 0, Xe, 'lo.core#,..'(Xe, 'lo.core#[]')):- !.
'lo.list@replaceNth'('lo.core#,..'(XE, XL), XIx, Xe, 'lo.core#,..'(XE, XXd22888)):- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    !,
    ocall('-%1'(XXV2955),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe2746),XXV2955,XXV2955),
    'lo.list@replaceNth'(XL, XXe2746, Xe, XXd22888).
'lo.list@replaceNth'(_, _, _, _):- raise_exception('error'("lo.list@replaceNth", 114, 3, 34)).
'lo.list@dropNth'('lo.core#[]', X_19580, 'lo.core#[]'):- !.
'lo.list@dropNth'('lo.core#,..'(X_19582, XL), 0, XL):- !.
'lo.list@dropNth'('lo.core#,..'(XE, XL), XIx, 'lo.core#,..'(XE, XXd22890)):- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    !,
    ocall('-%1'(XXV2956),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe2747),XXV2956,XXV2956),
    'lo.list@dropNth'(XL, XXe2747, XXd22890).
'lo.list@dropNth'(_, _, _):- raise_exception('error'("lo.list@dropNth", 109, 3, 19)).
'lo.list@nthEl'('lo.core#,..'(Xe, X_19586), 0, Xe).
'lo.list@nthEl'('lo.core#,..'(X_19588, XL), XIx, Xe):- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    ocall('-%1'(XXV2957),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe2748),XXV2957,XXV2957),
    'lo.list@nthEl'(XL, XXe2748, Xe).
'lo.collection$map$lo.core*list'('lo.collection$map$lo.core*list%1'('lo.collection$map$lo.core*list')):- !.
'lo.collection$map$lo.core*list'('present%3'(XV18968, XV18969, XV18970), XLbl1587, XThis1587):- !,
    'lo.collection$map$lo.core*list@present'(XV18968, XV18969, XV18970, XLbl1587, XThis1587).
'lo.collection$map$lo.core*list'('present%1'('lo.collection$map$lo.core*list^present'(XLbl1588, XThis1588)), XLbl1588, XThis1588).
'lo.collection$map$lo.core*list'('_remove%3'(XV18974, XV18975, XV18976), XLbl1589, XThis1589):- !,
    'lo.collection$map$lo.core*list@_remove'(XV18974, XV18975, XV18976, XLbl1589, XThis1589).
'lo.collection$map$lo.core*list'('_remove%1'('lo.collection$map$lo.core*list^_remove'(XLbl1590, XThis1590)), XLbl1590, XThis1590).
'lo.collection$map$lo.core*list'('_put%1'(XV18977), XLbl1591, XThis1591):- !,
    'lo.collection$map$lo.core*list@_put'(XV18977, XLbl1591, XThis1591).
'lo.collection$map$lo.core*list'('keys%2'(XV18980, XV18981), XLbl1592, XThis1592):- !,
    'lo.collection$map$lo.core*list@keys'(XV18980, XV18981, XLbl1592, XThis1592).
'lo.collection$map$lo.core*list'('keys%1'('lo.collection$map$lo.core*list^keys'(XLbl1593, XThis1593)), XLbl1593, XThis1593).
'lo.collection$map$lo.core*list'('pairs%2'(XV18984, XV18985), XLbl1594, XThis1594):- !,
    'lo.collection$map$lo.core*list@pairs'(XV18984, XV18985, XLbl1594, XThis1594).
'lo.collection$map$lo.core*list'('pairs%1'('lo.collection$map$lo.core*list^pairs'(XLbl1595, XThis1595)), XLbl1595, XThis1595).
'lo.collection$map$lo.core*list'('values%2'(XV18988, XV18989), XLbl1596, XThis1596):- !,
    'lo.collection$map$lo.core*list@values'(XV18988, XV18989, XLbl1596, XThis1596).
'lo.collection$map$lo.core*list'('values%1'('lo.collection$map$lo.core*list^values'(XLbl1597, XThis1597)), XLbl1597, XThis1597).
'lo.collection$map$lo.core*list'('_empty%1'(XV18990), XLbl1598, XThis1598):- !,
    'lo.collection$map$lo.core*list@_empty'(XV18990, XLbl1598, XThis1598).
'lo.collection$map$lo.core*list@present'(XL, XIx, XV, XLbV1516, XThV1516):- 'lo.collection$map$lo.core*list@one168'(XV, XIx, XL).
'lo.collection$map$lo.core*list@_remove'(XL, XIx, XXd22892, XLbV1516, XThV1516):- !,
    'lo.list@dropNth'(XL, XIx, XXd22892).
'lo.collection$map$lo.core*list@_remove'(_, _, _):- raise_exception('error'("lo.collection$map$lo.core*list@_remove", 89, 5, 30)).
'lo.collection$map$lo.core*list@_put'('lo.list^replaceNth', XLbV1516, XThV1516):- !.
'lo.collection$map$lo.core*list@keys'(XL, XXd22893, XLbV1516, XThV1516):- !,
    'lo.list@indexes'(XL, 0, XXd22893).
'lo.collection$map$lo.core*list@keys'(_, _):- raise_exception('error'("lo.collection$map$lo.core*list@keys", 91, 5, 23)).
'lo.collection$map$lo.core*list@pairs'(XL, XXd22894, XLbV1516, XThV1516):- !,
    'lo.list@listPairs'(XL, 0, XXd22894).
'lo.collection$map$lo.core*list@pairs'(_, _):- raise_exception('error'("lo.collection$map$lo.core*list@pairs", 92, 5, 26)).
'lo.collection$map$lo.core*list@values'(XL, XL, XLbV1516, XThV1516):- !.
'lo.collection$map$lo.core*list@values'(_, _):- raise_exception('error'("lo.collection$map$lo.core*list@values", 93, 5, 14)).
'lo.collection$map$lo.core*list@_empty'('lo.core#[]', XLbV1516, XThV1516):- !.
'lo.collection$membership$lo.core*list'('lo.collection$membership$lo.core*list%1'('lo.collection$membership$lo.core*list')):- !.
'lo.collection$membership$lo.core*list'('empty%1'(XV18991), XLbl1599, XThis1599):- !,
    'lo.collection$membership$lo.core*list@empty'(XV18991, XLbl1599, XThis1599).
'lo.collection$membership$lo.core*list'('addMem%3'(XV18995, XV18996, XV18997), XLbl1600, XThis1600):- !,
    'lo.collection$membership$lo.core*list@addMem'(XV18995, XV18996, XV18997, XLbl1600, XThis1600).
'lo.collection$membership$lo.core*list'('addMem%1'('lo.collection$membership$lo.core*list^addMem'(XLbl1601, XThis1601)), XLbl1601, XThis1601).
'lo.collection$membership$lo.core*list'('delMem%3'(XV19001, XV19002, XV19003), XLbl1602, XThis1602):- !,
    'lo.collection$membership$lo.core*list@delMem'(XV19001, XV19002, XV19003, XLbl1602, XThis1602).
'lo.collection$membership$lo.core*list'('delMem%1'('lo.collection$membership$lo.core*list^delMem'(XLbl1603, XThis1603)), XLbl1603, XThis1603).
'lo.collection$membership$lo.core*list'('in%2'(XV19008, XV19009), XLbl1604, XThis1604):- !,
    'lo.collection$membership$lo.core*list@in'(XV19008, XV19009, XLbl1604, XThis1604).
'lo.collection$membership$lo.core*list'('in%1'('lo.collection$membership$lo.core*list^in'(XLbl1605, XThis1605)), XLbl1605, XThis1605).
'lo.collection$membership$lo.core*list@empty'('lo.core#[]', XLbV1517, XThV1517):- !.
'lo.collection$membership$lo.core*list@addMem'(XL, Xe, 'lo.core#,..'(Xe, XL), XLbV1517, XThV1517):- !.
'lo.collection$membership$lo.core*list@addMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.core*list@addMem", 99, 5, 22)).
'lo.collection$membership$lo.core*list@delMem'(XL, Xe, XXd22896, XLbV1517, XThV1517):- !,
    'lo.list@subtract'(Xe, XL, XXd22896).
'lo.collection$membership$lo.core*list@delMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.core*list@delMem", 100, 5, 28)).
'lo.collection$membership$lo.core*list@in'(Xe, XL, XLbV1517, XThV1517):- 'lo.list@listEl'(Xe, XL).
'lo.list@lastEl'('lo.core#,..'(XX, 'lo.core#[]'), XX).
'lo.list@lastEl'('lo.core#,..'(X_19592, XL), XX):- 'lo.list@lastEl'(XL, XX).
'lo.list@mixin'('lo.core#[]', X_19593, 'lo.core#[]'):- !.
'lo.list@mixin'('lo.core#,..'(XF, XL), XS, 'lo.core#,..'(XS, 'lo.core#,..'(XF, XXd22897))):- !,
    'lo.list@mixin'(XL, XS, XXd22897).
'lo.list@mixin'(_, _, _):- raise_exception('error'("lo.list@mixin", 135, 3, 15)).
'lo.list@interleave'('lo.core#[]', X_19597, 'lo.core#[]'):- !.
'lo.list@interleave'('lo.core#,..'(XF, XL), XS, 'lo.core#,..'(XF, XXd22900)):- !,
    'lo.list@mixin'(XL, XS, XXd22900).
'lo.list@interleave'(_, _, _):- raise_exception('error'("lo.list@interleave", 131, 3, 20)).
'lo.list@zip'('lo.core#[]', X_19600, 'lo.core#[]'):- !.
'lo.list@zip'(X_19601, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.list@zip'('lo.core#,..'(Xe1, Xl1), 'lo.core#,..'(Xe2, Xl2), 'lo.core#,..'('()2'(Xe1, Xe2), XXd22902)):- !,
    'lo.list@zip'(Xl1, Xl2, XXd22902).
'lo.list@zip'(_, _, _):- raise_exception('error'("lo.list@zip", 139, 3, 15)).
'lo.list@unzip'('lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.list@unzip'('lo.core#,..'('()2'(Xe, Xf), Xl), 'lo.core#,..'(Xe, Xr1), 'lo.core#,..'(Xf, Xr2)):- 'lo.list@unzip'(Xl, Xr1, Xr2).
'lo.list@iota'(XF, XT, 'lo.core#[]'):- 'lo.core@>'('lo.core$comp$lo.core*integer', XF, XT),
    !.
'lo.list@iota'(XF, XT, 'lo.core#,..'(XF, XXd22904)):- 'lo.core@=<'('lo.core$comp$lo.core*integer', XF, XT),
    !,
    ocall('+%1'(XXV2958),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XF, 1, XXe2749),XXV2958,XXV2958),
    'lo.list@iota'(XXe2749, XT, XXd22904).
'lo.list@iota'(_, _, _):- raise_exception('error'("lo.list@iota", 148, 3, 22)).
'lo.list@head'('lo.core#,..'(XE, X_19610), XE):- !.
'lo.list@head'(_, _):- raise_exception('error'("lo.list@head", 152, 3, 18)).
'lo.list@tail'('lo.core#,..'(X_19612, XL), XL):- !.
'lo.list@tail'(_, _):- raise_exception('error'("lo.list@tail", 155, 3, 18)).
'lo.list^<>'('_call%3'(XV18873, XV18874, XV18875), 'lo.list^<>', _):- 'lo.list@<>'(XV18873, XV18874, XV18875).
'lo.list^listEl'('_call%2'(XV18876, XV18877), 'lo.list^listEl', _):- 'lo.list@listEl'(XV18876, XV18877).
'lo.list^merge'('_call%3'(XV18878, XV18879, XV18880), 'lo.list^merge', _):- 'lo.list@merge'(XV18878, XV18879, XV18880).
'lo.list^cat'('_call%3'(XV18881, XV18882, XV18883), 'lo.list^cat', _):- 'lo.list@cat'(XV18881, XV18882, XV18883).
'lo.list^multiCat'('_call%2'(XV18884, XV18885), 'lo.list^multiCat', _):- 'lo.list@multiCat'(XV18884, XV18885).
'lo.list^rev'('_call%3'(XV18886, XV18887, XV18888), 'lo.list^rev', _):- 'lo.list@rev'(XV18886, XV18887, XV18888).
'lo.list^reverse'('_call%2'(XV18889, XV18890), 'lo.list^reverse', _):- 'lo.list@reverse'(XV18889, XV18890).
'lo.list^drop'('_call%2'(XV18891, XV18892), 'lo.list^drop', _):- 'lo.list@drop'(XV18891, XV18892).
'lo.list@neg184'(XX, XE):- XE = XX,
    !,
    fail.
'lo.list@neg184'(XX, XE).
'lo.list^subtract'('_call%3'(XV18893, XV18894, XV18895), 'lo.list^subtract', _):- 'lo.list@subtract'(XV18893, XV18894, XV18895).
'lo.list^length'('_call%2'(XV18896, XV18897), 'lo.list^length', _):- 'lo.list@length'(XV18896, XV18897).
'lo.core$sizeable$lo.core*list^size'('_call%2'(XV18898, XV18899), 'lo.core$sizeable$lo.core*list^size'(XLbV1511, XThV1511), _):- 'lo.core$sizeable$lo.core*list@size'(XV18898, XV18899, XLbV1511, XThV1511).
'lo.core$sizeable$lo.core*list^isEmpty'('_call%3'(XV18902, XV18903, XV18904), 'lo.core$sizeable$lo.core*list^isEmpty'(XLbV1511, XThV1511), _):- 'lo.core$sizeable$lo.core*list@isEmpty'(XV18902, XV18903, XV18904, XLbV1511, XThV1511).
'lo.collection$folding$lo.core*list^foldLeft'('_call%4'(XV18906, XV18907, XV18908, XV18909), 'lo.collection$folding$lo.core*list^foldLeft'(XLbV1512, XThV1512), _):- 'lo.collection$folding$lo.core*list@foldLeft'(XV18906, XV18907, XV18908, XV18909, XLbV1512, XThV1512).
'lo.collection$folding$lo.core*list^foldRight'('_call%4'(XV18914, XV18915, XV18916, XV18917), 'lo.collection$folding$lo.core*list^foldRight'(XLbV1512, XThV1512), _):- 'lo.collection$folding$lo.core*list@foldRight'(XV18914, XV18915, XV18916, XV18917, XLbV1512, XThV1512).
'lo.list^mapList'('_call%3'(XV18922, XV18923, XV18924), 'lo.list^mapList', _):- 'lo.list@mapList'(XV18922, XV18923, XV18924).
'lo.collection$mapping$lo.core*list^//'('_call%3'(XV18925, XV18926, XV18927), 'lo.collection$mapping$lo.core*list^//'(XLbV1513, XThV1513), _):- 'lo.collection$mapping$lo.core*list@//'(XV18925, XV18926, XV18927, XLbV1513, XThV1513).
'lo.list^filterList'('_call%3'(XV18931, XV18932, XV18933), 'lo.list^filterList', _):- 'lo.list@filterList'(XV18931, XV18932, XV18933).
'lo.collection$filter$lo.core*list^^/'('_call%3'(XV18934, XV18935, XV18936), 'lo.collection$filter$lo.core*list^^/'(XLbV1514, XThV1514), _):- 'lo.collection$filter$lo.core*list@^/'(XV18934, XV18935, XV18936, XLbV1514, XThV1514).
'lo.list^coerceList'('_call%3'(XV18940, XV18941, XV18942), 'lo.list^coerceList', _):- 'lo.list@coerceList'(XV18940, XV18941, XV18942).
'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'('_call%2'(XV18943, XV18944), 'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'(XLbV1515, XThV1515), _):- 'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XV18943, XV18944, XLbV1515, XThV1515).
'lo.list^listPairs'('_call%3'(XV18947, XV18948, XV18949), 'lo.list^listPairs', _):- 'lo.list@listPairs'(XV18947, XV18948, XV18949).
'lo.list^indexes'('_call%3'(XV18950, XV18951, XV18952), 'lo.list^indexes', _):- 'lo.list@indexes'(XV18950, XV18951, XV18952).
'lo.list^replaceNth'('_call%4'(XV18953, XV18954, XV18955, XV18956), 'lo.list^replaceNth', _):- 'lo.list@replaceNth'(XV18953, XV18954, XV18955, XV18956).
'lo.list^dropNth'('_call%3'(XV18957, XV18958, XV18959), 'lo.list^dropNth', _):- 'lo.list@dropNth'(XV18957, XV18958, XV18959).
'lo.list^nthEl'('_call%3'(XV18960, XV18961, XV18962), 'lo.list^nthEl', _):- 'lo.list@nthEl'(XV18960, XV18961, XV18962).
'lo.collection$map$lo.core*list@one168'(XV, XIx, XL):- 'lo.list@nthEl'(XL, XIx, XV),
    !.
'lo.collection$map$lo.core*list^present'('_call%5'(XV18963, XV18964, XV18965, XV18966, XV18967), 'lo.collection$map$lo.core*list^present'(XLbV1516, XThV1516), _):- 'lo.collection$map$lo.core*list@present'(XV18963, XV18964, XV18965, XV18966, XV18967, XLbV1516, XThV1516).
'lo.collection$map$lo.core*list^_remove'('_call%3'(XV18971, XV18972, XV18973), 'lo.collection$map$lo.core*list^_remove'(XLbV1516, XThV1516), _):- 'lo.collection$map$lo.core*list@_remove'(XV18971, XV18972, XV18973, XLbV1516, XThV1516).
'lo.collection$map$lo.core*list^keys'('_call%2'(XV18978, XV18979), 'lo.collection$map$lo.core*list^keys'(XLbV1516, XThV1516), _):- 'lo.collection$map$lo.core*list@keys'(XV18978, XV18979, XLbV1516, XThV1516).
'lo.collection$map$lo.core*list^pairs'('_call%2'(XV18982, XV18983), 'lo.collection$map$lo.core*list^pairs'(XLbV1516, XThV1516), _):- 'lo.collection$map$lo.core*list@pairs'(XV18982, XV18983, XLbV1516, XThV1516).
'lo.collection$map$lo.core*list^values'('_call%2'(XV18986, XV18987), 'lo.collection$map$lo.core*list^values'(XLbV1516, XThV1516), _):- 'lo.collection$map$lo.core*list@values'(XV18986, XV18987, XLbV1516, XThV1516).
'lo.collection$membership$lo.core*list^addMem'('_call%3'(XV18992, XV18993, XV18994), 'lo.collection$membership$lo.core*list^addMem'(XLbV1517, XThV1517), _):- 'lo.collection$membership$lo.core*list@addMem'(XV18992, XV18993, XV18994, XLbV1517, XThV1517).
'lo.collection$membership$lo.core*list^delMem'('_call%3'(XV18998, XV18999, XV19000), 'lo.collection$membership$lo.core*list^delMem'(XLbV1517, XThV1517), _):- 'lo.collection$membership$lo.core*list@delMem'(XV18998, XV18999, XV19000, XLbV1517, XThV1517).
'lo.collection$membership$lo.core*list^in'('_call%4'(XV19004, XV19005, XV19006, XV19007), 'lo.collection$membership$lo.core*list^in'(XLbV1517, XThV1517), _):- 'lo.collection$membership$lo.core*list@in'(XV19004, XV19005, XV19006, XV19007, XLbV1517, XThV1517).
'lo.list^lastEl'('_call%2'(XV19010, XV19011), 'lo.list^lastEl', _):- 'lo.list@lastEl'(XV19010, XV19011).
'lo.list^mixin'('_call%3'(XV19012, XV19013, XV19014), 'lo.list^mixin', _):- 'lo.list@mixin'(XV19012, XV19013, XV19014).
'lo.list^interleave'('_call%3'(XV19015, XV19016, XV19017), 'lo.list^interleave', _):- 'lo.list@interleave'(XV19015, XV19016, XV19017).
'lo.list^zip'('_call%3'(XV19018, XV19019, XV19020), 'lo.list^zip', _):- 'lo.list@zip'(XV19018, XV19019, XV19020).
'lo.list^unzip'('_call%3'(XV19021, XV19022, XV19023), 'lo.list^unzip', _):- 'lo.list@unzip'(XV19021, XV19022, XV19023).
'lo.list^iota'('_call%3'(XV19024, XV19025, XV19026), 'lo.list^iota', _):- 'lo.list@iota'(XV19024, XV19025, XV19026).
'lo.list^head'('_call%2'(XV19027, XV19028), 'lo.list^head', _):- 'lo.list@head'(XV19027, XV19028).
'lo.list^tail'('_call%2'(XV19029, XV19030), 'lo.list^tail', _):- 'lo.list@tail'(XV19029, XV19030).
