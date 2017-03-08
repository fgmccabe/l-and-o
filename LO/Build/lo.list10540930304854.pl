'#pkg'("n7o7'()7'n2o2'pkg's'lo.list's'1.0.0'n3o3'()3'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'s\"I17'<>':k't'FT2Lk't'Lk't'Lk't''listEl':k't'PT2k't'Lk't''merge':k't'FT2Lk't'Lk't'Lk't''multiCat':k'x'FT1LLk'x'Lk'x''reverse':k't'FT1Lk't'Lk't''drop':k't'FT1Lk't'Lk't''subtract':k't'FT2k't'Lk't'Lk't''length':k't'FT1Lk't'i'mapList':k'v':k'w'FT2Lk'v'FT1k'v'k'w'Lk'w''nthEl':k'e'PT3Lk'e'ik'e''lastEl':k'e'PT2Lk'e'k'e''interleave':k'x'FT2Lk'x'k'x'Lk'x''zip':k'e':k'f'FT2Lk'e'Lk'f'LT2k'e'k'f''unzip':k'e':k'f'PT3LT2k'e'k'f'Lk'e'Lk'f''iota'FT2iiLi'head':k'e'FT1Lk'e'k'e''tail':k'e'FT1Lk'e'Lk'e'\"s'I0'n0o0'()0'n0o0'()0'n7o7'()7'n2o2'()2's'lo.core$sizeable$lo.core*list's\":k'c'c'lo.core$sizeable'T1Lk'c'T0\"n2o2'()2's'lo.collection$folding$lo.core*list's\":k'e'c'lo.collection$folding'T1Lk'e'T1k'e'\"n2o2'()2's'lo.collection$mapping$lo.core*list's\":k'e':k'f'c'lo.collection$mapping'T1z1'lo.core*list'T2k'e'k'f'\"n2o2'()2's'lo.collection$filter$lo.core*list's\":k'e'c'lo.collection$filter'T1Lk'e'T1k'e'\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.core*list's\":k'e':k'f'|c'lo.coerce$coercion'T2Lk'e'Lk'f'T0c'lo.coerce$coercion'T2k'e'k'f'T0\"n2o2'()2's'lo.collection$map$lo.core*list's\":k'e'c'lo.collection$map'T1Lk'e'T2ik'e'\"n2o2'()2's'lo.collection$membership$lo.core*list's\":k'e'c'lo.collection$membership'T1Lk'e'T1k'e'\"").
'lo.list@init'():- !.
'lo.list@<>'('lo.core#[]', XX, XX):- !.
'lo.list@<>'('lo.core#,..'(XE, XX), XY, 'lo.core#,..'(XE, XXd8440)):- !,
    'lo.list@<>'(XX, XY, XXd8440).
'lo.list@<>'(_, _, _):- raise_exception('error'("lo.list@<>", 8, 3, 12)).
'lo.list@listEl'(XX, 'lo.core#,..'(XX, X_5198)).
'lo.list@listEl'(XX, 'lo.core#,..'(X_5200, XY)):- 'lo.list@listEl'(XX, XY).
'lo.list@merge'('lo.core#[]', XX, XX):- !.
'lo.list@merge'('lo.core#,..'(Xe, Xl), Xx, XXd8442):- 'lo.list@listEl'(Xe, Xx),
    !,
    'lo.list@merge'(Xl, Xx, XXd8442).
'lo.list@merge'('lo.core#,..'(Xe, Xl), Xx, 'lo.core#,..'(Xe, XXd8443)):- !,
    'lo.list@merge'(Xl, Xx, XXd8443).
'lo.list@merge'(_, _, _):- raise_exception('error'("lo.list@merge", 16, 3, 16)).
'lo.list@cat'('lo.core#[]', XM, XXd8445):- !,
    'lo.list@multiCat'(XM, XXd8445).
'lo.list@cat'('lo.core#,..'(XE, XL), XM, 'lo.core#,..'(XE, XXd8446)):- !,
    'lo.list@cat'(XL, XM, XXd8446).
'lo.list@cat'(_, _, _):- raise_exception('error'("lo.list@cat", 25, 3, 24)).
'lo.list@multiCat'('lo.core#[]', 'lo.core#[]'):- !.
'lo.list@multiCat'('lo.core#,..'(XE, XL), XXd8448):- !,
    'lo.list@cat'(XE, XL, XXd8448).
'lo.list@multiCat'(_, _):- raise_exception('error'("lo.list@multiCat", 21, 3, 18)).
'lo.list@rev'('lo.core#[]', XR, XR):- !.
'lo.list@rev'('lo.core#,..'(XE, XL), XR, XXd8450):- !,
    'lo.list@rev'(XL, 'lo.core#,..'(XE, XR), XXd8450).
'lo.list@rev'(_, _, _):- raise_exception('error'("lo.list@rev", 33, 3, 14)).
'lo.list@reverse'(XX, XXd8451):- !,
    'lo.list@rev'(XX, 'lo.core#[]', XXd8451).
'lo.list@reverse'(_, _):- raise_exception('error'("lo.list@reverse", 29, 3, 23)).
'lo.list@drop'('lo.core#,..'(X_5210, XL), XL):- !.
'lo.list@drop'(_, _):- raise_exception('error'("lo.list@drop", 37, 3, 18)).
'lo.list@subtract'(X_5211, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.list@subtract'(XE, 'lo.core#,..'(XE, XR), XR):- !.
'lo.list@subtract'(XE, 'lo.core#,..'(XX, XR), 'lo.core#,..'(XX, XXd8452)):- 'lo.list@neg38'(XX, XE),
    !,
    'lo.list@subtract'(XE, XR, XXd8452).
'lo.list@subtract'(_, _, _):- raise_exception('error'("lo.list@subtract", 40, 3, 20)).
'lo.list@length'('lo.core#[]', 0):- !.
'lo.list@length'('lo.core#,..'(X_5216, XL), XXe1819):- !,
    ocall('+%1'(XXV1842),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XL, XXd8454),
    ocall('_call%3'(XXd8454, 1, XXe1819),XXV1842,XXV1842).
'lo.list@length'(_, _):- raise_exception('error'("lo.list@length", 46, 3, 15)).
'lo.core$sizeable$lo.core*list'('lo.core$sizeable$lo.core*list%1'('lo.core$sizeable$lo.core*list')):- !.
'lo.core$sizeable$lo.core*list'('size%2'(XV17611, XV17612), XLbl3671, XThis3671):- !,
    'lo.core$sizeable$lo.core*list@size'(XV17611, XV17612, XLbl3671, XThis3671).
'lo.core$sizeable$lo.core*list'('size%1'('lo.core$sizeable$lo.core*list^size'(XLbl3672, XThis3672)), XLbl3672, XThis3672).
'lo.core$sizeable$lo.core*list'('isEmpty%1'(XV17616), XLbl3673, XThis3673):- !,
    'lo.core$sizeable$lo.core*list@isEmpty'(XV17616, XLbl3673, XThis3673).
'lo.core$sizeable$lo.core*list'('isEmpty%1'('lo.core$sizeable$lo.core*list^isEmpty'(XLbl3674, XThis3674)), XLbl3674, XThis3674).
'lo.core$sizeable$lo.core*list@size'(XL, XXd8455, XLbV1600, XThV1600):- !,
    'lo.list@length'(XL, XXd8455).
'lo.core$sizeable$lo.core*list@size'(_, _):- raise_exception('error'("lo.core$sizeable$lo.core*list@size", 50, 5, 20)).
'lo.core$sizeable$lo.core*list@isEmpty'('lo.core#[]', XLbV1600, XThV1600).
'lo.collection$folding$lo.core*list'('lo.collection$folding$lo.core*list%1'('lo.collection$folding$lo.core*list')):- !.
'lo.collection$folding$lo.core*list'('foldLeft%4'(XV17621, XV17622, XV17623, XV17624), XLbl3675, XThis3675):- !,
    'lo.collection$folding$lo.core*list@foldLeft'(XV17621, XV17622, XV17623, XV17624, XLbl3675, XThis3675).
'lo.collection$folding$lo.core*list'('foldLeft%1'('lo.collection$folding$lo.core*list^foldLeft'(XLbl3676, XThis3676)), XLbl3676, XThis3676).
'lo.collection$folding$lo.core*list'('foldRight%4'(XV17629, XV17630, XV17631, XV17632), XLbl3677, XThis3677):- !,
    'lo.collection$folding$lo.core*list@foldRight'(XV17629, XV17630, XV17631, XV17632, XLbl3677, XThis3677).
'lo.collection$folding$lo.core*list'('foldRight%1'('lo.collection$folding$lo.core*list^foldRight'(XLbl3678, XThis3678)), XLbl3678, XThis3678).
'lo.collection$folding$lo.core*list@foldLeft'(Xf, Xx, 'lo.core#[]', Xx, XLbV1601, XThV1601):- !.
'lo.collection$folding$lo.core*list@foldLeft'(Xf, Xx, 'lo.core#,..'(Xe, Xl), XXd8457, XLbV1601, XThV1601):- !,
    ocall('_call%3'(Xx, Xe, XXe1820),Xf,Xf),
    'lo.collection$folding$lo.core*list@foldLeft'(Xf, XXe1820, Xl, XXd8457, XLbV1601, XThV1601).
'lo.collection$folding$lo.core*list@foldLeft'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.core*list@foldLeft", 55, 5, 21)).
'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, 'lo.core#[]', Xx, XLbV1601, XThV1601):- !.
'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, 'lo.core#,..'(Xe, Xl), XXe1821, XLbV1601, XThV1601):- !,
    'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, Xl, XXd8458, XLbV1601, XThV1601),
    ocall('_call%3'(Xe, XXd8458, XXe1821),Xf,Xf).
'lo.collection$folding$lo.core*list@foldRight'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.core*list@foldRight", 58, 5, 22)).
'lo.list@mapList'('lo.core#[]', X_5219, 'lo.core#[]'):- !.
'lo.list@mapList'('lo.core#,..'(Xe, Xl), Xf, 'lo.core#,..'(XXe1822, XXd8461)):- !,
    ocall('_call%2'(Xe, XXe1822),Xf,Xf),
    'lo.list@mapList'(Xl, Xf, XXd8461).
'lo.list@mapList'(_, _, _):- raise_exception('error'("lo.list@mapList", 67, 3, 19)).
'lo.collection$mapping$lo.core*list'('lo.collection$mapping$lo.core*list%1'('lo.collection$mapping$lo.core*list')):- !.
'lo.collection$mapping$lo.core*list'('//%3'(XV17639, XV17640, XV17641), XLbl3679, XThis3679):- !,
    'lo.collection$mapping$lo.core*list@//'(XV17639, XV17640, XV17641, XLbl3679, XThis3679).
'lo.collection$mapping$lo.core*list'('//%1'('lo.collection$mapping$lo.core*list^//'(XLbl3680, XThis3680)), XLbl3680, XThis3680).
'lo.collection$mapping$lo.core*list@//'(XL, XF, XXd8463, XLbV1602, XThV1602):- !,
    'lo.list@mapList'(XL, XF, XXd8463).
'lo.collection$mapping$lo.core*list@//'(_, _, _):- raise_exception('error'("lo.collection$mapping$lo.core*list@//", 63, 5, 20)).
'lo.list@filterList'('lo.core#[]', X_5222, 'lo.core#[]'):- !.
'lo.list@filterList'('lo.core#,..'(Xe, Xl), Xp, 'lo.core#,..'(Xe, XXd8464)):- ocall('_call%1'(Xe),Xp,Xp),
    !,
    'lo.list@filterList'(Xl, Xp, XXd8464).
'lo.list@filterList'('lo.core#,..'(X_5226, Xl), Xp, XXd8466):- !,
    'lo.list@filterList'(Xl, Xp, XXd8466).
'lo.list@filterList'(_, _, _):- raise_exception('error'("lo.list@filterList", 75, 3, 22)).
'lo.collection$filter$lo.core*list'('lo.collection$filter$lo.core*list%1'('lo.collection$filter$lo.core*list')):- !.
'lo.collection$filter$lo.core*list'('^/%3'(XV17648, XV17649, XV17650), XLbl3681, XThis3681):- !,
    'lo.collection$filter$lo.core*list@^/'(XV17648, XV17649, XV17650, XLbl3681, XThis3681).
'lo.collection$filter$lo.core*list'('^/%1'('lo.collection$filter$lo.core*list^^/'(XLbl3682, XThis3682)), XLbl3682, XThis3682).
'lo.collection$filter$lo.core*list@^/'(XL, XP, XXd8467, XLbV1603, XThV1603):- !,
    'lo.list@filterList'(XL, XP, XXd8467).
'lo.collection$filter$lo.core*list@^/'(_, _, _):- raise_exception('error'("lo.collection$filter$lo.core*list@^/", 71, 5, 23)).
'lo.list@coerceList'(Xcoercion68, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.list@coerceList'(Xcoercion68, 'lo.core#,..'(Xe, Xl), 'lo.core#,..'(XXe1823, XXd8468)):- !,
    ocall('_coerce%1'(XXV1843),Xcoercion68,Xcoercion68),
    ocall('_call%2'(Xe, XXe1823),XXV1843,XXV1843),
    'lo.list@coerceList'(Xcoercion68, Xl, XXd8468).
'lo.list@coerceList'(_, _, _):- raise_exception('error'("lo.list@coerceList", 84, 3, 20)).
'lo.coerce$coercion$lo.core*list$lo.core*list'('lo.coerce$coercion$lo.core*list$lo.core*list%1'('lo.coerce$coercion$lo.core*list$lo.core*list')):- !.
'lo.coerce$coercion$lo.core*list$lo.core*list'('_coerce%2'(XV17656, XV17657), XLbl3683, XThis3683):- !,
    'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XV17656, XV17657, XLbl3683, XThis3683).
'lo.coerce$coercion$lo.core*list$lo.core*list'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'(XLbl3684, XThis3684)), XLbl3684, XThis3684).
'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XSx, XXd8470, XLbV1604, XThV1604):- XLbV1604 = 'lo.coerce$coercion$lo.core*list$lo.core*list'(Xcoercion69),
    !,
    'lo.list@coerceList'(Xcoercion69, XSx, XXd8470).
'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.core*list@_coerce", 80, 5, 29)).
'lo.list@listPairs'('lo.core#[]', X_5229, 'lo.core#[]'):- !.
'lo.list@listPairs'('lo.core#,..'(XE, XL), XIx, 'lo.core#,..'('()2'(XIx, XE), XXd8471)):- !,
    ocall('+%1'(XXV1844),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1824),XXV1844,XXV1844),
    'lo.list@listPairs'(XL, XXe1824, XXd8471).
'lo.list@listPairs'(_, _, _):- raise_exception('error'("lo.list@listPairs", 127, 3, 21)).
'lo.list@indexes'('lo.core#[]', X_5232, 'lo.core#[]'):- !.
'lo.list@indexes'('lo.core#,..'(X_5234, XL), XIx, 'lo.core#,..'(XIx, XXd8473)):- !,
    ocall('+%1'(XXV1845),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1825),XXV1845,XXV1845),
    'lo.list@indexes'(XL, XXe1825, XXd8473).
'lo.list@indexes'(_, _, _):- raise_exception('error'("lo.list@indexes", 123, 3, 19)).
'lo.list@replaceNth'('lo.core#,..'(X_5237, XL), 0, Xe, 'lo.core#,..'(Xe, XL)):- !.
'lo.list@replaceNth'('lo.core#[]', 0, Xe, 'lo.core#,..'(Xe, 'lo.core#[]')):- !.
'lo.list@replaceNth'('lo.core#,..'(XE, XL), XIx, Xe, 'lo.core#,..'(XE, XXd8477)):- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    !,
    ocall('-%1'(XXV1846),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1826),XXV1846,XXV1846),
    'lo.list@replaceNth'(XL, XXe1826, Xe, XXd8477).
'lo.list@replaceNth'(_, _, _, _):- raise_exception('error'("lo.list@replaceNth", 114, 3, 34)).
'lo.list@dropNth'('lo.core#[]', X_5242, 'lo.core#[]'):- !.
'lo.list@dropNth'('lo.core#,..'(X_5244, XL), 0, XL):- !.
'lo.list@dropNth'('lo.core#,..'(XE, XL), XIx, 'lo.core#,..'(XE, XXd8479)):- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    !,
    ocall('-%1'(XXV1847),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1827),XXV1847,XXV1847),
    'lo.list@dropNth'(XL, XXe1827, XXd8479).
'lo.list@dropNth'(_, _, _):- raise_exception('error'("lo.list@dropNth", 109, 3, 19)).
'lo.list@nthEl'('lo.core#,..'(Xe, X_5248), 0, Xe).
'lo.list@nthEl'('lo.core#,..'(X_5250, XL), XIx, Xe):- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    ocall('-%1'(XXV1848),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1828),XXV1848,XXV1848),
    'lo.list@nthEl'(XL, XXe1828, Xe).
'lo.collection$map$lo.core*list'('lo.collection$map$lo.core*list%1'('lo.collection$map$lo.core*list')):- !.
'lo.collection$map$lo.core*list'('present%3'(XV17679, XV17680, XV17681), XLbl3685, XThis3685):- !,
    'lo.collection$map$lo.core*list@present'(XV17679, XV17680, XV17681, XLbl3685, XThis3685).
'lo.collection$map$lo.core*list'('present%1'('lo.collection$map$lo.core*list^present'(XLbl3686, XThis3686)), XLbl3686, XThis3686).
'lo.collection$map$lo.core*list'('_remove%3'(XV17685, XV17686, XV17687), XLbl3687, XThis3687):- !,
    'lo.collection$map$lo.core*list@_remove'(XV17685, XV17686, XV17687, XLbl3687, XThis3687).
'lo.collection$map$lo.core*list'('_remove%1'('lo.collection$map$lo.core*list^_remove'(XLbl3688, XThis3688)), XLbl3688, XThis3688).
'lo.collection$map$lo.core*list'('_put%1'(XV17688), XLbl3689, XThis3689):- !,
    'lo.collection$map$lo.core*list@_put'(XV17688, XLbl3689, XThis3689).
'lo.collection$map$lo.core*list'('keys%2'(XV17691, XV17692), XLbl3690, XThis3690):- !,
    'lo.collection$map$lo.core*list@keys'(XV17691, XV17692, XLbl3690, XThis3690).
'lo.collection$map$lo.core*list'('keys%1'('lo.collection$map$lo.core*list^keys'(XLbl3691, XThis3691)), XLbl3691, XThis3691).
'lo.collection$map$lo.core*list'('pairs%2'(XV17695, XV17696), XLbl3692, XThis3692):- !,
    'lo.collection$map$lo.core*list@pairs'(XV17695, XV17696, XLbl3692, XThis3692).
'lo.collection$map$lo.core*list'('pairs%1'('lo.collection$map$lo.core*list^pairs'(XLbl3693, XThis3693)), XLbl3693, XThis3693).
'lo.collection$map$lo.core*list'('values%2'(XV17699, XV17700), XLbl3694, XThis3694):- !,
    'lo.collection$map$lo.core*list@values'(XV17699, XV17700, XLbl3694, XThis3694).
'lo.collection$map$lo.core*list'('values%1'('lo.collection$map$lo.core*list^values'(XLbl3695, XThis3695)), XLbl3695, XThis3695).
'lo.collection$map$lo.core*list'('_empty%1'(XV17701), XLbl3696, XThis3696):- !,
    'lo.collection$map$lo.core*list@_empty'(XV17701, XLbl3696, XThis3696).
'lo.collection$map$lo.core*list@present'(XL, XIx, XV, XLbV1605, XThV1605):- 'lo.collection$map$lo.core*list@one62'(XV, XIx, XL).
'lo.collection$map$lo.core*list@_remove'(XL, XIx, XXd8481, XLbV1605, XThV1605):- !,
    'lo.list@dropNth'(XL, XIx, XXd8481).
'lo.collection$map$lo.core*list@_remove'(_, _, _):- raise_exception('error'("lo.collection$map$lo.core*list@_remove", 89, 5, 30)).
'lo.collection$map$lo.core*list@_put'('lo.list^replaceNth', XLbV1605, XThV1605):- !.
'lo.collection$map$lo.core*list@keys'(XL, XXd8482, XLbV1605, XThV1605):- !,
    'lo.list@indexes'(XL, 0, XXd8482).
'lo.collection$map$lo.core*list@keys'(_, _):- raise_exception('error'("lo.collection$map$lo.core*list@keys", 91, 5, 23)).
'lo.collection$map$lo.core*list@pairs'(XL, XXd8483, XLbV1605, XThV1605):- !,
    'lo.list@listPairs'(XL, 0, XXd8483).
'lo.collection$map$lo.core*list@pairs'(_, _):- raise_exception('error'("lo.collection$map$lo.core*list@pairs", 92, 5, 26)).
'lo.collection$map$lo.core*list@values'(XL, XL, XLbV1605, XThV1605):- !.
'lo.collection$map$lo.core*list@values'(_, _):- raise_exception('error'("lo.collection$map$lo.core*list@values", 93, 5, 14)).
'lo.collection$map$lo.core*list@_empty'('lo.core#[]', XLbV1605, XThV1605):- !.
'lo.collection$membership$lo.core*list'('lo.collection$membership$lo.core*list%1'('lo.collection$membership$lo.core*list')):- !.
'lo.collection$membership$lo.core*list'('empty%1'(XV17702), XLbl3697, XThis3697):- !,
    'lo.collection$membership$lo.core*list@empty'(XV17702, XLbl3697, XThis3697).
'lo.collection$membership$lo.core*list'('addMem%3'(XV17706, XV17707, XV17708), XLbl3698, XThis3698):- !,
    'lo.collection$membership$lo.core*list@addMem'(XV17706, XV17707, XV17708, XLbl3698, XThis3698).
'lo.collection$membership$lo.core*list'('addMem%1'('lo.collection$membership$lo.core*list^addMem'(XLbl3699, XThis3699)), XLbl3699, XThis3699).
'lo.collection$membership$lo.core*list'('delMem%3'(XV17712, XV17713, XV17714), XLbl3700, XThis3700):- !,
    'lo.collection$membership$lo.core*list@delMem'(XV17712, XV17713, XV17714, XLbl3700, XThis3700).
'lo.collection$membership$lo.core*list'('delMem%1'('lo.collection$membership$lo.core*list^delMem'(XLbl3701, XThis3701)), XLbl3701, XThis3701).
'lo.collection$membership$lo.core*list'('in%2'(XV17719, XV17720), XLbl3702, XThis3702):- !,
    'lo.collection$membership$lo.core*list@in'(XV17719, XV17720, XLbl3702, XThis3702).
'lo.collection$membership$lo.core*list'('in%1'('lo.collection$membership$lo.core*list^in'(XLbl3703, XThis3703)), XLbl3703, XThis3703).
'lo.collection$membership$lo.core*list@empty'('lo.core#[]', XLbV1606, XThV1606):- !.
'lo.collection$membership$lo.core*list@addMem'(XL, Xe, 'lo.core#,..'(Xe, XL), XLbV1606, XThV1606):- !.
'lo.collection$membership$lo.core*list@addMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.core*list@addMem", 99, 5, 22)).
'lo.collection$membership$lo.core*list@delMem'(XL, Xe, XXd8485, XLbV1606, XThV1606):- !,
    'lo.list@subtract'(Xe, XL, XXd8485).
'lo.collection$membership$lo.core*list@delMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.core*list@delMem", 100, 5, 28)).
'lo.collection$membership$lo.core*list@in'(Xe, XL, XLbV1606, XThV1606):- 'lo.list@listEl'(Xe, XL).
'lo.list@lastEl'('lo.core#,..'(XX, 'lo.core#[]'), XX).
'lo.list@lastEl'('lo.core#,..'(X_5254, XL), XX):- 'lo.list@lastEl'(XL, XX).
'lo.list@mixin'('lo.core#[]', X_5255, 'lo.core#[]'):- !.
'lo.list@mixin'('lo.core#,..'(XF, XL), XS, 'lo.core#,..'(XS, 'lo.core#,..'(XF, XXd8486))):- !,
    'lo.list@mixin'(XL, XS, XXd8486).
'lo.list@mixin'(_, _, _):- raise_exception('error'("lo.list@mixin", 135, 3, 15)).
'lo.list@interleave'('lo.core#[]', X_5259, 'lo.core#[]'):- !.
'lo.list@interleave'('lo.core#,..'(XF, XL), XS, 'lo.core#,..'(XF, XXd8489)):- !,
    'lo.list@mixin'(XL, XS, XXd8489).
'lo.list@interleave'(_, _, _):- raise_exception('error'("lo.list@interleave", 131, 3, 20)).
'lo.list@zip'('lo.core#[]', X_5262, 'lo.core#[]'):- !.
'lo.list@zip'(X_5263, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.list@zip'('lo.core#,..'(Xe1, Xl1), 'lo.core#,..'(Xe2, Xl2), 'lo.core#,..'('()2'(Xe1, Xe2), XXd8491)):- !,
    'lo.list@zip'(Xl1, Xl2, XXd8491).
'lo.list@zip'(_, _, _):- raise_exception('error'("lo.list@zip", 139, 3, 15)).
'lo.list@unzip'('lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.list@unzip'('lo.core#,..'('()2'(Xe, Xf), Xl), 'lo.core#,..'(Xe, Xr1), 'lo.core#,..'(Xf, Xr2)):- 'lo.list@unzip'(Xl, Xr1, Xr2).
'lo.list@iota'(XF, XT, 'lo.core#[]'):- 'lo.core@>'('lo.core$comp$lo.core*integer', XF, XT),
    !.
'lo.list@iota'(XF, XT, 'lo.core#,..'(XF, XXd8493)):- 'lo.core@=<'('lo.core$comp$lo.core*integer', XF, XT),
    !,
    ocall('+%1'(XXV1849),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XF, 1, XXe1829),XXV1849,XXV1849),
    'lo.list@iota'(XXe1829, XT, XXd8493).
'lo.list@iota'(_, _, _):- raise_exception('error'("lo.list@iota", 148, 3, 22)).
'lo.list@head'('lo.core#,..'(XE, X_5272), XE):- !.
'lo.list@head'(_, _):- raise_exception('error'("lo.list@head", 152, 3, 18)).
'lo.list@tail'('lo.core#,..'(X_5274, XL), XL):- !.
'lo.list@tail'(_, _):- raise_exception('error'("lo.list@tail", 155, 3, 18)).
'lo.list^<>'('_call%3'(XV17584, XV17585, XV17586), 'lo.list^<>', _):- 'lo.list@<>'(XV17584, XV17585, XV17586).
'lo.list^listEl'('_call%2'(XV17587, XV17588), 'lo.list^listEl', _):- 'lo.list@listEl'(XV17587, XV17588).
'lo.list^merge'('_call%3'(XV17589, XV17590, XV17591), 'lo.list^merge', _):- 'lo.list@merge'(XV17589, XV17590, XV17591).
'lo.list^cat'('_call%3'(XV17592, XV17593, XV17594), 'lo.list^cat', _):- 'lo.list@cat'(XV17592, XV17593, XV17594).
'lo.list^multiCat'('_call%2'(XV17595, XV17596), 'lo.list^multiCat', _):- 'lo.list@multiCat'(XV17595, XV17596).
'lo.list^rev'('_call%3'(XV17597, XV17598, XV17599), 'lo.list^rev', _):- 'lo.list@rev'(XV17597, XV17598, XV17599).
'lo.list^reverse'('_call%2'(XV17600, XV17601), 'lo.list^reverse', _):- 'lo.list@reverse'(XV17600, XV17601).
'lo.list^drop'('_call%2'(XV17602, XV17603), 'lo.list^drop', _):- 'lo.list@drop'(XV17602, XV17603).
'lo.list@neg38'(XX, XE):- XE = XX,
    !,
    fail.
'lo.list@neg38'(XX, XE).
'lo.list^subtract'('_call%3'(XV17604, XV17605, XV17606), 'lo.list^subtract', _):- 'lo.list@subtract'(XV17604, XV17605, XV17606).
'lo.list^length'('_call%2'(XV17607, XV17608), 'lo.list^length', _):- 'lo.list@length'(XV17607, XV17608).
'lo.core$sizeable$lo.core*list^size'('_call%2'(XV17609, XV17610), 'lo.core$sizeable$lo.core*list^size'(XLbV1600, XThV1600), _):- 'lo.core$sizeable$lo.core*list@size'(XV17609, XV17610, XLbV1600, XThV1600).
'lo.core$sizeable$lo.core*list^isEmpty'('_call%3'(XV17613, XV17614, XV17615), 'lo.core$sizeable$lo.core*list^isEmpty'(XLbV1600, XThV1600), _):- 'lo.core$sizeable$lo.core*list@isEmpty'(XV17613, XV17614, XV17615, XLbV1600, XThV1600).
'lo.collection$folding$lo.core*list^foldLeft'('_call%4'(XV17617, XV17618, XV17619, XV17620), 'lo.collection$folding$lo.core*list^foldLeft'(XLbV1601, XThV1601), _):- 'lo.collection$folding$lo.core*list@foldLeft'(XV17617, XV17618, XV17619, XV17620, XLbV1601, XThV1601).
'lo.collection$folding$lo.core*list^foldRight'('_call%4'(XV17625, XV17626, XV17627, XV17628), 'lo.collection$folding$lo.core*list^foldRight'(XLbV1601, XThV1601), _):- 'lo.collection$folding$lo.core*list@foldRight'(XV17625, XV17626, XV17627, XV17628, XLbV1601, XThV1601).
'lo.list^mapList'('_call%3'(XV17633, XV17634, XV17635), 'lo.list^mapList', _):- 'lo.list@mapList'(XV17633, XV17634, XV17635).
'lo.collection$mapping$lo.core*list^//'('_call%3'(XV17636, XV17637, XV17638), 'lo.collection$mapping$lo.core*list^//'(XLbV1602, XThV1602), _):- 'lo.collection$mapping$lo.core*list@//'(XV17636, XV17637, XV17638, XLbV1602, XThV1602).
'lo.list^filterList'('_call%3'(XV17642, XV17643, XV17644), 'lo.list^filterList', _):- 'lo.list@filterList'(XV17642, XV17643, XV17644).
'lo.collection$filter$lo.core*list^^/'('_call%3'(XV17645, XV17646, XV17647), 'lo.collection$filter$lo.core*list^^/'(XLbV1603, XThV1603), _):- 'lo.collection$filter$lo.core*list@^/'(XV17645, XV17646, XV17647, XLbV1603, XThV1603).
'lo.list^coerceList'('_call%3'(XV17651, XV17652, XV17653), 'lo.list^coerceList', _):- 'lo.list@coerceList'(XV17651, XV17652, XV17653).
'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'('_call%2'(XV17654, XV17655), 'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'(XLbV1604, XThV1604), _):- 'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XV17654, XV17655, XLbV1604, XThV1604).
'lo.list^listPairs'('_call%3'(XV17658, XV17659, XV17660), 'lo.list^listPairs', _):- 'lo.list@listPairs'(XV17658, XV17659, XV17660).
'lo.list^indexes'('_call%3'(XV17661, XV17662, XV17663), 'lo.list^indexes', _):- 'lo.list@indexes'(XV17661, XV17662, XV17663).
'lo.list^replaceNth'('_call%4'(XV17664, XV17665, XV17666, XV17667), 'lo.list^replaceNth', _):- 'lo.list@replaceNth'(XV17664, XV17665, XV17666, XV17667).
'lo.list^dropNth'('_call%3'(XV17668, XV17669, XV17670), 'lo.list^dropNth', _):- 'lo.list@dropNth'(XV17668, XV17669, XV17670).
'lo.list^nthEl'('_call%3'(XV17671, XV17672, XV17673), 'lo.list^nthEl', _):- 'lo.list@nthEl'(XV17671, XV17672, XV17673).
'lo.collection$map$lo.core*list@one62'(XV, XIx, XL):- 'lo.list@nthEl'(XL, XIx, XV),
    !.
'lo.collection$map$lo.core*list^present'('_call%5'(XV17674, XV17675, XV17676, XV17677, XV17678), 'lo.collection$map$lo.core*list^present'(XLbV1605, XThV1605), _):- 'lo.collection$map$lo.core*list@present'(XV17674, XV17675, XV17676, XV17677, XV17678, XLbV1605, XThV1605).
'lo.collection$map$lo.core*list^_remove'('_call%3'(XV17682, XV17683, XV17684), 'lo.collection$map$lo.core*list^_remove'(XLbV1605, XThV1605), _):- 'lo.collection$map$lo.core*list@_remove'(XV17682, XV17683, XV17684, XLbV1605, XThV1605).
'lo.collection$map$lo.core*list^keys'('_call%2'(XV17689, XV17690), 'lo.collection$map$lo.core*list^keys'(XLbV1605, XThV1605), _):- 'lo.collection$map$lo.core*list@keys'(XV17689, XV17690, XLbV1605, XThV1605).
'lo.collection$map$lo.core*list^pairs'('_call%2'(XV17693, XV17694), 'lo.collection$map$lo.core*list^pairs'(XLbV1605, XThV1605), _):- 'lo.collection$map$lo.core*list@pairs'(XV17693, XV17694, XLbV1605, XThV1605).
'lo.collection$map$lo.core*list^values'('_call%2'(XV17697, XV17698), 'lo.collection$map$lo.core*list^values'(XLbV1605, XThV1605), _):- 'lo.collection$map$lo.core*list@values'(XV17697, XV17698, XLbV1605, XThV1605).
'lo.collection$membership$lo.core*list^addMem'('_call%3'(XV17703, XV17704, XV17705), 'lo.collection$membership$lo.core*list^addMem'(XLbV1606, XThV1606), _):- 'lo.collection$membership$lo.core*list@addMem'(XV17703, XV17704, XV17705, XLbV1606, XThV1606).
'lo.collection$membership$lo.core*list^delMem'('_call%3'(XV17709, XV17710, XV17711), 'lo.collection$membership$lo.core*list^delMem'(XLbV1606, XThV1606), _):- 'lo.collection$membership$lo.core*list@delMem'(XV17709, XV17710, XV17711, XLbV1606, XThV1606).
'lo.collection$membership$lo.core*list^in'('_call%4'(XV17715, XV17716, XV17717, XV17718), 'lo.collection$membership$lo.core*list^in'(XLbV1606, XThV1606), _):- 'lo.collection$membership$lo.core*list@in'(XV17715, XV17716, XV17717, XV17718, XLbV1606, XThV1606).
'lo.list^lastEl'('_call%2'(XV17721, XV17722), 'lo.list^lastEl', _):- 'lo.list@lastEl'(XV17721, XV17722).
'lo.list^mixin'('_call%3'(XV17723, XV17724, XV17725), 'lo.list^mixin', _):- 'lo.list@mixin'(XV17723, XV17724, XV17725).
'lo.list^interleave'('_call%3'(XV17726, XV17727, XV17728), 'lo.list^interleave', _):- 'lo.list@interleave'(XV17726, XV17727, XV17728).
'lo.list^zip'('_call%3'(XV17729, XV17730, XV17731), 'lo.list^zip', _):- 'lo.list@zip'(XV17729, XV17730, XV17731).
'lo.list^unzip'('_call%3'(XV17732, XV17733, XV17734), 'lo.list^unzip', _):- 'lo.list@unzip'(XV17732, XV17733, XV17734).
'lo.list^iota'('_call%3'(XV17735, XV17736, XV17737), 'lo.list^iota', _):- 'lo.list@iota'(XV17735, XV17736, XV17737).
'lo.list^head'('_call%2'(XV17738, XV17739), 'lo.list^head', _):- 'lo.list@head'(XV17738, XV17739).
'lo.list^tail'('_call%2'(XV17740, XV17741), 'lo.list^tail', _):- 'lo.list@tail'(XV17740, XV17741).
