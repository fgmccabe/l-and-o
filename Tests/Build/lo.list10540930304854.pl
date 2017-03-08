'#pkg'("n7o7'()7'n2o2'pkg's'lo.list's'1.0.0'n3o3'()3'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'s\"I17'<>':k't'FT2Lk't'Lk't'Lk't''listEl':k't'PT2k't'Lk't''merge':k't'FT2Lk't'Lk't'Lk't''multiCat':k'x'FT1LLk'x'Lk'x''reverse':k't'FT1Lk't'Lk't''drop':k't'FT1Lk't'Lk't''subtract':k't'FT2k't'Lk't'Lk't''length':k't'FT1Lk't'i'mapList':k'v':k'w'FT2Lk'v'FT1k'v'k'w'Lk'w''nthEl':k'e'PT3Lk'e'ik'e''lastEl':k'e'PT2Lk'e'k'e''interleave':k'x'FT2Lk'x'k'x'Lk'x''zip':k'e':k'f'FT2Lk'e'Lk'f'LT2k'e'k'f''unzip':k'e':k'f'PT3LT2k'e'k'f'Lk'e'Lk'f''iota'FT2iiLi'head':k'e'FT1Lk'e'k'e''tail':k'e'FT1Lk'e'Lk'e'\"s'I0'n0o0'()0'n0o0'()0'n7o7'()7'n2o2'()2's'lo.core$sizeable$lo.core*list's\":k'c'c'lo.core$sizeable'T1Lk'c'T0\"n2o2'()2's'lo.collection$folding$lo.core*list's\":k'e'c'lo.collection$folding'T1Lk'e'T1k'e'\"n2o2'()2's'lo.collection$mapping$lo.core*list's\":k'e':k'f'c'lo.collection$mapping'T1z1'lo.core*list'T2k'e'k'f'\"n2o2'()2's'lo.collection$filter$lo.core*list's\":k'e'c'lo.collection$filter'T1Lk'e'T1k'e'\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.core*list's\":k'e':k'f'|c'lo.coerce$coercion'T2Lk'e'Lk'f'T0c'lo.coerce$coercion'T2k'e'k'f'T0\"n2o2'()2's'lo.collection$map$lo.core*list's\":k'e'c'lo.collection$map'T1Lk'e'T2ik'e'\"n2o2'()2's'lo.collection$membership$lo.core*list's\":k'e'c'lo.collection$membership'T1Lk'e'T1k'e'\"").
'lo.list@init'():- !.
'lo.list@<>'('lo.core#[]', XX, XX):- !.
'lo.list@<>'('lo.core#,..'(XE, XX), XY, 'lo.core#,..'(XE, XXd9201)):- !,
    'lo.list@<>'(XX, XY, XXd9201).
'lo.list@<>'(_, _, _):- raise_exception('error'("lo.list@<>", 8, 3, 12)).
'lo.list@listEl'(XX, 'lo.core#,..'(XX, X_5682)).
'lo.list@listEl'(XX, 'lo.core#,..'(X_5684, XY)):- 'lo.list@listEl'(XX, XY).
'lo.list@merge'('lo.core#[]', XX, XX):- !.
'lo.list@merge'('lo.core#,..'(Xe, Xl), Xx, XXd9203):- 'lo.list@listEl'(Xe, Xx),
    !,
    'lo.list@merge'(Xl, Xx, XXd9203).
'lo.list@merge'('lo.core#,..'(Xe, Xl), Xx, 'lo.core#,..'(Xe, XXd9204)):- !,
    'lo.list@merge'(Xl, Xx, XXd9204).
'lo.list@merge'(_, _, _):- raise_exception('error'("lo.list@merge", 16, 3, 16)).
'lo.list@cat'('lo.core#[]', XM, XXd9206):- !,
    'lo.list@multiCat'(XM, XXd9206).
'lo.list@cat'('lo.core#,..'(XE, XL), XM, 'lo.core#,..'(XE, XXd9207)):- !,
    'lo.list@cat'(XL, XM, XXd9207).
'lo.list@cat'(_, _, _):- raise_exception('error'("lo.list@cat", 25, 3, 24)).
'lo.list@multiCat'('lo.core#[]', 'lo.core#[]'):- !.
'lo.list@multiCat'('lo.core#,..'(XE, XL), XXd9209):- !,
    'lo.list@cat'(XE, XL, XXd9209).
'lo.list@multiCat'(_, _):- raise_exception('error'("lo.list@multiCat", 21, 3, 18)).
'lo.list@rev'('lo.core#[]', XR, XR):- !.
'lo.list@rev'('lo.core#,..'(XE, XL), XR, XXd9211):- !,
    'lo.list@rev'(XL, 'lo.core#,..'(XE, XR), XXd9211).
'lo.list@rev'(_, _, _):- raise_exception('error'("lo.list@rev", 33, 3, 14)).
'lo.list@reverse'(XX, XXd9212):- !,
    'lo.list@rev'(XX, 'lo.core#[]', XXd9212).
'lo.list@reverse'(_, _):- raise_exception('error'("lo.list@reverse", 29, 3, 23)).
'lo.list@drop'('lo.core#,..'(X_5694, XL), XL):- !.
'lo.list@drop'(_, _):- raise_exception('error'("lo.list@drop", 37, 3, 18)).
'lo.list@subtract'(X_5695, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.list@subtract'(XE, 'lo.core#,..'(XE, XR), XR):- !.
'lo.list@subtract'(XE, 'lo.core#,..'(XX, XR), 'lo.core#,..'(XX, XXd9213)):- 'lo.list@neg41'(XX, XE),
    !,
    'lo.list@subtract'(XE, XR, XXd9213).
'lo.list@subtract'(_, _, _):- raise_exception('error'("lo.list@subtract", 40, 3, 20)).
'lo.list@length'('lo.core#[]', 0):- !.
'lo.list@length'('lo.core#,..'(X_5700, XL), XXe1981):- !,
    ocall('+%1'(XXV2008),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XL, XXd9215),
    ocall('_call%3'(XXd9215, 1, XXe1981),XXV2008,XXV2008).
'lo.list@length'(_, _):- raise_exception('error'("lo.list@length", 46, 3, 15)).
'lo.core$sizeable$lo.core*list'('lo.core$sizeable$lo.core*list%1'('lo.core$sizeable$lo.core*list')):- !.
'lo.core$sizeable$lo.core*list'('size%2'(XV19201, XV19202), XLbl3975, XThis3975):- !,
    'lo.core$sizeable$lo.core*list@size'(XV19201, XV19202, XLbl3975, XThis3975).
'lo.core$sizeable$lo.core*list'('size%1'('lo.core$sizeable$lo.core*list^size'(XLbl3976, XThis3976)), XLbl3976, XThis3976).
'lo.core$sizeable$lo.core*list'('isEmpty%1'(XV19206), XLbl3977, XThis3977):- !,
    'lo.core$sizeable$lo.core*list@isEmpty'(XV19206, XLbl3977, XThis3977).
'lo.core$sizeable$lo.core*list'('isEmpty%1'('lo.core$sizeable$lo.core*list^isEmpty'(XLbl3978, XThis3978)), XLbl3978, XThis3978).
'lo.core$sizeable$lo.core*list@size'(XL, XXd9216, XLbV1737, XThV1737):- !,
    'lo.list@length'(XL, XXd9216).
'lo.core$sizeable$lo.core*list@size'(_, _):- raise_exception('error'("lo.core$sizeable$lo.core*list@size", 50, 5, 20)).
'lo.core$sizeable$lo.core*list@isEmpty'('lo.core#[]', XLbV1737, XThV1737).
'lo.collection$folding$lo.core*list'('lo.collection$folding$lo.core*list%1'('lo.collection$folding$lo.core*list')):- !.
'lo.collection$folding$lo.core*list'('foldLeft%4'(XV19211, XV19212, XV19213, XV19214), XLbl3979, XThis3979):- !,
    'lo.collection$folding$lo.core*list@foldLeft'(XV19211, XV19212, XV19213, XV19214, XLbl3979, XThis3979).
'lo.collection$folding$lo.core*list'('foldLeft%1'('lo.collection$folding$lo.core*list^foldLeft'(XLbl3980, XThis3980)), XLbl3980, XThis3980).
'lo.collection$folding$lo.core*list'('foldRight%4'(XV19219, XV19220, XV19221, XV19222), XLbl3981, XThis3981):- !,
    'lo.collection$folding$lo.core*list@foldRight'(XV19219, XV19220, XV19221, XV19222, XLbl3981, XThis3981).
'lo.collection$folding$lo.core*list'('foldRight%1'('lo.collection$folding$lo.core*list^foldRight'(XLbl3982, XThis3982)), XLbl3982, XThis3982).
'lo.collection$folding$lo.core*list@foldLeft'(Xf, Xx, 'lo.core#[]', Xx, XLbV1738, XThV1738):- !.
'lo.collection$folding$lo.core*list@foldLeft'(Xf, Xx, 'lo.core#,..'(Xe, Xl), XXd9218, XLbV1738, XThV1738):- !,
    ocall('_call%3'(Xx, Xe, XXe1982),Xf,Xf),
    'lo.collection$folding$lo.core*list@foldLeft'(Xf, XXe1982, Xl, XXd9218, XLbV1738, XThV1738).
'lo.collection$folding$lo.core*list@foldLeft'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.core*list@foldLeft", 55, 5, 21)).
'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, 'lo.core#[]', Xx, XLbV1738, XThV1738):- !.
'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, 'lo.core#,..'(Xe, Xl), XXe1983, XLbV1738, XThV1738):- !,
    'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, Xl, XXd9219, XLbV1738, XThV1738),
    ocall('_call%3'(Xe, XXd9219, XXe1983),Xf,Xf).
'lo.collection$folding$lo.core*list@foldRight'(_, _, _, _):- raise_exception('error'("lo.collection$folding$lo.core*list@foldRight", 58, 5, 22)).
'lo.list@mapList'('lo.core#[]', X_5703, 'lo.core#[]'):- !.
'lo.list@mapList'('lo.core#,..'(Xe, Xl), Xf, 'lo.core#,..'(XXe1984, XXd9222)):- !,
    ocall('_call%2'(Xe, XXe1984),Xf,Xf),
    'lo.list@mapList'(Xl, Xf, XXd9222).
'lo.list@mapList'(_, _, _):- raise_exception('error'("lo.list@mapList", 67, 3, 19)).
'lo.collection$mapping$lo.core*list'('lo.collection$mapping$lo.core*list%1'('lo.collection$mapping$lo.core*list')):- !.
'lo.collection$mapping$lo.core*list'('//%3'(XV19229, XV19230, XV19231), XLbl3983, XThis3983):- !,
    'lo.collection$mapping$lo.core*list@//'(XV19229, XV19230, XV19231, XLbl3983, XThis3983).
'lo.collection$mapping$lo.core*list'('//%1'('lo.collection$mapping$lo.core*list^//'(XLbl3984, XThis3984)), XLbl3984, XThis3984).
'lo.collection$mapping$lo.core*list@//'(XL, XF, XXd9224, XLbV1739, XThV1739):- !,
    'lo.list@mapList'(XL, XF, XXd9224).
'lo.collection$mapping$lo.core*list@//'(_, _, _):- raise_exception('error'("lo.collection$mapping$lo.core*list@//", 63, 5, 20)).
'lo.list@filterList'('lo.core#[]', X_5706, 'lo.core#[]'):- !.
'lo.list@filterList'('lo.core#,..'(Xe, Xl), Xp, 'lo.core#,..'(Xe, XXd9225)):- ocall('_call%1'(Xe),Xp,Xp),
    !,
    'lo.list@filterList'(Xl, Xp, XXd9225).
'lo.list@filterList'('lo.core#,..'(X_5710, Xl), Xp, XXd9227):- !,
    'lo.list@filterList'(Xl, Xp, XXd9227).
'lo.list@filterList'(_, _, _):- raise_exception('error'("lo.list@filterList", 75, 3, 22)).
'lo.collection$filter$lo.core*list'('lo.collection$filter$lo.core*list%1'('lo.collection$filter$lo.core*list')):- !.
'lo.collection$filter$lo.core*list'('^/%3'(XV19238, XV19239, XV19240), XLbl3985, XThis3985):- !,
    'lo.collection$filter$lo.core*list@^/'(XV19238, XV19239, XV19240, XLbl3985, XThis3985).
'lo.collection$filter$lo.core*list'('^/%1'('lo.collection$filter$lo.core*list^^/'(XLbl3986, XThis3986)), XLbl3986, XThis3986).
'lo.collection$filter$lo.core*list@^/'(XL, XP, XXd9228, XLbV1740, XThV1740):- !,
    'lo.list@filterList'(XL, XP, XXd9228).
'lo.collection$filter$lo.core*list@^/'(_, _, _):- raise_exception('error'("lo.collection$filter$lo.core*list@^/", 71, 5, 23)).
'lo.list@coerceList'(Xcoercion75, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.list@coerceList'(Xcoercion75, 'lo.core#,..'(Xe, Xl), 'lo.core#,..'(XXe1985, XXd9229)):- !,
    ocall('_coerce%1'(XXV2009),Xcoercion75,Xcoercion75),
    ocall('_call%2'(Xe, XXe1985),XXV2009,XXV2009),
    'lo.list@coerceList'(Xcoercion75, Xl, XXd9229).
'lo.list@coerceList'(_, _, _):- raise_exception('error'("lo.list@coerceList", 84, 3, 20)).
'lo.coerce$coercion$lo.core*list$lo.core*list'('lo.coerce$coercion$lo.core*list$lo.core*list%1'('lo.coerce$coercion$lo.core*list$lo.core*list')):- !.
'lo.coerce$coercion$lo.core*list$lo.core*list'('_coerce%2'(XV19246, XV19247), XLbl3987, XThis3987):- !,
    'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XV19246, XV19247, XLbl3987, XThis3987).
'lo.coerce$coercion$lo.core*list$lo.core*list'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'(XLbl3988, XThis3988)), XLbl3988, XThis3988).
'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XSx, XXd9231, XLbV1741, XThV1741):- XLbV1741 = 'lo.coerce$coercion$lo.core*list$lo.core*list'(Xcoercion76),
    !,
    'lo.list@coerceList'(Xcoercion76, XSx, XXd9231).
'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.core*list$lo.core*list@_coerce", 80, 5, 29)).
'lo.list@listPairs'('lo.core#[]', X_5713, 'lo.core#[]'):- !.
'lo.list@listPairs'('lo.core#,..'(XE, XL), XIx, 'lo.core#,..'('()2'(XIx, XE), XXd9232)):- !,
    ocall('+%1'(XXV2010),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1986),XXV2010,XXV2010),
    'lo.list@listPairs'(XL, XXe1986, XXd9232).
'lo.list@listPairs'(_, _, _):- raise_exception('error'("lo.list@listPairs", 127, 3, 21)).
'lo.list@indexes'('lo.core#[]', X_5716, 'lo.core#[]'):- !.
'lo.list@indexes'('lo.core#,..'(X_5718, XL), XIx, 'lo.core#,..'(XIx, XXd9234)):- !,
    ocall('+%1'(XXV2011),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1987),XXV2011,XXV2011),
    'lo.list@indexes'(XL, XXe1987, XXd9234).
'lo.list@indexes'(_, _, _):- raise_exception('error'("lo.list@indexes", 123, 3, 19)).
'lo.list@replaceNth'('lo.core#,..'(X_5721, XL), 0, Xe, 'lo.core#,..'(Xe, XL)):- !.
'lo.list@replaceNth'('lo.core#[]', 0, Xe, 'lo.core#,..'(Xe, 'lo.core#[]')):- !.
'lo.list@replaceNth'('lo.core#,..'(XE, XL), XIx, Xe, 'lo.core#,..'(XE, XXd9238)):- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    !,
    ocall('-%1'(XXV2012),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1988),XXV2012,XXV2012),
    'lo.list@replaceNth'(XL, XXe1988, Xe, XXd9238).
'lo.list@replaceNth'(_, _, _, _):- raise_exception('error'("lo.list@replaceNth", 114, 3, 34)).
'lo.list@dropNth'('lo.core#[]', X_5726, 'lo.core#[]'):- !.
'lo.list@dropNth'('lo.core#,..'(X_5728, XL), 0, XL):- !.
'lo.list@dropNth'('lo.core#,..'(XE, XL), XIx, 'lo.core#,..'(XE, XXd9240)):- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    !,
    ocall('-%1'(XXV2013),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1989),XXV2013,XXV2013),
    'lo.list@dropNth'(XL, XXe1989, XXd9240).
'lo.list@dropNth'(_, _, _):- raise_exception('error'("lo.list@dropNth", 109, 3, 19)).
'lo.list@nthEl'('lo.core#,..'(Xe, X_5732), 0, Xe).
'lo.list@nthEl'('lo.core#,..'(X_5734, XL), XIx, Xe):- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    ocall('-%1'(XXV2014),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe1990),XXV2014,XXV2014),
    'lo.list@nthEl'(XL, XXe1990, Xe).
'lo.collection$map$lo.core*list'('lo.collection$map$lo.core*list%1'('lo.collection$map$lo.core*list')):- !.
'lo.collection$map$lo.core*list'('present%3'(XV19269, XV19270, XV19271), XLbl3989, XThis3989):- !,
    'lo.collection$map$lo.core*list@present'(XV19269, XV19270, XV19271, XLbl3989, XThis3989).
'lo.collection$map$lo.core*list'('present%1'('lo.collection$map$lo.core*list^present'(XLbl3990, XThis3990)), XLbl3990, XThis3990).
'lo.collection$map$lo.core*list'('_remove%3'(XV19275, XV19276, XV19277), XLbl3991, XThis3991):- !,
    'lo.collection$map$lo.core*list@_remove'(XV19275, XV19276, XV19277, XLbl3991, XThis3991).
'lo.collection$map$lo.core*list'('_remove%1'('lo.collection$map$lo.core*list^_remove'(XLbl3992, XThis3992)), XLbl3992, XThis3992).
'lo.collection$map$lo.core*list'('_put%1'(XV19278), XLbl3993, XThis3993):- !,
    'lo.collection$map$lo.core*list@_put'(XV19278, XLbl3993, XThis3993).
'lo.collection$map$lo.core*list'('keys%2'(XV19281, XV19282), XLbl3994, XThis3994):- !,
    'lo.collection$map$lo.core*list@keys'(XV19281, XV19282, XLbl3994, XThis3994).
'lo.collection$map$lo.core*list'('keys%1'('lo.collection$map$lo.core*list^keys'(XLbl3995, XThis3995)), XLbl3995, XThis3995).
'lo.collection$map$lo.core*list'('pairs%2'(XV19285, XV19286), XLbl3996, XThis3996):- !,
    'lo.collection$map$lo.core*list@pairs'(XV19285, XV19286, XLbl3996, XThis3996).
'lo.collection$map$lo.core*list'('pairs%1'('lo.collection$map$lo.core*list^pairs'(XLbl3997, XThis3997)), XLbl3997, XThis3997).
'lo.collection$map$lo.core*list'('values%2'(XV19289, XV19290), XLbl3998, XThis3998):- !,
    'lo.collection$map$lo.core*list@values'(XV19289, XV19290, XLbl3998, XThis3998).
'lo.collection$map$lo.core*list'('values%1'('lo.collection$map$lo.core*list^values'(XLbl3999, XThis3999)), XLbl3999, XThis3999).
'lo.collection$map$lo.core*list'('_empty%1'(XV19291), XLbl4000, XThis4000):- !,
    'lo.collection$map$lo.core*list@_empty'(XV19291, XLbl4000, XThis4000).
'lo.collection$map$lo.core*list@present'(XL, XIx, XV, XLbV1742, XThV1742):- 'lo.collection$map$lo.core*list@one68'(XV, XIx, XL).
'lo.collection$map$lo.core*list@_remove'(XL, XIx, XXd9242, XLbV1742, XThV1742):- !,
    'lo.list@dropNth'(XL, XIx, XXd9242).
'lo.collection$map$lo.core*list@_remove'(_, _, _):- raise_exception('error'("lo.collection$map$lo.core*list@_remove", 89, 5, 30)).
'lo.collection$map$lo.core*list@_put'('lo.list^replaceNth', XLbV1742, XThV1742):- !.
'lo.collection$map$lo.core*list@keys'(XL, XXd9243, XLbV1742, XThV1742):- !,
    'lo.list@indexes'(XL, 0, XXd9243).
'lo.collection$map$lo.core*list@keys'(_, _):- raise_exception('error'("lo.collection$map$lo.core*list@keys", 91, 5, 23)).
'lo.collection$map$lo.core*list@pairs'(XL, XXd9244, XLbV1742, XThV1742):- !,
    'lo.list@listPairs'(XL, 0, XXd9244).
'lo.collection$map$lo.core*list@pairs'(_, _):- raise_exception('error'("lo.collection$map$lo.core*list@pairs", 92, 5, 26)).
'lo.collection$map$lo.core*list@values'(XL, XL, XLbV1742, XThV1742):- !.
'lo.collection$map$lo.core*list@values'(_, _):- raise_exception('error'("lo.collection$map$lo.core*list@values", 93, 5, 14)).
'lo.collection$map$lo.core*list@_empty'('lo.core#[]', XLbV1742, XThV1742):- !.
'lo.collection$membership$lo.core*list'('lo.collection$membership$lo.core*list%1'('lo.collection$membership$lo.core*list')):- !.
'lo.collection$membership$lo.core*list'('empty%1'(XV19292), XLbl4001, XThis4001):- !,
    'lo.collection$membership$lo.core*list@empty'(XV19292, XLbl4001, XThis4001).
'lo.collection$membership$lo.core*list'('addMem%3'(XV19296, XV19297, XV19298), XLbl4002, XThis4002):- !,
    'lo.collection$membership$lo.core*list@addMem'(XV19296, XV19297, XV19298, XLbl4002, XThis4002).
'lo.collection$membership$lo.core*list'('addMem%1'('lo.collection$membership$lo.core*list^addMem'(XLbl4003, XThis4003)), XLbl4003, XThis4003).
'lo.collection$membership$lo.core*list'('delMem%3'(XV19302, XV19303, XV19304), XLbl4004, XThis4004):- !,
    'lo.collection$membership$lo.core*list@delMem'(XV19302, XV19303, XV19304, XLbl4004, XThis4004).
'lo.collection$membership$lo.core*list'('delMem%1'('lo.collection$membership$lo.core*list^delMem'(XLbl4005, XThis4005)), XLbl4005, XThis4005).
'lo.collection$membership$lo.core*list'('in%2'(XV19309, XV19310), XLbl4006, XThis4006):- !,
    'lo.collection$membership$lo.core*list@in'(XV19309, XV19310, XLbl4006, XThis4006).
'lo.collection$membership$lo.core*list'('in%1'('lo.collection$membership$lo.core*list^in'(XLbl4007, XThis4007)), XLbl4007, XThis4007).
'lo.collection$membership$lo.core*list@empty'('lo.core#[]', XLbV1743, XThV1743):- !.
'lo.collection$membership$lo.core*list@addMem'(XL, Xe, 'lo.core#,..'(Xe, XL), XLbV1743, XThV1743):- !.
'lo.collection$membership$lo.core*list@addMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.core*list@addMem", 99, 5, 22)).
'lo.collection$membership$lo.core*list@delMem'(XL, Xe, XXd9246, XLbV1743, XThV1743):- !,
    'lo.list@subtract'(Xe, XL, XXd9246).
'lo.collection$membership$lo.core*list@delMem'(_, _, _):- raise_exception('error'("lo.collection$membership$lo.core*list@delMem", 100, 5, 28)).
'lo.collection$membership$lo.core*list@in'(Xe, XL, XLbV1743, XThV1743):- 'lo.list@listEl'(Xe, XL).
'lo.list@lastEl'('lo.core#,..'(XX, 'lo.core#[]'), XX).
'lo.list@lastEl'('lo.core#,..'(X_5738, XL), XX):- 'lo.list@lastEl'(XL, XX).
'lo.list@mixin'('lo.core#[]', X_5739, 'lo.core#[]'):- !.
'lo.list@mixin'('lo.core#,..'(XF, XL), XS, 'lo.core#,..'(XS, 'lo.core#,..'(XF, XXd9247))):- !,
    'lo.list@mixin'(XL, XS, XXd9247).
'lo.list@mixin'(_, _, _):- raise_exception('error'("lo.list@mixin", 135, 3, 15)).
'lo.list@interleave'('lo.core#[]', X_5743, 'lo.core#[]'):- !.
'lo.list@interleave'('lo.core#,..'(XF, XL), XS, 'lo.core#,..'(XF, XXd9250)):- !,
    'lo.list@mixin'(XL, XS, XXd9250).
'lo.list@interleave'(_, _, _):- raise_exception('error'("lo.list@interleave", 131, 3, 20)).
'lo.list@zip'('lo.core#[]', X_5746, 'lo.core#[]'):- !.
'lo.list@zip'(X_5747, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.list@zip'('lo.core#,..'(Xe1, Xl1), 'lo.core#,..'(Xe2, Xl2), 'lo.core#,..'('()2'(Xe1, Xe2), XXd9252)):- !,
    'lo.list@zip'(Xl1, Xl2, XXd9252).
'lo.list@zip'(_, _, _):- raise_exception('error'("lo.list@zip", 139, 3, 15)).
'lo.list@unzip'('lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.list@unzip'('lo.core#,..'('()2'(Xe, Xf), Xl), 'lo.core#,..'(Xe, Xr1), 'lo.core#,..'(Xf, Xr2)):- 'lo.list@unzip'(Xl, Xr1, Xr2).
'lo.list@iota'(XF, XT, 'lo.core#[]'):- 'lo.core@>'('lo.core$comp$lo.core*integer', XF, XT),
    !.
'lo.list@iota'(XF, XT, 'lo.core#,..'(XF, XXd9254)):- 'lo.core@=<'('lo.core$comp$lo.core*integer', XF, XT),
    !,
    ocall('+%1'(XXV2015),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XF, 1, XXe1991),XXV2015,XXV2015),
    'lo.list@iota'(XXe1991, XT, XXd9254).
'lo.list@iota'(_, _, _):- raise_exception('error'("lo.list@iota", 148, 3, 22)).
'lo.list@head'('lo.core#,..'(XE, X_5756), XE):- !.
'lo.list@head'(_, _):- raise_exception('error'("lo.list@head", 152, 3, 18)).
'lo.list@tail'('lo.core#,..'(X_5758, XL), XL):- !.
'lo.list@tail'(_, _):- raise_exception('error'("lo.list@tail", 155, 3, 18)).
'lo.list^<>'('_call%3'(XV19174, XV19175, XV19176), 'lo.list^<>', _):- 'lo.list@<>'(XV19174, XV19175, XV19176).
'lo.list^listEl'('_call%2'(XV19177, XV19178), 'lo.list^listEl', _):- 'lo.list@listEl'(XV19177, XV19178).
'lo.list^merge'('_call%3'(XV19179, XV19180, XV19181), 'lo.list^merge', _):- 'lo.list@merge'(XV19179, XV19180, XV19181).
'lo.list^cat'('_call%3'(XV19182, XV19183, XV19184), 'lo.list^cat', _):- 'lo.list@cat'(XV19182, XV19183, XV19184).
'lo.list^multiCat'('_call%2'(XV19185, XV19186), 'lo.list^multiCat', _):- 'lo.list@multiCat'(XV19185, XV19186).
'lo.list^rev'('_call%3'(XV19187, XV19188, XV19189), 'lo.list^rev', _):- 'lo.list@rev'(XV19187, XV19188, XV19189).
'lo.list^reverse'('_call%2'(XV19190, XV19191), 'lo.list^reverse', _):- 'lo.list@reverse'(XV19190, XV19191).
'lo.list^drop'('_call%2'(XV19192, XV19193), 'lo.list^drop', _):- 'lo.list@drop'(XV19192, XV19193).
'lo.list@neg41'(XX, XE):- XE = XX,
    !,
    fail.
'lo.list@neg41'(XX, XE).
'lo.list^subtract'('_call%3'(XV19194, XV19195, XV19196), 'lo.list^subtract', _):- 'lo.list@subtract'(XV19194, XV19195, XV19196).
'lo.list^length'('_call%2'(XV19197, XV19198), 'lo.list^length', _):- 'lo.list@length'(XV19197, XV19198).
'lo.core$sizeable$lo.core*list^size'('_call%2'(XV19199, XV19200), 'lo.core$sizeable$lo.core*list^size'(XLbV1737, XThV1737), _):- 'lo.core$sizeable$lo.core*list@size'(XV19199, XV19200, XLbV1737, XThV1737).
'lo.core$sizeable$lo.core*list^isEmpty'('_call%3'(XV19203, XV19204, XV19205), 'lo.core$sizeable$lo.core*list^isEmpty'(XLbV1737, XThV1737), _):- 'lo.core$sizeable$lo.core*list@isEmpty'(XV19203, XV19204, XV19205, XLbV1737, XThV1737).
'lo.collection$folding$lo.core*list^foldLeft'('_call%4'(XV19207, XV19208, XV19209, XV19210), 'lo.collection$folding$lo.core*list^foldLeft'(XLbV1738, XThV1738), _):- 'lo.collection$folding$lo.core*list@foldLeft'(XV19207, XV19208, XV19209, XV19210, XLbV1738, XThV1738).
'lo.collection$folding$lo.core*list^foldRight'('_call%4'(XV19215, XV19216, XV19217, XV19218), 'lo.collection$folding$lo.core*list^foldRight'(XLbV1738, XThV1738), _):- 'lo.collection$folding$lo.core*list@foldRight'(XV19215, XV19216, XV19217, XV19218, XLbV1738, XThV1738).
'lo.list^mapList'('_call%3'(XV19223, XV19224, XV19225), 'lo.list^mapList', _):- 'lo.list@mapList'(XV19223, XV19224, XV19225).
'lo.collection$mapping$lo.core*list^//'('_call%3'(XV19226, XV19227, XV19228), 'lo.collection$mapping$lo.core*list^//'(XLbV1739, XThV1739), _):- 'lo.collection$mapping$lo.core*list@//'(XV19226, XV19227, XV19228, XLbV1739, XThV1739).
'lo.list^filterList'('_call%3'(XV19232, XV19233, XV19234), 'lo.list^filterList', _):- 'lo.list@filterList'(XV19232, XV19233, XV19234).
'lo.collection$filter$lo.core*list^^/'('_call%3'(XV19235, XV19236, XV19237), 'lo.collection$filter$lo.core*list^^/'(XLbV1740, XThV1740), _):- 'lo.collection$filter$lo.core*list@^/'(XV19235, XV19236, XV19237, XLbV1740, XThV1740).
'lo.list^coerceList'('_call%3'(XV19241, XV19242, XV19243), 'lo.list^coerceList', _):- 'lo.list@coerceList'(XV19241, XV19242, XV19243).
'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'('_call%2'(XV19244, XV19245), 'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'(XLbV1741, XThV1741), _):- 'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XV19244, XV19245, XLbV1741, XThV1741).
'lo.list^listPairs'('_call%3'(XV19248, XV19249, XV19250), 'lo.list^listPairs', _):- 'lo.list@listPairs'(XV19248, XV19249, XV19250).
'lo.list^indexes'('_call%3'(XV19251, XV19252, XV19253), 'lo.list^indexes', _):- 'lo.list@indexes'(XV19251, XV19252, XV19253).
'lo.list^replaceNth'('_call%4'(XV19254, XV19255, XV19256, XV19257), 'lo.list^replaceNth', _):- 'lo.list@replaceNth'(XV19254, XV19255, XV19256, XV19257).
'lo.list^dropNth'('_call%3'(XV19258, XV19259, XV19260), 'lo.list^dropNth', _):- 'lo.list@dropNth'(XV19258, XV19259, XV19260).
'lo.list^nthEl'('_call%3'(XV19261, XV19262, XV19263), 'lo.list^nthEl', _):- 'lo.list@nthEl'(XV19261, XV19262, XV19263).
'lo.collection$map$lo.core*list@one68'(XV, XIx, XL):- 'lo.list@nthEl'(XL, XIx, XV),
    !.
'lo.collection$map$lo.core*list^present'('_call%5'(XV19264, XV19265, XV19266, XV19267, XV19268), 'lo.collection$map$lo.core*list^present'(XLbV1742, XThV1742), _):- 'lo.collection$map$lo.core*list@present'(XV19264, XV19265, XV19266, XV19267, XV19268, XLbV1742, XThV1742).
'lo.collection$map$lo.core*list^_remove'('_call%3'(XV19272, XV19273, XV19274), 'lo.collection$map$lo.core*list^_remove'(XLbV1742, XThV1742), _):- 'lo.collection$map$lo.core*list@_remove'(XV19272, XV19273, XV19274, XLbV1742, XThV1742).
'lo.collection$map$lo.core*list^keys'('_call%2'(XV19279, XV19280), 'lo.collection$map$lo.core*list^keys'(XLbV1742, XThV1742), _):- 'lo.collection$map$lo.core*list@keys'(XV19279, XV19280, XLbV1742, XThV1742).
'lo.collection$map$lo.core*list^pairs'('_call%2'(XV19283, XV19284), 'lo.collection$map$lo.core*list^pairs'(XLbV1742, XThV1742), _):- 'lo.collection$map$lo.core*list@pairs'(XV19283, XV19284, XLbV1742, XThV1742).
'lo.collection$map$lo.core*list^values'('_call%2'(XV19287, XV19288), 'lo.collection$map$lo.core*list^values'(XLbV1742, XThV1742), _):- 'lo.collection$map$lo.core*list@values'(XV19287, XV19288, XLbV1742, XThV1742).
'lo.collection$membership$lo.core*list^addMem'('_call%3'(XV19293, XV19294, XV19295), 'lo.collection$membership$lo.core*list^addMem'(XLbV1743, XThV1743), _):- 'lo.collection$membership$lo.core*list@addMem'(XV19293, XV19294, XV19295, XLbV1743, XThV1743).
'lo.collection$membership$lo.core*list^delMem'('_call%3'(XV19299, XV19300, XV19301), 'lo.collection$membership$lo.core*list^delMem'(XLbV1743, XThV1743), _):- 'lo.collection$membership$lo.core*list@delMem'(XV19299, XV19300, XV19301, XLbV1743, XThV1743).
'lo.collection$membership$lo.core*list^in'('_call%4'(XV19305, XV19306, XV19307, XV19308), 'lo.collection$membership$lo.core*list^in'(XLbV1743, XThV1743), _):- 'lo.collection$membership$lo.core*list@in'(XV19305, XV19306, XV19307, XV19308, XLbV1743, XThV1743).
'lo.list^lastEl'('_call%2'(XV19311, XV19312), 'lo.list^lastEl', _):- 'lo.list@lastEl'(XV19311, XV19312).
'lo.list^mixin'('_call%3'(XV19313, XV19314, XV19315), 'lo.list^mixin', _):- 'lo.list@mixin'(XV19313, XV19314, XV19315).
'lo.list^interleave'('_call%3'(XV19316, XV19317, XV19318), 'lo.list^interleave', _):- 'lo.list@interleave'(XV19316, XV19317, XV19318).
'lo.list^zip'('_call%3'(XV19319, XV19320, XV19321), 'lo.list^zip', _):- 'lo.list@zip'(XV19319, XV19320, XV19321).
'lo.list^unzip'('_call%3'(XV19322, XV19323, XV19324), 'lo.list^unzip', _):- 'lo.list@unzip'(XV19322, XV19323, XV19324).
'lo.list^iota'('_call%3'(XV19325, XV19326, XV19327), 'lo.list^iota', _):- 'lo.list@iota'(XV19325, XV19326, XV19327).
'lo.list^head'('_call%2'(XV19328, XV19329), 'lo.list^head', _):- 'lo.list@head'(XV19328, XV19329).
'lo.list^tail'('_call%2'(XV19330, XV19331), 'lo.list^tail', _):- 'lo.list@tail'(XV19330, XV19331).
