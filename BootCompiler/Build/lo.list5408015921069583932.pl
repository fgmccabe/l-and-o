'#pkg'("n7o7'()7'n2o2'pkg's'lo.list'e'*'n3o3'()3'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I17'<>':k't'FT2Lk't'Lk't'Lk't''listEl':k't'PT2k't'Lk't''merge':k't'FT2Lk't'Lk't'Lk't''multiCat':k'x'FT1LLk'x'Lk'x''reverse':k't'FT1Lk't'Lk't''drop':k't'FT1Lk't'Lk't''subtract':k't'FT2k't'Lk't'Lk't''length':k't'FT1Lk't'i'mapList':k'v':k'w'FT2Lk'v'FT1k'v'k'w'Lk'w''nthEl':k'e'PT3Lk'e'ik'e''lastEl':k'e'PT2Lk'e'k'e''interleave':k'x'FT2Lk'x'k'x'Lk'x''zip':k'e':k'f'FT2Lk'e'Lk'f'LT2k'e'k'f''unzip':k'e':k'f'PT3LT2k'e'k'f'Lk'e'Lk'f''iota'FT2iiLi'head':k'e'FT1Lk'e'k'e''tail':k'e'FT1Lk'e'Lk'e'\"s'I0'n0o0'()0'n0o0'()0'n7o7'()7'n2o2'()2's'lo.core$sizeable$lo.core*list's\":k'c'c'lo.core$sizeable'T1Lk'c'T0\"n2o2'()2's'lo.collection$folding$lo.core*list's\":k'e'c'lo.collection$folding'T1Lk'e'T1k'e'\"n2o2'()2's'lo.collection$mapping$lo.core*list's\":k'e':k'f'c'lo.collection$mapping'T1z1'lo.core*list'T2k'e'k'f'\"n2o2'()2's'lo.collection$filter$lo.core*list's\":k'e'c'lo.collection$filter'T1Lk'e'T1k'e'\"n2o2'()2's'lo.coerce$coercion$lo.core*list$lo.core*list's\":k'e':k'f'|c'lo.coerce$coercion'T2Lk'e'Lk'f'T0c'lo.coerce$coercion'T2k'e'k'f'T0\"n2o2'()2's'lo.collection$map$lo.core*list's\":k'e'c'lo.collection$map'T1Lk'e'T2ik'e'\"n2o2'()2's'lo.collection$membership$lo.core*list's\":k'e'c'lo.collection$membership'T1Lk'e'T1k'e'\"").
'lo.list@init'() :- !.
'lo.list@<>'('lo.core#[]', XX, XX) :- !.
'lo.list@<>'('lo.core#,..'(XE, XX), XY, 'lo.core#,..'(XE, XX457)) :- !,
    'lo.list@<>'(XX, XY, XX457).
'lo.list@<>'(_, _, _) :- raise_exception('error'("<>", 8, 3, 12)).
'lo.list@listEl'(XX, 'lo.core#,..'(XX, X_11)).
'lo.list@listEl'(XX, 'lo.core#,..'(X_12, XY)) :- 'lo.list@listEl'(XX, XY).
'lo.list@merge'('lo.core#[]', XX, XX) :- !.
'lo.list@merge'('lo.core#,..'(Xe, Xl), Xx, XX480) :- 'lo.list@listEl'(Xe, Xx),
    !,
    'lo.list@merge'(Xl, Xx, XX480).
'lo.list@merge'('lo.core#,..'(Xe, Xl), Xx, 'lo.core#,..'(Xe, XX488)) :- !,
    'lo.list@merge'(Xl, Xx, XX488).
'lo.list@merge'(_, _, _) :- raise_exception('error'("merge", 16, 3, 16)).
'lo.list@cat'('lo.core#[]', XM, XX493) :- !,
    'lo.list@multiCat'(XM, XX493).
'lo.list@cat'('lo.core#,..'(XE, XL), XM, 'lo.core#,..'(XE, XX501)) :- !,
    'lo.list@cat'(XL, XM, XX501).
'lo.list@cat'(_, _, _) :- raise_exception('error'("cat", 25, 3, 24)).
'lo.list@multiCat'('lo.core#[]', 'lo.core#[]') :- !.
'lo.list@multiCat'('lo.core#,..'(XE, XL), XX510) :- !,
    'lo.list@cat'(XE, XL, XX510).
'lo.list@multiCat'(_, _) :- raise_exception('error'("multiCat", 21, 3, 18)).
'lo.list@rev'('lo.core#[]', XR, XR) :- !.
'lo.list@rev'('lo.core#,..'(XE, XL), XR, XX522) :- !,
    'lo.list@rev'(XL, 'lo.core#,..'(XE, XR), XX522).
'lo.list@rev'(_, _, _) :- raise_exception('error'("rev", 33, 3, 14)).
'lo.list@reverse'(XX, XX526) :- !,
    'lo.list@rev'(XX, 'lo.core#[]', XX526).
'lo.list@reverse'(_, _) :- raise_exception('error'("reverse", 29, 3, 23)).
'lo.list@drop'('lo.core#,..'(X_13, XL), XL) :- !.
'lo.list@drop'(_, _) :- raise_exception('error'("drop", 37, 3, 18)).
'lo.list@subtract'(X_14, 'lo.core#[]', 'lo.core#[]') :- !.
'lo.list@subtract'(XE, 'lo.core#,..'(XE, XR), XR) :- !.
'lo.list@subtract'(XE, 'lo.core#,..'(XX, XR), 'lo.core#,..'(XX, XX548)) :- 'lo.list@neg2'(XX, XE),
    !,
    'lo.list@subtract'(XE, XR, XX548).
'lo.list@subtract'(_, _, _) :- raise_exception('error'("subtract", 40, 3, 20)).
'lo.list@length'('lo.core#[]', 0) :- !.
'lo.list@length'('lo.core#,..'(X_15, XL), XX556) :- !,
    'lo.list@length'(XL, XX555),
    ocall('+%3'(XX555, 1, XX556),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.list@length'(_, _) :- raise_exception('error'("length", 46, 3, 15)).
'lo.core$sizeable$lo.core*list'('lo.core$sizeable$lo.core*list%1'('lo.core$sizeable$lo.core*list')) :- !.
'lo.core$sizeable$lo.core*list'('size%2'(XV386, XV387), XLbl98, XThis98) :- !,
    'lo.core$sizeable$lo.core*list@size'(XV386, XV387, XLbl98, XThis98).
'lo.core$sizeable$lo.core*list'('size%1'('lo.core$sizeable$lo.core*list^size'(XLbl99, XThis99)), XLbl99, XThis99).
'lo.core$sizeable$lo.core*list'('isEmpty%1'(XV391), XLbl100, XThis100) :- !,
    'lo.core$sizeable$lo.core*list@isEmpty'(XV391, XLbl100, XThis100).
'lo.core$sizeable$lo.core*list'('isEmpty%1'('lo.core$sizeable$lo.core*list^isEmpty'(XLbl101, XThis101)), XLbl101, XThis101).
'lo.core$sizeable$lo.core*list@size'(XL, XX560, XLbV39, XThV39) :- !,
    'lo.list@length'(XL, XX560).
'lo.core$sizeable$lo.core*list@size'(_, _, _, _) :- raise_exception('error'("size", 50, 5, 20)).
'lo.core$sizeable$lo.core*list@isEmpty'('lo.core#[]', XLbV39, XThV39).
'lo.collection$folding$lo.core*list'('lo.collection$folding$lo.core*list%1'('lo.collection$folding$lo.core*list')) :- !.
'lo.collection$folding$lo.core*list'('foldLeft%4'(XV397, XV398, XV399, XV400), XLbl102, XThis102) :- !,
    'lo.collection$folding$lo.core*list@foldLeft'(XV397, XV398, XV399, XV400, XLbl102, XThis102).
'lo.collection$folding$lo.core*list'('foldLeft%1'('lo.collection$folding$lo.core*list^foldLeft'(XLbl103, XThis103)), XLbl103, XThis103).
'lo.collection$folding$lo.core*list'('foldRight%4'(XV409, XV410, XV411, XV412), XLbl104, XThis104) :- !,
    'lo.collection$folding$lo.core*list@foldRight'(XV409, XV410, XV411, XV412, XLbl104, XThis104).
'lo.collection$folding$lo.core*list'('foldRight%1'('lo.collection$folding$lo.core*list^foldRight'(XLbl105, XThis105)), XLbl105, XThis105).
'lo.collection$folding$lo.core*list@foldLeft'(Xf, Xx, 'lo.core#[]', Xx, XLbV40, XThV40) :- !.
'lo.collection$folding$lo.core*list@foldLeft'(Xf, Xx, 'lo.core#,..'(Xe, Xl), XX577, XLbV40, XThV40) :- !,
    ocall('_call%3'(Xx, Xe, XX574),Xf,Xf),
    'lo.collection$folding$lo.core*list@foldLeft'(Xf, XX574, Xl, XX577, XLbV40, XThV40).
'lo.collection$folding$lo.core*list@foldLeft'(_, _, _, _, _, _) :- raise_exception('error'("foldLeft", 55, 5, 21)).
'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, 'lo.core#[]', Xx, XLbV40, XThV40) :- !.
'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, 'lo.core#,..'(Xe, Xl), XX592, XLbV40, XThV40) :- !,
    'lo.collection$folding$lo.core*list@foldRight'(Xf, Xx, Xl, XX591, XLbV40, XThV40),
    ocall('_call%3'(Xe, XX591, XX592),Xf,Xf).
'lo.collection$folding$lo.core*list@foldRight'(_, _, _, _, _, _) :- raise_exception('error'("foldRight", 58, 5, 22)).
'lo.list@mapList'('lo.core#[]', X_16, 'lo.core#[]') :- !.
'lo.list@mapList'('lo.core#,..'(Xe, Xl), Xf, 'lo.core#,..'(XX602, XX606)) :- !,
    ocall('_call%2'(Xe, XX602),Xf,Xf),
    'lo.list@mapList'(Xl, Xf, XX606).
'lo.list@mapList'(_, _, _) :- raise_exception('error'("mapList", 67, 3, 19)).
'lo.collection$mapping$lo.core*list'('lo.collection$mapping$lo.core*list%1'('lo.collection$mapping$lo.core*list')) :- !.
'lo.collection$mapping$lo.core*list'('//%3'(XV423, XV424, XV425), XLbl106, XThis106) :- !,
    'lo.collection$mapping$lo.core*list@//'(XV423, XV424, XV425, XLbl106, XThis106).
'lo.collection$mapping$lo.core*list'('//%1'('lo.collection$mapping$lo.core*list^//'(XLbl107, XThis107)), XLbl107, XThis107).
'lo.collection$mapping$lo.core*list@//'(XL, XF, XX612, XLbV41, XThV41) :- !,
    'lo.list@mapList'(XL, XF, XX612).
'lo.collection$mapping$lo.core*list@//'(_, _, _, _, _) :- raise_exception('error'("//", 63, 5, 20)).
'lo.list@filterList'('lo.core#[]', X_17, 'lo.core#[]') :- !.
'lo.list@filterList'('lo.core#,..'(Xe, Xl), Xp, 'lo.core#,..'(Xe, XX625)) :- ocall('_call%1'(Xe),Xp,Xp),
    !,
    'lo.list@filterList'(Xl, Xp, XX625).
'lo.list@filterList'('lo.core#,..'(X_18, Xl), Xp, XX633) :- !,
    'lo.list@filterList'(Xl, Xp, XX633).
'lo.list@filterList'(_, _, _) :- raise_exception('error'("filterList", 75, 3, 22)).
'lo.collection$filter$lo.core*list'('lo.collection$filter$lo.core*list%1'('lo.collection$filter$lo.core*list')) :- !.
'lo.collection$filter$lo.core*list'('^/%3'(XV435, XV436, XV437), XLbl108, XThis108) :- !,
    'lo.collection$filter$lo.core*list@^/'(XV435, XV436, XV437, XLbl108, XThis108).
'lo.collection$filter$lo.core*list'('^/%1'('^/'(XLbl109, XThis109)), XLbl109, XThis109).
'lo.collection$filter$lo.core*list@^/'(XL, XP, XX638, XLbV42, XThV42) :- !,
    'lo.list@filterList'(XL, XP, XX638).
'lo.collection$filter$lo.core*list@^/'(_, _, _, _, _) :- raise_exception('error'("^/", 71, 5, 23)).
'lo.list@coerceList'(Xlo_coerce_coercion_e_f1, 'lo.core#[]', 'lo.core#[]') :- !.
'lo.list@coerceList'(Xlo_coerce_coercion_e_f1, 'lo.core#,..'(Xe, Xl), 'lo.core#,..'(XX647, XX651)) :- !,
    ocall('_coerce%2'(Xe, XX647),Xlo_coerce_coercion_e_f1,Xlo_coerce_coercion_e_f1),
    'lo.list@coerceList'(Xlo_coerce_coercion_e_f1, Xl, XX651).
'lo.list@coerceList'(_, _) :- raise_exception('error'("coerceList", 84, 3, 20)).
'lo.coerce$coercion$lo.core*list$lo.core*list'('lo.coerce$coercion$lo.core*list$lo.core*list%1'('lo.coerce$coercion$lo.core*list$lo.core*list')) :- !.
'lo.coerce$coercion$lo.core*list$lo.core*list'('_coerce%2'(XV445, XV446), XLbl110, XThis110) :- !,
    'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XV445, XV446, XLbl110, XThis110).
'lo.coerce$coercion$lo.core*list$lo.core*list'('_coerce%1'('lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'(XLbl111, XThis111)), XLbl111, XThis111).
'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XSx, XX658, XLbV43, XThV43) :- XLbV43 = 'lo.coerce$coercion$lo.core*list$lo.core*list'(Xlo_coerce_coercion_e_f2),
    !,
    'lo.list@coerceList'(Xlo_coerce_coercion_e_f2, XSx, XX658).
'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 80, 5, 29)).
'lo.list@nthEl'('lo.core#,..'(Xe, X_19), 0, Xe).
'lo.list@nthEl'('lo.core#,..'(X_20, XL), XIx, Xe) :- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    ocall('-%3'(XIx, 1, XX672),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@nthEl'(XL, XX672, Xe).
'lo.list@dropNth'('lo.core#[]', X_21, 'lo.core#[]') :- !.
'lo.list@dropNth'('lo.core#,..'(X_22, XL), 0, XL) :- !.
'lo.list@dropNth'('lo.core#,..'(XE, XL), XIx, 'lo.core#,..'(XE, XX693)) :- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    !,
    ocall('-%3'(XIx, 1, XX691),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@dropNth'(XL, XX691, XX693).
'lo.list@dropNth'(_, _, _) :- raise_exception('error'("dropNth", 109, 3, 19)).
'lo.list@replaceNth'('lo.core#,..'(X_23, XL), 0, Xe, 'lo.core#,..'(Xe, XL)) :- !.
'lo.list@replaceNth'('lo.core#[]', 0, Xe, 'lo.core#,..'(Xe, 'lo.core#[]')) :- !.
'lo.list@replaceNth'('lo.core#,..'(XE, XL), XIx, Xe, 'lo.core#,..'(XE, XX720)) :- 'lo.core@>'('lo.core$comp$lo.core*integer', XIx, 0),
    !,
    ocall('-%3'(XIx, 1, XX717),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@replaceNth'(XL, XX717, Xe, XX720).
'lo.list@replaceNth'(_, _, _, _) :- raise_exception('error'("replaceNth", 114, 3, 34)).
'lo.list@indexes'('lo.core#[]', X_24, 'lo.core#[]') :- !.
'lo.list@indexes'('lo.core#,..'(X_25, XL), XIx, 'lo.core#,..'(XIx, XX734)) :- !,
    ocall('+%3'(XIx, 1, XX732),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@indexes'(XL, XX732, XX734).
'lo.list@indexes'(_, _, _) :- raise_exception('error'("indexes", 123, 3, 19)).
'lo.list@listPairs'('lo.core#[]', X_26, 'lo.core#[]') :- !.
'lo.list@listPairs'('lo.core#,..'(XE, XL), XIx, 'lo.core#,..'((XIx, XE), XX749)) :- !,
    ocall('+%3'(XIx, 1, XX747),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@listPairs'(XL, XX747, XX749).
'lo.list@listPairs'(_, _, _) :- raise_exception('error'("listPairs", 127, 3, 21)).
'lo.collection$map$lo.core*list'('lo.collection$map$lo.core*list%1'('lo.collection$map$lo.core*list')) :- !.
'lo.collection$map$lo.core*list'('present%3'(XV468, XV469, XV470), XLbl112, XThis112) :- !,
    'lo.collection$map$lo.core*list@present'(XV468, XV469, XV470, XLbl112, XThis112).
'lo.collection$map$lo.core*list'('present%1'('lo.collection$map$lo.core*list^present'(XLbl113, XThis113)), XLbl113, XThis113).
'lo.collection$map$lo.core*list'('_remove%3'(XV477, XV478, XV479), XLbl114, XThis114) :- !,
    'lo.collection$map$lo.core*list@_remove'(XV477, XV478, XV479, XLbl114, XThis114).
'lo.collection$map$lo.core*list'('_remove%1'('lo.collection$map$lo.core*list^_remove'(XLbl115, XThis115)), XLbl115, XThis115).
'lo.collection$map$lo.core*list'('_put%1'(XV483), XLbl116, XThis116) :- !,
    'lo.collection$map$lo.core*list@_put'(XV483, XLbl116, XThis116).
'lo.collection$map$lo.core*list'('keys%2'(XV486, XV487), XLbl117, XThis117) :- !,
    'lo.collection$map$lo.core*list@keys'(XV486, XV487, XLbl117, XThis117).
'lo.collection$map$lo.core*list'('keys%1'('lo.collection$map$lo.core*list^keys'(XLbl118, XThis118)), XLbl118, XThis118).
'lo.collection$map$lo.core*list'('pairs%2'(XV492, XV493), XLbl119, XThis119) :- !,
    'lo.collection$map$lo.core*list@pairs'(XV492, XV493, XLbl119, XThis119).
'lo.collection$map$lo.core*list'('pairs%1'('lo.collection$map$lo.core*list^pairs'(XLbl120, XThis120)), XLbl120, XThis120).
'lo.collection$map$lo.core*list'('values%2'(XV498, XV499), XLbl121, XThis121) :- !,
    'lo.collection$map$lo.core*list@values'(XV498, XV499, XLbl121, XThis121).
'lo.collection$map$lo.core*list'('values%1'('lo.collection$map$lo.core*list^values'(XLbl122, XThis122)), XLbl122, XThis122).
'lo.collection$map$lo.core*list'('_empty%1'(XV502), XLbl123, XThis123) :- !,
    'lo.collection$map$lo.core*list@_empty'(XV502, XLbl123, XThis123).
'lo.collection$map$lo.core*list@present'(XL, XIx, XV, XLbV44, XThV44) :- 'lo.collection$map$lo.core*list@one1'(XV, XIx, XL).
'lo.collection$map$lo.core*list@_remove'(XL, XIx, XX761, XLbV44, XThV44) :- !,
    'lo.list@dropNth'(XL, XIx, XX761).
'lo.collection$map$lo.core*list@_remove'(_, _, _, _, _) :- raise_exception('error'("_remove", 89, 5, 30)).
'lo.collection$map$lo.core*list@_put'('lo.list^replaceNth', XLbV44, XThV44) :- !.
'lo.collection$map$lo.core*list@keys'(XL, XX765, XLbV44, XThV44) :- !,
    'lo.list@indexes'(XL, 0, XX765).
'lo.collection$map$lo.core*list@keys'(_, _, _, _) :- raise_exception('error'("keys", 91, 5, 23)).
'lo.collection$map$lo.core*list@pairs'(XL, XX768, XLbV44, XThV44) :- !,
    'lo.list@listPairs'(XL, 0, XX768).
'lo.collection$map$lo.core*list@pairs'(_, _, _, _) :- raise_exception('error'("pairs", 92, 5, 26)).
'lo.collection$map$lo.core*list@values'(XL, XL, XLbV44, XThV44) :- !.
'lo.collection$map$lo.core*list@values'(_, _, _, _) :- raise_exception('error'("values", 93, 5, 14)).
'lo.collection$map$lo.core*list@_empty'('lo.core#[]', XLbV44, XThV44) :- !.
'lo.collection$membership$lo.core*list'('lo.collection$membership$lo.core*list%1'('lo.collection$membership$lo.core*list')) :- !.
'lo.collection$membership$lo.core*list'('empty%1'(XV503), XLbl124, XThis124) :- !,
    'lo.collection$membership$lo.core*list@empty'(XV503, XLbl124, XThis124).
'lo.collection$membership$lo.core*list'('addMem%3'(XV507, XV508, XV509), XLbl125, XThis125) :- !,
    'lo.collection$membership$lo.core*list@addMem'(XV507, XV508, XV509, XLbl125, XThis125).
'lo.collection$membership$lo.core*list'('addMem%1'('lo.collection$membership$lo.core*list^addMem'(XLbl126, XThis126)), XLbl126, XThis126).
'lo.collection$membership$lo.core*list'('delMem%3'(XV516, XV517, XV518), XLbl127, XThis127) :- !,
    'lo.collection$membership$lo.core*list@delMem'(XV516, XV517, XV518, XLbl127, XThis127).
'lo.collection$membership$lo.core*list'('delMem%1'('lo.collection$membership$lo.core*list^delMem'(XLbl128, XThis128)), XLbl128, XThis128).
'lo.collection$membership$lo.core*list'('in%2'(XV524, XV525), XLbl129, XThis129) :- !,
    'lo.collection$membership$lo.core*list@in'(XV524, XV525, XLbl129, XThis129).
'lo.collection$membership$lo.core*list'('in%1'('lo.collection$membership$lo.core*list^in'(XLbl130, XThis130)), XLbl130, XThis130).
'lo.collection$membership$lo.core*list@empty'('lo.core#[]', XLbV45, XThV45) :- !.
'lo.collection$membership$lo.core*list@addMem'(XL, Xe, 'lo.core#,..'(Xe, XL), XLbV45, XThV45) :- !.
'lo.collection$membership$lo.core*list@addMem'(_, _, _, _, _) :- raise_exception('error'("addMem", 99, 5, 22)).
'lo.collection$membership$lo.core*list@delMem'(XL, Xe, XX782, XLbV45, XThV45) :- !,
    'lo.list@subtract'(Xe, XL, XX782).
'lo.collection$membership$lo.core*list@delMem'(_, _, _, _, _) :- raise_exception('error'("delMem", 100, 5, 28)).
'lo.collection$membership$lo.core*list@in'(Xe, XL, XLbV45, XThV45) :- 'lo.list@listEl'(Xe, XL).
'lo.list@lastEl'('lo.core#,..'(XX, 'lo.core#[]'), XX).
'lo.list@lastEl'('lo.core#,..'(X_27, XL), XX) :- 'lo.list@lastEl'(XL, XX).
'lo.list@mixin'('lo.core#[]', X_28, 'lo.core#[]') :- !.
'lo.list@mixin'('lo.core#,..'(XF, XL), XS, 'lo.core#,..'(XS, 'lo.core#,..'(XF, XX808))) :- !,
    'lo.list@mixin'(XL, XS, XX808).
'lo.list@mixin'(_, _, _) :- raise_exception('error'("mixin", 135, 3, 15)).
'lo.list@interleave'('lo.core#[]', X_29, 'lo.core#[]') :- !.
'lo.list@interleave'('lo.core#,..'(XF, XL), XS, 'lo.core#,..'(XF, XX821)) :- !,
    'lo.list@mixin'(XL, XS, XX821).
'lo.list@interleave'(_, _, _) :- raise_exception('error'("interleave", 131, 3, 20)).
'lo.list@zip'('lo.core#[]', X_30, 'lo.core#[]') :- !.
'lo.list@zip'(X_31, 'lo.core#[]', 'lo.core#[]') :- !.
'lo.list@zip'('lo.core#,..'(Xe1, Xl1), 'lo.core#,..'(Xe2, Xl2), 'lo.core#,..'((Xe1, Xe2), XX839)) :- !,
    'lo.list@zip'(Xl1, Xl2, XX839).
'lo.list@zip'(_, _, _) :- raise_exception('error'("zip", 139, 3, 15)).
'lo.list@unzip'('lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.list@unzip'('lo.core#,..'((Xe, Xf), Xl), 'lo.core#,..'(Xe, Xr1), 'lo.core#,..'(Xf, Xr2)) :- 'lo.list@unzip'(Xl, Xr1, Xr2).
'lo.list@iota'(XF, XT, 'lo.core#[]') :- 'lo.core@>'('lo.core$comp$lo.core*integer', XF, XT),
    !.
'lo.list@iota'(XF, XT, 'lo.core#,..'(XF, XX873)) :- 'lo.core@=<'('lo.core$comp$lo.core*integer', XF, XT),
    !,
    ocall('+%3'(XF, 1, XX870),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@iota'(XX870, XT, XX873).
'lo.list@iota'(_, _, _) :- raise_exception('error'("iota", 148, 3, 22)).
'lo.list@head'('lo.core#,..'(XE, X_32), XE) :- !.
'lo.list@head'(_, _) :- raise_exception('error'("head", 152, 3, 18)).
'lo.list@tail'('lo.core#,..'(X_33, XL), XL) :- !.
'lo.list@tail'(_, _) :- raise_exception('error'("tail", 155, 3, 18)).
'lo.list^<>'('_call%3'(XV359, XV360, XV361), 'lo.list^<>', _) :- 'lo.list@<>'(XV359, XV360, XV361).
'lo.list^listEl'('_call%2'(XV362, XV363), 'lo.list^listEl', _) :- 'lo.list@listEl'(XV362, XV363).
'lo.list^merge'('_call%3'(XV364, XV365, XV366), 'lo.list^merge', _) :- 'lo.list@merge'(XV364, XV365, XV366).
'lo.list^cat'('_call%3'(XV367, XV368, XV369), 'lo.list^cat', _) :- 'lo.list@cat'(XV367, XV368, XV369).
'lo.list^multiCat'('_call%2'(XV370, XV371), 'lo.list^multiCat', _) :- 'lo.list@multiCat'(XV370, XV371).
'lo.list^rev'('_call%3'(XV372, XV373, XV374), 'lo.list^rev', _) :- 'lo.list@rev'(XV372, XV373, XV374).
'lo.list^reverse'('_call%2'(XV375, XV376), 'lo.list^reverse', _) :- 'lo.list@reverse'(XV375, XV376).
'lo.list^drop'('_call%2'(XV377, XV378), 'lo.list^drop', _) :- 'lo.list@drop'(XV377, XV378).
'lo.list@neg2'(XX, XE) :- XE = XX,
    !,
    fail.
'lo.list@neg2'(XX, XE).
'lo.list^subtract'('_call%3'(XV379, XV380, XV381), 'lo.list^subtract', _) :- 'lo.list@subtract'(XV379, XV380, XV381).
'lo.list^length'('_call%2'(XV382, XV383), 'lo.list^length', _) :- 'lo.list@length'(XV382, XV383).
'lo.core$sizeable$lo.core*list^size'('_call%2'(XV384, XV385), 'lo.core$sizeable$lo.core*list^size'(XLbV39, XThV39), _) :- 'lo.core$sizeable$lo.core*list@size'(XV384, XV385, XLbV39, XThV39).
'lo.core$sizeable$lo.core*list^size'('_call%2'(XV388, XV389), 'lo.core$sizeable$lo.core*list^size'(XLbV39, XThV39), _) :- 'lo.core$sizeable$lo.core*list@size'(XV388, XV389, XLbV39, XThV39).
'lo.core$sizeable$lo.core*list^isEmpty'('_call%1'(XV390), 'lo.core$sizeable$lo.core*list^isEmpty'(XLbV39, XThV39), _) :- 'lo.core$sizeable$lo.core*list@isEmpty'(XV390, XLbV39, XThV39).
'lo.core$sizeable$lo.core*list^isEmpty'('_call%1'(XV392), 'lo.core$sizeable$lo.core*list^isEmpty'(XLbV39, XThV39), _) :- 'lo.core$sizeable$lo.core*list@isEmpty'(XV392, XLbV39, XThV39).
'lo.collection$folding$lo.core*list^foldLeft'('_call%4'(XV393, XV394, XV395, XV396), 'lo.collection$folding$lo.core*list^foldLeft'(XLbV40, XThV40), _) :- 'lo.collection$folding$lo.core*list@foldLeft'(XV393, XV394, XV395, XV396, XLbV40, XThV40).
'lo.collection$folding$lo.core*list^foldLeft'('_call%4'(XV401, XV402, XV403, XV404), 'lo.collection$folding$lo.core*list^foldLeft'(XLbV40, XThV40), _) :- 'lo.collection$folding$lo.core*list@foldLeft'(XV401, XV402, XV403, XV404, XLbV40, XThV40).
'lo.collection$folding$lo.core*list^foldRight'('_call%4'(XV405, XV406, XV407, XV408), 'lo.collection$folding$lo.core*list^foldRight'(XLbV40, XThV40), _) :- 'lo.collection$folding$lo.core*list@foldRight'(XV405, XV406, XV407, XV408, XLbV40, XThV40).
'lo.collection$folding$lo.core*list^foldRight'('_call%4'(XV413, XV414, XV415, XV416), 'lo.collection$folding$lo.core*list^foldRight'(XLbV40, XThV40), _) :- 'lo.collection$folding$lo.core*list@foldRight'(XV413, XV414, XV415, XV416, XLbV40, XThV40).
'lo.list^mapList'('_call%3'(XV417, XV418, XV419), 'lo.list^mapList', _) :- 'lo.list@mapList'(XV417, XV418, XV419).
'lo.collection$mapping$lo.core*list^//'('_call%3'(XV420, XV421, XV422), 'lo.collection$mapping$lo.core*list^//'(XLbV41, XThV41), _) :- 'lo.collection$mapping$lo.core*list@//'(XV420, XV421, XV422, XLbV41, XThV41).
'lo.collection$mapping$lo.core*list^//'('_call%3'(XV426, XV427, XV428), 'lo.collection$mapping$lo.core*list^//'(XLbV41, XThV41), _) :- 'lo.collection$mapping$lo.core*list@//'(XV426, XV427, XV428, XLbV41, XThV41).
'lo.list^filterList'('_call%3'(XV429, XV430, XV431), 'lo.list^filterList', _) :- 'lo.list@filterList'(XV429, XV430, XV431).
'^/'('_call%3'(XV432, XV433, XV434), '^/'(XLbV42, XThV42), _) :- 'lo.collection$filter$lo.core*list@^/'(XV432, XV433, XV434, XLbV42, XThV42).
'^/'('_call%3'(XV438, XV439, XV440), '^/'(XLbV42, XThV42), _) :- 'lo.collection$filter$lo.core*list@^/'(XV438, XV439, XV440, XLbV42, XThV42).
'lo.list^coerceList'('_call%2'(XV441, XV442), 'lo.list^coerceList', _) :- 'lo.list@coerceList'(XV441, XV442).
'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'('_call%2'(XV443, XV444), 'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'(XLbV43, XThV43), _) :- 'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XV443, XV444, XLbV43, XThV43).
'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'('_call%2'(XV447, XV448), 'lo.coerce$coercion$lo.core*list$lo.core*list^_coerce'(XLbV43, XThV43), _) :- 'lo.coerce$coercion$lo.core*list$lo.core*list@_coerce'(XV447, XV448, XLbV43, XThV43).
'lo.list^nthEl'('_call%3'(XV449, XV450, XV451), 'lo.list^nthEl', _) :- 'lo.list@nthEl'(XV449, XV450, XV451).
'lo.list^dropNth'('_call%3'(XV452, XV453, XV454), 'lo.list^dropNth', _) :- 'lo.list@dropNth'(XV452, XV453, XV454).
'lo.list^replaceNth'('_call%4'(XV455, XV456, XV457, XV458), 'lo.list^replaceNth', _) :- 'lo.list@replaceNth'(XV455, XV456, XV457, XV458).
'lo.list^indexes'('_call%3'(XV459, XV460, XV461), 'lo.list^indexes', _) :- 'lo.list@indexes'(XV459, XV460, XV461).
'lo.list^listPairs'('_call%3'(XV462, XV463, XV464), 'lo.list^listPairs', _) :- 'lo.list@listPairs'(XV462, XV463, XV464).
'lo.collection$map$lo.core*list@one1'(XV, XIx, XL) :- 'lo.list@nthEl'(XL, XIx, XV),
    !.
'lo.collection$map$lo.core*list^present'('_call%3'(XV465, XV466, XV467), 'lo.collection$map$lo.core*list^present'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@present'(XV465, XV466, XV467, XLbV44, XThV44).
'lo.collection$map$lo.core*list^present'('_call%3'(XV471, XV472, XV473), 'lo.collection$map$lo.core*list^present'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@present'(XV471, XV472, XV473, XLbV44, XThV44).
'lo.collection$map$lo.core*list^_remove'('_call%3'(XV474, XV475, XV476), 'lo.collection$map$lo.core*list^_remove'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@_remove'(XV474, XV475, XV476, XLbV44, XThV44).
'lo.collection$map$lo.core*list^_remove'('_call%3'(XV480, XV481, XV482), 'lo.collection$map$lo.core*list^_remove'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@_remove'(XV480, XV481, XV482, XLbV44, XThV44).
'lo.collection$map$lo.core*list^keys'('_call%2'(XV484, XV485), 'lo.collection$map$lo.core*list^keys'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@keys'(XV484, XV485, XLbV44, XThV44).
'lo.collection$map$lo.core*list^keys'('_call%2'(XV488, XV489), 'lo.collection$map$lo.core*list^keys'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@keys'(XV488, XV489, XLbV44, XThV44).
'lo.collection$map$lo.core*list^pairs'('_call%2'(XV490, XV491), 'lo.collection$map$lo.core*list^pairs'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@pairs'(XV490, XV491, XLbV44, XThV44).
'lo.collection$map$lo.core*list^pairs'('_call%2'(XV494, XV495), 'lo.collection$map$lo.core*list^pairs'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@pairs'(XV494, XV495, XLbV44, XThV44).
'lo.collection$map$lo.core*list^values'('_call%2'(XV496, XV497), 'lo.collection$map$lo.core*list^values'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@values'(XV496, XV497, XLbV44, XThV44).
'lo.collection$map$lo.core*list^values'('_call%2'(XV500, XV501), 'lo.collection$map$lo.core*list^values'(XLbV44, XThV44), _) :- 'lo.collection$map$lo.core*list@values'(XV500, XV501, XLbV44, XThV44).
'lo.collection$membership$lo.core*list^addMem'('_call%3'(XV504, XV505, XV506), 'lo.collection$membership$lo.core*list^addMem'(XLbV45, XThV45), _) :- 'lo.collection$membership$lo.core*list@addMem'(XV504, XV505, XV506, XLbV45, XThV45).
'lo.collection$membership$lo.core*list^addMem'('_call%3'(XV510, XV511, XV512), 'lo.collection$membership$lo.core*list^addMem'(XLbV45, XThV45), _) :- 'lo.collection$membership$lo.core*list@addMem'(XV510, XV511, XV512, XLbV45, XThV45).
'lo.collection$membership$lo.core*list^delMem'('_call%3'(XV513, XV514, XV515), 'lo.collection$membership$lo.core*list^delMem'(XLbV45, XThV45), _) :- 'lo.collection$membership$lo.core*list@delMem'(XV513, XV514, XV515, XLbV45, XThV45).
'lo.collection$membership$lo.core*list^delMem'('_call%3'(XV519, XV520, XV521), 'lo.collection$membership$lo.core*list^delMem'(XLbV45, XThV45), _) :- 'lo.collection$membership$lo.core*list@delMem'(XV519, XV520, XV521, XLbV45, XThV45).
'lo.collection$membership$lo.core*list^in'('_call%2'(XV522, XV523), 'lo.collection$membership$lo.core*list^in'(XLbV45, XThV45), _) :- 'lo.collection$membership$lo.core*list@in'(XV522, XV523, XLbV45, XThV45).
'lo.collection$membership$lo.core*list^in'('_call%2'(XV526, XV527), 'lo.collection$membership$lo.core*list^in'(XLbV45, XThV45), _) :- 'lo.collection$membership$lo.core*list@in'(XV526, XV527, XLbV45, XThV45).
'lo.list^lastEl'('_call%2'(XV528, XV529), 'lo.list^lastEl', _) :- 'lo.list@lastEl'(XV528, XV529).
'lo.list^mixin'('_call%3'(XV530, XV531, XV532), 'lo.list^mixin', _) :- 'lo.list@mixin'(XV530, XV531, XV532).
'lo.list^interleave'('_call%3'(XV533, XV534, XV535), 'lo.list^interleave', _) :- 'lo.list@interleave'(XV533, XV534, XV535).
'lo.list^zip'('_call%3'(XV536, XV537, XV538), 'lo.list^zip', _) :- 'lo.list@zip'(XV536, XV537, XV538).
'lo.list^unzip'('_call%3'(XV539, XV540, XV541), 'lo.list^unzip', _) :- 'lo.list@unzip'(XV539, XV540, XV541).
'lo.list^iota'('_call%3'(XV542, XV543, XV544), 'lo.list^iota', _) :- 'lo.list@iota'(XV542, XV543, XV544).
'lo.list^head'('_call%2'(XV545, XV546), 'lo.list^head', _) :- 'lo.list@head'(XV545, XV546).
'lo.list^tail'('_call%2'(XV547, XV548), 'lo.list^tail', _) :- 'lo.list@tail'(XV547, XV548).
