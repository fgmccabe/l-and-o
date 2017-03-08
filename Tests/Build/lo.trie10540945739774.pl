'#pkg'("n7o7'()7'n2o2'pkg's'lo.trie's'1.0.0'n4o4'()4'n2o2'import'e'private'n2o2'pkg's'lo.index'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'s'I0's\"I1'trie':k'k':k'v':k'k':k'v'YUz2'lo.trie*trie'2k'k'k'v'I2'put'FT2Lk'k'k'v'Uz2'lo.trie*trie'2k'k'k'v''get'FT1Lk'k'Uz1'lo.core*option'1k'v'\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.trie@init'():- !.
'lo.trie#trie'('trie%1'('lo.trie@trie'())):- !.
'lo.trie#trie'('find%3'(XV19829, XV19830, XV19831), XLbl4118, XThis4118):- !,
    'lo.trie#trie@find'(XV19829, XV19830, XV19831, XLbl4118, XThis4118).
'lo.trie#trie'('find%1'('lo.trie#trie^find'(XLbl4119, XThis4119)), XLbl4119, XThis4119).
'lo.trie#trie'('put%3'(XV19835, XV19836, XV19837), XLbl4120, XThis4120):- !,
    'lo.trie#trie@put'(XV19835, XV19836, XV19837, XLbl4120, XThis4120).
'lo.trie#trie'('put%1'('lo.trie#trie^put'(XLbl4121, XThis4121)), XLbl4121, XThis4121).
'lo.trie#trie'('get%2'(XV19840, XV19841), XLbl4122, XThis4122):- !,
    'lo.trie#trie@get'(XV19840, XV19841, XLbl4122, XThis4122).
'lo.trie#trie'('get%1'('lo.trie#trie^get'(XLbl4123, XThis4123)), XLbl4123, XThis4123).
'lo.trie#trie@find'(XM, XK, XT, XLbV1790, XThV1790):- XLbV1790 = 'lo.trie#trie'(Xequality848, XVl, XEntries),
    ocall('present%3'(XM, XK, XT),'lo.collection$map$lo.index*map'(Xequality848),'lo.collection$map$lo.index*map'(Xequality848)),
    !.
'lo.trie#trie@find'(XM, XK, 'lo.trie#trie'(Xequality848, 'lo.core#none', XXV2064), XLbV1790, XThV1790):- XLbV1790 = 'lo.trie#trie'(Xequality848, XVl, XEntries),
    !,
    ocall('_empty%1'(XXV2064),'lo.collection$map$lo.index*map'(Xequality848),'lo.collection$map$lo.index*map'(Xequality848)).
'lo.trie#trie@find'(_, _, _):- raise_exception('error'("lo.trie#trie@find", 15, 5, 32)).
'lo.trie#trie@put'('lo.core#[]', XV, 'lo.trie#trie'(Xequality848, 'lo.core#some'(XV), XEntries), XLbV1790, XThV1790):- XLbV1790 = 'lo.trie#trie'(Xequality848, XVl, XEntries),
    !.
'lo.trie#trie@put'('lo.core#,..'(Xk, Xl), XV, 'lo.trie#trie'(Xequality848, XVl, XXe2045), XLbV1790, XThV1790):- XLbV1790 = 'lo.trie#trie'(Xequality848, XVl, XEntries),
    !,
    'lo.trie#trie@find'(XEntries, Xk, XXd9561, XLbV1790, XThV1790),
    ocall('put%1'(XXV2065),XXd9561,XXd9561),
    ocall('_put%1'(XXV2066),'lo.collection$map$lo.index*map'(Xequality848),'lo.collection$map$lo.index*map'(Xequality848)),
    ocall('_call%3'(Xl, Xv, XXe2044),XXV2065,XXV2065),
    ocall('_call%4'(XEntries, Xk, XXe2044, XXe2045),XXV2066,XXV2066).
'lo.trie#trie@put'(_, _, _):- raise_exception('error'("lo.trie#trie@put", 11, 5, 34)).
'lo.trie#trie@get'('lo.core#[]', XVl, XLbV1790, XThV1790):- XLbV1790 = 'lo.trie#trie'(Xequality848, XVl, XEntries),
    !.
'lo.trie#trie@get'('lo.core#,..'(Xk, Xl), XXe2046, XLbV1790, XThV1790):- XLbV1790 = 'lo.trie#trie'(Xequality848, XVl, XEntries),
    ocall('present%3'(XEntries, Xk, Xsub),'lo.collection$map$lo.index*map'(Xequality848),'lo.collection$map$lo.index*map'(Xequality848)),
    !,
    ocall('get%1'(XXV2067),Xsub,Xsub),
    ocall('_call%2'(Xl, XXe2046),XXV2067,XXV2067).
'lo.trie#trie@get'(_, _):- raise_exception('error'("lo.trie#trie@get", 18, 5, 13)).
'lo.trie#trie^find'('_call%3'(XV19826, XV19827, XV19828), 'lo.trie#trie^find'(XLbV1790, XThV1790), _):- 'lo.trie#trie@find'(XV19826, XV19827, XV19828, XLbV1790, XThV1790).
'lo.trie#trie^put'('_call%3'(XV19832, XV19833, XV19834), 'lo.trie#trie^put'(XLbV1790, XThV1790), _):- 'lo.trie#trie@put'(XV19832, XV19833, XV19834, XLbV1790, XThV1790).
'lo.trie#trie^get'('_call%2'(XV19838, XV19839), 'lo.trie#trie^get'(XLbV1790, XThV1790), _):- 'lo.trie#trie@get'(XV19838, XV19839, XLbV1790, XThV1790).
