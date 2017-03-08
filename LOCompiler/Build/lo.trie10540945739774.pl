'#pkg'("n7o7'()7'n2o2'pkg's'lo.trie's'1.0.0'n4o4'()4'n2o2'import'e'private'n2o2'pkg's'lo.index'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'s'I0's\"I1'trie':k'k':k'v':k'k':k'v'YUz2'lo.trie*trie'2k'k'k'v'I2'put'FT2Lk'k'k'v'Uz2'lo.trie*trie'2k'k'k'v''get'FT1Lk'k'Uz1'lo.core*option'1k'v'\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.trie@init'():- !.
'lo.trie#trie'('trie%1'('lo.trie@trie'())):- !.
'lo.trie#trie'('find%3'(XV19297, XV19298, XV19299), XLbl1643, XThis1643):- !,
    'lo.trie#trie@find'(XV19297, XV19298, XV19299, XLbl1643, XThis1643).
'lo.trie#trie'('find%1'('lo.trie#trie^find'(XLbl1644, XThis1644)), XLbl1644, XThis1644).
'lo.trie#trie'('put%3'(XV19303, XV19304, XV19305), XLbl1645, XThis1645):- !,
    'lo.trie#trie@put'(XV19303, XV19304, XV19305, XLbl1645, XThis1645).
'lo.trie#trie'('put%1'('lo.trie#trie^put'(XLbl1646, XThis1646)), XLbl1646, XThis1646).
'lo.trie#trie'('get%2'(XV19308, XV19309), XLbl1647, XThis1647):- !,
    'lo.trie#trie@get'(XV19308, XV19309, XLbl1647, XThis1647).
'lo.trie#trie'('get%1'('lo.trie#trie^get'(XLbl1648, XThis1648)), XLbl1648, XThis1648).
'lo.trie#trie@find'(XM, XK, XT, XLbV1529, XThV1529):- XLbV1529 = 'lo.trie#trie'(Xequality248, XVl, XEntries),
    ocall('present%3'(XM, XK, XT),'lo.collection$map$lo.index*map'(Xequality248),'lo.collection$map$lo.index*map'(Xequality248)),
    !.
'lo.trie#trie@find'(XM, XK, 'lo.trie#trie'(Xequality248, 'lo.core#none', XXV2978), XLbV1529, XThV1529):- XLbV1529 = 'lo.trie#trie'(Xequality248, XVl, XEntries),
    !,
    ocall('_empty%1'(XXV2978),'lo.collection$map$lo.index*map'(Xequality248),'lo.collection$map$lo.index*map'(Xequality248)).
'lo.trie#trie@find'(_, _, _):- raise_exception('error'("lo.trie#trie@find", 15, 5, 32)).
'lo.trie#trie@put'('lo.core#[]', XV, 'lo.trie#trie'(Xequality248, 'lo.core#some'(XV), XEntries), XLbV1529, XThV1529):- XLbV1529 = 'lo.trie#trie'(Xequality248, XVl, XEntries),
    !.
'lo.trie#trie@put'('lo.core#,..'(Xk, Xl), XV, 'lo.trie#trie'(Xequality248, XVl, XXe2775), XLbV1529, XThV1529):- XLbV1529 = 'lo.trie#trie'(Xequality248, XVl, XEntries),
    !,
    'lo.trie#trie@find'(XEntries, Xk, XXd23079, XLbV1529, XThV1529),
    ocall('put%1'(XXV2979),XXd23079,XXd23079),
    ocall('_put%1'(XXV2980),'lo.collection$map$lo.index*map'(Xequality248),'lo.collection$map$lo.index*map'(Xequality248)),
    ocall('_call%3'(Xl, Xv, XXe2774),XXV2979,XXV2979),
    ocall('_call%4'(XEntries, Xk, XXe2774, XXe2775),XXV2980,XXV2980).
'lo.trie#trie@put'(_, _, _):- raise_exception('error'("lo.trie#trie@put", 11, 5, 34)).
'lo.trie#trie@get'('lo.core#[]', XVl, XLbV1529, XThV1529):- XLbV1529 = 'lo.trie#trie'(Xequality248, XVl, XEntries),
    !.
'lo.trie#trie@get'('lo.core#,..'(Xk, Xl), XXe2776, XLbV1529, XThV1529):- XLbV1529 = 'lo.trie#trie'(Xequality248, XVl, XEntries),
    ocall('present%3'(XEntries, Xk, Xsub),'lo.collection$map$lo.index*map'(Xequality248),'lo.collection$map$lo.index*map'(Xequality248)),
    !,
    ocall('get%1'(XXV2981),Xsub,Xsub),
    ocall('_call%2'(Xl, XXe2776),XXV2981,XXV2981).
'lo.trie#trie@get'(_, _):- raise_exception('error'("lo.trie#trie@get", 18, 5, 13)).
'lo.trie#trie^find'('_call%3'(XV19294, XV19295, XV19296), 'lo.trie#trie^find'(XLbV1529, XThV1529), _):- 'lo.trie#trie@find'(XV19294, XV19295, XV19296, XLbV1529, XThV1529).
'lo.trie#trie^put'('_call%3'(XV19300, XV19301, XV19302), 'lo.trie#trie^put'(XLbV1529, XThV1529), _):- 'lo.trie#trie@put'(XV19300, XV19301, XV19302, XLbV1529, XThV1529).
'lo.trie#trie^get'('_call%2'(XV19306, XV19307), 'lo.trie#trie^get'(XLbV1529, XThV1529), _):- 'lo.trie#trie@get'(XV19306, XV19307, XLbV1529, XThV1529).
