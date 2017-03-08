'#pkg'("n7o7'()7'n2o2'pkg's'lo.trie's'1.0.0'n4o4'()4'n2o2'import'e'private'n2o2'pkg's'lo.index'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'s'I0's\"I1'trie':k'k':k'v':k'k':k'v'YUz2'lo.trie*trie'2k'k'k'v'I2'put'FT2Lk'k'k'v'Uz2'lo.trie*trie'2k'k'k'v''get'FT1Lk'k'Uz1'lo.core*option'1k'v'\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.trie@init'():- !.
'lo.trie#trie'('trie%1'('lo.trie@trie'())):- !.
'lo.trie#trie'('find%3'(XV18008, XV18009, XV18010), XLbl3741, XThis3741):- !,
    'lo.trie#trie@find'(XV18008, XV18009, XV18010, XLbl3741, XThis3741).
'lo.trie#trie'('find%1'('lo.trie#trie^find'(XLbl3742, XThis3742)), XLbl3742, XThis3742).
'lo.trie#trie'('put%3'(XV18014, XV18015, XV18016), XLbl3743, XThis3743):- !,
    'lo.trie#trie@put'(XV18014, XV18015, XV18016, XLbl3743, XThis3743).
'lo.trie#trie'('put%1'('lo.trie#trie^put'(XLbl3744, XThis3744)), XLbl3744, XThis3744).
'lo.trie#trie'('get%2'(XV18019, XV18020), XLbl3745, XThis3745):- !,
    'lo.trie#trie@get'(XV18019, XV18020, XLbl3745, XThis3745).
'lo.trie#trie'('get%1'('lo.trie#trie^get'(XLbl3746, XThis3746)), XLbl3746, XThis3746).
'lo.trie#trie@find'(XM, XK, XT, XLbV1618, XThV1618):- XLbV1618 = 'lo.trie#trie'(Xequality775, XVl, XEntries),
    ocall('present%3'(XM, XK, XT),'lo.collection$map$lo.index*map'(Xequality775),'lo.collection$map$lo.index*map'(Xequality775)),
    !.
'lo.trie#trie@find'(XM, XK, 'lo.trie#trie'(Xequality775, 'lo.core#none', XXV1869), XLbV1618, XThV1618):- XLbV1618 = 'lo.trie#trie'(Xequality775, XVl, XEntries),
    !,
    ocall('_empty%1'(XXV1869),'lo.collection$map$lo.index*map'(Xequality775),'lo.collection$map$lo.index*map'(Xequality775)).
'lo.trie#trie@find'(_, _, _):- raise_exception('error'("lo.trie#trie@find", 15, 5, 32)).
'lo.trie#trie@put'('lo.core#[]', XV, 'lo.trie#trie'(Xequality775, 'lo.core#some'(XV), XEntries), XLbV1618, XThV1618):- XLbV1618 = 'lo.trie#trie'(Xequality775, XVl, XEntries),
    !.
'lo.trie#trie@put'('lo.core#,..'(Xk, Xl), XV, 'lo.trie#trie'(Xequality775, XVl, XXe1855), XLbV1618, XThV1618):- XLbV1618 = 'lo.trie#trie'(Xequality775, XVl, XEntries),
    !,
    'lo.trie#trie@find'(XEntries, Xk, XXd8668, XLbV1618, XThV1618),
    ocall('put%1'(XXV1870),XXd8668,XXd8668),
    ocall('_put%1'(XXV1871),'lo.collection$map$lo.index*map'(Xequality775),'lo.collection$map$lo.index*map'(Xequality775)),
    ocall('_call%3'(Xl, Xv, XXe1854),XXV1870,XXV1870),
    ocall('_call%4'(XEntries, Xk, XXe1854, XXe1855),XXV1871,XXV1871).
'lo.trie#trie@put'(_, _, _):- raise_exception('error'("lo.trie#trie@put", 11, 5, 34)).
'lo.trie#trie@get'('lo.core#[]', XVl, XLbV1618, XThV1618):- XLbV1618 = 'lo.trie#trie'(Xequality775, XVl, XEntries),
    !.
'lo.trie#trie@get'('lo.core#,..'(Xk, Xl), XXe1856, XLbV1618, XThV1618):- XLbV1618 = 'lo.trie#trie'(Xequality775, XVl, XEntries),
    ocall('present%3'(XEntries, Xk, Xsub),'lo.collection$map$lo.index*map'(Xequality775),'lo.collection$map$lo.index*map'(Xequality775)),
    !,
    ocall('get%1'(XXV1872),Xsub,Xsub),
    ocall('_call%2'(Xl, XXe1856),XXV1872,XXV1872).
'lo.trie#trie@get'(_, _):- raise_exception('error'("lo.trie#trie@get", 18, 5, 13)).
'lo.trie#trie^find'('_call%3'(XV18005, XV18006, XV18007), 'lo.trie#trie^find'(XLbV1618, XThV1618), _):- 'lo.trie#trie@find'(XV18005, XV18006, XV18007, XLbV1618, XThV1618).
'lo.trie#trie^put'('_call%3'(XV18011, XV18012, XV18013), 'lo.trie#trie^put'(XLbV1618, XThV1618), _):- 'lo.trie#trie@put'(XV18011, XV18012, XV18013, XLbV1618, XThV1618).
'lo.trie#trie^get'('_call%2'(XV18017, XV18018), 'lo.trie#trie^get'(XLbV1618, XThV1618), _):- 'lo.trie#trie@get'(XV18017, XV18018, XLbV1618, XThV1618).
