'#pkg'("n7o7'()7'n2o2'pkg's'lo.trie'e'*'n4o4'()4'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.index'e'*'s'I0's\"I1'trie':k'k':k'v'YUz2'lo.trie*trie'2k'k'k'v'I2'get'FT1Lk'k'Uz1'lo.core*option'1k'v''put'FT2Lk'k'k'v'Uz2'lo.trie*trie'2k'k'k'v'\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.trie@init'() :- !.
'lo.trie#trie'('trie%1'('lo.trie@trie'())) :- !.
'lo.trie#trie'('find%3'(XV260, XV261, XV262), XLbl74, XThis74) :- !,
    'lo.trie#trie@find'(XV260, XV261, XV262, XLbl74, XThis74).
'lo.trie#trie'('find%1'('lo.trie#trie^find'(XLbl75, XThis75)), XLbl75, XThis75).
'lo.trie#trie'('put%3'(XV269, XV270, XV271), XLbl76, XThis76) :- !,
    'lo.trie#trie@put'(XV269, XV270, XV271, XLbl76, XThis76).
'lo.trie#trie'('put%1'('lo.trie#trie^put'(XLbl77, XThis77)), XLbl77, XThis77).
'lo.trie#trie'('get%2'(XV277, XV278), XLbl78, XThis78) :- !,
    'lo.trie#trie@get'(XV277, XV278, XLbl78, XThis78).
'lo.trie#trie'('get%1'('lo.trie#trie^get'(XLbl79, XThis79)), XLbl79, XThis79).
'lo.trie#trie@find'(XM, XK, XT, XLbV36, XThV36) :- XLbV36 = 'lo.trie#trie'(Xlo_core_equality_k1, XVl, XEntries),
    ocall('present%3'(XM, XK, XT),'lo.collection$map$lo.index*map'(Xlo_core_equality_k1),'lo.collection$map$lo.index*map'(Xlo_core_equality_k1)),
    !.
'lo.trie#trie@find'(XM, XK, 'lo.trie#trie'(Xlo_core_equality_k1, 'lo.core#none', XXV4), XLbV36, XThV36) :- XLbV36 = 'lo.trie#trie'(Xlo_core_equality_k1, XVl, XEntries),
    !,
    ocall('_empty%1'(XXV4),'lo.collection$map$lo.index*map'(Xlo_core_equality_k1),'lo.collection$map$lo.index*map'(Xlo_core_equality_k1)).
'lo.trie#trie@find'(_, _, _, _, _) :- raise_exception('error'("find", 15, 5, 32)).
'lo.trie#trie@put'('lo.core#[]', XV, 'lo.trie#trie'(Xlo_core_equality_k1, 'lo.core#some'(XV), XEntries), XLbV36, XThV36) :- XLbV36 = 'lo.trie#trie'(Xlo_core_equality_k1, XVl, XEntries),
    !.
'lo.trie#trie@put'('lo.core#,..'(Xk, Xl), XV, 'lo.trie#trie'(Xlo_core_equality_k1, XVl, XX685), XLbV36, XThV36) :- XLbV36 = 'lo.trie#trie'(Xlo_core_equality_k1, XVl, XEntries),
    !,
    'lo.trie#trie@find'(XEntries, Xk, XX684, XLbV36, XThV36),
    ocall('put%3'(Xl, Xv, XX681),XX684,XX684),
    ocall('_put%4'(XEntries, Xk, XX681, XX685),'lo.collection$map$lo.index*map'(Xlo_core_equality_k1),'lo.collection$map$lo.index*map'(Xlo_core_equality_k1)).
'lo.trie#trie@put'(_, _, _, _, _) :- raise_exception('error'("put", 11, 5, 34)).
'lo.trie#trie@get'('lo.core#[]', XVl, XLbV36, XThV36) :- XLbV36 = 'lo.trie#trie'(Xlo_core_equality_k1, XVl, XEntries),
    !.
'lo.trie#trie@get'('lo.core#,..'(Xk, Xl), XX700, XLbV36, XThV36) :- XLbV36 = 'lo.trie#trie'(Xlo_core_equality_k1, XVl, XEntries),
    ocall('present%3'(XEntries, Xk, Xsub),'lo.collection$map$lo.index*map'(Xlo_core_equality_k1),'lo.collection$map$lo.index*map'(Xlo_core_equality_k1)),
    !,
    ocall('get%2'(Xl, XX700),Xsub,Xsub).
'lo.trie#trie@get'(_, _, _, _) :- raise_exception('error'("get", 18, 5, 13)).
'lo.trie#trie^find'('_call%3'(XV257, XV258, XV259), 'lo.trie#trie^find'(XLbV36, XThV36), _) :- 'lo.trie#trie@find'(XV257, XV258, XV259, XLbV36, XThV36).
'lo.trie#trie^find'('_call%3'(XV263, XV264, XV265), 'lo.trie#trie^find'(XLbV36, XThV36), _) :- 'lo.trie#trie@find'(XV263, XV264, XV265, XLbV36, XThV36).
'lo.trie#trie^put'('_call%3'(XV266, XV267, XV268), 'lo.trie#trie^put'(XLbV36, XThV36), _) :- 'lo.trie#trie@put'(XV266, XV267, XV268, XLbV36, XThV36).
'lo.trie#trie^put'('_call%3'(XV272, XV273, XV274), 'lo.trie#trie^put'(XLbV36, XThV36), _) :- 'lo.trie#trie@put'(XV272, XV273, XV274, XLbV36, XThV36).
'lo.trie#trie^get'('_call%2'(XV275, XV276), 'lo.trie#trie^get'(XLbV36, XThV36), _) :- 'lo.trie#trie@get'(XV275, XV276, XLbV36, XThV36).
'lo.trie#trie^get'('_call%2'(XV279, XV280), 'lo.trie#trie^get'(XLbV36, XThV36), _) :- 'lo.trie#trie@get'(XV279, XV280, XLbV36, XThV36).
