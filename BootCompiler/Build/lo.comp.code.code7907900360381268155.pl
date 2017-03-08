'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.code'e'*'n14o14'()14'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.registers'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.instructions'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I2'assem'CT4t'lo.comp.term*term'Lt'lo.comp.code.instructions*instruction'Lt'lo.comp.code.code*litrl'LT3SSt'lo.comp.term*tloc't'lo.comp.code.code*assem''litrl'CT2St'lo.comp.term*term't'lo.comp.code.code*litrl'\"s\"I2'assem'Yt'lo.comp.code.code*assem'I0'litrl'Yt'lo.comp.code.code*litrl'I0\"n0o0'()0'n0o0'()0'n1o1'()1'n2o2'()2's'lo.core$display$lo.comp.code.code*assem's\"c'lo.core$display'T1t'lo.comp.code.code*assem'T0\"").
'lo.comp.code.code@init'() :- !.
'lo.comp.code.code#assem'('assem%1'('lo.comp.code.code@assem'())) :- !.
'lo.comp.code.code#litrl'('litrl%1'('lo.comp.code.code@litrl'())) :- !.
'lo.comp.code.code@dispIns'('lo.core#[]', X_2532, 'lo.core#[]') :- !.
'lo.comp.code.code@dispIns'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XLb), XL), XLits, 'lo.core#,..'('lo.core#ss'(XLb), 'lo.core#,..'('lo.core#ss'(":
"), XX38323))) :- !,
    'lo.comp.code.code@dispIns'(XL, XLits, XX38323).
'lo.comp.code.code@dispIns'('lo.core#,..'(XI, XL), XLits, 'lo.core#,..'('lo.core#ss'("  "), 'lo.core#,..'(XX38333, 'lo.core#,..'('lo.core#ss'("
"), XX38337)))) :- !,
    'lo.comp.code.instructions@showIns'(XI, XLits, XX38333),
    'lo.comp.code.code@dispIns'(XL, XLits, XX38337).
'lo.comp.code.code@dispIns'(_, _, _) :- raise_exception('error'("dispIns", 19, 3, 19)).
'lo.comp.code.code@anyLits'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.code.code@anyLits'(XL, 'lo.core#,..'(XX38347, 'lo.core#,..'('lo.core#ss'(" literals
"), 'lo.core#[]'))) :- !,
    ocall('size%2'(XL, XX38345),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('disp%2'(XX38345, XX38347),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer').
'lo.comp.code.code@anyLits'(_, _) :- raise_exception('error'("anyLits", 24, 3, 17)).
'lo.comp.code.code@dispCodeSeg'('lo.comp.code.code#assem'(XNm, XIns, XLits, XSrcMap), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("program: "), 'lo.core#,..'(XX38360, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#,..'('lo.core#ssSeq'(XX38379), 'lo.core#,..'('lo.core#ssSeq'(XX38382), 'lo.core#[]'))))))) :- !,
    ocall('disp%2'(XNm, XX38360),'lo.core$display$lo.comp.term*term','lo.core$display$lo.comp.term*term'),
    ocall('_empty%1'(XXV78),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('foldLeft%4'('lo.comp.code.code@$12', XXV78, XLits, XX38377),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    'lo.comp.code.code@dispIns'(XIns, XX38377, XX38379),
    'lo.comp.code.code@anyLits'(XLits, XX38382).
'lo.comp.code.code@dispCodeSeg'(_, _) :- raise_exception('error'("dispCodeSeg", 16, 3, 167)).
'lo.core$display$lo.comp.code.code*assem'('lo.core$display$lo.comp.code.code*assem%1'('lo.core$display$lo.comp.code.code*assem')) :- !.
'lo.core$display$lo.comp.code.code*assem'('disp%2'(XV4807, XV4808), XLbl315, XThis315) :- !,
    'lo.core$display$lo.comp.code.code*assem@disp'(XV4807, XV4808, XLbl315, XThis315).
'lo.core$display$lo.comp.code.code*assem'('disp%1'('lo.core$display$lo.comp.code.code*assem^disp'(XLbl316, XThis316)), XLbl316, XThis316).
'lo.core$display$lo.comp.code.code*assem@disp'(XSeg, XX38393, XLbV421, XThV421) :- !,
    'lo.comp.code.code@dispCodeSeg'(XSeg, XX38393).
'lo.core$display$lo.comp.code.code*assem@disp'(_, _, _, _) :- raise_exception('error'("disp", 12, 5, 29)).
'lo.comp.code.code@dispLits'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.code.code@dispLits'('lo.core#,..'('lo.comp.code.code#litrl'(XLbl, XTerm), XL), 'lo.core#,..'('lo.core#ss'(XLbl), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XX38405, 'lo.core#,..'('lo.core#ss'("
"), XX38409))))) :- !,
    ocall('disp%2'(XTerm, XX38405),'lo.core$display$lo.comp.term*term','lo.core$display$lo.comp.term*term'),
    'lo.comp.code.code@dispLits'(XL, XX38409).
'lo.comp.code.code@dispLits'(_, _) :- raise_exception('error'("dispLits", 28, 3, 18)).
'lo.comp.code.code^dispIns'('_call%3'(XV4798, XV4799, XV4800), 'lo.comp.code.code^dispIns', _) :- 'lo.comp.code.code@dispIns'(XV4798, XV4799, XV4800).
'lo.comp.code.code^anyLits'('_call%2'(XV4801, XV4802), 'lo.comp.code.code^anyLits', _) :- 'lo.comp.code.code@anyLits'(XV4801, XV4802).
'lo.comp.code.code@$12'('_call%3'(XM, 'lo.comp.code.code#litrl'(XN, XT), XX38371), 'lo.comp.code.code@$12', _) :- !,
    ocall('_put%4'(XM, XN, XT, XX38371),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.code.code@$12'(_, _, _) :- raise_exception('error'("lambda", 16, 113, 23)).
'lo.comp.code.code^dispCodeSeg'('_call%2'(XV4803, XV4804), 'lo.comp.code.code^dispCodeSeg', _) :- 'lo.comp.code.code@dispCodeSeg'(XV4803, XV4804).
'lo.core$display$lo.comp.code.code*assem^disp'('_call%2'(XV4805, XV4806), 'lo.core$display$lo.comp.code.code*assem^disp'(XLbV421, XThV421), _) :- 'lo.core$display$lo.comp.code.code*assem@disp'(XV4805, XV4806, XLbV421, XThV421).
'lo.core$display$lo.comp.code.code*assem^disp'('_call%2'(XV4809, XV4810), 'lo.core$display$lo.comp.code.code*assem^disp'(XLbV421, XThV421), _) :- 'lo.core$display$lo.comp.code.code*assem@disp'(XV4809, XV4810, XLbV421, XThV421).
'lo.comp.code.code^dispLits'('_call%2'(XV4811, XV4812), 'lo.comp.code.code^dispLits', _) :- 'lo.comp.code.code@dispLits'(XV4811, XV4812).
