'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.base64'e'*'n10o10'()10'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I2'encode64'FT1LiLi'decode64'FT1LiLi\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.base64@init'() :- !.
'lo.comp.base64@mapToSix'(XW, XX9448, XX9451, XX9454, XX9456) :- 'lo.bits@.>>.'(XW, 18, XX9447),
    'lo.bits@.&.'(XX9447, 63, XX9448),
    'lo.bits@.>>.'(XW, 12, XX9450),
    'lo.bits@.&.'(XX9450, 63, XX9451),
    'lo.bits@.>>.'(XW, 6, XX9453),
    'lo.bits@.&.'(XX9453, 63, XX9454),
    'lo.bits@.&.'(XW, 63, XX9456).
'lo.comp.base64@enc64'(0, 65).
'lo.comp.base64@enc64'(1, 66).
'lo.comp.base64@enc64'(2, 67).
'lo.comp.base64@enc64'(3, 68).
'lo.comp.base64@enc64'(4, 69).
'lo.comp.base64@enc64'(5, 70).
'lo.comp.base64@enc64'(6, 71).
'lo.comp.base64@enc64'(7, 72).
'lo.comp.base64@enc64'(8, 73).
'lo.comp.base64@enc64'(9, 74).
'lo.comp.base64@enc64'(10, 75).
'lo.comp.base64@enc64'(11, 76).
'lo.comp.base64@enc64'(12, 77).
'lo.comp.base64@enc64'(13, 78).
'lo.comp.base64@enc64'(14, 79).
'lo.comp.base64@enc64'(15, 80).
'lo.comp.base64@enc64'(16, 81).
'lo.comp.base64@enc64'(17, 82).
'lo.comp.base64@enc64'(18, 83).
'lo.comp.base64@enc64'(19, 84).
'lo.comp.base64@enc64'(20, 85).
'lo.comp.base64@enc64'(21, 86).
'lo.comp.base64@enc64'(22, 87).
'lo.comp.base64@enc64'(23, 88).
'lo.comp.base64@enc64'(24, 89).
'lo.comp.base64@enc64'(25, 90).
'lo.comp.base64@enc64'(26, 97).
'lo.comp.base64@enc64'(27, 98).
'lo.comp.base64@enc64'(28, 99).
'lo.comp.base64@enc64'(29, 100).
'lo.comp.base64@enc64'(30, 101).
'lo.comp.base64@enc64'(31, 102).
'lo.comp.base64@enc64'(32, 103).
'lo.comp.base64@enc64'(33, 104).
'lo.comp.base64@enc64'(34, 105).
'lo.comp.base64@enc64'(35, 106).
'lo.comp.base64@enc64'(36, 107).
'lo.comp.base64@enc64'(37, 108).
'lo.comp.base64@enc64'(38, 109).
'lo.comp.base64@enc64'(39, 110).
'lo.comp.base64@enc64'(40, 111).
'lo.comp.base64@enc64'(41, 112).
'lo.comp.base64@enc64'(42, 113).
'lo.comp.base64@enc64'(43, 114).
'lo.comp.base64@enc64'(44, 115).
'lo.comp.base64@enc64'(45, 116).
'lo.comp.base64@enc64'(46, 117).
'lo.comp.base64@enc64'(47, 118).
'lo.comp.base64@enc64'(48, 119).
'lo.comp.base64@enc64'(49, 120).
'lo.comp.base64@enc64'(50, 121).
'lo.comp.base64@enc64'(51, 122).
'lo.comp.base64@enc64'(52, 48).
'lo.comp.base64@enc64'(53, 49).
'lo.comp.base64@enc64'(54, 50).
'lo.comp.base64@enc64'(55, 51).
'lo.comp.base64@enc64'(56, 52).
'lo.comp.base64@enc64'(57, 53).
'lo.comp.base64@enc64'(58, 54).
'lo.comp.base64@enc64'(59, 55).
'lo.comp.base64@enc64'(60, 56).
'lo.comp.base64@enc64'(61, 57).
'lo.comp.base64@enc64'(62, 43).
'lo.comp.base64@enc64'(63, 47).
'lo.comp.base64@encByte'(XB, Xb) :- 'lo.comp.base64@enc64'(XB, Xb),
    !.
'lo.comp.base64@encByte'(_, _) :- raise_exception('error'("encByte", 46, 3, 29)).
'lo.comp.base64@encodeWord'(XW, XO, 'lo.core#,..'(XX9469, 'lo.core#,..'(XX9471, 'lo.core#,..'(XX9473, 'lo.core#,..'(XX9475, XO))))) :- 'lo.comp.base64@mapToSix'(XW, XUp, XHi, XMd, XLw),
    !,
    'lo.comp.base64@encByte'(XUp, XX9469),
    'lo.comp.base64@encByte'(XHi, XX9471),
    'lo.comp.base64@encByte'(XMd, XX9473),
    'lo.comp.base64@encByte'(XLw, XX9475).
'lo.comp.base64@encodeWord'(_, _, _) :- raise_exception('error'("encodeWord", 18, 3, 101)).
'lo.comp.base64@encLast'(XUp, XHi, 0, XRest, 'lo.core#,..'(XX9485, 'lo.core#,..'(XX9487, XRest))) :- !,
    'lo.comp.base64@encByte'(XUp, XX9485),
    'lo.comp.base64@encByte'(XHi, XX9487).
'lo.comp.base64@encLast'(XUp, XHi, XMd, XRest, 'lo.core#,..'(XX9496, 'lo.core#,..'(XX9498, 'lo.core#,..'(XX9500, XRest)))) :- !,
    'lo.comp.base64@encByte'(XUp, XX9496),
    'lo.comp.base64@encByte'(XHi, XX9498),
    'lo.comp.base64@encByte'(XMd, XX9500).
'lo.comp.base64@encLast'(_, _, _, _, _) :- raise_exception('error'("encLast", 11, 3, 57)).
'lo.comp.base64@encode64'('lo.core#,..'(XH, 'lo.core#,..'(XM, 'lo.core#,..'(XL, XR))), XX9521) :- !,
    'lo.bits@.<<.'(XH, 16, XX9513),
    'lo.bits@.<<.'(XM, 8, XX9515),
    'lo.bits@.|.'(XX9513, XX9515, XX9516),
    'lo.bits@.|.'(XX9516, XL, XX9518),
    'lo.comp.base64@encode64'(XR, XX9520),
    'lo.comp.base64@encodeWord'(XX9518, XX9520, XX9521).
'lo.comp.base64@encode64'('lo.core#,..'(XH, 'lo.core#,..'(XM, 'lo.core#[]')), XX9541) :- 'lo.bits@.<<.'(XH, 16, XX9528),
    'lo.bits@.<<.'(XM, 8, XX9530),
    'lo.bits@.|.'(XX9528, XX9530, XX9531),
    'lo.comp.base64@mapToSix'(XX9531, XUp, XHi, XMd, X_667),
    !,
    'lo.comp.base64@encLast'(XUp, XHi, XMd, 'lo.core#,..'(61, 'lo.core#[]'), XX9541).
'lo.comp.base64@encode64'('lo.core#,..'(XH, 'lo.core#[]'), XX9557) :- 'lo.bits@.<<.'(XH, 16, XX9546),
    'lo.comp.base64@mapToSix'(XX9546, XUp, XHi, XMd, X_668),
    !,
    'lo.comp.base64@encLast'(XUp, XHi, XMd, 'lo.core#,..'(61, 'lo.core#,..'(61, 'lo.core#[]')), XX9557).
'lo.comp.base64@encode64'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.base64@encode64'(_, _) :- raise_exception('error'("encode64", 5, 3, 73)).
'lo.comp.base64@decode64'('lo.core#,..'(XU, 'lo.core#,..'(XH, 'lo.core#,..'(61, 'lo.core#,..'(61, 'lo.core#[]')))), 'lo.core#,..'(XB1, 'lo.core#[]')) :- 'lo.comp.base64@enc64'(XUp, XU),
    'lo.comp.base64@enc64'(XHi, XH),
    'lo.bits@.<<.'(XUp, 18, XX9573),
    'lo.bits@.<<.'(XHi, 12, XX9575),
    'lo.bits@.|.'(XX9573, XX9575, XX9576),
    XW = XX9576,
    'lo.bits@.>>.'(XW, 16, XX9579),
    'lo.bits@.&.'(XX9579, 255, XX9580),
    XB1 = XX9580,
    !.
'lo.comp.base64@decode64'('lo.core#,..'(XU, 'lo.core#,..'(XH, 'lo.core#,..'(XM, 'lo.core#,..'(61, 'lo.core#[]')))), 'lo.core#,..'(XB1, 'lo.core#,..'(XB2, 'lo.core#[]'))) :- 'lo.comp.base64@enc64'(XUp, XU),
    'lo.comp.base64@enc64'(XHi, XH),
    'lo.comp.base64@enc64'(XMd, XM),
    'lo.bits@.<<.'(XUp, 18, XX9600),
    'lo.bits@.<<.'(XHi, 12, XX9602),
    'lo.bits@.|.'(XX9600, XX9602, XX9603),
    'lo.bits@.<<.'(XMd, 6, XX9605),
    'lo.bits@.|.'(XX9603, XX9605, XX9606),
    XW = XX9606,
    'lo.bits@.>>.'(XW, 16, XX9609),
    'lo.bits@.&.'(XX9609, 255, XX9610),
    XB1 = XX9610,
    'lo.bits@.>>.'(XW, 8, XX9613),
    'lo.bits@.&.'(XX9613, 255, XX9614),
    XB2 = XX9614,
    !.
'lo.comp.base64@decode64'('lo.core#,..'(XU, 'lo.core#,..'(XH, 'lo.core#,..'(XM, 'lo.core#,..'(XL, XRest)))), 'lo.core#,..'(XB1, 'lo.core#,..'(XB2, 'lo.core#,..'(XB3, XX9663)))) :- 'lo.comp.base64@enc64'(XUp, XU),
    'lo.comp.base64@enc64'(XHi, XH),
    'lo.comp.base64@enc64'(XMd, XM),
    'lo.comp.base64@enc64'(XLw, XL),
    'lo.bits@.<<.'(XUp, 18, XX9639),
    'lo.bits@.<<.'(XHi, 12, XX9641),
    'lo.bits@.|.'(XX9639, XX9641, XX9642),
    'lo.bits@.<<.'(XMd, 6, XX9644),
    'lo.bits@.|.'(XX9642, XX9644, XX9645),
    'lo.bits@.|.'(XX9645, XLw, XX9647),
    XW = XX9647,
    'lo.bits@.>>.'(XW, 16, XX9650),
    'lo.bits@.&.'(XX9650, 255, XX9651),
    XB1 = XX9651,
    'lo.bits@.>>.'(XW, 8, XX9654),
    'lo.bits@.&.'(XX9654, 255, XX9655),
    XB2 = XX9655,
    'lo.bits@.&.'(XW, 255, XX9658),
    XB3 = XX9658,
    !,
    'lo.comp.base64@decode64'(XRest, XX9663).
'lo.comp.base64@decode64'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.base64@decode64'(_, _) :- raise_exception('error'("decode64", 22, 3, 126)).
'lo.comp.base64^mapToSix'('_call%5'(XV1553, XV1554, XV1555, XV1556, XV1557), 'lo.comp.base64^mapToSix', _) :- 'lo.comp.base64@mapToSix'(XV1553, XV1554, XV1555, XV1556, XV1557).
'lo.comp.base64^enc64'('_call%2'(XV1558, XV1559), 'lo.comp.base64^enc64', _) :- 'lo.comp.base64@enc64'(XV1558, XV1559).
'lo.comp.base64^encByte'('_call%2'(XV1560, XV1561), 'lo.comp.base64^encByte', _) :- 'lo.comp.base64@encByte'(XV1560, XV1561).
'lo.comp.base64^encodeWord'('_call%3'(XV1562, XV1563, XV1564), 'lo.comp.base64^encodeWord', _) :- 'lo.comp.base64@encodeWord'(XV1562, XV1563, XV1564).
'lo.comp.base64^encLast'('_call%5'(XV1565, XV1566, XV1567, XV1568, XV1569), 'lo.comp.base64^encLast', _) :- 'lo.comp.base64@encLast'(XV1565, XV1566, XV1567, XV1568, XV1569).
'lo.comp.base64^encode64'('_call%2'(XV1570, XV1571), 'lo.comp.base64^encode64', _) :- 'lo.comp.base64@encode64'(XV1570, XV1571).
'lo.comp.base64^decode64'('_call%2'(XV1572, XV1573), 'lo.comp.base64^decode64', _) :- 'lo.comp.base64@decode64'(XV1572, XV1573).
