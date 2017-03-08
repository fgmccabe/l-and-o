'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.base64's'0.0.1'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I2'encode64'FT1LiLi'decode64'FT1LiLi\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.base64@init'():- !.
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
'lo.comp.base64@encByte'(XB, Xb):- 'lo.comp.base64@enc64'(XB, Xb),
    !.
'lo.comp.base64@encByte'(_, _):- raise_exception('error'("lo.comp.base64@encByte", 46, 3, 29)).
'lo.comp.base64@encLast'(XUp, XHi, 0, XRest, 'lo.core#,..'(XXd26063, 'lo.core#,..'(XXd26064, XRest))):- !,
    'lo.comp.base64@encByte'(XUp, XXd26063),
    'lo.comp.base64@encByte'(XHi, XXd26064).
'lo.comp.base64@encLast'(XUp, XHi, XMd, XRest, 'lo.core#,..'(XXd26067, 'lo.core#,..'(XXd26068, 'lo.core#,..'(XXd26069, XRest)))):- !,
    'lo.comp.base64@encByte'(XUp, XXd26067),
    'lo.comp.base64@encByte'(XHi, XXd26068),
    'lo.comp.base64@encByte'(XMd, XXd26069).
'lo.comp.base64@encLast'(_, _, _, _, _):- raise_exception('error'("lo.comp.base64@encLast", 11, 3, 57)).
'lo.comp.base64@mapToSix'(XW, XXb11637, XXb11639, XXb11641, XXb11642):- 'lo.bits@.>>.'(XW, 18, XXb11636),
    'lo.bits@.&.'(XXb11636, 63, XXb11637),
    'lo.bits@.>>.'(XW, 12, XXb11638),
    'lo.bits@.&.'(XXb11638, 63, XXb11639),
    'lo.bits@.>>.'(XW, 6, XXb11640),
    'lo.bits@.&.'(XXb11640, 63, XXb11641),
    'lo.bits@.&.'(XW, 63, XXb11642).
'lo.comp.base64@encodeWord'(XW, XO, 'lo.core#,..'(XXd26073, 'lo.core#,..'(XXd26074, 'lo.core#,..'(XXd26075, 'lo.core#,..'(XXd26076, XO))))):- 'lo.comp.base64@mapToSix'(XW, XUp, XHi, XMd, XLw),
    !,
    'lo.comp.base64@encByte'(XUp, XXd26073),
    'lo.comp.base64@encByte'(XHi, XXd26074),
    'lo.comp.base64@encByte'(XMd, XXd26075),
    'lo.comp.base64@encByte'(XLw, XXd26076).
'lo.comp.base64@encodeWord'(_, _, _):- raise_exception('error'("lo.comp.base64@encodeWord", 18, 3, 101)).
'lo.comp.base64@encode64'('lo.core#,..'(XH, 'lo.core#,..'(XM, 'lo.core#,..'(XL, XR))), XXd26086):- !,
    'lo.bits@.<<.'(XH, 16, XXd26081),
    'lo.bits@.<<.'(XM, 8, XXd26082),
    'lo.bits@.|.'(XXd26081, XXd26082, XXd26083),
    'lo.bits@.|.'(XXd26083, XL, XXd26084),
    'lo.comp.base64@encode64'(XR, XXd26085),
    'lo.comp.base64@encodeWord'(XXd26084, XXd26085, XXd26086).
'lo.comp.base64@encode64'('lo.core#,..'(XH, 'lo.core#,..'(XM, 'lo.core#[]')), XXd26091):- 'lo.bits@.<<.'(XH, 16, XXd26087),
    'lo.bits@.<<.'(XM, 8, XXd26088),
    'lo.bits@.|.'(XXd26087, XXd26088, XXd26089),
    'lo.comp.base64@mapToSix'(XXd26089, XUp, XHi, XMd, X_21295),
    !,
    'lo.comp.base64@encLast'(XUp, XHi, XMd, 'lo.core#,..'(61, 'lo.core#[]'), XXd26091).
'lo.comp.base64@encode64'('lo.core#,..'(XH, 'lo.core#[]'), XXd26095):- 'lo.bits@.<<.'(XH, 16, XXd26092),
    'lo.comp.base64@mapToSix'(XXd26092, XUp, XHi, XMd, X_21298),
    !,
    'lo.comp.base64@encLast'(XUp, XHi, XMd, 'lo.core#,..'(61, 'lo.core#,..'(61, 'lo.core#[]')), XXd26095).
'lo.comp.base64@encode64'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.base64@encode64'(_, _):- raise_exception('error'("lo.comp.base64@encode64", 5, 3, 73)).
'lo.comp.base64@decode64'('lo.core#,..'(XU, 'lo.core#,..'(XH, 'lo.core#,..'(61, 'lo.core#,..'(61, 'lo.core#[]')))), 'lo.core#,..'(XB1, 'lo.core#[]')):- 'lo.comp.base64@enc64'(XUp, XU),
    'lo.comp.base64@enc64'(XHi, XH),
    'lo.bits@.<<.'(XUp, 18, XXd26096),
    'lo.bits@.<<.'(XHi, 12, XXd26097),
    'lo.bits@.|.'(XXd26096, XXd26097, XXd26098),
    XW = XXd26098,
    'lo.bits@.>>.'(XW, 16, XXd26099),
    'lo.bits@.&.'(XXd26099, 255, XXd26100),
    XB1 = XXd26100,
    !.
'lo.comp.base64@decode64'('lo.core#,..'(XU, 'lo.core#,..'(XH, 'lo.core#,..'(XM, 'lo.core#,..'(61, 'lo.core#[]')))), 'lo.core#,..'(XB1, 'lo.core#,..'(XB2, 'lo.core#[]'))):- 'lo.comp.base64@enc64'(XUp, XU),
    'lo.comp.base64@enc64'(XHi, XH),
    'lo.comp.base64@enc64'(XMd, XM),
    'lo.bits@.<<.'(XUp, 18, XXd26102),
    'lo.bits@.<<.'(XHi, 12, XXd26103),
    'lo.bits@.|.'(XXd26102, XXd26103, XXd26104),
    'lo.bits@.<<.'(XMd, 6, XXd26105),
    'lo.bits@.|.'(XXd26104, XXd26105, XXd26106),
    XW = XXd26106,
    'lo.bits@.>>.'(XW, 16, XXd26107),
    'lo.bits@.&.'(XXd26107, 255, XXd26108),
    XB1 = XXd26108,
    'lo.bits@.>>.'(XW, 8, XXd26109),
    'lo.bits@.&.'(XXd26109, 255, XXd26110),
    XB2 = XXd26110,
    !.
'lo.comp.base64@decode64'('lo.core#,..'(XU, 'lo.core#,..'(XH, 'lo.core#,..'(XM, 'lo.core#,..'(XL, XRest)))), 'lo.core#,..'(XB1, 'lo.core#,..'(XB2, 'lo.core#,..'(XB3, XXd26124)))):- 'lo.comp.base64@enc64'(XUp, XU),
    'lo.comp.base64@enc64'(XHi, XH),
    'lo.comp.base64@enc64'(XMd, XM),
    'lo.comp.base64@enc64'(XLw, XL),
    'lo.bits@.<<.'(XUp, 18, XXd26113),
    'lo.bits@.<<.'(XHi, 12, XXd26114),
    'lo.bits@.|.'(XXd26113, XXd26114, XXd26115),
    'lo.bits@.<<.'(XMd, 6, XXd26116),
    'lo.bits@.|.'(XXd26115, XXd26116, XXd26117),
    'lo.bits@.|.'(XXd26117, XLw, XXd26118),
    XW = XXd26118,
    'lo.bits@.>>.'(XW, 16, XXd26119),
    'lo.bits@.&.'(XXd26119, 255, XXd26120),
    XB1 = XXd26120,
    'lo.bits@.>>.'(XW, 8, XXd26121),
    'lo.bits@.&.'(XXd26121, 255, XXd26122),
    XB2 = XXd26122,
    'lo.bits@.&.'(XW, 255, XXd26123),
    XB3 = XXd26123,
    !,
    'lo.comp.base64@decode64'(XRest, XXd26124).
'lo.comp.base64@decode64'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.base64@decode64'(_, _):- raise_exception('error'("lo.comp.base64@decode64", 22, 3, 126)).
'lo.comp.base64^enc64'('_call%2'(XV20173, XV20174), 'lo.comp.base64^enc64', _):- 'lo.comp.base64@enc64'(XV20173, XV20174).
'lo.comp.base64^encByte'('_call%2'(XV20175, XV20176), 'lo.comp.base64^encByte', _):- 'lo.comp.base64@encByte'(XV20175, XV20176).
'lo.comp.base64^encLast'('_call%5'(XV20177, XV20178, XV20179, XV20180, XV20181), 'lo.comp.base64^encLast', _):- 'lo.comp.base64@encLast'(XV20177, XV20178, XV20179, XV20180, XV20181).
'lo.comp.base64^mapToSix'('_call%5'(XV20182, XV20183, XV20184, XV20185, XV20186), 'lo.comp.base64^mapToSix', _):- 'lo.comp.base64@mapToSix'(XV20182, XV20183, XV20184, XV20185, XV20186).
'lo.comp.base64^encodeWord'('_call%3'(XV20187, XV20188, XV20189), 'lo.comp.base64^encodeWord', _):- 'lo.comp.base64@encodeWord'(XV20187, XV20188, XV20189).
'lo.comp.base64^encode64'('_call%2'(XV20190, XV20191), 'lo.comp.base64^encode64', _):- 'lo.comp.base64@encode64'(XV20190, XV20191).
'lo.comp.base64^decode64'('_call%2'(XV20192, XV20193), 'lo.comp.base64^decode64', _):- 'lo.comp.base64@decode64'(XV20192, XV20193).
