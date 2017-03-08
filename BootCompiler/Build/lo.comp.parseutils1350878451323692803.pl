'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.parseutils'e'*'n10o10'()10'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I8'alpha'GT1iLi'digit'GT1iLi'alphanum'GT1iLi'iden'GT1LiLi'pkgIden'GT1SLi'natural'GT1LiLi'spaces'GT0Li'digitVal'PT2ii\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.parseutils@init'() :- !.
'lo.comp.parseutils@isLowAlpha'(97).
'lo.comp.parseutils@isLowAlpha'(98).
'lo.comp.parseutils@isLowAlpha'(99).
'lo.comp.parseutils@isLowAlpha'(100).
'lo.comp.parseutils@isLowAlpha'(101).
'lo.comp.parseutils@isLowAlpha'(102).
'lo.comp.parseutils@isLowAlpha'(103).
'lo.comp.parseutils@isLowAlpha'(104).
'lo.comp.parseutils@isLowAlpha'(105).
'lo.comp.parseutils@isLowAlpha'(106).
'lo.comp.parseutils@isLowAlpha'(107).
'lo.comp.parseutils@isLowAlpha'(108).
'lo.comp.parseutils@isLowAlpha'(109).
'lo.comp.parseutils@isLowAlpha'(110).
'lo.comp.parseutils@isLowAlpha'(111).
'lo.comp.parseutils@isLowAlpha'(112).
'lo.comp.parseutils@isLowAlpha'(113).
'lo.comp.parseutils@isLowAlpha'(114).
'lo.comp.parseutils@isLowAlpha'(115).
'lo.comp.parseutils@isLowAlpha'(116).
'lo.comp.parseutils@isLowAlpha'(117).
'lo.comp.parseutils@isLowAlpha'(118).
'lo.comp.parseutils@isLowAlpha'(119).
'lo.comp.parseutils@isLowAlpha'(120).
'lo.comp.parseutils@isLowAlpha'(121).
'lo.comp.parseutils@isLowAlpha'(122).
'lo.comp.parseutils@lowAlpha'(XStIn138, XNStrm108, XC) :- ocall('_hdtl%3'(XStIn138, XC, XNStrm108),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@isLowAlpha'(XC).
'lo.comp.parseutils@isUpAlpha'(65).
'lo.comp.parseutils@isUpAlpha'(66).
'lo.comp.parseutils@isUpAlpha'(67).
'lo.comp.parseutils@isUpAlpha'(68).
'lo.comp.parseutils@isUpAlpha'(69).
'lo.comp.parseutils@isUpAlpha'(70).
'lo.comp.parseutils@isUpAlpha'(71).
'lo.comp.parseutils@isUpAlpha'(72).
'lo.comp.parseutils@isUpAlpha'(73).
'lo.comp.parseutils@isUpAlpha'(74).
'lo.comp.parseutils@isUpAlpha'(75).
'lo.comp.parseutils@isUpAlpha'(76).
'lo.comp.parseutils@isUpAlpha'(77).
'lo.comp.parseutils@isUpAlpha'(78).
'lo.comp.parseutils@isUpAlpha'(79).
'lo.comp.parseutils@isUpAlpha'(80).
'lo.comp.parseutils@isUpAlpha'(81).
'lo.comp.parseutils@isUpAlpha'(82).
'lo.comp.parseutils@isUpAlpha'(83).
'lo.comp.parseutils@isUpAlpha'(84).
'lo.comp.parseutils@isUpAlpha'(85).
'lo.comp.parseutils@isUpAlpha'(86).
'lo.comp.parseutils@isUpAlpha'(87).
'lo.comp.parseutils@isUpAlpha'(88).
'lo.comp.parseutils@isUpAlpha'(89).
'lo.comp.parseutils@isUpAlpha'(90).
'lo.comp.parseutils@upAlpha'(XStIn139, XNStrm109, XC) :- ocall('_hdtl%3'(XStIn139, XC, XNStrm109),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@isUpAlpha'(XC).
'lo.comp.parseutils@alpha'(XStIn140, XDjOut12, XC) :- 'lo.comp.parseutils@Disj11'(XStIn140, XDjOut12, XC).
'lo.comp.parseutils@isDigit'(48).
'lo.comp.parseutils@isDigit'(49).
'lo.comp.parseutils@isDigit'(50).
'lo.comp.parseutils@isDigit'(51).
'lo.comp.parseutils@isDigit'(52).
'lo.comp.parseutils@isDigit'(53).
'lo.comp.parseutils@isDigit'(54).
'lo.comp.parseutils@isDigit'(55).
'lo.comp.parseutils@isDigit'(56).
'lo.comp.parseutils@isDigit'(57).
'lo.comp.parseutils@digit'(XStIn141, XNStrm110, XC) :- ocall('_hdtl%3'(XStIn141, XC, XNStrm110),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@isDigit'(XC).
'lo.comp.parseutils@alphanum'(XStIn142, XDjOut13, XC) :- 'lo.comp.parseutils@Disj12'(XStIn142, XDjOut13, XC).
'lo.comp.parseutils@alphaNumStar'(XStIn143, XStx147, 'lo.core#,..'(XC, XMore), XEnd) :- 'lo.comp.parseutils@alphanum'(XStIn143, XStx146, XC),
    'lo.comp.parseutils@alphaNumStar'(XStx146, XStx147, XMore, XEnd).
'lo.comp.parseutils@alphaNumStar'(XStIn144, XStIn144, XEnd, XEnd) :- 'lo.comp.parseutils@Neg4'(XStIn144, X_664).
'lo.comp.parseutils@iden'(XStIn145, XStx150, 'lo.core#,..'(XF, XRest)) :- 'lo.comp.parseutils@alpha'(XStIn145, XStx149, XF),
    'lo.comp.parseutils@alphaNumStar'(XStx149, XStx150, XRest, 'lo.core#[]').
'lo.comp.parseutils@restPkgIden'(XStIn146, XStx152, 'lo.core#,..'(46, XRest)) :- ocall('_hdtl%3'(XStIn146, 46, XNStrm111),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@alphaNumStar'(XNStrm111, XStx151, XRest, XR0),
    'lo.comp.parseutils@restPkgIden'(XStx151, XStx152, XR0).
'lo.comp.parseutils@restPkgIden'(XStIn147, XStIn147, 'lo.core#[]') :- 'lo.comp.parseutils@Neg5'(XStIn147, XNStrm112, XNStrm112, XNegStrm5).
'lo.comp.parseutils@pkgIden'(XStIn148, XStx155, XX9364) :- 'lo.comp.parseutils@alpha'(XStIn148, XStx153, XF),
    'lo.comp.parseutils@alphaNumStar'(XStx153, XStx154, XRest, XR0),
    'lo.comp.parseutils@restPkgIden'(XStx154, XStx155, XR0),
    'implode'('lo.core#,..'(XF, XRest), XX9364).
'lo.comp.parseutils@digitStar'(XStIn149, XStx157, 'lo.core#,..'(XX, XM)) :- 'lo.comp.parseutils@digit'(XStIn149, XStx156, XX),
    'lo.comp.parseutils@digitStar'(XStx156, XStx157, XM).
'lo.comp.parseutils@digitStar'(XStIn150, XStIn150, 'lo.core#[]') :- 'lo.comp.parseutils@Neg6'(XStIn150, X_665).
'lo.comp.parseutils@natural'(XStIn151, XStx160, 'lo.core#,..'(XF, XR)) :- 'lo.comp.parseutils@digit'(XStIn151, XStx159, XF),
    'lo.comp.parseutils@digitStar'(XStx159, XStx160, XR).
'lo.comp.parseutils@isHexDigit'(XX) :- 'lo.comp.parseutils@isDigit'(XX).
'lo.comp.parseutils@isHexDigit'(97).
'lo.comp.parseutils@isHexDigit'(98).
'lo.comp.parseutils@isHexDigit'(99).
'lo.comp.parseutils@isHexDigit'(100).
'lo.comp.parseutils@isHexDigit'(101).
'lo.comp.parseutils@isHexDigit'(102).
'lo.comp.parseutils@isHexDigit'(65).
'lo.comp.parseutils@isHexDigit'(66).
'lo.comp.parseutils@isHexDigit'(67).
'lo.comp.parseutils@isHexDigit'(68).
'lo.comp.parseutils@isHexDigit'(69).
'lo.comp.parseutils@isHexDigit'(70).
'lo.comp.parseutils@hex'(XStIn152, XNStrm113, XC) :- ocall('_hdtl%3'(XStIn152, XC, XNStrm113),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@one7'(XC).
'lo.comp.parseutils@eol'(XStIn153, XNStrm114) :- ocall('_hdtl%3'(XStIn153, 10, XNStrm114),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@eol'(XStIn154, XStx161) :- ocall('_hdtl%3'(XStIn154, XC, XNStrm115),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@neg15'(XC),
    'lo.comp.parseutils@eol'(XNStrm115, XStx161).
'lo.comp.parseutils@eol'(XStIn155, XStIn155) :- ocall('_eof%1'(XStIn155),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@block_comment'(XStIn156, XNStrm117) :- ocall('_hdtl%3'(XStIn156, 42, XNStrm116),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm116, 47, XNStrm117),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@block_comment'(XStIn157, XStx162) :- ocall('_hdtl%3'(XStIn157, 42, XNStrm118),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@Neg7'(XNStrm118, XNStrm119, XNStrm119, XNegStrm7),
    'lo.comp.parseutils@block_comment'(XNStrm118, XStx162).
'lo.comp.parseutils@block_comment'(XStIn158, XStx163) :- ocall('_hdtl%3'(XStIn158, X_666, XNStrm120),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@block_comment'(XNStrm120, XStx163).
'lo.comp.parseutils@space'(XStIn159, XDjOut14) :- 'lo.comp.parseutils@One2'(XStIn159, XDjOut14, XDjStrm13, XNStrm121, XNStrm123, XNStrm122, XDjStrm14).
'lo.comp.parseutils@space'(XStIn160, XStx164) :- ocall('_hdtl%3'(XStIn160, 45, XNStrm124),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm124, 45, XNStrm125),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@Disj16'(XNStrm125, XDjOut17, XDjStrm16, XNStrm127, XNStrm128, XNStrm126, XNStrm126, XDjStrm15),
    'lo.comp.parseutils@eol'(XDjOut17, XStx164).
'lo.comp.parseutils@space'(XStIn161, XStx165) :- ocall('_hdtl%3'(XStIn161, 47, XNStrm129),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm129, 42, XNStrm130),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@block_comment'(XNStrm130, XStx165).
'lo.comp.parseutils@spaces'(XStIn162, XStx167) :- 'lo.comp.parseutils@space'(XStIn162, XStx166),
    'lo.comp.parseutils@spaces'(XStx166, XStx167).
'lo.comp.parseutils@spaces'(XStIn163, XStIn163) :- 'lo.comp.parseutils@Neg8'(XStIn163).
'lo.comp.parseutils@digitVal'(48, 0).
'lo.comp.parseutils@digitVal'(49, 1).
'lo.comp.parseutils@digitVal'(50, 2).
'lo.comp.parseutils@digitVal'(51, 3).
'lo.comp.parseutils@digitVal'(52, 4).
'lo.comp.parseutils@digitVal'(53, 5).
'lo.comp.parseutils@digitVal'(54, 6).
'lo.comp.parseutils@digitVal'(55, 7).
'lo.comp.parseutils@digitVal'(56, 8).
'lo.comp.parseutils@digitVal'(57, 9).
'lo.comp.parseutils^isLowAlpha'('_call%1'(XV1502), 'lo.comp.parseutils^isLowAlpha', _) :- 'lo.comp.parseutils@isLowAlpha'(XV1502).
'lo.comp.parseutils^lowAlpha'('_call%3'(XV1503, XV1504, XV1505), 'lo.comp.parseutils^lowAlpha', _) :- 'lo.comp.parseutils@lowAlpha'(XV1503, XV1504, XV1505).
'lo.comp.parseutils^isUpAlpha'('_call%1'(XV1506), 'lo.comp.parseutils^isUpAlpha', _) :- 'lo.comp.parseutils@isUpAlpha'(XV1506).
'lo.comp.parseutils^upAlpha'('_call%3'(XV1507, XV1508, XV1509), 'lo.comp.parseutils^upAlpha', _) :- 'lo.comp.parseutils@upAlpha'(XV1507, XV1508, XV1509).
'lo.comp.parseutils@Disj11'(XDjStrm11, XStx142, XC) :- 'lo.comp.parseutils@lowAlpha'(XDjStrm11, XStx142, XC).
'lo.comp.parseutils@Disj11'(XDjStrm11, XStx143, XC) :- 'lo.comp.parseutils@upAlpha'(XDjStrm11, XStx143, XC).
'lo.comp.parseutils^alpha'('_call%3'(XV1510, XV1511, XV1512), 'lo.comp.parseutils^alpha', _) :- 'lo.comp.parseutils@alpha'(XV1510, XV1511, XV1512).
'lo.comp.parseutils^isDigit'('_call%1'(XV1513), 'lo.comp.parseutils^isDigit', _) :- 'lo.comp.parseutils@isDigit'(XV1513).
'lo.comp.parseutils^digit'('_call%3'(XV1514, XV1515, XV1516), 'lo.comp.parseutils^digit', _) :- 'lo.comp.parseutils@digit'(XV1514, XV1515, XV1516).
'lo.comp.parseutils@Disj12'(XDjStrm12, XStx144, XC) :- 'lo.comp.parseutils@alpha'(XDjStrm12, XStx144, XC).
'lo.comp.parseutils@Disj12'(XDjStrm12, XStx145, XC) :- 'lo.comp.parseutils@digit'(XDjStrm12, XStx145, XC).
'lo.comp.parseutils^alphanum'('_call%3'(XV1517, XV1518, XV1519), 'lo.comp.parseutils^alphanum', _) :- 'lo.comp.parseutils@alphanum'(XV1517, XV1518, XV1519).
'lo.comp.parseutils@Neg4'(XNegStrm4, X_664) :- 'lo.comp.parseutils@alphanum'(XNegStrm4, XStx148, X_664),
    !,
    fail.
'lo.comp.parseutils@Neg4'(XNegStrm4, X_664).
'lo.comp.parseutils^alphaNumStar'('_call%4'(XV1520, XV1521, XV1522, XV1523), 'lo.comp.parseutils^alphaNumStar', _) :- 'lo.comp.parseutils@alphaNumStar'(XV1520, XV1521, XV1522, XV1523).
'lo.comp.parseutils^iden'('_call%3'(XV1524, XV1525, XV1526), 'lo.comp.parseutils^iden', _) :- 'lo.comp.parseutils@iden'(XV1524, XV1525, XV1526).
'lo.comp.parseutils@Neg5'(XNegStrm5, XNStrm112, XNStrm112, XNegStrm5) :- ocall('_hdtl%3'(XNegStrm5, 46, XNStrm112),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    fail.
'lo.comp.parseutils@Neg5'(XNegStrm5, XNStrm112, XNStrm112, XNegStrm5).
'lo.comp.parseutils^restPkgIden'('_call%3'(XV1527, XV1528, XV1529), 'lo.comp.parseutils^restPkgIden', _) :- 'lo.comp.parseutils@restPkgIden'(XV1527, XV1528, XV1529).
'lo.comp.parseutils^pkgIden'('_call%3'(XV1530, XV1531, XV1532), 'lo.comp.parseutils^pkgIden', _) :- 'lo.comp.parseutils@pkgIden'(XV1530, XV1531, XV1532).
'lo.comp.parseutils@Neg6'(XNegStrm6, X_665) :- 'lo.comp.parseutils@digit'(XNegStrm6, XStx158, X_665),
    !,
    fail.
'lo.comp.parseutils@Neg6'(XNegStrm6, X_665).
'lo.comp.parseutils^digitStar'('_call%3'(XV1533, XV1534, XV1535), 'lo.comp.parseutils^digitStar', _) :- 'lo.comp.parseutils@digitStar'(XV1533, XV1534, XV1535).
'lo.comp.parseutils^natural'('_call%3'(XV1536, XV1537, XV1538), 'lo.comp.parseutils^natural', _) :- 'lo.comp.parseutils@natural'(XV1536, XV1537, XV1538).
'lo.comp.parseutils^isHexDigit'('_call%1'(XV1539), 'lo.comp.parseutils^isHexDigit', _) :- 'lo.comp.parseutils@isHexDigit'(XV1539).
'lo.comp.parseutils@one7'(XC) :- 'lo.comp.parseutils@isHexDigit'(XC),
    !.
'lo.comp.parseutils^hex'('_call%3'(XV1540, XV1541, XV1542), 'lo.comp.parseutils^hex', _) :- 'lo.comp.parseutils@hex'(XV1540, XV1541, XV1542).
'lo.comp.parseutils@neg15'(XC) :- XC = 10,
    !,
    fail.
'lo.comp.parseutils@neg15'(XC).
'lo.comp.parseutils^eol'('_call%2'(XV1543, XV1544), 'lo.comp.parseutils^eol', _) :- 'lo.comp.parseutils@eol'(XV1543, XV1544).
'lo.comp.parseutils@Neg7'(XNegStrm7, XNStrm119, XNStrm119, XNegStrm7) :- ocall('_hdtl%3'(XNegStrm7, 47, XNStrm119),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    fail.
'lo.comp.parseutils@Neg7'(XNegStrm7, XNStrm119, XNStrm119, XNegStrm7).
'lo.comp.parseutils^block_comment'('_call%2'(XV1545, XV1546), 'lo.comp.parseutils^block_comment', _) :- 'lo.comp.parseutils@block_comment'(XV1545, XV1546).
'lo.comp.parseutils@Disj13'(XDjStrm14, XNStrm122, XNStrm123, XNStrm123, XNStrm122, XNStrm122, XDjStrm14) :- ocall('_hdtl%3'(XDjStrm14, 9, XNStrm122),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj13'(XDjStrm14, XNStrm123, XNStrm123, XNStrm123, XNStrm122, XNStrm122, XDjStrm14) :- ocall('_hdtl%3'(XDjStrm14, 10, XNStrm123),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj14'(XDjStrm13, XNStrm121, XDjStrm14, XNStrm122, XNStrm123, XNStrm121, XNStrm121, XDjStrm13) :- ocall('_hdtl%3'(XDjStrm13, 32, XNStrm121),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj14'(XDjStrm13, XDjOut16, XDjStrm14, XNStrm122, XNStrm123, XNStrm121, XNStrm121, XDjStrm13) :- 'lo.comp.parseutils@Disj13'(XDjStrm13, XDjOut16, XNStrm123, XNStrm123, XNStrm122, XNStrm122, XDjStrm14).
'lo.comp.parseutils@One2'(XOneStm2, XDjOut15, XDjStrm13, XNStrm121, XNStrm123, XNStrm122, XDjStrm14) :- 'lo.comp.parseutils@Disj14'(XOneStm2, XDjOut15, XDjStrm14, XNStrm122, XNStrm123, XNStrm121, XNStrm121, XDjStrm13),
    !.
'lo.comp.parseutils@Disj15'(XDjStrm16, XNStrm127, XNStrm128, XNStrm128, XNStrm127, XNStrm127, XDjStrm16) :- ocall('_hdtl%3'(XDjStrm16, 9, XNStrm127),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj15'(XDjStrm16, XNStrm128, XNStrm128, XNStrm128, XNStrm127, XNStrm127, XDjStrm16) :- ocall('_hdtl%3'(XDjStrm16, 10, XNStrm128),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj16'(XDjStrm15, XNStrm126, XDjStrm16, XNStrm127, XNStrm128, XNStrm126, XNStrm126, XDjStrm15) :- ocall('_hdtl%3'(XDjStrm15, 32, XNStrm126),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj16'(XDjStrm15, XDjOut18, XDjStrm16, XNStrm127, XNStrm128, XNStrm126, XNStrm126, XDjStrm15) :- 'lo.comp.parseutils@Disj15'(XDjStrm15, XDjOut18, XNStrm128, XNStrm128, XNStrm127, XNStrm127, XDjStrm16).
'lo.comp.parseutils^space'('_call%2'(XV1547, XV1548), 'lo.comp.parseutils^space', _) :- 'lo.comp.parseutils@space'(XV1547, XV1548).
'lo.comp.parseutils@Neg8'(XNegStrm8) :- 'lo.comp.parseutils@space'(XNegStrm8, XStx168),
    !,
    fail.
'lo.comp.parseutils@Neg8'(XNegStrm8).
'lo.comp.parseutils^spaces'('_call%2'(XV1549, XV1550), 'lo.comp.parseutils^spaces', _) :- 'lo.comp.parseutils@spaces'(XV1549, XV1550).
'lo.comp.parseutils^digitVal'('_call%2'(XV1551, XV1552), 'lo.comp.parseutils^digitVal', _) :- 'lo.comp.parseutils@digitVal'(XV1551, XV1552).
