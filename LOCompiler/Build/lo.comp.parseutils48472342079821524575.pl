'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.parseutils's'0.0.1'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I8'alpha'GT1iLi'digit'GT1iLi'alphanum'GT1iLi'iden'GT1LiLi'pkgIden'GT1SLi'natural'GT1LiLi'spaces'GT0Li'digitVal'PT2ii\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.parseutils@init'():- !.
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
'lo.comp.parseutils@upAlpha'(XStIn1281, XNStrm1142, XC):- ocall('_hdtl%3'(XStIn1281, XC, XNStrm1142),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@isUpAlpha'(XC).
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
'lo.comp.parseutils@lowAlpha'(XStIn1282, XNStrm1143, XC):- ocall('_hdtl%3'(XStIn1282, XC, XNStrm1143),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@isLowAlpha'(XC).
'lo.comp.parseutils@alpha'(XStIn1283, XDjOut89, XC):- 'lo.comp.parseutils@Disj65'(XStIn1283, XDjOut89, XStx1191, XStx1190, XC).
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
'lo.comp.parseutils@digit'(XStIn1284, XNStrm1144, XC):- ocall('_hdtl%3'(XStIn1284, XC, XNStrm1144),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@isDigit'(XC).
'lo.comp.parseutils@alphanum'(XStIn1285, XDjOut90, XC):- 'lo.comp.parseutils@Disj66'(XStIn1285, XDjOut90, XStx1193, XStx1192, XC).
'lo.comp.parseutils@alphaNumStar'(XStIn1286, XStx1195, 'lo.core#,..'(XC, XMore), XEnd):- 'lo.comp.parseutils@alphanum'(XStIn1286, XStx1194, XC),
    'lo.comp.parseutils@alphaNumStar'(XStx1194, XStx1195, XMore, XEnd).
'lo.comp.parseutils@alphaNumStar'(XStIn1287, XStIn1287, XEnd, XEnd):- 'lo.comp.parseutils@Neg38'(XStIn1287, XStx1196, X_21272).
'lo.comp.parseutils@iden'(XStIn1288, XStx1198, 'lo.core#,..'(XF, XRest)):- 'lo.comp.parseutils@alpha'(XStIn1288, XStx1197, XF),
    'lo.comp.parseutils@alphaNumStar'(XStx1197, XStx1198, XRest, 'lo.core#[]').
'lo.comp.parseutils@restPkgIden'(XStIn1289, XStx1200, 'lo.core#,..'(46, XRest)):- ocall('_hdtl%3'(XStIn1289, 46, XNStrm1145),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@alphaNumStar'(XNStrm1145, XStx1199, XRest, XR0),
    'lo.comp.parseutils@restPkgIden'(XStx1199, XStx1200, XR0).
'lo.comp.parseutils@restPkgIden'(XStIn1290, XStIn1290, 'lo.core#[]'):- 'lo.comp.parseutils@Neg39'(XStIn1290, XNStrm1146, XNStrm1146, XNegStrm39).
'lo.comp.parseutils@pkgIden'(XStIn1291, XStx1203, XXa68):- 'lo.comp.parseutils@alpha'(XStIn1291, XStx1201, XF),
    'lo.comp.parseutils@alphaNumStar'(XStx1201, XStx1202, XRest, XR0),
    'lo.comp.parseutils@restPkgIden'(XStx1202, XStx1203, XR0),
    'implode'('lo.core#,..'(XF, XRest), XXa68).
'lo.comp.parseutils@digitStar'(XStIn1292, XStx1205, 'lo.core#,..'(XX, XM)):- 'lo.comp.parseutils@digit'(XStIn1292, XStx1204, XX),
    'lo.comp.parseutils@digitStar'(XStx1204, XStx1205, XM).
'lo.comp.parseutils@digitStar'(XStIn1293, XStIn1293, 'lo.core#[]'):- 'lo.comp.parseutils@Neg40'(XStIn1293, XStx1206, X_21277).
'lo.comp.parseutils@natural'(XStIn1294, XStx1208, 'lo.core#,..'(XF, XR)):- 'lo.comp.parseutils@digit'(XStIn1294, XStx1207, XF),
    'lo.comp.parseutils@digitStar'(XStx1207, XStx1208, XR).
'lo.comp.parseutils@isHexDigit'(XX):- 'lo.comp.parseutils@isDigit'(XX).
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
'lo.comp.parseutils@hex'(XStIn1295, XNStrm1147, XC):- ocall('_hdtl%3'(XStIn1295, XC, XNStrm1147),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@one172'(XC).
'lo.comp.parseutils@block_comment'(XStIn1296, XNStrm1149):- ocall('_hdtl%3'(XStIn1296, 42, XNStrm1148),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1148, 47, XNStrm1149),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@block_comment'(XStIn1297, XStx1209):- ocall('_hdtl%3'(XStIn1297, 42, XNStrm1150),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@Neg41'(XNStrm1150, XNStrm1151, XNStrm1151, XNegStrm41),
    'lo.comp.parseutils@block_comment'(XNStrm1150, XStx1209).
'lo.comp.parseutils@block_comment'(XStIn1298, XStx1210):- ocall('_hdtl%3'(XStIn1298, X_21279, XNStrm1152),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@block_comment'(XNStrm1152, XStx1210).
'lo.comp.parseutils@eol'(XStIn1299, XNStrm1153):- ocall('_hdtl%3'(XStIn1299, 10, XNStrm1153),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@eol'(XStIn1300, XStx1211):- ocall('_hdtl%3'(XStIn1300, XC, XNStrm1154),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@neg200'(XC),
    'lo.comp.parseutils@eol'(XNStrm1154, XStx1211).
'lo.comp.parseutils@eol'(XStIn1301, XStIn1301):- XStIn1301 = X_21280,
    ocall('_eof%1'(X_21280),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@space'(XStIn1302, XDjOut91):- 'lo.comp.parseutils@One25'(XStIn1302, XDjOut91, XNStrm1157, XNStrm1157, XNStrm1156, XNStrm1156, XDjStrm68, XDjOut93, XNStrm1155, XNStrm1155, XDjStrm67, XDjOut92).
'lo.comp.parseutils@space'(XStIn1303, XStx1212):- ocall('_hdtl%3'(XStIn1303, 45, XNStrm1158),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1158, 45, XNStrm1159),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@Disj70'(XNStrm1159, XDjOut94, XNStrm1162, XNStrm1162, XNStrm1161, XNStrm1161, XDjStrm70, XDjOut95, XNStrm1160, XNStrm1160, XDjStrm69),
    'lo.comp.parseutils@eol'(XDjOut94, XStx1212).
'lo.comp.parseutils@space'(XStIn1304, XStx1213):- ocall('_hdtl%3'(XStIn1304, 47, XNStrm1163),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1163, 42, XNStrm1164),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.parseutils@block_comment'(XNStrm1164, XStx1213).
'lo.comp.parseutils@spaces'(XStIn1305, XStx1215):- 'lo.comp.parseutils@space'(XStIn1305, XStx1214),
    'lo.comp.parseutils@spaces'(XStx1214, XStx1215).
'lo.comp.parseutils@spaces'(XStIn1306, XStIn1306):- 'lo.comp.parseutils@Neg42'(XStIn1306, XStx1216).
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
'lo.comp.parseutils^isUpAlpha'('_call%1'(XV20122), 'lo.comp.parseutils^isUpAlpha', _):- 'lo.comp.parseutils@isUpAlpha'(XV20122).
'lo.comp.parseutils^upAlpha'('_call%3'(XV20123, XV20124, XV20125), 'lo.comp.parseutils^upAlpha', _):- 'lo.comp.parseutils@upAlpha'(XV20123, XV20124, XV20125).
'lo.comp.parseutils^isLowAlpha'('_call%1'(XV20126), 'lo.comp.parseutils^isLowAlpha', _):- 'lo.comp.parseutils@isLowAlpha'(XV20126).
'lo.comp.parseutils^lowAlpha'('_call%3'(XV20127, XV20128, XV20129), 'lo.comp.parseutils^lowAlpha', _):- 'lo.comp.parseutils@lowAlpha'(XV20127, XV20128, XV20129).
'lo.comp.parseutils@Disj65'(XDjStrm65, XStx1190, XStx1191, XStx1190, XC):- 'lo.comp.parseutils@lowAlpha'(XDjStrm65, XStx1190, XC).
'lo.comp.parseutils@Disj65'(XDjStrm65, XStx1191, XStx1191, XStx1190, XC):- 'lo.comp.parseutils@upAlpha'(XDjStrm65, XStx1191, XC).
'lo.comp.parseutils^alpha'('_call%3'(XV20130, XV20131, XV20132), 'lo.comp.parseutils^alpha', _):- 'lo.comp.parseutils@alpha'(XV20130, XV20131, XV20132).
'lo.comp.parseutils^isDigit'('_call%1'(XV20133), 'lo.comp.parseutils^isDigit', _):- 'lo.comp.parseutils@isDigit'(XV20133).
'lo.comp.parseutils^digit'('_call%3'(XV20134, XV20135, XV20136), 'lo.comp.parseutils^digit', _):- 'lo.comp.parseutils@digit'(XV20134, XV20135, XV20136).
'lo.comp.parseutils@Disj66'(XDjStrm66, XStx1192, XStx1193, XStx1192, XC):- 'lo.comp.parseutils@alpha'(XDjStrm66, XStx1192, XC).
'lo.comp.parseutils@Disj66'(XDjStrm66, XStx1193, XStx1193, XStx1192, XC):- 'lo.comp.parseutils@digit'(XDjStrm66, XStx1193, XC).
'lo.comp.parseutils^alphanum'('_call%3'(XV20137, XV20138, XV20139), 'lo.comp.parseutils^alphanum', _):- 'lo.comp.parseutils@alphanum'(XV20137, XV20138, XV20139).
'lo.comp.parseutils@Neg38'(XNegStrm38, XStx1196, X_21272):- 'lo.comp.parseutils@alphanum'(XNegStrm38, XStx1196, X_21272),
    !,
    fail.
'lo.comp.parseutils@Neg38'(XNegStrm38, XStx1196, X_21272).
'lo.comp.parseutils^alphaNumStar'('_call%4'(XV20140, XV20141, XV20142, XV20143), 'lo.comp.parseutils^alphaNumStar', _):- 'lo.comp.parseutils@alphaNumStar'(XV20140, XV20141, XV20142, XV20143).
'lo.comp.parseutils^iden'('_call%3'(XV20144, XV20145, XV20146), 'lo.comp.parseutils^iden', _):- 'lo.comp.parseutils@iden'(XV20144, XV20145, XV20146).
'lo.comp.parseutils@Neg39'(XNegStrm39, XNStrm1146, XNStrm1146, XNegStrm39):- ocall('_hdtl%3'(XNegStrm39, 46, XNStrm1146),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    fail.
'lo.comp.parseutils@Neg39'(XNegStrm39, XNStrm1146, XNStrm1146, XNegStrm39).
'lo.comp.parseutils^restPkgIden'('_call%3'(XV20147, XV20148, XV20149), 'lo.comp.parseutils^restPkgIden', _):- 'lo.comp.parseutils@restPkgIden'(XV20147, XV20148, XV20149).
'lo.comp.parseutils^pkgIden'('_call%3'(XV20150, XV20151, XV20152), 'lo.comp.parseutils^pkgIden', _):- 'lo.comp.parseutils@pkgIden'(XV20150, XV20151, XV20152).
'lo.comp.parseutils@Neg40'(XNegStrm40, XStx1206, X_21277):- 'lo.comp.parseutils@digit'(XNegStrm40, XStx1206, X_21277),
    !,
    fail.
'lo.comp.parseutils@Neg40'(XNegStrm40, XStx1206, X_21277).
'lo.comp.parseutils^digitStar'('_call%3'(XV20153, XV20154, XV20155), 'lo.comp.parseutils^digitStar', _):- 'lo.comp.parseutils@digitStar'(XV20153, XV20154, XV20155).
'lo.comp.parseutils^natural'('_call%3'(XV20156, XV20157, XV20158), 'lo.comp.parseutils^natural', _):- 'lo.comp.parseutils@natural'(XV20156, XV20157, XV20158).
'lo.comp.parseutils^isHexDigit'('_call%1'(XV20159), 'lo.comp.parseutils^isHexDigit', _):- 'lo.comp.parseutils@isHexDigit'(XV20159).
'lo.comp.parseutils@one172'(XC):- 'lo.comp.parseutils@isHexDigit'(XC),
    !.
'lo.comp.parseutils^hex'('_call%3'(XV20160, XV20161, XV20162), 'lo.comp.parseutils^hex', _):- 'lo.comp.parseutils@hex'(XV20160, XV20161, XV20162).
'lo.comp.parseutils@Neg41'(XNegStrm41, XNStrm1151, XNStrm1151, XNegStrm41):- ocall('_hdtl%3'(XNegStrm41, 47, XNStrm1151),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    fail.
'lo.comp.parseutils@Neg41'(XNegStrm41, XNStrm1151, XNStrm1151, XNegStrm41).
'lo.comp.parseutils^block_comment'('_call%2'(XV20163, XV20164), 'lo.comp.parseutils^block_comment', _):- 'lo.comp.parseutils@block_comment'(XV20163, XV20164).
'lo.comp.parseutils@neg200'(XC):- XC = 10,
    !,
    fail.
'lo.comp.parseutils@neg200'(XC).
'lo.comp.parseutils^eol'('_call%2'(XV20165, XV20166), 'lo.comp.parseutils^eol', _):- 'lo.comp.parseutils@eol'(XV20165, XV20166).
'lo.comp.parseutils@Disj67'(XDjStrm68, XNStrm1156, XNStrm1157, XNStrm1157, XNStrm1156, XNStrm1156, XDjStrm68):- ocall('_hdtl%3'(XDjStrm68, 9, XNStrm1156),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj67'(XDjStrm68, XNStrm1157, XNStrm1157, XNStrm1157, XNStrm1156, XNStrm1156, XDjStrm68):- ocall('_hdtl%3'(XDjStrm68, 10, XNStrm1157),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj68'(XDjStrm67, XNStrm1155, XNStrm1157, XNStrm1157, XNStrm1156, XNStrm1156, XDjStrm68, XDjOut93, XNStrm1155, XNStrm1155, XDjStrm67):- ocall('_hdtl%3'(XDjStrm67, 32, XNStrm1155),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj68'(XDjStrm67, XDjOut93, XNStrm1157, XNStrm1157, XNStrm1156, XNStrm1156, XDjStrm68, XDjOut93, XNStrm1155, XNStrm1155, XDjStrm67):- 'lo.comp.parseutils@Disj67'(XDjStrm67, XDjOut93, XNStrm1157, XNStrm1157, XNStrm1156, XNStrm1156, XDjStrm68).
'lo.comp.parseutils@One25'(XOneStm25, XDjOut92, XNStrm1157, XNStrm1157, XNStrm1156, XNStrm1156, XDjStrm68, XDjOut93, XNStrm1155, XNStrm1155, XDjStrm67, XDjOut92):- 'lo.comp.parseutils@Disj68'(XOneStm25, XDjOut92, XNStrm1157, XNStrm1157, XNStrm1156, XNStrm1156, XDjStrm68, XDjOut93, XNStrm1155, XNStrm1155, XDjStrm67),
    !.
'lo.comp.parseutils@Disj69'(XDjStrm70, XNStrm1161, XNStrm1162, XNStrm1162, XNStrm1161, XNStrm1161, XDjStrm70):- ocall('_hdtl%3'(XDjStrm70, 9, XNStrm1161),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj69'(XDjStrm70, XNStrm1162, XNStrm1162, XNStrm1162, XNStrm1161, XNStrm1161, XDjStrm70):- ocall('_hdtl%3'(XDjStrm70, 10, XNStrm1162),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj70'(XDjStrm69, XNStrm1160, XNStrm1162, XNStrm1162, XNStrm1161, XNStrm1161, XDjStrm70, XDjOut95, XNStrm1160, XNStrm1160, XDjStrm69):- ocall('_hdtl%3'(XDjStrm69, 32, XNStrm1160),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.parseutils@Disj70'(XDjStrm69, XDjOut95, XNStrm1162, XNStrm1162, XNStrm1161, XNStrm1161, XDjStrm70, XDjOut95, XNStrm1160, XNStrm1160, XDjStrm69):- 'lo.comp.parseutils@Disj69'(XDjStrm69, XDjOut95, XNStrm1162, XNStrm1162, XNStrm1161, XNStrm1161, XDjStrm70).
'lo.comp.parseutils^space'('_call%2'(XV20167, XV20168), 'lo.comp.parseutils^space', _):- 'lo.comp.parseutils@space'(XV20167, XV20168).
'lo.comp.parseutils@Neg42'(XNegStrm42, XStx1216):- 'lo.comp.parseutils@space'(XNegStrm42, XStx1216),
    !,
    fail.
'lo.comp.parseutils@Neg42'(XNegStrm42, XStx1216).
'lo.comp.parseutils^spaces'('_call%2'(XV20169, XV20170), 'lo.comp.parseutils^spaces', _):- 'lo.comp.parseutils@spaces'(XV20169, XV20170).
'lo.comp.parseutils^digitVal'('_call%2'(XV20171, XV20172), 'lo.comp.parseutils^digitVal', _):- 'lo.comp.parseutils@digitVal'(XV20171, XV20172).
