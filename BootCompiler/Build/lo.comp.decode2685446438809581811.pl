'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.decode'e'*'n16o16'()16'n2o2'import'e'private'n2o2'pkg's'lo.comp.base64'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.parseutils'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I4'decodeValue'FT1St'lo.comp.term*term''decodeTuple'FT1SLt'lo.comp.term*term''decodeType'GT1t'lo.comp.types*tipe'Li'decodeConstraint'PT2St'lo.comp.types*constraint'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.decode@init'() :- !.
'lo.comp.decode@digits'(XStIn164, XStx170, XSoFar, XIx) :- 'lo.comp.parseutils@digit'(XStIn164, XStx169, XD),
    'lo.comp.parseutils@digitVal'(XD, XDv),
    ocall('*%3'(XSoFar, 10, XX9675),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%3'(XX9675, XDv, XX9678),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.decode@digits'(XStx169, XStx170, XX9678, XIx).
'lo.comp.decode@digits'(XStIn165, XStIn165, XIx, XIx) :- 'lo.comp.decode@Neg9'(XStIn165, X_669).
'lo.comp.decode@decInt'(XStIn166, XStx172, XX9686) :- ocall('_hdtl%3'(XStIn166, 45, XNStrm131),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm131, XStx172, XIx),
    ocall('zero%1'(XXV24),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('-%3'(XXV24, XIx, XX9686),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.decode@decInt'(XStIn167, XStx173, XIx) :- 'lo.comp.decode@digits'(XStIn167, XStx173, 0, XIx).
'lo.comp.decode@collectUntil'(XStIn168, XNStrm132, XC, 'lo.core#[]') :- ocall('_hdtl%3'(XStIn168, XC, XNStrm132),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@collectUntil'(XStIn169, XStx174, XC, 'lo.core#,..'(XCh, XL)) :- ocall('_hdtl%3'(XStIn169, XCh, XNStrm133),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectUntil'(XNStrm133, XStx174, XC, XL).
'lo.comp.decode@decodeName'(XStIn170, XStx175, XX9711) :- ocall('_hdtl%3'(XStIn170, XC, XNStrm134),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectUntil'(XNStrm134, XStx175, XC, XText),
    'implode'(XText, XX9711).
'lo.comp.decode@decFloat'(XStIn171, XStx176, XX9719) :- 'lo.comp.decode@decodeName'(XStIn171, XStx176, XSx),
    ocall('_coerce%2'(XSx, XX9719),'lo.coerce$coercion$lo.core*string$lo.core*float','lo.coerce$coercion$lo.core*string$lo.core*float').
'lo.comp.decode@collectQuoted'(XStIn172, XNStrm135, XC, 'lo.core#[]') :- ocall('_hdtl%3'(XStIn172, XC, XNStrm135),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@collectQuoted'(XStIn173, XStx177, XC, 'lo.core#,..'(XCh, XL)) :- ocall('_hdtl%3'(XStIn173, 92, XNStrm136),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm136, XCh, XNStrm137),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectQuoted'(XNStrm137, XStx177, XC, XL).
'lo.comp.decode@collectQuoted'(XStIn174, XStx178, XC, 'lo.core#,..'(XCh, XL)) :- ocall('_hdtl%3'(XStIn174, XCh, XNStrm138),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectQuoted'(XNStrm138, XStx178, XC, XL).
'lo.comp.decode@decodeText'(XStIn175, XStx179, XChrs) :- ocall('_hdtl%3'(XStIn175, XC, XNStrm139),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectQuoted'(XNStrm139, XStx179, XC, XChrs).
'lo.comp.decode@decTerms'(XStIn176, XStIn176, 0, 'lo.core#[]').
'lo.comp.decode@decTerms'(XStIn177, XStx181, XN, 'lo.core#,..'(XT, XL)) :- 'lo.comp.decode@decodeTerm'(XStIn177, XStx180, XT),
    ocall('-%3'(XN, 1, XX9765),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.decode@decTerms'(XStx180, XStx181, XX9765, XL).
'lo.comp.decode@decodeTerm'(XStIn178, XNStrm140, 'lo.comp.term#anon') :- ocall('_hdtl%3'(XStIn178, 97, XNStrm140),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeTerm'(XStIn179, XStx182, 'lo.comp.term#intgr'(XIx)) :- ocall('_hdtl%3'(XStIn179, 120, XNStrm141),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm141, XStx182, XIx).
'lo.comp.decode@decodeTerm'(XStIn180, XStx183, 'lo.comp.term#flot'(XDx)) :- ocall('_hdtl%3'(XStIn180, 100, XNStrm142),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decFloat'(XNStrm142, XStx183, XDx).
'lo.comp.decode@decodeTerm'(XStIn181, XStx184, 'lo.comp.term#enum'(XNm)) :- ocall('_hdtl%3'(XStIn181, 101, XNStrm143),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeName'(XNStrm143, XStx184, XNm).
'lo.comp.decode@decodeTerm'(XStIn182, XStx185, 'lo.comp.term#strng'(XSx)) :- ocall('_hdtl%3'(XStIn182, 115, XNStrm144),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeText'(XNStrm144, XStx185, XChrs),
    'implode'(XChrs, XX9798),
    XSx = XX9798.
'lo.comp.decode@decodeTerm'(XStIn183, XStx187, 'lo.comp.term#strct'(XNm, XAr)) :- ocall('_hdtl%3'(XStIn183, 111, XNStrm145),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm145, XStx186, XAr),
    'lo.comp.decode@decodeName'(XStx186, XStx187, XNm).
'lo.comp.decode@decodeTerm'(XStIn184, XStx189, 'lo.comp.term#prg'(XNm, XAr)) :- ocall('_hdtl%3'(XStIn184, 112, XNStrm146),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm146, XStx188, XAr),
    'lo.comp.decode@decodeName'(XStx188, XStx189, XNm).
'lo.comp.decode@decodeTerm'(XStIn185, XStx192, 'lo.comp.term#cons'(XOp, XArgs)) :- ocall('_hdtl%3'(XStIn185, 110, XNStrm147),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm147, XStx190, XLen),
    'lo.comp.decode@decodeTerm'(XStx190, XStx191, XOp),
    'lo.comp.decode@decTerms'(XStx191, XStx192, XLen, XArgs).
'lo.comp.decode@decodeValue'(XTxt, XTerm) :- 'explode'(XTxt, XX9827),
    'lo.comp.decode@decodeTerm'(XX9827, XStx193, XTerm),
    ocall('_eof%1'(XStx193),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !.
'lo.comp.decode@decodeValue'(_, _) :- raise_exception('error'("decodeValue", 10, 3, 60)).
'lo.comp.decode@decodeTuple'(XTxt, XEls) :- 'explode'(XTxt, XX9834),
    'lo.comp.decode@decodeTerm'(XX9834, XStx194, 'lo.comp.term#cons'(X_670, XEls)),
    ocall('_eof%1'(XStx194),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !.
'lo.comp.decode@decodeTuple'(_, _) :- raise_exception('error'("decodeTuple", 13, 3, 66)).
'lo.comp.decode@typeLen'(XStIn186, XStx195, XIx) :- 'lo.comp.decode@digits'(XStIn186, XStx195, 0, XIx).
'lo.comp.decode@decodeFields'(XStIn187, XStIn187, 0, 'lo.core#[]').
'lo.comp.decode@decodeFields'(XStIn188, XStx198, XIx, 'lo.core#,..'((XNm, XT), XL)) :- 'lo.comp.decode@decodeName'(XStIn188, XStx196, XNm),
    'lo.comp.decode@decodeType'(XStx196, XStx197, XT),
    ocall('-%3'(XIx, 1, XX9852),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.decode@decodeFields'(XStx197, XStx198, XX9852, XL).
'lo.comp.decode@decConstraint'(XStIn189, XStx200, 'lo.comp.types#conCon'(XCon, XExtra)) :- ocall('_hdtl%3'(XStIn189, 124, XNStrm148),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decConstraint'(XNStrm148, XStx199, XCon),
    'lo.comp.decode@decConstraint'(XStx199, XStx200, XExtra).
'lo.comp.decode@decConstraint'(XStIn190, XStx203, 'lo.comp.types#conTract'(XNm, XArgs, XDeps)) :- ocall('_hdtl%3'(XStIn190, 99, XNStrm149),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeName'(XNStrm149, XStx201, XNm),
    'lo.comp.decode@decodeType'(XStx201, XStx202, 'lo.comp.types#tupleType'(XArgs)),
    'lo.comp.decode@decodeType'(XStx202, XStx203, 'lo.comp.types#tupleType'(XDeps)).
'lo.comp.decode@decConstraint'(XStIn191, XStx205, 'lo.comp.types#implementsFace'(XTp, XFace)) :- ocall('_hdtl%3'(XStIn191, 97, XNStrm150),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm150, XStx204, XTp),
    'lo.comp.decode@decodeType'(XStx204, XStx205, 'lo.comp.types#faceType'(XFace)).
'lo.comp.decode@decConstraint'(XStIn192, XStx207, 'lo.comp.types#univCon'(XTV, XCon)) :- ocall('_hdtl%3'(XStIn192, 58, XNStrm151),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm151, XStx206, XTV),
    'lo.comp.decode@decConstraint'(XStx206, XStx207, XCon).
'lo.comp.decode@decodeTypes'(XStIn193, XStIn193, 0, 'lo.core#[]').
'lo.comp.decode@decodeTypes'(XStIn194, XStx209, XIx, 'lo.core#,..'(XT, XL)) :- 'lo.comp.decode@decodeType'(XStIn194, XStx208, XT),
    ocall('-%3'(XIx, 1, XX9899),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.decode@decodeTypes'(XStx208, XStx209, XX9899, XL).
'lo.comp.decode@decodeType'(XStIn195, XNStrm152, 'lo.comp.types#anonType') :- ocall('_hdtl%3'(XStIn195, 95, XNStrm152),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn196, XNStrm153, 'lo.comp.types#voidType') :- ocall('_hdtl%3'(XStIn196, 118, XNStrm153),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn197, XNStrm154, 'lo.comp.types#thisType') :- ocall('_hdtl%3'(XStIn197, 104, XNStrm154),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn198, XNStrm155, 'lo.comp.types#tipe'("lo.core*integer")) :- ocall('_hdtl%3'(XStIn198, 105, XNStrm155),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn199, XNStrm156, 'lo.comp.types#tipe'("lo.core*float")) :- ocall('_hdtl%3'(XStIn199, 102, XNStrm156),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn200, XNStrm157, 'lo.comp.types#tipe'("lo.core*string")) :- ocall('_hdtl%3'(XStIn200, 83, XNStrm157),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn201, XNStrm158, 'lo.comp.types#tipe'("lo.core*logical")) :- ocall('_hdtl%3'(XStIn201, 108, XNStrm158),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn202, XStx210, 'lo.comp.types#kVar'(XNm)) :- ocall('_hdtl%3'(XStIn202, 107, XNStrm159),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeName'(XNStrm159, XStx210, XNm).
'lo.comp.decode@decodeType'(XStIn203, XStx212, 'lo.comp.types#kFun'(XNm, XAr)) :- ocall('_hdtl%3'(XStIn203, 75, XNStrm160),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@typeLen'(XNStrm160, XStx211, XAr),
    'lo.comp.decode@decodeName'(XStx211, XStx212, XNm).
'lo.comp.decode@decodeType'(XStIn204, XStx213, 'lo.comp.types#tipe'(XNm)) :- ocall('_hdtl%3'(XStIn204, 116, XNStrm161),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeName'(XNStrm161, XStx213, XNm).
'lo.comp.decode@decodeType'(XStIn205, XStx215, 'lo.comp.types#tpFun'(XNm, XAr)) :- ocall('_hdtl%3'(XStIn205, 122, XNStrm162),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@typeLen'(XNStrm162, XStx214, XAr),
    'lo.comp.decode@decodeName'(XStx214, XStx215, XNm).
'lo.comp.decode@decodeType'(XStIn206, XStx216, 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'(XElTp, 'lo.core#[]'))) :- ocall('_hdtl%3'(XStIn206, 76, XNStrm163),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm163, XStx216, XElTp).
'lo.comp.decode@decodeType'(XStIn207, XStx219, 'lo.comp.types#typeExp'(XNm, XArgTypes)) :- ocall('_hdtl%3'(XStIn207, 85, XNStrm164),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm164, XStx217, XNm),
    'lo.comp.decode@typeLen'(XStx217, XStx218, XLn),
    'lo.comp.decode@decodeTypes'(XStx218, XStx219, XLn, XArgTypes).
'lo.comp.decode@decodeType'(XStIn208, XStx221, 'lo.comp.types#univType'(XTV, XTp)) :- ocall('_hdtl%3'(XStIn208, 58, XNStrm165),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm165, XStx220, XTV),
    'lo.comp.decode@decodeType'(XStx220, XStx221, XTp).
'lo.comp.decode@decodeType'(XStIn209, XStx223, 'lo.comp.types#constrained'(XTp, XCon)) :- ocall('_hdtl%3'(XStIn209, 124, XNStrm166),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm166, XStx222, XTp),
    'lo.comp.decode@decConstraint'(XStx222, XStx223, XCon).
'lo.comp.decode@decodeType'(XStIn210, XStx225, 'lo.comp.types#faceType'(XFields)) :- ocall('_hdtl%3'(XStIn210, 73, XNStrm167),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@typeLen'(XNStrm167, XStx224, XLn),
    'lo.comp.decode@decodeFields'(XStx224, XStx225, XLn, XFields).
'lo.comp.decode@decodeType'(XStIn211, XStx227, 'lo.comp.types#funType'(XA, XT)) :- ocall('_hdtl%3'(XStIn211, 70, XNStrm168),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm168, XStx226, XA),
    'lo.comp.decode@decodeType'(XStx226, XStx227, XT).
'lo.comp.decode@decodeType'(XStIn212, XStx229, 'lo.comp.types#grammarType'(XA, XT)) :- ocall('_hdtl%3'(XStIn212, 71, XNStrm169),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm169, XStx228, XA),
    'lo.comp.decode@decodeType'(XStx228, XStx229, XT).
'lo.comp.decode@decodeType'(XStIn213, XStx230, 'lo.comp.types#predType'(XA)) :- ocall('_hdtl%3'(XStIn213, 80, XNStrm170),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm170, XStx230, XA).
'lo.comp.decode@decodeType'(XStIn214, XStx232, 'lo.comp.types#classType'(XA, XT)) :- ocall('_hdtl%3'(XStIn214, 67, XNStrm171),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm171, XStx231, XA),
    'lo.comp.decode@decodeType'(XStx231, XStx232, XT).
'lo.comp.decode@decodeType'(XStIn215, XStx234, 'lo.comp.types#tupleType'(XTps)) :- ocall('_hdtl%3'(XStIn215, 84, XNStrm172),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@typeLen'(XNStrm172, XStx233, XLn),
    'lo.comp.decode@decodeTypes'(XStx233, XStx234, XLn, XTps).
'lo.comp.decode@decodeType'(XStIn216, XStx236, 'lo.comp.types#typeRule'(XL, XR)) :- ocall('_hdtl%3'(XStIn216, 89, XNStrm173),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm173, XStx235, XL),
    'lo.comp.decode@decodeType'(XStx235, XStx236, XR).
'lo.comp.decode@decodeConstraint'(XTxt, XCon) :- 'explode'(XTxt, XX10050),
    'lo.comp.decode@decConstraint'(XX10050, XStx237, XCon),
    ocall('_eof%1'(XStx237),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@Neg9'(XNegStrm9, X_669) :- 'lo.comp.parseutils@digit'(XNegStrm9, XStx171, X_669),
    !,
    fail.
'lo.comp.decode@Neg9'(XNegStrm9, X_669).
'lo.comp.decode^digits'('_call%4'(XV1574, XV1575, XV1576, XV1577), 'lo.comp.decode^digits', _) :- 'lo.comp.decode@digits'(XV1574, XV1575, XV1576, XV1577).
'lo.comp.decode^decInt'('_call%3'(XV1578, XV1579, XV1580), 'lo.comp.decode^decInt', _) :- 'lo.comp.decode@decInt'(XV1578, XV1579, XV1580).
'lo.comp.decode^collectUntil'('_call%4'(XV1581, XV1582, XV1583, XV1584), 'lo.comp.decode^collectUntil', _) :- 'lo.comp.decode@collectUntil'(XV1581, XV1582, XV1583, XV1584).
'lo.comp.decode^decodeName'('_call%3'(XV1585, XV1586, XV1587), 'lo.comp.decode^decodeName', _) :- 'lo.comp.decode@decodeName'(XV1585, XV1586, XV1587).
'lo.comp.decode^decFloat'('_call%3'(XV1588, XV1589, XV1590), 'lo.comp.decode^decFloat', _) :- 'lo.comp.decode@decFloat'(XV1588, XV1589, XV1590).
'lo.comp.decode^collectQuoted'('_call%4'(XV1591, XV1592, XV1593, XV1594), 'lo.comp.decode^collectQuoted', _) :- 'lo.comp.decode@collectQuoted'(XV1591, XV1592, XV1593, XV1594).
'lo.comp.decode^decodeText'('_call%3'(XV1595, XV1596, XV1597), 'lo.comp.decode^decodeText', _) :- 'lo.comp.decode@decodeText'(XV1595, XV1596, XV1597).
'lo.comp.decode^decTerms'('_call%4'(XV1598, XV1599, XV1600, XV1601), 'lo.comp.decode^decTerms', _) :- 'lo.comp.decode@decTerms'(XV1598, XV1599, XV1600, XV1601).
'lo.comp.decode^decodeTerm'('_call%3'(XV1602, XV1603, XV1604), 'lo.comp.decode^decodeTerm', _) :- 'lo.comp.decode@decodeTerm'(XV1602, XV1603, XV1604).
'lo.comp.decode^decodeValue'('_call%2'(XV1605, XV1606), 'lo.comp.decode^decodeValue', _) :- 'lo.comp.decode@decodeValue'(XV1605, XV1606).
'lo.comp.decode^decodeTuple'('_call%2'(XV1607, XV1608), 'lo.comp.decode^decodeTuple', _) :- 'lo.comp.decode@decodeTuple'(XV1607, XV1608).
'lo.comp.decode^typeLen'('_call%3'(XV1609, XV1610, XV1611), 'lo.comp.decode^typeLen', _) :- 'lo.comp.decode@typeLen'(XV1609, XV1610, XV1611).
'lo.comp.decode^decodeFields'('_call%4'(XV1612, XV1613, XV1614, XV1615), 'lo.comp.decode^decodeFields', _) :- 'lo.comp.decode@decodeFields'(XV1612, XV1613, XV1614, XV1615).
'lo.comp.decode^decConstraint'('_call%3'(XV1616, XV1617, XV1618), 'lo.comp.decode^decConstraint', _) :- 'lo.comp.decode@decConstraint'(XV1616, XV1617, XV1618).
'lo.comp.decode^decodeTypes'('_call%4'(XV1619, XV1620, XV1621, XV1622), 'lo.comp.decode^decodeTypes', _) :- 'lo.comp.decode@decodeTypes'(XV1619, XV1620, XV1621, XV1622).
'lo.comp.decode^decodeType'('_call%3'(XV1623, XV1624, XV1625), 'lo.comp.decode^decodeType', _) :- 'lo.comp.decode@decodeType'(XV1623, XV1624, XV1625).
'lo.comp.decode^decodeConstraint'('_call%2'(XV1626, XV1627), 'lo.comp.decode^decodeConstraint', _) :- 'lo.comp.decode@decodeConstraint'(XV1626, XV1627).
