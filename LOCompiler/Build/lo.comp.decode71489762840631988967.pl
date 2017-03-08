'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.decode's'0.0.1'n6o6'()6'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.parseutils'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.base64'e'*'s\"I4'decodeValue'FT1St'lo.comp.term*term''decodeTuple'FT1SLt'lo.comp.term*term''decodeType'GT1t'lo.comp.types*tipe'Li'decodeConstraint'PT2St'lo.comp.types*constraint'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.decode@init'():- !.
'lo.comp.decode@collectQuoted'(XStIn1738, XNStrm1540, XC, 'lo.core#[]'):- ocall('_hdtl%3'(XStIn1738, XC, XNStrm1540),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@collectQuoted'(XStIn1739, XStx1640, XC, 'lo.core#,..'(XCh, XL)):- ocall('_hdtl%3'(XStIn1739, 92, XNStrm1541),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1541, XCh, XNStrm1542),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectQuoted'(XNStrm1542, XStx1640, XC, XL).
'lo.comp.decode@collectQuoted'(XStIn1740, XStx1641, XC, 'lo.core#,..'(XCh, XL)):- ocall('_hdtl%3'(XStIn1740, XCh, XNStrm1543),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectQuoted'(XNStrm1543, XStx1641, XC, XL).
'lo.comp.decode@decodeText'(XStIn1741, XStx1642, XChrs):- ocall('_hdtl%3'(XStIn1741, XC, XNStrm1544),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectQuoted'(XNStrm1544, XStx1642, XC, XChrs).
'lo.comp.decode@collectUntil'(XStIn1742, XNStrm1545, XC, 'lo.core#[]'):- ocall('_hdtl%3'(XStIn1742, XC, XNStrm1545),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@collectUntil'(XStIn1743, XStx1643, XC, 'lo.core#,..'(XCh, XL)):- ocall('_hdtl%3'(XStIn1743, XCh, XNStrm1546),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectUntil'(XNStrm1546, XStx1643, XC, XL).
'lo.comp.decode@decodeName'(XStIn1744, XStx1644, XXa95):- ocall('_hdtl%3'(XStIn1744, XC, XNStrm1547),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@collectUntil'(XNStrm1547, XStx1644, XC, XText),
    'implode'(XText, XXa95).
'lo.comp.decode@decFloat'(XStIn1745, XStx1645, XXe4867):- 'lo.comp.decode@decodeName'(XStIn1745, XStx1645, XSx),
    ocall('_coerce%1'(XXV5214),'lo.coerce$coercion$lo.core*string$lo.core*float','lo.coerce$coercion$lo.core*string$lo.core*float'),
    ocall('_call%2'(XSx, XXe4867),XXV5214,XXV5214).
'lo.comp.decode@digits'(XStIn1746, XStx1647, XSoFar, XIx):- 'lo.comp.parseutils@digit'(XStIn1746, XStx1646, XD),
    'lo.comp.parseutils@digitVal'(XD, XDv),
    ocall('*%1'(XXV5215),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%1'(XXV5216),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XSoFar, 10, XXe4868),XXV5215,XXV5215),
    ocall('_call%3'(XXe4868, XDv, XXe4869),XXV5216,XXV5216),
    'lo.comp.decode@digits'(XStx1646, XStx1647, XXe4869, XIx).
'lo.comp.decode@digits'(XStIn1747, XStIn1747, XIx, XIx):- 'lo.comp.decode@Neg51'(XStIn1747, XStx1648, X_33393).
'lo.comp.decode@decInt'(XStIn1748, XStx1649, XXe4870):- ocall('_hdtl%3'(XStIn1748, 45, XNStrm1548),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm1548, XStx1649, XIx),
    ocall('-%1'(XXV5218),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('zero%1'(XXV5217),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XXV5217, XIx, XXe4870),XXV5218,XXV5218).
'lo.comp.decode@decInt'(XStIn1749, XStx1650, XIx):- 'lo.comp.decode@digits'(XStIn1749, XStx1650, 0, XIx).
'lo.comp.decode@decTerms'(XStIn1750, XStIn1750, 0, 'lo.core#[]').
'lo.comp.decode@decTerms'(XStIn1751, XStx1652, XN, 'lo.core#,..'(XT, XL)):- 'lo.comp.decode@decodeTerm'(XStIn1751, XStx1651, XT),
    ocall('-%1'(XXV5219),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XN, 1, XXe4871),XXV5219,XXV5219),
    'lo.comp.decode@decTerms'(XStx1651, XStx1652, XXe4871, XL).
'lo.comp.decode@decodeTerm'(XStIn1752, XNStrm1549, 'lo.comp.term#anon'):- ocall('_hdtl%3'(XStIn1752, 97, XNStrm1549),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeTerm'(XStIn1753, XStx1653, 'lo.comp.term#intgr'(XIx)):- ocall('_hdtl%3'(XStIn1753, 120, XNStrm1550),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm1550, XStx1653, XIx).
'lo.comp.decode@decodeTerm'(XStIn1754, XStx1654, 'lo.comp.term#flot'(XDx)):- ocall('_hdtl%3'(XStIn1754, 100, XNStrm1551),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decFloat'(XNStrm1551, XStx1654, XDx).
'lo.comp.decode@decodeTerm'(XStIn1755, XStx1655, 'lo.comp.term#enum'(XNm)):- ocall('_hdtl%3'(XStIn1755, 101, XNStrm1552),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeName'(XNStrm1552, XStx1655, XNm).
'lo.comp.decode@decodeTerm'(XStIn1756, XStx1656, 'lo.comp.term#strng'(XSx)):- ocall('_hdtl%3'(XStIn1756, 115, XNStrm1553),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeText'(XNStrm1553, XStx1656, XChrs),
    'implode'(XChrs, XXc511),
    XSx = XXc511.
'lo.comp.decode@decodeTerm'(XStIn1757, XStx1658, 'lo.comp.term#strct'(XNm, XAr)):- ocall('_hdtl%3'(XStIn1757, 111, XNStrm1554),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm1554, XStx1657, XAr),
    'lo.comp.decode@decodeName'(XStx1657, XStx1658, XNm).
'lo.comp.decode@decodeTerm'(XStIn1758, XStx1660, 'lo.comp.term#prg'(XNm, XAr)):- ocall('_hdtl%3'(XStIn1758, 112, XNStrm1555),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm1555, XStx1659, XAr),
    'lo.comp.decode@decodeName'(XStx1659, XStx1660, XNm).
'lo.comp.decode@decodeTerm'(XStIn1759, XStx1663, 'lo.comp.term#cons'(XOp, XArgs)):- ocall('_hdtl%3'(XStIn1759, 110, XNStrm1556),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decInt'(XNStrm1556, XStx1661, XLen),
    'lo.comp.decode@decodeTerm'(XStx1661, XStx1662, XOp),
    'lo.comp.decode@decTerms'(XStx1662, XStx1663, XLen, XArgs).
'lo.comp.decode@decodeValue'(XTxt, XTerm):- 'explode'(XTxt, XXc512),
    'lo.comp.decode@decodeTerm'(XXc512, XStx1664, XTerm),
    XStx1664 = X_33396,
    ocall('_eof%1'(X_33396),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_33395 = XStx1664,
    !.
'lo.comp.decode@decodeValue'(_, _):- raise_exception('error'("lo.comp.decode@decodeValue", 10, 3, 60)).
'lo.comp.decode@decodeTuple'(XTxt, XEls):- 'explode'(XTxt, XXc513),
    'lo.comp.decode@decodeTerm'(XXc513, XStx1665, 'lo.comp.term#cons'(X_33398, XEls)),
    XStx1665 = X_33399,
    ocall('_eof%1'(X_33399),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_33397 = XStx1665,
    !.
'lo.comp.decode@decodeTuple'(_, _):- raise_exception('error'("lo.comp.decode@decodeTuple", 13, 3, 66)).
'lo.comp.decode@typeLen'(XStIn1760, XStx1666, XIx):- 'lo.comp.decode@digits'(XStIn1760, XStx1666, 0, XIx).
'lo.comp.decode@decodeTypes'(XStIn1761, XStIn1761, 0, 'lo.core#[]').
'lo.comp.decode@decodeTypes'(XStIn1762, XStx1668, XIx, 'lo.core#,..'(XT, XL)):- 'lo.comp.decode@decodeType'(XStIn1762, XStx1667, XT),
    ocall('-%1'(XXV5220),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe4872),XXV5220,XXV5220),
    'lo.comp.decode@decodeTypes'(XStx1667, XStx1668, XXe4872, XL).
'lo.comp.decode@decConstraint'(XStIn1763, XStx1670, 'lo.comp.types#conCon'(XCon, XExtra)):- ocall('_hdtl%3'(XStIn1763, 124, XNStrm1557),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decConstraint'(XNStrm1557, XStx1669, XCon),
    'lo.comp.decode@decConstraint'(XStx1669, XStx1670, XExtra).
'lo.comp.decode@decConstraint'(XStIn1764, XStx1673, 'lo.comp.types#conTract'(XNm, XArgs, XDeps)):- ocall('_hdtl%3'(XStIn1764, 99, XNStrm1558),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeName'(XNStrm1558, XStx1671, XNm),
    'lo.comp.decode@decodeType'(XStx1671, XStx1672, 'lo.comp.types#tupleType'(XArgs)),
    'lo.comp.decode@decodeType'(XStx1672, XStx1673, 'lo.comp.types#tupleType'(XDeps)).
'lo.comp.decode@decConstraint'(XStIn1765, XStx1675, 'lo.comp.types#implementsFace'(XTp, XFace)):- ocall('_hdtl%3'(XStIn1765, 97, XNStrm1559),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1559, XStx1674, XTp),
    'lo.comp.decode@decodeType'(XStx1674, XStx1675, 'lo.comp.types#faceType'(XFace)).
'lo.comp.decode@decConstraint'(XStIn1766, XStx1677, 'lo.comp.types#univCon'(XTV, XCon)):- ocall('_hdtl%3'(XStIn1766, 58, XNStrm1560),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1560, XStx1676, XTV),
    'lo.comp.decode@decConstraint'(XStx1676, XStx1677, XCon).
'lo.comp.decode@decodeFields'(XStIn1767, XStIn1767, 0, 'lo.core#[]').
'lo.comp.decode@decodeFields'(XStIn1768, XStx1680, XIx, 'lo.core#,..'('()2'(XNm, XT), XL)):- 'lo.comp.decode@decodeName'(XStIn1768, XStx1678, XNm),
    'lo.comp.decode@decodeType'(XStx1678, XStx1679, XT),
    ocall('-%1'(XXV5221),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XIx, 1, XXe4873),XXV5221,XXV5221),
    'lo.comp.decode@decodeFields'(XStx1679, XStx1680, XXe4873, XL).
'lo.comp.decode@decodeType'(XStIn1769, XNStrm1561, 'lo.comp.types#anonType'):- ocall('_hdtl%3'(XStIn1769, 95, XNStrm1561),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn1770, XNStrm1562, 'lo.comp.types#voidType'):- ocall('_hdtl%3'(XStIn1770, 118, XNStrm1562),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn1771, XNStrm1563, 'lo.comp.types#thisType'):- ocall('_hdtl%3'(XStIn1771, 104, XNStrm1563),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn1772, XNStrm1564, 'lo.comp.types#tipe'("lo.core*integer")):- ocall('_hdtl%3'(XStIn1772, 105, XNStrm1564),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn1773, XNStrm1565, 'lo.comp.types#tipe'("lo.core*float")):- ocall('_hdtl%3'(XStIn1773, 102, XNStrm1565),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn1774, XNStrm1566, 'lo.comp.types#tipe'("lo.core*string")):- ocall('_hdtl%3'(XStIn1774, 83, XNStrm1566),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn1775, XNStrm1567, 'lo.comp.types#tipe'("lo.core*logical")):- ocall('_hdtl%3'(XStIn1775, 108, XNStrm1567),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.decode@decodeType'(XStIn1776, XStx1681, 'lo.comp.types#kVar'(XNm)):- ocall('_hdtl%3'(XStIn1776, 107, XNStrm1568),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeName'(XNStrm1568, XStx1681, XNm).
'lo.comp.decode@decodeType'(XStIn1777, XStx1683, 'lo.comp.types#kFun'(XNm, XAr)):- ocall('_hdtl%3'(XStIn1777, 75, XNStrm1569),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@typeLen'(XNStrm1569, XStx1682, XAr),
    'lo.comp.decode@decodeName'(XStx1682, XStx1683, XNm).
'lo.comp.decode@decodeType'(XStIn1778, XStx1684, 'lo.comp.types#tipe'(XNm)):- ocall('_hdtl%3'(XStIn1778, 116, XNStrm1570),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeName'(XNStrm1570, XStx1684, XNm).
'lo.comp.decode@decodeType'(XStIn1779, XStx1686, 'lo.comp.types#tpFun'(XNm, XAr)):- ocall('_hdtl%3'(XStIn1779, 122, XNStrm1571),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@typeLen'(XNStrm1571, XStx1685, XAr),
    'lo.comp.decode@decodeName'(XStx1685, XStx1686, XNm).
'lo.comp.decode@decodeType'(XStIn1780, XStx1687, 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'(XElTp, 'lo.core#[]'))):- ocall('_hdtl%3'(XStIn1780, 76, XNStrm1572),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1572, XStx1687, XElTp).
'lo.comp.decode@decodeType'(XStIn1781, XStx1690, 'lo.comp.types#typeExp'(XNm, XArgTypes)):- ocall('_hdtl%3'(XStIn1781, 85, XNStrm1573),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1573, XStx1688, XNm),
    'lo.comp.decode@typeLen'(XStx1688, XStx1689, XLn),
    'lo.comp.decode@decodeTypes'(XStx1689, XStx1690, XLn, XArgTypes).
'lo.comp.decode@decodeType'(XStIn1782, XStx1692, 'lo.comp.types#univType'(XTV, XTp)):- ocall('_hdtl%3'(XStIn1782, 58, XNStrm1574),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1574, XStx1691, XTV),
    'lo.comp.decode@decodeType'(XStx1691, XStx1692, XTp).
'lo.comp.decode@decodeType'(XStIn1783, XStx1694, 'lo.comp.types#constrained'(XTp, XCon)):- ocall('_hdtl%3'(XStIn1783, 124, XNStrm1575),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1575, XStx1693, XTp),
    'lo.comp.decode@decConstraint'(XStx1693, XStx1694, XCon).
'lo.comp.decode@decodeType'(XStIn1784, XStx1696, 'lo.comp.types#faceType'(XFields)):- ocall('_hdtl%3'(XStIn1784, 73, XNStrm1576),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@typeLen'(XNStrm1576, XStx1695, XLn),
    'lo.comp.decode@decodeFields'(XStx1695, XStx1696, XLn, XFields).
'lo.comp.decode@decodeType'(XStIn1785, XStx1698, 'lo.comp.types#funType'(XA, XT)):- ocall('_hdtl%3'(XStIn1785, 70, XNStrm1577),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1577, XStx1697, XA),
    'lo.comp.decode@decodeType'(XStx1697, XStx1698, XT).
'lo.comp.decode@decodeType'(XStIn1786, XStx1700, 'lo.comp.types#grammarType'(XA, XT)):- ocall('_hdtl%3'(XStIn1786, 71, XNStrm1578),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1578, XStx1699, XA),
    'lo.comp.decode@decodeType'(XStx1699, XStx1700, XT).
'lo.comp.decode@decodeType'(XStIn1787, XStx1701, 'lo.comp.types#predType'(XA)):- ocall('_hdtl%3'(XStIn1787, 80, XNStrm1579),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1579, XStx1701, XA).
'lo.comp.decode@decodeType'(XStIn1788, XStx1703, 'lo.comp.types#classType'(XA, XT)):- ocall('_hdtl%3'(XStIn1788, 67, XNStrm1580),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1580, XStx1702, XA),
    'lo.comp.decode@decodeType'(XStx1702, XStx1703, XT).
'lo.comp.decode@decodeType'(XStIn1789, XStx1705, 'lo.comp.types#tupleType'(XTps)):- ocall('_hdtl%3'(XStIn1789, 84, XNStrm1581),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@typeLen'(XNStrm1581, XStx1704, XLn),
    'lo.comp.decode@decodeTypes'(XStx1704, XStx1705, XLn, XTps).
'lo.comp.decode@decodeType'(XStIn1790, XStx1707, 'lo.comp.types#typeRule'(XL, XR)):- ocall('_hdtl%3'(XStIn1790, 89, XNStrm1582),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.decode@decodeType'(XNStrm1582, XStx1706, XL),
    'lo.comp.decode@decodeType'(XStx1706, XStx1707, XR).
'lo.comp.decode@decodeConstraint'(XTxt, XCon):- 'explode'(XTxt, XXc514),
    'lo.comp.decode@decConstraint'(XXc514, XStx1708, XCon),
    XStx1708 = X_33404,
    ocall('_eof%1'(X_33404),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_33403 = XStx1708.
'lo.comp.decode^collectQuoted'('_call%4'(XV30030, XV30031, XV30032, XV30033), 'lo.comp.decode^collectQuoted', _):- 'lo.comp.decode@collectQuoted'(XV30030, XV30031, XV30032, XV30033).
'lo.comp.decode^decodeText'('_call%3'(XV30034, XV30035, XV30036), 'lo.comp.decode^decodeText', _):- 'lo.comp.decode@decodeText'(XV30034, XV30035, XV30036).
'lo.comp.decode^collectUntil'('_call%4'(XV30037, XV30038, XV30039, XV30040), 'lo.comp.decode^collectUntil', _):- 'lo.comp.decode@collectUntil'(XV30037, XV30038, XV30039, XV30040).
'lo.comp.decode^decodeName'('_call%3'(XV30041, XV30042, XV30043), 'lo.comp.decode^decodeName', _):- 'lo.comp.decode@decodeName'(XV30041, XV30042, XV30043).
'lo.comp.decode^decFloat'('_call%3'(XV30044, XV30045, XV30046), 'lo.comp.decode^decFloat', _):- 'lo.comp.decode@decFloat'(XV30044, XV30045, XV30046).
'lo.comp.decode@Neg51'(XNegStrm51, XStx1648, X_33393):- 'lo.comp.parseutils@digit'(XNegStrm51, XStx1648, X_33393),
    !,
    fail.
'lo.comp.decode@Neg51'(XNegStrm51, XStx1648, X_33393).
'lo.comp.decode^digits'('_call%4'(XV30047, XV30048, XV30049, XV30050), 'lo.comp.decode^digits', _):- 'lo.comp.decode@digits'(XV30047, XV30048, XV30049, XV30050).
'lo.comp.decode^decInt'('_call%3'(XV30051, XV30052, XV30053), 'lo.comp.decode^decInt', _):- 'lo.comp.decode@decInt'(XV30051, XV30052, XV30053).
'lo.comp.decode^decTerms'('_call%4'(XV30054, XV30055, XV30056, XV30057), 'lo.comp.decode^decTerms', _):- 'lo.comp.decode@decTerms'(XV30054, XV30055, XV30056, XV30057).
'lo.comp.decode^decodeTerm'('_call%3'(XV30058, XV30059, XV30060), 'lo.comp.decode^decodeTerm', _):- 'lo.comp.decode@decodeTerm'(XV30058, XV30059, XV30060).
'lo.comp.decode^decodeValue'('_call%2'(XV30061, XV30062), 'lo.comp.decode^decodeValue', _):- 'lo.comp.decode@decodeValue'(XV30061, XV30062).
'lo.comp.decode^decodeTuple'('_call%2'(XV30063, XV30064), 'lo.comp.decode^decodeTuple', _):- 'lo.comp.decode@decodeTuple'(XV30063, XV30064).
'lo.comp.decode^typeLen'('_call%3'(XV30065, XV30066, XV30067), 'lo.comp.decode^typeLen', _):- 'lo.comp.decode@typeLen'(XV30065, XV30066, XV30067).
'lo.comp.decode^decodeTypes'('_call%4'(XV30068, XV30069, XV30070, XV30071), 'lo.comp.decode^decodeTypes', _):- 'lo.comp.decode@decodeTypes'(XV30068, XV30069, XV30070, XV30071).
'lo.comp.decode^decConstraint'('_call%3'(XV30072, XV30073, XV30074), 'lo.comp.decode^decConstraint', _):- 'lo.comp.decode@decConstraint'(XV30072, XV30073, XV30074).
'lo.comp.decode^decodeFields'('_call%4'(XV30075, XV30076, XV30077, XV30078), 'lo.comp.decode^decodeFields', _):- 'lo.comp.decode@decodeFields'(XV30075, XV30076, XV30077, XV30078).
'lo.comp.decode^decodeType'('_call%3'(XV30079, XV30080, XV30081), 'lo.comp.decode^decodeType', _):- 'lo.comp.decode@decodeType'(XV30079, XV30080, XV30081).
'lo.comp.decode^decodeConstraint'('_call%2'(XV30082, XV30083), 'lo.comp.decode^decodeConstraint', _):- 'lo.comp.decode@decodeConstraint'(XV30082, XV30083).
