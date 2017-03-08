'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.lexer's'0.0.1'n6o6'()6'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.token'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.operators'e'*'s\"I8'charRef'GT1it'lo.comp.lexer*tokenState''readIden'GT1Lit'lo.comp.lexer*tokenState''tokenize'GT1Lt'lo.comp.token*token't'lo.comp.lexer*tokenState''startState'FT2LiSt'lo.comp.lexer*tokenState''tokenizeFile'FT1t'lo.uri*uri'Lt'lo.comp.token*token''tokenCodes'FT2LiSLt'lo.comp.token*token''getNTokens'FT3LiSiLt'lo.comp.token*token''subTokenize'FT2t'lo.comp.location*location'SLt'lo.comp.token*token'\"s\"I1'tokenState'Yt'lo.comp.lexer*tokenState'I1'currentLocation'PT1T4iiiS\"n0o0'()0'n0o0'()0'n1o1'()1'n2o2'()2's'lo.core$stream$lo.comp.lexer*tokenState's\"c'lo.core$stream'T1t'lo.comp.lexer*tokenState'T1i\"").
'lo.comp.lexer@init'():- !.
'lo.comp.lexer@blockComment'(XStIn1791, XStIn1791):- XStIn1791 = X_33436,
    ocall('_eof%1'(X_33436),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@blockComment'(XStIn1792, XNStrm1584):- ocall('_hdtl%3'(XStIn1792, 42, XNStrm1583),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm1583, 47, XNStrm1584),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@blockComment'(XStIn1793, XStx1709):- ocall('_hdtl%3'(XStIn1793, X_33437, XNStrm1585),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@blockComment'(XNStrm1585, XStx1709).
'lo.comp.lexer@lineComment'(XStIn1794, XNStrm1586):- ocall('_hdtl%3'(XStIn1794, 10, XNStrm1586),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@lineComment'(XStIn1795, XStx1710):- ocall('_hdtl%3'(XStIn1795, X_33438, XNStrm1587),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@lineComment'(XNStrm1587, XStx1710).
'lo.comp.lexer@whiteSpace'(XStIn1796, XDjOut118):- ocall('_hdtl%3'(XStIn1796, XX, XNStrm1588),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@One33'(XNStrm1588, XDjOut118, XX).
'lo.comp.lexer@skipSpaces'(XStIn1797, XStx1712):- 'lo.comp.lexer@whiteSpace'(XStIn1797, XStx1711),
    'lo.comp.lexer@skipSpaces'(XStx1711, XStx1712).
'lo.comp.lexer@skipSpaces'(XStIn1798, XStx1714):- ocall('_hdtl%3'(XStIn1798, 45, XNStrm1589),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm1589, 45, XNStrm1590),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@Disj86'(XNStrm1590, XDjOut119, XNStrm1592, XNStrm1592, XNStrm1591, XNStrm1591, XDjStrm86),
    'lo.comp.lexer@lineComment'(XDjOut119, XStx1713),
    'lo.comp.lexer@skipSpaces'(XStx1713, XStx1714).
'lo.comp.lexer@skipSpaces'(XStIn1799, XStx1716):- ocall('_hdtl%3'(XStIn1799, 47, XNStrm1593),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm1593, 42, XNStrm1594),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@blockComment'(XNStrm1594, XStx1715),
    'lo.comp.lexer@skipSpaces'(XStx1715, XStx1716).
'lo.comp.lexer@skipSpaces'(XStIn1800, XStIn1800).
'lo.comp.lexer@followGraph'(XStIn1801, XStx1717, XState, XOp):- ocall('_hdtl%3'(XStIn1801, XCh, XNStrm1595),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.operators@follows'(XState, XCh, XNext),
    'lo.comp.lexer@followGraph'(XNStrm1595, XStx1717, XNext, XOp).
'lo.comp.lexer@followGraph'(XStIn1802, XStIn1802, XOp, XOp):- 'lo.comp.operators@final'(XOp).
'lo.comp.lexer@digit'(XStIn1803, XNStrm1596, 0):- ocall('_hdtl%3'(XStIn1803, 48, XNStrm1596),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn1804, XNStrm1597, 1):- ocall('_hdtl%3'(XStIn1804, 49, XNStrm1597),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn1805, XNStrm1598, 2):- ocall('_hdtl%3'(XStIn1805, 50, XNStrm1598),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn1806, XNStrm1599, 3):- ocall('_hdtl%3'(XStIn1806, 51, XNStrm1599),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn1807, XNStrm1600, 4):- ocall('_hdtl%3'(XStIn1807, 52, XNStrm1600),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn1808, XNStrm1601, 5):- ocall('_hdtl%3'(XStIn1808, 53, XNStrm1601),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn1809, XNStrm1602, 6):- ocall('_hdtl%3'(XStIn1809, 54, XNStrm1602),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn1810, XNStrm1603, 7):- ocall('_hdtl%3'(XStIn1810, 55, XNStrm1603),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn1811, XNStrm1604, 8):- ocall('_hdtl%3'(XStIn1811, 56, XNStrm1604),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn1812, XNStrm1605, 9):- ocall('_hdtl%3'(XStIn1812, 57, XNStrm1605),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1813, XStx1718, XD):- 'lo.comp.lexer@digit'(XStIn1813, XStx1718, XD).
'lo.comp.lexer@hexDigit'(XStIn1814, XNStrm1606, 10):- ocall('_hdtl%3'(XStIn1814, 97, XNStrm1606),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1815, XNStrm1607, 11):- ocall('_hdtl%3'(XStIn1815, 98, XNStrm1607),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1816, XNStrm1608, 12):- ocall('_hdtl%3'(XStIn1816, 99, XNStrm1608),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1817, XNStrm1609, 13):- ocall('_hdtl%3'(XStIn1817, 100, XNStrm1609),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1818, XNStrm1610, 14):- ocall('_hdtl%3'(XStIn1818, 101, XNStrm1610),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1819, XNStrm1611, 15):- ocall('_hdtl%3'(XStIn1819, 102, XNStrm1611),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1820, XNStrm1612, 10):- ocall('_hdtl%3'(XStIn1820, 65, XNStrm1612),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1821, XNStrm1613, 11):- ocall('_hdtl%3'(XStIn1821, 66, XNStrm1613),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1822, XNStrm1614, 12):- ocall('_hdtl%3'(XStIn1822, 67, XNStrm1614),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1823, XNStrm1615, 13):- ocall('_hdtl%3'(XStIn1823, 68, XNStrm1615),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1824, XNStrm1616, 14):- ocall('_hdtl%3'(XStIn1824, 69, XNStrm1616),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn1825, XNStrm1617, 15):- ocall('_hdtl%3'(XStIn1825, 70, XNStrm1617),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexChars'(XStIn1826, XStx1720, XSoFar, XCh):- 'lo.comp.lexer@hexDigit'(XStIn1826, XStx1719, XN),
    ocall('*%1'(XXV5227),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%1'(XXV5228),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XSoFar, 16, XXe4879),XXV5227,XXV5227),
    ocall('_call%3'(XXe4879, XN, XXe4880),XXV5228,XXV5228),
    'lo.comp.lexer@hexChars'(XStx1719, XStx1720, XXe4880, XCh).
'lo.comp.lexer@hexChars'(XStIn1827, XNStrm1618, XSoFar, XSoFar):- ocall('_hdtl%3'(XStIn1827, 59, XNStrm1618),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn1828, XNStrm1619, 7):- ocall('_hdtl%3'(XStIn1828, 97, XNStrm1619),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn1829, XNStrm1620, 8):- ocall('_hdtl%3'(XStIn1829, 98, XNStrm1620),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn1830, XNStrm1621, 9):- ocall('_hdtl%3'(XStIn1830, 116, XNStrm1621),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn1831, XNStrm1622, 10):- ocall('_hdtl%3'(XStIn1831, 110, XNStrm1622),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn1832, XNStrm1623, 13):- ocall('_hdtl%3'(XStIn1832, 114, XNStrm1623),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn1833, XStx1721, XCh):- ocall('_hdtl%3'(XStIn1833, 117, XNStrm1624),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@hexChars'(XNStrm1624, XStx1721, 0, XCh).
'lo.comp.lexer@backSlashRef'(XStIn1834, XNStrm1625, XCh):- ocall('_hdtl%3'(XStIn1834, XCh, XNStrm1625),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@charRef'(XStIn1835, XStx1722, XChr):- ocall('_hdtl%3'(XStIn1835, 92, XNStrm1626),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@backSlashRef'(XNStrm1626, XStx1722, XChr).
'lo.comp.lexer@charRef'(XStIn1836, XNStrm1627, XChr):- ocall('_hdtl%3'(XStIn1836, XChr, XNStrm1627),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@readStr'(XStIn1837, XStIn1837, 'lo.core#[]'):- 'lo.comp.lexer@Hed104'(XStIn1837, XNStrm1628, XNStrm1628, XHedStrm104).
'lo.comp.lexer@readStr'(XStIn1838, XStIn1838, 'lo.core#[]'):- 'lo.comp.lexer@Hed105'(XStIn1838, XNStrm1630, XNStrm1630, XNStrm1629, XNStrm1629, XHedStrm105).
'lo.comp.lexer@readStr'(XStIn1839, XStx1724, 'lo.core#,..'(XCh, XMore)):- 'lo.comp.lexer@charRef'(XStIn1839, XStx1723, XCh),
    'lo.comp.lexer@readStr'(XStx1723, XStx1724, XMore).
'lo.comp.lexer@makeLocation'('()4'(XStartLine, XStartOff, XStartCol, XPth), '()4'(X_33440, XEndOff, X_33441, X_33442), 'lo.comp.location#loc'(XStartLine, XStartOff, XStartCol, XXe4881, XPth)):- !,
    ocall('-%1'(XXV5229),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XEndOff, XStartOff, XXe4881),XXV5229,XXV5229).
'lo.comp.lexer@makeLocation'(_, _, _):- raise_exception('error'("lo.comp.lexer@makeLocation", 45, 3, 124)).
'lo.comp.lexer@countBrackets'(XStIn1840, XStIn1840, 'lo.core#[]', 'lo.core#[]'):- 'lo.comp.lexer@Hed106'(XStIn1840, XNStrm1631, XNStrm1631, XC, XHedStrm106),
    'lo.comp.lexer@neg318'(XXd38948, XXd38947, XXd38946, XC).
'lo.comp.lexer@countBrackets'(XStIn1841, XStx1725, 'lo.core#,..'(XCh, XMore), 'lo.core#,..'(XCh, XStack)):- ocall('_hdtl%3'(XStIn1841, XCh, XNStrm1632),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm1632, XStx1725, XMore, XStack).
'lo.comp.lexer@countBrackets'(XStIn1842, XStx1726, 'lo.core#,..'(40, XMore), XStack):- ocall('_hdtl%3'(XStIn1842, 40, XNStrm1633),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm1633, XStx1726, XMore, 'lo.core#,..'(41, XStack)).
'lo.comp.lexer@countBrackets'(XStIn1843, XStx1727, 'lo.core#,..'(91, XMore), XStack):- ocall('_hdtl%3'(XStIn1843, 91, XNStrm1634),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm1634, XStx1727, XMore, 'lo.core#,..'(93, XStack)).
'lo.comp.lexer@countBrackets'(XStIn1844, XStx1728, 'lo.core#,..'(123, XMore), XStack):- ocall('_hdtl%3'(XStIn1844, 123, XNStrm1635),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm1635, XStx1728, XMore, 'lo.core#,..'(125, XStack)).
'lo.comp.lexer@countBrackets'(XStIn1845, XStx1729, 'lo.core#,..'(34, XMore), XStack):- ocall('_hdtl%3'(XStIn1845, 34, XNStrm1636),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@neg319'(XStack),
    'lo.comp.lexer@countBrackets'(XNStrm1636, XStx1729, XMore, 'lo.core#,..'(34, XStack)).
'lo.comp.lexer@countBrackets'(XStIn1846, XStIn1846, 'lo.core#[]', 'lo.core#[]'):- 'lo.comp.lexer@Hed107'(XStIn1846, XNStrm1637, XNStrm1637, XHedStrm107).
'lo.comp.lexer@countBrackets'(XStIn1847, XStx1730, 'lo.core#,..'(XCh, XMore), XStack):- ocall('_hdtl%3'(XStIn1847, XCh, XNStrm1638),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm1638, XStx1730, XMore, XStack).
'lo.comp.lexer@readFormat'(XStIn1848, XNStrm1639, 'lo.core#[]'):- ocall('_hdtl%3'(XStIn1848, 59, XNStrm1639),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@readFormat'(XStIn1849, XStx1731, 'lo.core#,..'(XCh, XMore)):- ocall('_hdtl%3'(XStIn1849, XCh, XNStrm1640),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@neg320'(XCh),
    'lo.comp.lexer@neg321'(XCh),
    'lo.comp.lexer@readFormat'(XNStrm1640, XStx1731, XMore).
'lo.comp.lexer@readFormat'(XStIn1850, XStIn1850, 'lo.core#[]'):- XStIn1850 = X_33461,
    ocall('_eof%1'(X_33461),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@interpolation'(XStIn1851, XDjOut120, 'lo.comp.token#interpolate'(XLc, XXa96, XXa97)):- XStIn1851 = X_33462,
    ocall('currentLocation%1'(XStart),X_33462,X_33462),
    'lo.comp.lexer@countBrackets'(XStIn1851, XStx1732, XText, 'lo.core#[]'),
    'lo.comp.lexer@Disj87'(XStx1732, XDjOut120, XStx1733, XFormat, XNStrm1641, XNStrm1641, XDjStrm87),
    XDjOut120 = X_33464,
    ocall('currentLocation%1'(XEnd),X_33464,X_33464),
    'lo.comp.lexer@makeLocation'(XStart, XEnd, XXd38953),
    XLc = XXd38953,
    'implode'(XText, XXa96),
    'implode'(XFormat, XXa97).
'lo.comp.lexer@readStringSegments'(XStIn1852, XStIn1852, 'lo.core#[]'):- 'lo.comp.lexer@Hed108'(XStIn1852, XNStrm1642, XNStrm1642, XHedStrm108).
'lo.comp.lexer@readStringSegments'(XStIn1853, XStx1735, 'lo.core#,..'(XSeg, XSegs)):- ocall('_hdtl%3'(XStIn1853, 92, XNStrm1643),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@Hed109'(XNStrm1643, XNStrm1644, XNStrm1644, XHedStrm109),
    'lo.comp.lexer@interpolation'(XNStrm1643, XStx1734, XSeg),
    'lo.comp.lexer@readStringSegments'(XStx1734, XStx1735, XSegs).
'lo.comp.lexer@readStringSegments'(XStIn1854, XStx1737, 'lo.core#,..'('lo.comp.token#segment'(XLc, XSeg), XSegs)):- XStIn1854 = X_33468,
    ocall('currentLocation%1'(XStart),X_33468,X_33468),
    'lo.comp.lexer@One34'(XStIn1854, XDjOut121, XStx1736, XChars),
    XDjOut121 = X_33469,
    ocall('currentLocation%1'(XEnd),X_33469,X_33469),
    'implode'(XChars, XXc515),
    XSeg = XXc515,
    'lo.comp.lexer@makeLocation'(XStart, XEnd, XXd38954),
    XLc = XXd38954,
    'lo.comp.lexer@readStringSegments'(XDjOut121, XStx1737, XSegs).
'lo.comp.lexer@readQuoted'(XStIn1855, XStIn1855, 'lo.core#[]'):- 'lo.comp.lexer@Hed110'(XStIn1855, XNStrm1645, XNStrm1645, XHedStrm110).
'lo.comp.lexer@readQuoted'(XStIn1856, XStx1739, 'lo.core#,..'(XC, XR)):- 'lo.comp.lexer@charRef'(XStIn1856, XStx1738, XC),
    'lo.comp.lexer@neg322'(XC),
    'lo.comp.lexer@readQuoted'(XStx1738, XStx1739, XR).
'lo.comp.lexer@idStart'(XStIn1857, XNStrm1646, 95):- ocall('_hdtl%3'(XStIn1857, 95, XNStrm1646),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@idStart'(XStIn1858, XNStrm1647, XCh):- ocall('_hdtl%3'(XStIn1858, XCh, XNStrm1647),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    '_isLetterChar'(XCh).
'lo.comp.lexer@alphaNum'(XStIn1859, XStx1740, XCh):- 'lo.comp.lexer@idStart'(XStIn1859, XStx1740, XCh).
'lo.comp.lexer@alphaNum'(XStIn1860, XDjOut122, XCh):- ocall('_hdtl%3'(XStIn1860, XCh, XNStrm1648),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@One35'(XNStrm1648, XDjOut122, XCh).
'lo.comp.lexer@readIden'(XStIn1861, XStx1742, 'lo.core#,..'(XC, XR)):- 'lo.comp.lexer@alphaNum'(XStIn1861, XStx1741, XC),
    'lo.comp.lexer@readIden'(XStx1741, XStx1742, XR).
'lo.comp.lexer@readIden'(XStIn1862, XStIn1862, 'lo.core#[]').
'lo.comp.lexer@regExp'(XStIn1863, XStIn1863, 'lo.core#[]'):- 'lo.comp.lexer@Hed111'(XStIn1863, XNStrm1649, XNStrm1649, XHedStrm111).
'lo.comp.lexer@regExp'(XStIn1864, XStx1743, 'lo.core#,..'(92, 'lo.core#,..'(XCh, XMore))):- ocall('_hdtl%3'(XStIn1864, 92, XNStrm1650),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm1650, XCh, XNStrm1651),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@regExp'(XNStrm1651, XStx1743, XMore).
'lo.comp.lexer@regExp'(XStIn1865, XStx1744, 'lo.core#,..'(XCh, XMore)):- ocall('_hdtl%3'(XStIn1865, XCh, XNStrm1652),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@regExp'(XNStrm1652, XStx1744, XMore).
'lo.comp.lexer@readNatural'(XStIn1866, XStx1746, XSoFar, XInt):- 'lo.comp.lexer@digit'(XStIn1866, XStx1745, XD),
    ocall('*%1'(XXV5230),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%1'(XXV5231),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XSoFar, 10, XXe4882),XXV5230,XXV5230),
    ocall('_call%3'(XXe4882, XD, XXe4883),XXV5231,XXV5231),
    'lo.comp.lexer@readNatural'(XStx1745, XStx1746, XXe4883, XInt).
'lo.comp.lexer@readNatural'(XStIn1867, XStIn1867, XSoFar, XSoFar):- 'lo.comp.lexer@Neg52'(XStIn1867, XStx1747, X_33476).
'lo.comp.lexer@fraction'(XStIn1868, XStx1749, XScale, XSoFar, XResult):- 'lo.comp.lexer@digit'(XStIn1868, XStx1748, XD),
    ocall('*%1'(XXV5232),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    ocall('*%1'(XXV5233),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    ocall('+%1'(XXV5234),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('_call%3'(XScale, 0.1, XXe4884),XXV5232,XXV5232),
    '_int2flt'(XD, XXc516),
    ocall('_call%3'(XXc516, XScale, XXe4885),XXV5233,XXV5233),
    ocall('_call%3'(XSoFar, XXe4885, XXe4886),XXV5234,XXV5234),
    'lo.comp.lexer@fraction'(XStx1748, XStx1749, XXe4884, XXe4886, XResult).
'lo.comp.lexer@fraction'(XStIn1869, XStIn1869, X_33477, XFract, XFract).
'lo.comp.lexer@readDecimal'(XStIn1870, XStx1750, XIn):- ocall('_hdtl%3'(XStIn1870, 45, XNStrm1653),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@readNatural'(XNStrm1653, XStx1750, 0, XPl),
    ocall('-%1'(XXV5235),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(0, XPl, XXe4887),XXV5235,XXV5235),
    XIn = XXe4887.
'lo.comp.lexer@readDecimal'(XStIn1871, XStx1751, XIn):- 'lo.comp.lexer@readNatural'(XStIn1871, XStx1751, 0, XIn).
'lo.comp.lexer@exponent'(XStIn1872, XStx1752, XSoFar, XFp):- 'lo.comp.lexer@Disj88'(XStIn1872, XDjOut123, XNStrm1655, XNStrm1655, XNStrm1654, XNStrm1654, XDjStrm88),
    'lo.comp.lexer@readDecimal'(XDjOut123, XStx1752, XExp),
    ocall('*%1'(XXV5236),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    '_int2flt'(XExp, XXc517),
    '_pwr'(10.0, XXc517, XXc518),
    ocall('_call%3'(XSoFar, XXc518, XXe4888),XXV5236,XXV5236),
    XFp = XXe4888.
'lo.comp.lexer@exponent'(XStIn1873, XStIn1873, XFp, XFp).
'lo.comp.lexer@readMoreNumber'(XStIn1874, XStx1755, 'lo.comp.token#fltTok'(XFp), XDecimal):- ocall('_hdtl%3'(XStIn1874, 46, XNStrm1656),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@Hed112'(XNStrm1656, XStx1753, X_33480),
    'lo.comp.lexer@fraction'(XNStrm1656, XStx1754, 0.1, 0.0, XFr),
    ocall('+%1'(XXV5237),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    '_int2flt'(XDecimal, XXc519),
    ocall('_call%3'(XXc519, XFr, XXe4889),XXV5237,XXV5237),
    'lo.comp.lexer@exponent'(XStx1754, XStx1755, XXe4889, XFp).
'lo.comp.lexer@readMoreNumber'(XStIn1875, XStIn1875, 'lo.comp.token#intTok'(XIx), XIx).
'lo.comp.lexer@readNumber'(XStIn1876, XStx1757, XTk):- 'lo.comp.lexer@readNatural'(XStIn1876, XStx1756, 0, XFirst),
    'lo.comp.lexer@readMoreNumber'(XStx1756, XStx1757, XTk, XFirst).
'lo.comp.lexer@hexInt'(XStIn1877, XStx1759, XSoFar, XCh):- 'lo.comp.lexer@hexDigit'(XStIn1877, XStx1758, XN),
    ocall('*%1'(XXV5238),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%1'(XXV5239),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XSoFar, 16, XXe4890),XXV5238,XXV5238),
    ocall('_call%3'(XXe4890, XN, XXe4891),XXV5239,XXV5239),
    'lo.comp.lexer@hexInt'(XStx1758, XStx1759, XXe4891, XCh).
'lo.comp.lexer@hexInt'(XStIn1878, XStIn1878, XSoFar, XSoFar):- 'lo.comp.lexer@Neg53'(XStIn1878, XStx1760, X_33481).
'lo.comp.lexer@nxtTok'(XStIn1879, XStx1761, 'lo.comp.token#intTok'(XCh)):- ocall('_hdtl%3'(XStIn1879, 48, XNStrm1657),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm1657, 99, XNStrm1658),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@charRef'(XNStrm1658, XStx1761, XCh).
'lo.comp.lexer@nxtTok'(XStIn1880, XStx1762, 'lo.comp.token#intTok'(XHx)):- ocall('_hdtl%3'(XStIn1880, 48, XNStrm1659),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm1659, 120, XNStrm1660),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@hexInt'(XNStrm1660, XStx1762, 0, XHx).
'lo.comp.lexer@nxtTok'(XStIn1881, XStx1764, XNum):- 'lo.comp.lexer@Hed113'(XStIn1881, XStx1763, X_33482),
    'lo.comp.lexer@readNumber'(XStIn1881, XStx1764, XNum).
'lo.comp.lexer@nxtTok'(XStIn1882, XNStrm1661, 'lo.comp.token#lpar'):- ocall('_hdtl%3'(XStIn1882, 40, XNStrm1661),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn1883, XNStrm1662, 'lo.comp.token#rpar'):- ocall('_hdtl%3'(XStIn1883, 41, XNStrm1662),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn1884, XNStrm1663, 'lo.comp.token#lbra'):- ocall('_hdtl%3'(XStIn1884, 91, XNStrm1663),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn1885, XNStrm1664, 'lo.comp.token#rbra'):- ocall('_hdtl%3'(XStIn1885, 93, XNStrm1664),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn1886, XNStrm1665, 'lo.comp.token#lbrce'):- ocall('_hdtl%3'(XStIn1886, 123, XNStrm1665),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn1887, XNStrm1666, 'lo.comp.token#rbrce'):- ocall('_hdtl%3'(XStIn1887, 125, XNStrm1666),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn1888, XNStrm1668, 'lo.comp.token#lqpar'):- ocall('_hdtl%3'(XStIn1888, 60, XNStrm1667),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm1667, 124, XNStrm1668),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn1889, XNStrm1670, 'lo.comp.token#rqpar'):- ocall('_hdtl%3'(XStIn1889, 124, XNStrm1669),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm1669, 62, XNStrm1670),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn1890, XStx1765, 'lo.comp.token#period'):- ocall('_hdtl%3'(XStIn1890, 46, XNStrm1671),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@whiteSpace'(XNStrm1671, XStx1765).
'lo.comp.lexer@nxtTok'(XStIn1891, XNStrm1673, 'lo.comp.token#regTok'(XRg)):- ocall('_hdtl%3'(XStIn1891, 96, XNStrm1672),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@regExp'(XNStrm1672, XStx1766, XChars),
    ocall('_hdtl%3'(XStx1766, 96, XNStrm1673),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'implode'(XChars, XXc520),
    XRg = XXc520.
'lo.comp.lexer@nxtTok'(XStIn1892, XStx1768, 'lo.comp.token#idTok'(XId)):- 'lo.comp.lexer@idStart'(XStIn1892, XStx1767, XC),
    'lo.comp.lexer@readIden'(XStx1767, XStx1768, XR),
    'implode'('lo.core#,..'(XC, XR), XXc521),
    XId = XXc521.
'lo.comp.lexer@nxtTok'(XStIn1893, XNStrm1675, 'lo.comp.token#idQTok'(XId)):- ocall('_hdtl%3'(XStIn1893, 39, XNStrm1674),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@readQuoted'(XNStrm1674, XStx1769, XQ),
    ocall('_hdtl%3'(XStx1769, 39, XNStrm1675),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'implode'(XQ, XXc522),
    XId = XXc522.
'lo.comp.lexer@nxtTok'(XStIn1894, XNStrm1677, 'lo.comp.token#stringTok'(XText)):- ocall('_hdtl%3'(XStIn1894, 34, XNStrm1676),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@readStringSegments'(XNStrm1676, XStx1770, XText),
    ocall('_hdtl%3'(XStx1770, 34, XNStrm1677),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn1895, XDjOut124, 'lo.comp.token#idTok'(XOp)):- ocall('_hdtl%3'(XStIn1895, XCh, XNStrm1678),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.operators@follows'("", XCh, XNext),
    'lo.comp.lexer@One36'(XNStrm1678, XDjOut124, XStx1771, XOp, XNext).
'lo.comp.lexer@allTokens'(XStIn1896, XStIn1896, 'lo.core#[]', 0).
'lo.comp.lexer@allTokens'(XStIn1897, XStIn1897, 'lo.core#[]', X_33484):- XStIn1897 = X_33485,
    ocall('_eof%1'(X_33485),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@allTokens'(XStIn1898, XStx1774, 'lo.core#,..'('lo.comp.token#tok'(XTk, XLc), XMore), XCx):- XStIn1898 = X_33487,
    ocall('currentLocation%1'(XStart),X_33487,X_33487),
    'lo.comp.lexer@One37'(XStIn1898, XDjOut125, XStx1772, XTk),
    XDjOut125 = X_33488,
    ocall('currentLocation%1'(XEnd),X_33488,X_33488),
    'lo.comp.lexer@makeLocation'(XStart, XEnd, XXd38956),
    XLc = XXd38956,
    'lo.comp.lexer@skipSpaces'(XDjOut125, XStx1773),
    ocall('-%1'(XXV5240),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XCx, 1, XXe4892),XXV5240,XXV5240),
    'lo.comp.lexer@allTokens'(XStx1773, XStx1774, XMore, XXe4892).
'lo.comp.lexer@tokenize'(XStIn1899, XStx1776, XAllTokens):- 'lo.comp.lexer@skipSpaces'(XStIn1899, XStx1775),
    'lo.comp.lexer@allTokens'(XStx1775, XStx1776, XAllTokens, -1).
'lo.comp.lexer#tkState'('tkState%1'('lo.comp.lexer@tkState'())):- !.
'lo.comp.lexer#tkState'('currentLocation%1'(XV30203), XLbl2195, XThis2195):- !,
    'lo.comp.lexer#tkState@currentLocation'(XV30203, XLbl2195, XThis2195).
'lo.comp.lexer#tkState'('currentLocation%1'('lo.comp.lexer#tkState^currentLocation'(XLbl2196, XThis2196)), XLbl2196, XThis2196).
'lo.comp.lexer#tkState@currentLocation'('()4'(XLine, XOff, XCol, XPth), XLbV2500, XThV2500):- XLbV2500 = 'lo.comp.lexer#tkState'(XChars, XLine, XOff, XCol, XPth).
'lo.comp.lexer@startState'(XSrc, XPth, 'lo.comp.lexer#tkState'(XSrc, 1, 0, 1, XPth)):- !.
'lo.comp.lexer@startState'(_, _, _):- raise_exception('error'("lo.comp.lexer@startState", 27, 3, 45)).
'lo.comp.lexer@tokenizeFile'(XU, XToks):- 'lo.resources@getResource'(XU, XXd38958),
    'explode'(XXd38958, XXc523),
    'lo.uri@getUriPath'(XU, XXd38959),
    'lo.comp.lexer@startState'(XXc523, XXd38959, XXd38960),
    'lo.comp.lexer@tokenize'(XXd38960, XStx1777, XToks),
    XStx1777 = X_33491,
    ocall('_eof%1'(X_33491),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    X_33490 = XStx1777,
    !.
'lo.comp.lexer@tokenizeFile'(_, _):- raise_exception('error'("lo.comp.lexer@tokenizeFile", 13, 3, 95)).
'lo.comp.lexer@tokenCodes'(XChars, XPth, XToks):- 'lo.comp.lexer@startState'(XChars, XPth, XXd38961),
    'lo.comp.lexer@tokenize'(XXd38961, XStx1778, XToks),
    XStx1778 = X_33493,
    ocall('_eof%1'(X_33493),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    X_33492 = XStx1778,
    !.
'lo.comp.lexer@tokenCodes'(_, _, _):- raise_exception('error'("lo.comp.lexer@tokenCodes", 16, 3, 72)).
'lo.comp.lexer@getNTokens'(XChars, XPth, XCount, XToks):- 'lo.comp.lexer@startState'(XChars, XPth, XXd38962),
    'lo.comp.lexer@skipSpaces'(XXd38962, XStx1779),
    'lo.comp.lexer@allTokens'(XStx1779, XStx1780, XToks, XCount),
    X_33494 = XStx1780,
    !.
'lo.comp.lexer@getNTokens'(_, _, _, _):- raise_exception('error'("lo.comp.lexer@getNTokens", 19, 3, 105)).
'lo.core$stream$lo.comp.lexer*tokenState'('lo.core$stream$lo.comp.lexer*tokenState%1'('lo.core$stream$lo.comp.lexer*tokenState')):- !.
'lo.core$stream$lo.comp.lexer*tokenState'('_eof%1'(XV30219), XLbl2197, XThis2197):- !,
    'lo.core$stream$lo.comp.lexer*tokenState@_eof'(XV30219, XLbl2197, XThis2197).
'lo.core$stream$lo.comp.lexer*tokenState'('_eof%1'('lo.core$stream$lo.comp.lexer*tokenState^_eof'(XLbl2198, XThis2198)), XLbl2198, XThis2198).
'lo.core$stream$lo.comp.lexer*tokenState'('_hdtl%3'(XV30225, XV30226, XV30227), XLbl2199, XThis2199):- !,
    'lo.core$stream$lo.comp.lexer*tokenState@_hdtl'(XV30225, XV30226, XV30227, XLbl2199, XThis2199).
'lo.core$stream$lo.comp.lexer*tokenState'('_hdtl%1'('lo.core$stream$lo.comp.lexer*tokenState^_hdtl'(XLbl2200, XThis2200)), XLbl2200, XThis2200).
'lo.core$stream$lo.comp.lexer*tokenState@_eof'('lo.comp.lexer#tkState'('lo.core#[]', X_33495, X_33496, X_33497, X_33498), XLbV2501, XThV2501).
'lo.core$stream$lo.comp.lexer*tokenState@_hdtl'('lo.comp.lexer#tkState'('lo.core#,..'(10, XRest), XLine, XOff, XCol, XPth), 10, 'lo.comp.lexer#tkState'(XRest, XXe4893, XXe4894, 1, XPth), XLbV2501, XThV2501):- ocall('+%1'(XXV5241),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XLine, 1, XXe4893),XXV5241,XXV5241),
    ocall('+%1'(XXV5242),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XOff, 1, XXe4894),XXV5242,XXV5242).
'lo.core$stream$lo.comp.lexer*tokenState@_hdtl'('lo.comp.lexer#tkState'('lo.core#,..'(XCh, XRest), XLine, XOff, XCol, XPth), XCh, 'lo.comp.lexer#tkState'(XRest, XLine, XXe4895, XXe4896, XPth), XLbV2501, XThV2501):- 'lo.core$stream$lo.comp.lexer*tokenState@neg323'(XCh),
    ocall('+%1'(XXV5243),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XOff, 1, XXe4895),XXV5243,XXV5243),
    ocall('+%1'(XXV5244),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XCol, 1, XXe4896),XXV5244,XXV5244).
'lo.comp.lexer@subTokenize'('lo.comp.location#loc'(XLn, XOff, XCol, X_33501, XPth), XText, XToks):- 'explode'(XText, XXc524),
    'lo.comp.lexer@tokenize'('lo.comp.lexer#tkState'(XXc524, XLn, XOff, XCol, XPth), XStx1781, XToks),
    XStx1781 = X_33503,
    ocall('_eof%1'(X_33503),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    X_33502 = XStx1781,
    !.
'lo.comp.lexer@subTokenize'(_, _, _):- raise_exception('error'("lo.comp.lexer@subTokenize", 68, 3, 110)).
'lo.comp.lexer^blockComment'('_call%2'(XV30101, XV30102), 'lo.comp.lexer^blockComment', _):- 'lo.comp.lexer@blockComment'(XV30101, XV30102).
'lo.comp.lexer^lineComment'('_call%2'(XV30103, XV30104), 'lo.comp.lexer^lineComment', _):- 'lo.comp.lexer@lineComment'(XV30103, XV30104).
'lo.comp.lexer@or185'(XX):- '_isZpChar'(XX).
'lo.comp.lexer@or185'(XX):- '_isCcChar'(XX).
'lo.comp.lexer@or186'(XX):- '_isZlChar'(XX).
'lo.comp.lexer@or186'(XX):- 'lo.comp.lexer@or185'(XX).
'lo.comp.lexer@or187'(XX):- '_isZsChar'(XX).
'lo.comp.lexer@or187'(XX):- 'lo.comp.lexer@or186'(XX).
'lo.comp.lexer@One33'(XOneStm33, XOneStm33, XX):- 'lo.comp.lexer@or187'(XX),
    !.
'lo.comp.lexer^whiteSpace'('_call%2'(XV30105, XV30106), 'lo.comp.lexer^whiteSpace', _):- 'lo.comp.lexer@whiteSpace'(XV30105, XV30106).
'lo.comp.lexer@Disj86'(XDjStrm86, XNStrm1591, XNStrm1592, XNStrm1592, XNStrm1591, XNStrm1591, XDjStrm86):- ocall('_hdtl%3'(XDjStrm86, 32, XNStrm1591),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@Disj86'(XDjStrm86, XNStrm1592, XNStrm1592, XNStrm1592, XNStrm1591, XNStrm1591, XDjStrm86):- ocall('_hdtl%3'(XDjStrm86, 9, XNStrm1592),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^skipSpaces'('_call%2'(XV30107, XV30108), 'lo.comp.lexer^skipSpaces', _):- 'lo.comp.lexer@skipSpaces'(XV30107, XV30108).
'lo.comp.lexer^followGraph'('_call%4'(XV30109, XV30110, XV30111, XV30112), 'lo.comp.lexer^followGraph', _):- 'lo.comp.lexer@followGraph'(XV30109, XV30110, XV30111, XV30112).
'lo.comp.lexer^digit'('_call%3'(XV30113, XV30114, XV30115), 'lo.comp.lexer^digit', _):- 'lo.comp.lexer@digit'(XV30113, XV30114, XV30115).
'lo.comp.lexer^hexDigit'('_call%3'(XV30116, XV30117, XV30118), 'lo.comp.lexer^hexDigit', _):- 'lo.comp.lexer@hexDigit'(XV30116, XV30117, XV30118).
'lo.comp.lexer^hexChars'('_call%4'(XV30119, XV30120, XV30121, XV30122), 'lo.comp.lexer^hexChars', _):- 'lo.comp.lexer@hexChars'(XV30119, XV30120, XV30121, XV30122).
'lo.comp.lexer^backSlashRef'('_call%3'(XV30123, XV30124, XV30125), 'lo.comp.lexer^backSlashRef', _):- 'lo.comp.lexer@backSlashRef'(XV30123, XV30124, XV30125).
'lo.comp.lexer^charRef'('_call%3'(XV30126, XV30127, XV30128), 'lo.comp.lexer^charRef', _):- 'lo.comp.lexer@charRef'(XV30126, XV30127, XV30128).
'lo.comp.lexer@Hed104'(XHedStrm104, XNStrm1628, XNStrm1628, XHedStrm104):- ocall('_hdtl%3'(XHedStrm104, 34, XNStrm1628),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@Hed105'(XHedStrm105, XNStrm1630, XNStrm1630, XNStrm1629, XNStrm1629, XHedStrm105):- ocall('_hdtl%3'(XHedStrm105, 92, XNStrm1629),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm1629, 40, XNStrm1630),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^readStr'('_call%3'(XV30129, XV30130, XV30131), 'lo.comp.lexer^readStr', _):- 'lo.comp.lexer@readStr'(XV30129, XV30130, XV30131).
'lo.comp.lexer^makeLocation'('_call%3'(XV30132, XV30133, XV30134), 'lo.comp.lexer^makeLocation', _):- 'lo.comp.lexer@makeLocation'(XV30132, XV30133, XV30134).
'lo.comp.lexer@Hed106'(XHedStrm106, XNStrm1631, XNStrm1631, XC, XHedStrm106):- ocall('_hdtl%3'(XHedStrm106, XC, XNStrm1631),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@neg318'(XXd38948, XXd38947, XXd38946, XC):- ocall('in%2'(XC, 'lo.core#,..'(40, 'lo.core#,..'(91, 'lo.core#,..'(123, 'lo.core#[]')))),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.lexer@neg318'(XXd38948, XXd38947, XXd38946, XC).
'lo.comp.lexer@neg319'(XStack):- XStack = 'lo.core#[]',
    !,
    fail.
'lo.comp.lexer@neg319'(XStack).
'lo.comp.lexer@Hed107'(XHedStrm107, XNStrm1637, XNStrm1637, XHedStrm107):- ocall('_hdtl%3'(XHedStrm107, 34, XNStrm1637),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^countBrackets'('_call%4'(XV30135, XV30136, XV30137, XV30138), 'lo.comp.lexer^countBrackets', _):- 'lo.comp.lexer@countBrackets'(XV30135, XV30136, XV30137, XV30138).
'lo.comp.lexer@neg320'(XCh):- XCh = 92,
    !,
    fail.
'lo.comp.lexer@neg320'(XCh).
'lo.comp.lexer@neg321'(XCh):- XCh = 34,
    !,
    fail.
'lo.comp.lexer@neg321'(XCh).
'lo.comp.lexer^readFormat'('_call%3'(XV30139, XV30140, XV30141), 'lo.comp.lexer^readFormat', _):- 'lo.comp.lexer@readFormat'(XV30139, XV30140, XV30141).
'lo.comp.lexer@Disj87'(XDjStrm87, XStx1733, XStx1733, XFormat, XNStrm1641, XNStrm1641, XDjStrm87):- ocall('_hdtl%3'(XDjStrm87, 58, XNStrm1641),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@readFormat'(XNStrm1641, XStx1733, XFormat).
'lo.comp.lexer@Disj87'(XDjStrm87, XDjStrm87, XStx1733, XFormat, XNStrm1641, XNStrm1641, XDjStrm87):- XFormat = 'lo.core#[]'.
'lo.comp.lexer^interpolation'('_call%3'(XV30142, XV30143, XV30144), 'lo.comp.lexer^interpolation', _):- 'lo.comp.lexer@interpolation'(XV30142, XV30143, XV30144).
'lo.comp.lexer@Hed108'(XHedStrm108, XNStrm1642, XNStrm1642, XHedStrm108):- ocall('_hdtl%3'(XHedStrm108, 34, XNStrm1642),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@Hed109'(XHedStrm109, XNStrm1644, XNStrm1644, XHedStrm109):- ocall('_hdtl%3'(XHedStrm109, 40, XNStrm1644),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@One34'(XOneStm34, XStx1736, XStx1736, XChars):- 'lo.comp.lexer@readStr'(XOneStm34, XStx1736, XChars),
    !.
'lo.comp.lexer^readStringSegments'('_call%3'(XV30145, XV30146, XV30147), 'lo.comp.lexer^readStringSegments', _):- 'lo.comp.lexer@readStringSegments'(XV30145, XV30146, XV30147).
'lo.comp.lexer@Hed110'(XHedStrm110, XNStrm1645, XNStrm1645, XHedStrm110):- ocall('_hdtl%3'(XHedStrm110, 39, XNStrm1645),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@neg322'(XC):- XC = 39,
    !,
    fail.
'lo.comp.lexer@neg322'(XC).
'lo.comp.lexer^readQuoted'('_call%3'(XV30148, XV30149, XV30150), 'lo.comp.lexer^readQuoted', _):- 'lo.comp.lexer@readQuoted'(XV30148, XV30149, XV30150).
'lo.comp.lexer^idStart'('_call%3'(XV30151, XV30152, XV30153), 'lo.comp.lexer^idStart', _):- 'lo.comp.lexer@idStart'(XV30151, XV30152, XV30153).
'lo.comp.lexer@or188'(XCh):- '_isPcChar'(XCh).
'lo.comp.lexer@or188'(XCh):- '_isCfChar'(XCh).
'lo.comp.lexer@or189'(XCh):- '_isMcChar'(XCh).
'lo.comp.lexer@or189'(XCh):- 'lo.comp.lexer@or188'(XCh).
'lo.comp.lexer@or190'(XCh):- '_isMnChar'(XCh).
'lo.comp.lexer@or190'(XCh):- 'lo.comp.lexer@or189'(XCh).
'lo.comp.lexer@or191'(XCh):- '_isNdChar'(XCh).
'lo.comp.lexer@or191'(XCh):- 'lo.comp.lexer@or190'(XCh).
'lo.comp.lexer@One35'(XOneStm35, XOneStm35, XCh):- 'lo.comp.lexer@or191'(XCh),
    !.
'lo.comp.lexer^alphaNum'('_call%3'(XV30154, XV30155, XV30156), 'lo.comp.lexer^alphaNum', _):- 'lo.comp.lexer@alphaNum'(XV30154, XV30155, XV30156).
'lo.comp.lexer^readIden'('_call%3'(XV30157, XV30158, XV30159), 'lo.comp.lexer^readIden', _):- 'lo.comp.lexer@readIden'(XV30157, XV30158, XV30159).
'lo.comp.lexer@Hed111'(XHedStrm111, XNStrm1649, XNStrm1649, XHedStrm111):- ocall('_hdtl%3'(XHedStrm111, 96, XNStrm1649),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^regExp'('_call%3'(XV30160, XV30161, XV30162), 'lo.comp.lexer^regExp', _):- 'lo.comp.lexer@regExp'(XV30160, XV30161, XV30162).
'lo.comp.lexer@Neg52'(XNegStrm52, XStx1747, X_33476):- 'lo.comp.lexer@digit'(XNegStrm52, XStx1747, X_33476),
    !,
    fail.
'lo.comp.lexer@Neg52'(XNegStrm52, XStx1747, X_33476).
'lo.comp.lexer^readNatural'('_call%4'(XV30163, XV30164, XV30165, XV30166), 'lo.comp.lexer^readNatural', _):- 'lo.comp.lexer@readNatural'(XV30163, XV30164, XV30165, XV30166).
'lo.comp.lexer^fraction'('_call%5'(XV30167, XV30168, XV30169, XV30170, XV30171), 'lo.comp.lexer^fraction', _):- 'lo.comp.lexer@fraction'(XV30167, XV30168, XV30169, XV30170, XV30171).
'lo.comp.lexer^readDecimal'('_call%3'(XV30172, XV30173, XV30174), 'lo.comp.lexer^readDecimal', _):- 'lo.comp.lexer@readDecimal'(XV30172, XV30173, XV30174).
'lo.comp.lexer@Disj88'(XDjStrm88, XNStrm1654, XNStrm1655, XNStrm1655, XNStrm1654, XNStrm1654, XDjStrm88):- ocall('_hdtl%3'(XDjStrm88, 101, XNStrm1654),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@Disj88'(XDjStrm88, XNStrm1655, XNStrm1655, XNStrm1655, XNStrm1654, XNStrm1654, XDjStrm88):- ocall('_hdtl%3'(XDjStrm88, 69, XNStrm1655),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^exponent'('_call%4'(XV30175, XV30176, XV30177, XV30178), 'lo.comp.lexer^exponent', _):- 'lo.comp.lexer@exponent'(XV30175, XV30176, XV30177, XV30178).
'lo.comp.lexer@Hed112'(XHedStrm112, XStx1753, X_33480):- 'lo.comp.lexer@digit'(XHedStrm112, XStx1753, X_33480).
'lo.comp.lexer^readMoreNumber'('_call%4'(XV30179, XV30180, XV30181, XV30182), 'lo.comp.lexer^readMoreNumber', _):- 'lo.comp.lexer@readMoreNumber'(XV30179, XV30180, XV30181, XV30182).
'lo.comp.lexer^readNumber'('_call%3'(XV30183, XV30184, XV30185), 'lo.comp.lexer^readNumber', _):- 'lo.comp.lexer@readNumber'(XV30183, XV30184, XV30185).
'lo.comp.lexer@Neg53'(XNegStrm53, XStx1760, X_33481):- 'lo.comp.lexer@hexDigit'(XNegStrm53, XStx1760, X_33481),
    !,
    fail.
'lo.comp.lexer@Neg53'(XNegStrm53, XStx1760, X_33481).
'lo.comp.lexer^hexInt'('_call%4'(XV30186, XV30187, XV30188, XV30189), 'lo.comp.lexer^hexInt', _):- 'lo.comp.lexer@hexInt'(XV30186, XV30187, XV30188, XV30189).
'lo.comp.lexer@Hed113'(XHedStrm113, XStx1763, X_33482):- 'lo.comp.lexer@digit'(XHedStrm113, XStx1763, X_33482).
'lo.comp.lexer@One36'(XOneStm36, XStx1771, XStx1771, XOp, XNext):- 'lo.comp.lexer@followGraph'(XOneStm36, XStx1771, XNext, XOp),
    !.
'lo.comp.lexer^nxtTok'('_call%3'(XV30190, XV30191, XV30192), 'lo.comp.lexer^nxtTok', _):- 'lo.comp.lexer@nxtTok'(XV30190, XV30191, XV30192).
'lo.comp.lexer@One37'(XOneStm37, XStx1772, XStx1772, XTk):- 'lo.comp.lexer@nxtTok'(XOneStm37, XStx1772, XTk),
    !.
'lo.comp.lexer^allTokens'('_call%4'(XV30193, XV30194, XV30195, XV30196), 'lo.comp.lexer^allTokens', _):- 'lo.comp.lexer@allTokens'(XV30193, XV30194, XV30195, XV30196).
'lo.comp.lexer^tokenize'('_call%3'(XV30197, XV30198, XV30199), 'lo.comp.lexer^tokenize', _):- 'lo.comp.lexer@tokenize'(XV30197, XV30198, XV30199).
'lo.comp.lexer#tkState^currentLocation'('_call%3'(XV30200, XV30201, XV30202), 'lo.comp.lexer#tkState^currentLocation'(XLbV2500, XThV2500), _):- 'lo.comp.lexer#tkState@currentLocation'(XV30200, XV30201, XV30202, XLbV2500, XThV2500).
'lo.comp.lexer^startState'('_call%3'(XV30204, XV30205, XV30206), 'lo.comp.lexer^startState', _):- 'lo.comp.lexer@startState'(XV30204, XV30205, XV30206).
'lo.comp.lexer^tokenizeFile'('_call%2'(XV30207, XV30208), 'lo.comp.lexer^tokenizeFile', _):- 'lo.comp.lexer@tokenizeFile'(XV30207, XV30208).
'lo.comp.lexer^tokenCodes'('_call%3'(XV30209, XV30210, XV30211), 'lo.comp.lexer^tokenCodes', _):- 'lo.comp.lexer@tokenCodes'(XV30209, XV30210, XV30211).
'lo.comp.lexer^getNTokens'('_call%4'(XV30212, XV30213, XV30214, XV30215), 'lo.comp.lexer^getNTokens', _):- 'lo.comp.lexer@getNTokens'(XV30212, XV30213, XV30214, XV30215).
'lo.core$stream$lo.comp.lexer*tokenState^_eof'('_call%3'(XV30216, XV30217, XV30218), 'lo.core$stream$lo.comp.lexer*tokenState^_eof'(XLbV2501, XThV2501), _):- 'lo.core$stream$lo.comp.lexer*tokenState@_eof'(XV30216, XV30217, XV30218, XLbV2501, XThV2501).
'lo.core$stream$lo.comp.lexer*tokenState@neg323'(XCh):- XCh = 10,
    !,
    fail.
'lo.core$stream$lo.comp.lexer*tokenState@neg323'(XCh).
'lo.core$stream$lo.comp.lexer*tokenState^_hdtl'('_call%5'(XV30220, XV30221, XV30222, XV30223, XV30224), 'lo.core$stream$lo.comp.lexer*tokenState^_hdtl'(XLbV2501, XThV2501), _):- 'lo.core$stream$lo.comp.lexer*tokenState@_hdtl'(XV30220, XV30221, XV30222, XV30223, XV30224, XLbV2501, XThV2501).
'lo.comp.lexer^subTokenize'('_call%3'(XV30228, XV30229, XV30230), 'lo.comp.lexer^subTokenize', _):- 'lo.comp.lexer@subTokenize'(XV30228, XV30229, XV30230).
