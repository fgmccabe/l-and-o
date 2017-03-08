'#pkg'("n7o7'()7'n2o2'pkg's'lo.array's'1.0.0'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'private'n2o2'pkg's'lo.index'e'*'n2o2'import'e'private'n2o2'pkg's'lo.list'e'*'n2o2'import'e'private'n2o2'pkg's'lo.coerce'e'*'s\"I2'emptyArray':k't'Uz1'lo.array*array'1k't''displayArray':k'e'|FT1Uz1'lo.array*array'1k'e't'lo.core*ss'c'lo.core$display'T1k'e'T0\"s\"I1'array':k'e':k'e'YUz1'lo.array*array'1k'e'I0\"n0o0'()0'n0o0'()0'n4o4'()4'n2o2'()2's'lo.collection$map$lo.array*array's\":k'e'c'lo.collection$map'T1Uz1'lo.array*array'1k'e'T2ik'e'\"n2o2'()2's'lo.core$sizeable$lo.array*array's\":k'c'c'lo.core$sizeable'T1Uz1'lo.array*array'1k'c'T0\"n2o2'()2's'lo.core$display$lo.array*array's\":k'e'|c'lo.core$display'T1Uz1'lo.array*array'1k'e'T0c'lo.core$display'T1k'e'T0\"n2o2'()2's'lo.coerce$coercion$lo.array*array$lo.core*list's\":k'e'c'lo.coerce$coercion'T2Uz1'lo.array*array'1k'e'Lk'e'T0\"").
'lo.array@init'():- !.
'lo.array#array'('array%1'('lo.array@array'())):- !.
'lo.array@emptyArray'('lo.array#array'(XXV3335)):- !,
    ocall('_empty%1'(XXV3335),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')).
'lo.array@arrayValues'('lo.array#array'(XEls), XXe3121):- !,
    ocall('values%1'(XXV3336),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')),
    ocall('_call%2'(XEls, XXe3121),XXV3336,XXV3336).
'lo.array@arrayValues'(_, _):- raise_exception('error'("lo.array@arrayValues", 39, 3, 38)).
'lo.array@arrayPairs'('lo.array#array'(XEls), XXe3122):- !,
    ocall('pairs%1'(XXV3337),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')),
    ocall('_call%2'(XEls, XXe3122),XXV3337,XXV3337).
'lo.array@arrayPairs'(_, _):- raise_exception('error'("lo.array@arrayPairs", 36, 3, 36)).
'lo.array@arrayKeys'('lo.array#array'(XEls), XXe3123):- !,
    ocall('keys%1'(XXV3338),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')),
    ocall('_call%2'(XEls, XXe3123),XXV3338,XXV3338).
'lo.array@arrayKeys'(_, _):- raise_exception('error'("lo.array@arrayKeys", 33, 3, 34)).
'lo.array@addElement'('lo.array#array'(XEls), XIx, XEl, 'lo.array#array'(XXe3124)):- !,
    ocall('_put%1'(XXV3339),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')),
    ocall('_call%4'(XEls, XIx, XEl, XXe3124),XXV3339,XXV3339).
'lo.array@addElement'(_, _, _, _):- raise_exception('error'("lo.array@addElement", 30, 3, 54)).
'lo.array@removeElement'('lo.array#array'(XEls), XIx, 'lo.array#array'(XXe3125)):- !,
    ocall('_remove%1'(XXV3340),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')),
    ocall('_call%3'(XEls, XIx, XXe3125),XXV3340,XXV3340).
'lo.array@removeElement'(_, _, _):- raise_exception('error'("lo.array@removeElement", 27, 3, 54)).
'lo.array@arrayPresent'('lo.array#array'(XEls), XIx, XEl):- ocall('present%3'(XEls, XIx, XEl),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')).
'lo.collection$map$lo.array*array'('lo.collection$map$lo.array*array%1'('lo.collection$map$lo.array*array')):- !.
'lo.collection$map$lo.array*array'('present%3'(XV20353, XV20354, XV20355), XLbl1789, XThis1789):- !,
    'lo.collection$map$lo.array*array@present'(XV20353, XV20354, XV20355, XLbl1789, XThis1789).
'lo.collection$map$lo.array*array'('present%1'('lo.collection$map$lo.array*array^present'(XLbl1790, XThis1790)), XLbl1790, XThis1790).
'lo.collection$map$lo.array*array'('_remove%3'(XV20359, XV20360, XV20361), XLbl1791, XThis1791):- !,
    'lo.collection$map$lo.array*array@_remove'(XV20359, XV20360, XV20361, XLbl1791, XThis1791).
'lo.collection$map$lo.array*array'('_remove%1'('lo.collection$map$lo.array*array^_remove'(XLbl1792, XThis1792)), XLbl1792, XThis1792).
'lo.collection$map$lo.array*array'('_put%4'(XV20366, XV20367, XV20368, XV20369), XLbl1793, XThis1793):- !,
    'lo.collection$map$lo.array*array@_put'(XV20366, XV20367, XV20368, XV20369, XLbl1793, XThis1793).
'lo.collection$map$lo.array*array'('_put%1'('lo.collection$map$lo.array*array^_put'(XLbl1794, XThis1794)), XLbl1794, XThis1794).
'lo.collection$map$lo.array*array'('keys%2'(XV20372, XV20373), XLbl1795, XThis1795):- !,
    'lo.collection$map$lo.array*array@keys'(XV20372, XV20373, XLbl1795, XThis1795).
'lo.collection$map$lo.array*array'('keys%1'('lo.collection$map$lo.array*array^keys'(XLbl1796, XThis1796)), XLbl1796, XThis1796).
'lo.collection$map$lo.array*array'('pairs%2'(XV20376, XV20377), XLbl1797, XThis1797):- !,
    'lo.collection$map$lo.array*array@pairs'(XV20376, XV20377, XLbl1797, XThis1797).
'lo.collection$map$lo.array*array'('pairs%1'('lo.collection$map$lo.array*array^pairs'(XLbl1798, XThis1798)), XLbl1798, XThis1798).
'lo.collection$map$lo.array*array'('values%2'(XV20380, XV20381), XLbl1799, XThis1799):- !,
    'lo.collection$map$lo.array*array@values'(XV20380, XV20381, XLbl1799, XThis1799).
'lo.collection$map$lo.array*array'('values%1'('lo.collection$map$lo.array*array^values'(XLbl1800, XThis1800)), XLbl1800, XThis1800).
'lo.collection$map$lo.array*array'('_empty%1'(XV20382), XLbl1801, XThis1801):- !,
    'lo.collection$map$lo.array*array@_empty'(XV20382, XLbl1801, XThis1801).
'lo.collection$map$lo.array*array@present'(XA, XIx, XV, XLbV1759, XThV1759):- 'lo.array@arrayPresent'(XA, XIx, XV).
'lo.collection$map$lo.array*array@_remove'(XA, XIx, XXd26441, XLbV1759, XThV1759):- !,
    'lo.array@removeElement'(XA, Xix, XXd26441).
'lo.collection$map$lo.array*array@_remove'(_, _, _):- raise_exception('error'("lo.collection$map$lo.array*array@_remove", 15, 5, 36)).
'lo.collection$map$lo.array*array@_put'(XA, XIx, XV, XXd26442, XLbV1759, XThV1759):- !,
    'lo.array@addElement'(XA, XIx, XV, XXd26442).
'lo.collection$map$lo.array*array@_put'(_, _, _, _):- raise_exception('error'("lo.collection$map$lo.array*array@_put", 16, 5, 34)).
'lo.collection$map$lo.array*array@keys'(XA, XXd26443, XLbV1759, XThV1759):- !,
    'lo.array@arrayKeys'(XA, XXd26443).
'lo.collection$map$lo.array*array@keys'(_, _):- raise_exception('error'("lo.collection$map$lo.array*array@keys", 17, 5, 23)).
'lo.collection$map$lo.array*array@pairs'(XA, XXd26444, XLbV1759, XThV1759):- !,
    'lo.array@arrayPairs'(XA, XXd26444).
'lo.collection$map$lo.array*array@pairs'(_, _):- raise_exception('error'("lo.collection$map$lo.array*array@pairs", 18, 5, 25)).
'lo.collection$map$lo.array*array@values'(XA, XXd26445, XLbV1759, XThV1759):- !,
    'lo.array@arrayValues'(XA, XXd26445).
'lo.collection$map$lo.array*array@values'(_, _):- raise_exception('error'("lo.collection$map$lo.array*array@values", 19, 5, 27)).
'lo.collection$map$lo.array*array@_empty'(XemptyArray4, XLbV1759, XThV1759):- !,
    'lo.array@emptyArray'(XemptyArray4).
'lo.array@arrayIsEmpty'('lo.array#array'(XEls)):- ocall('isEmpty%1'(XEls),'lo.core$sizeable$lo.index*map','lo.core$sizeable$lo.index*map').
'lo.array@arraySize'('lo.array#array'(XEls), XXe3126):- !,
    ocall('size%1'(XXV3341),'lo.core$sizeable$lo.index*map','lo.core$sizeable$lo.index*map'),
    ocall('_call%2'(XEls, XXe3126),XXV3341,XXV3341).
'lo.array@arraySize'(_, _):- raise_exception('error'("lo.array@arraySize", 47, 3, 34)).
'lo.core$sizeable$lo.array*array'('lo.core$sizeable$lo.array*array%1'('lo.core$sizeable$lo.array*array')):- !.
'lo.core$sizeable$lo.array*array'('size%2'(XV20388, XV20389), XLbl1802, XThis1802):- !,
    'lo.core$sizeable$lo.array*array@size'(XV20388, XV20389, XLbl1802, XThis1802).
'lo.core$sizeable$lo.array*array'('size%1'('lo.core$sizeable$lo.array*array^size'(XLbl1803, XThis1803)), XLbl1803, XThis1803).
'lo.core$sizeable$lo.array*array'('isEmpty%1'(XV20393), XLbl1804, XThis1804):- !,
    'lo.core$sizeable$lo.array*array@isEmpty'(XV20393, XLbl1804, XThis1804).
'lo.core$sizeable$lo.array*array'('isEmpty%1'('lo.core$sizeable$lo.array*array^isEmpty'(XLbl1805, XThis1805)), XLbl1805, XThis1805).
'lo.core$sizeable$lo.array*array@size'(XA, XXd26446, XLbV1760, XThV1760):- !,
    'lo.array@arraySize'(XA, XXd26446).
'lo.core$sizeable$lo.array*array@size'(_, _):- raise_exception('error'("lo.core$sizeable$lo.array*array@size", 42, 5, 23)).
'lo.core$sizeable$lo.array*array@isEmpty'(XA, XLbV1760, XThV1760):- 'lo.array@arrayIsEmpty'(XA).
'lo.array@show2nd'(Xdisplay102, '()2'(X_21463, Xv), XXe3127):- !,
    ocall('disp%1'(XXV3342),Xdisplay102,Xdisplay102),
    ocall('_call%2'(Xv, XXe3127),XXV3342,XXV3342).
'lo.array@show2nd'(_, _, _):- raise_exception('error'("lo.array@show2nd", 63, 3, 25)).
'lo.array@displayArray'(Xdisplay103, 'lo.array#array'(XEls), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("[|"), 'lo.core#,..'('lo.core#ssSeq'(XXe3129), 'lo.core#,..'('lo.core#ss'("|]"), 'lo.core#[]'))))):- !,
    ocall('pairs%1'(XXV3343),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')),
    ocall('//%1'(XXV3344),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%2'(XEls, XXe3128),XXV3343,XXV3343),
    'lo.array@show2nd'(Xdisplay103, XXd26449),
    ocall('_call%3'(XXe3128, XXd26449, XXe3129),XXV3344,XXV3344).
'lo.array@displayArray'(_, _, _):- raise_exception('error'("lo.array@displayArray", 60, 3, 81)).
'lo.core$display$lo.array*array'('lo.core$display$lo.array*array%1'('lo.core$display$lo.array*array')):- !.
'lo.core$display$lo.array*array'('disp%2'(XV20402, XV20403), XLbl1806, XThis1806):- !,
    'lo.core$display$lo.array*array@disp'(XV20402, XV20403, XLbl1806, XThis1806).
'lo.core$display$lo.array*array'('disp%1'('lo.core$display$lo.array*array^disp'(XLbl1807, XThis1807)), XLbl1807, XThis1807).
'lo.core$display$lo.array*array@disp'(XA, XXd26456, XLbV1761, XThV1761):- XLbV1761 = 'lo.core$display$lo.array*array'(Xdisplay104),
    !,
    'lo.array@displayArray'(Xdisplay104, XA, XXd26456).
'lo.core$display$lo.array*array@disp'(_, _):- raise_exception('error'("lo.core$display$lo.array*array@disp", 56, 5, 26)).
'lo.coerce$coercion$lo.array*array$lo.core*list'('lo.coerce$coercion$lo.array*array$lo.core*list%1'('lo.coerce$coercion$lo.array*array$lo.core*list')):- !.
'lo.coerce$coercion$lo.array*array$lo.core*list'('_coerce%2'(XV20406, XV20407), XLbl1808, XThis1808):- !,
    'lo.coerce$coercion$lo.array*array$lo.core*list@_coerce'(XV20406, XV20407, XLbl1808, XThis1808).
'lo.coerce$coercion$lo.array*array$lo.core*list'('_coerce%1'('lo.coerce$coercion$lo.array*array$lo.core*list^_coerce'(XLbl1809, XThis1809)), XLbl1809, XThis1809).
'lo.coerce$coercion$lo.array*array$lo.core*list@_coerce'('lo.array#array'(XEls), XXe3130, XLbV1762, XThV1762):- !,
    ocall('values%1'(XXV3345),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*integer')),
    ocall('_call%2'(XEls, XXe3130),XXV3345,XXV3345).
'lo.coerce$coercion$lo.array*array$lo.core*list@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.array*array$lo.core*list@_coerce", 66, 5, 34)).
'lo.array^arrayValues'('_call%2'(XV20332, XV20333), 'lo.array^arrayValues', _):- 'lo.array@arrayValues'(XV20332, XV20333).
'lo.array^arrayPairs'('_call%2'(XV20334, XV20335), 'lo.array^arrayPairs', _):- 'lo.array@arrayPairs'(XV20334, XV20335).
'lo.array^arrayKeys'('_call%2'(XV20336, XV20337), 'lo.array^arrayKeys', _):- 'lo.array@arrayKeys'(XV20336, XV20337).
'lo.array^addElement'('_call%4'(XV20338, XV20339, XV20340, XV20341), 'lo.array^addElement', _):- 'lo.array@addElement'(XV20338, XV20339, XV20340, XV20341).
'lo.array^removeElement'('_call%3'(XV20342, XV20343, XV20344), 'lo.array^removeElement', _):- 'lo.array@removeElement'(XV20342, XV20343, XV20344).
'lo.array^arrayPresent'('_call%3'(XV20345, XV20346, XV20347), 'lo.array^arrayPresent', _):- 'lo.array@arrayPresent'(XV20345, XV20346, XV20347).
'lo.collection$map$lo.array*array^present'('_call%5'(XV20348, XV20349, XV20350, XV20351, XV20352), 'lo.collection$map$lo.array*array^present'(XLbV1759, XThV1759), _):- 'lo.collection$map$lo.array*array@present'(XV20348, XV20349, XV20350, XV20351, XV20352, XLbV1759, XThV1759).
'lo.collection$map$lo.array*array^_remove'('_call%3'(XV20356, XV20357, XV20358), 'lo.collection$map$lo.array*array^_remove'(XLbV1759, XThV1759), _):- 'lo.collection$map$lo.array*array@_remove'(XV20356, XV20357, XV20358, XLbV1759, XThV1759).
'lo.collection$map$lo.array*array^_put'('_call%4'(XV20362, XV20363, XV20364, XV20365), 'lo.collection$map$lo.array*array^_put'(XLbV1759, XThV1759), _):- 'lo.collection$map$lo.array*array@_put'(XV20362, XV20363, XV20364, XV20365, XLbV1759, XThV1759).
'lo.collection$map$lo.array*array^keys'('_call%2'(XV20370, XV20371), 'lo.collection$map$lo.array*array^keys'(XLbV1759, XThV1759), _):- 'lo.collection$map$lo.array*array@keys'(XV20370, XV20371, XLbV1759, XThV1759).
'lo.collection$map$lo.array*array^pairs'('_call%2'(XV20374, XV20375), 'lo.collection$map$lo.array*array^pairs'(XLbV1759, XThV1759), _):- 'lo.collection$map$lo.array*array@pairs'(XV20374, XV20375, XLbV1759, XThV1759).
'lo.collection$map$lo.array*array^values'('_call%2'(XV20378, XV20379), 'lo.collection$map$lo.array*array^values'(XLbV1759, XThV1759), _):- 'lo.collection$map$lo.array*array@values'(XV20378, XV20379, XLbV1759, XThV1759).
'lo.array^arrayIsEmpty'('_call%1'(XV20383), 'lo.array^arrayIsEmpty', _):- 'lo.array@arrayIsEmpty'(XV20383).
'lo.array^arraySize'('_call%2'(XV20384, XV20385), 'lo.array^arraySize', _):- 'lo.array@arraySize'(XV20384, XV20385).
'lo.core$sizeable$lo.array*array^size'('_call%2'(XV20386, XV20387), 'lo.core$sizeable$lo.array*array^size'(XLbV1760, XThV1760), _):- 'lo.core$sizeable$lo.array*array@size'(XV20386, XV20387, XLbV1760, XThV1760).
'lo.core$sizeable$lo.array*array^isEmpty'('_call%3'(XV20390, XV20391, XV20392), 'lo.core$sizeable$lo.array*array^isEmpty'(XLbV1760, XThV1760), _):- 'lo.core$sizeable$lo.array*array@isEmpty'(XV20390, XV20391, XV20392, XLbV1760, XThV1760).
'lo.array^show2nd'('_call%3'(XV20394, XV20395, XV20396), 'lo.array^show2nd', _):- 'lo.array@show2nd'(XV20394, XV20395, XV20396).
'lo.array^displayArray'('_call%3'(XV20397, XV20398, XV20399), 'lo.array^displayArray', _):- 'lo.array@displayArray'(XV20397, XV20398, XV20399).
'lo.core$display$lo.array*array^disp'('_call%2'(XV20400, XV20401), 'lo.core$display$lo.array*array^disp'(XLbV1761, XThV1761), _):- 'lo.core$display$lo.array*array@disp'(XV20400, XV20401, XLbV1761, XThV1761).
'lo.coerce$coercion$lo.array*array$lo.core*list^_coerce'('_call%2'(XV20404, XV20405), 'lo.coerce$coercion$lo.array*array$lo.core*list^_coerce'(XLbV1762, XThV1762), _):- 'lo.coerce$coercion$lo.array*array$lo.core*list@_coerce'(XV20404, XV20405, XLbV1762, XThV1762).
