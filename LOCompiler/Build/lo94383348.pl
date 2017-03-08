'#pkg'("n7o7'()7'n2o2'pkg's'lo's'1.0.0'n9o9'()9'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'s\"I2'flatten'FT1t'lo.core*ss'LS'formatSS'FT1t'lo.core*ss'S\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo@init'():- !.
'lo@flttnList'('lo.core#[]', XRest, XRest):- !.
'lo@flttnList'('lo.core#,..'(XE, XR), XRest, XXd23216):- !,
    'lo@flttnList'(XR, XRest, XXd23215),
    'lo@flttn'(XE, XXd23215, XXd23216).
'lo@flttnList'(_, _, _):- raise_exception('error'("lo@flttnList", 21, 3, 26)).
'lo@flttn'('lo.core#ss'(XS), XRest, 'lo.core#,..'(XS, XRest)):- !.
'lo@flttn'('lo.core#ssSeq'(XL), XRest, XXd23218):- !,
    'lo@flttnList'(XL, XRest, XXd23218).
'lo@flttn'('lo.core#sc'(XC), XRest, 'lo.core#,..'(XXc396, XRest)):- !,
    'implode'('lo.core#,..'(XC, 'lo.core#[]'), XXc396).
'lo@flttn'(_, _, _):- raise_exception('error'("lo@flttn", 16, 3, 31)).
'lo@flatten'(XD, XXd23221):- !,
    'lo@flttn'(XD, 'lo.core#[]', XXd23221).
'lo@flatten'(_, _):- raise_exception('error'("lo@flatten", 13, 3, 25)).
'lo@formatSS'(XS, XXc397):- !,
    'lo@flatten'(XS, XXd23222),
    '_str_multicat'(XXd23222, XXc397).
'lo@formatSS'(_, _):- raise_exception('error'("lo@formatSS", 25, 3, 40)).
'lo^flttnList'('_call%3'(XV19563, XV19564, XV19565), 'lo^flttnList', _):- 'lo@flttnList'(XV19563, XV19564, XV19565).
'lo^flttn'('_call%3'(XV19566, XV19567, XV19568), 'lo^flttn', _):- 'lo@flttn'(XV19566, XV19567, XV19568).
'lo^flatten'('_call%2'(XV19569, XV19570), 'lo^flatten', _):- 'lo@flatten'(XV19569, XV19570).
'lo^formatSS'('_call%2'(XV19571, XV19572), 'lo^formatSS', _):- 'lo@formatSS'(XV19571, XV19572).
