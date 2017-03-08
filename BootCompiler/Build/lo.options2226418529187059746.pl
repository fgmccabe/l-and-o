'#pkg'("n7o7'()7'n2o2'pkg's'lo.options'e'*'n11o11'()11'n2o2'import'e'private'n2o2'pkg's'lo.either'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'processOptions':k'o'FT3LSLUz1'lo.options*optionsProcessor'1k'o'k'o'Uz2'lo.either*either'2T2k'o'LSS\"s\"I1'optionsProcessor':k'o'YUz1'lo.options*optionsProcessor'1k'o'I5'setOption'FT2Sk'o'k'o''validator'Uz1'lo.core*option'1PT1S'usage'S'alternatives'LS'shortForm'S\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.options@init'() :- !.
'lo.options@collectUsage'(XSpecs, XX6628) :- !,
    ocall('//%3'(XSpecs, 'lo.options@$5', XX6618),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo.list@interleave'(XX6618, "
", XX6620),
    ocall('//%3'(XX6620, 'lo.options@$6', XX6624),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Usage: 
"), XX6624)), XX6628).
'lo.options@collectUsage'(_, _) :- raise_exception('error'("collectUsage", 36, 3, 112)).
'lo.options@checkOption'(XArgs, XO, 'lo.core#none', Xsetter, XSpecs, XSoFar, XX6641) :- !,
    ocall('_call%3'(XO, XSoFar, XX6639),Xsetter,Xsetter),
    'lo.options@processAll'(XArgs, XSpecs, XX6639, XX6641).
'lo.options@checkOption'('lo.core#,..'(XA, XArgs), XO, 'lo.core#some'(XV), XSetter, XSpecs, XSoFar, XX6659) :- ocall('_call%1'(XA),XV,XV),
    !,
    ocall('_call%3'(XA, XSoFar, XX6657),XSetter,XSetter),
    'lo.options@processAll'(XArgs, XSpecs, XX6657, XX6659).
'lo.options@checkOption'(X_562, X_563, X_564, X_565, XSpecs, X_566, 'lo.either#alternate'(XX6667)) :- !,
    'lo.options@collectUsage'(XSpecs, XX6667).
'lo.options@checkOption'(_, _, _, _, _, _, _) :- raise_exception('error'("checkOption", 30, 3, 85)).
'lo.options@processOption'(XA, XL, XSpecs, XSoFar, XX6687) :- 'lo.options@one2'(XA, XXV10, XXV11, XSpecs, XO),
    !,
    ocall('shortForm%1'(XXV12),XO,XO),
    ocall('validator%1'(XXV13),XO,XO),
    ocall('setOption%1'(XXV14),XO,XO),
    'lo.options@checkOption'(XL, XXV12, XXV13, XXV14, XSpecs, XSoFar, XX6687).
'lo.options@processOption'(XA, XL, X_567, XSoFar, 'lo.either#either'((XSoFar, 'lo.core#,..'(XA, XL)))) :- !.
'lo.options@processOption'(_, _, _, _, _) :- raise_exception('error'("processOption", 24, 3, 162)).
'lo.options@processAll'('lo.core#[]', X_568, XSoFar, 'lo.either#either'((XSoFar, 'lo.core#[]'))) :- !.
'lo.options@processAll'('lo.core#,..'("--", XA), X_569, XSoFar, 'lo.either#either'((XSoFar, XA))) :- !.
'lo.options@processAll'('lo.core#,..'(XA, XL), XSpecs, XSoFar, XX6719) :- !,
    'lo.options@processOption'(XA, XL, XSpecs, XSoFar, XX6719).
'lo.options@processAll'(_, _, _, _) :- raise_exception('error'("processAll", 19, 3, 44)).
'lo.options@processOptions'(XRaw, XSpecs, XOpts, XX6726) :- !,
    'lo.options@processAll'(XRaw, XSpecs, XOpts, XX6726).
'lo.options@processOptions'(_, _, _, _) :- raise_exception('error'("processOptions", 16, 3, 60)).
'lo.options@$5'('_call%2'(XO, XXV9), 'lo.options@$5', _) :- !,
    ocall('usage%1'(XXV9),XO,XO).
'lo.options@$5'(_, _, _) :- raise_exception('error'("lambda", 36, 79, 12)).
'lo.options@$6'('_call%2'(XX, 'lo.core#ss'(XX)), 'lo.options@$6', _) :- !.
'lo.options@$6'(_, _, _) :- raise_exception('error'("lambda", 36, 101, 10)).
'lo.options^collectUsage'('_call%2'(XV1217, XV1218), 'lo.options^collectUsage', _) :- 'lo.options@collectUsage'(XV1217, XV1218).
'lo.options^checkOption'('_call%7'(XV1219, XV1220, XV1221, XV1222, XV1223, XV1224, XV1225), 'lo.options^checkOption', _) :- 'lo.options@checkOption'(XV1219, XV1220, XV1221, XV1222, XV1223, XV1224, XV1225).
'lo.options@or4'(XXV11, XXV10, XO, XA) :- ocall('shortForm%1'(XXV10),XO,XO),
    ocall('==%2'(XA, XXV10),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.options@or4'(XXV11, XXV10, XO, XA) :- ocall('alternatives%1'(XXV11),XO,XO),
    ocall('in%2'(XA, XXV11),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.options@one2'(XA, XXV10, XXV11, XSpecs, XO) :- 'lo.list@listEl'(XO, XSpecs),
    'lo.options@or4'(XXV11, XXV10, XO, XA),
    !.
'lo.options^processOption'('_call%5'(XV1226, XV1227, XV1228, XV1229, XV1230), 'lo.options^processOption', _) :- 'lo.options@processOption'(XV1226, XV1227, XV1228, XV1229, XV1230).
'lo.options^processAll'('_call%4'(XV1231, XV1232, XV1233, XV1234), 'lo.options^processAll', _) :- 'lo.options@processAll'(XV1231, XV1232, XV1233, XV1234).
'lo.options^processOptions'('_call%4'(XV1235, XV1236, XV1237, XV1238), 'lo.options^processOptions', _) :- 'lo.options@processOptions'(XV1235, XV1236, XV1237, XV1238).
