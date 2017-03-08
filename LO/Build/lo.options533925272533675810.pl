'#pkg'("n7o7'()7'n2o2'pkg's'lo.options's'1.0.0'n2o2'()2'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.either'e'*'s\"I1'processOptions':k'o'FT3LSLUz1'lo.options*optionsProcessor'1k'o'k'o'Uz2'lo.either*either'2T2k'o'LSS\"s\"I1'optionsProcessor':k'o':k'o'YUz1'lo.options*optionsProcessor'1k'o'I5'shortForm'S'alternatives'LS'usage'S'validator'Uz1'lo.core*option'1PT1S'setOption'FT2Sk'o'k'o'\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.options@init'():- !.
'lo.options@collectUsage'(XSpecs, XXd8817):- !,
    ocall('//%1'(XXV1903),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('//%1'(XXV1904),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%3'(XSpecs, 'lo.options@fun84', XXe1885),XXV1903,XXV1903),
    'lo.list@interleave'(XXe1885, "
", XXd8813),
    ocall('_call%3'(XXd8813, 'lo.options@fun85', XXe1886),XXV1904,XXV1904),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Usage: 
"), XXe1886)), XXd8817).
'lo.options@collectUsage'(_, _):- raise_exception('error'("lo.options@collectUsage", 36, 3, 112)).
'lo.options@checkOption'(XArgs, XO, 'lo.core#none', Xsetter, XSpecs, XSoFar, XXd8819):- !,
    ocall('_call%3'(XO, XSoFar, XXe1887),Xsetter,Xsetter),
    'lo.options@processAll'(XArgs, XSpecs, XXe1887, XXd8819).
'lo.options@checkOption'('lo.core#,..'(XA, XArgs), XO, 'lo.core#some'(XV), XSetter, XSpecs, XSoFar, XXd8821):- ocall('_call%1'(XA),XV,XV),
    !,
    ocall('_call%3'(XA, XSoFar, XXe1888),XSetter,XSetter),
    'lo.options@processAll'(XArgs, XSpecs, XXe1888, XXd8821).
'lo.options@checkOption'(X_5421, X_5422, X_5423, X_5424, XSpecs, X_5425, 'lo.either#alternate'(XXd8822)):- !,
    'lo.options@collectUsage'(XSpecs, XXd8822).
'lo.options@checkOption'(_, _, _, _, _, _, _):- raise_exception('error'("lo.options@checkOption", 30, 3, 85)).
'lo.options@processOption'(XA, XL, XSpecs, XSoFar, XXd8824):- 'lo.options@one64'(XXV1906, XXV1905, XA, XSpecs, XO),
    !,
    ocall('shortForm%1'(XXV1907),XO,XO),
    ocall('validator%1'(XXV1908),XO,XO),
    ocall('setOption%1'(XXV1909),XO,XO),
    'lo.options@checkOption'(XL, XXV1907, XXV1908, XXV1909, XSpecs, XSoFar, XXd8824).
'lo.options@processOption'(XA, XL, X_5426, XSoFar, 'lo.either#either'('()2'(XSoFar, 'lo.core#,..'(XA, XL)))):- !.
'lo.options@processOption'(_, _, _, _, _):- raise_exception('error'("lo.options@processOption", 24, 3, 162)).
'lo.options@processAll'('lo.core#[]', X_5428, XSoFar, 'lo.either#either'('()2'(XSoFar, 'lo.core#[]'))):- !.
'lo.options@processAll'('lo.core#,..'("--", XA), X_5430, XSoFar, 'lo.either#either'('()2'(XSoFar, XA))):- !.
'lo.options@processAll'('lo.core#,..'(XA, XL), XSpecs, XSoFar, XXd8829):- !,
    'lo.options@processOption'(XA, XL, XSpecs, XSoFar, XXd8829).
'lo.options@processAll'(_, _, _, _):- raise_exception('error'("lo.options@processAll", 19, 3, 44)).
'lo.options@processOptions'(XRaw, XSpecs, XOpts, XXd8830):- !,
    'lo.options@processAll'(XRaw, XSpecs, XOpts, XXd8830).
'lo.options@processOptions'(_, _, _, _):- raise_exception('error'("lo.options@processOptions", 16, 3, 60)).
'lo.options@fun84'('_call%2'(XO, XXV1902), 'lo.options@fun84', _):- !,
    ocall('usage%1'(XXV1902),XO,XO).
'lo.options@fun84'(_, _, _):- raise_exception('error'("lo.options@fun84", 36, 79, 12)).
'lo.options@fun85'('_call%2'(XX, 'lo.core#ss'(XX)), 'lo.options@fun85', _):- !.
'lo.options@fun85'(_, _, _):- raise_exception('error'("lo.options@fun85", 36, 101, 10)).
'lo.options^collectUsage'('_call%2'(XV18284, XV18285), 'lo.options^collectUsage', _):- 'lo.options@collectUsage'(XV18284, XV18285).
'lo.options^checkOption'('_call%7'(XV18286, XV18287, XV18288, XV18289, XV18290, XV18291, XV18292), 'lo.options^checkOption', _):- 'lo.options@checkOption'(XV18286, XV18287, XV18288, XV18289, XV18290, XV18291, XV18292).
'lo.options@or17'(XXV1906, XXV1905, XO, XA):- ocall('shortForm%1'(XXV1905),XO,XO),
    ocall('==%2'(XA, XXV1905),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.options@or17'(XXV1906, XXV1905, XO, XA):- ocall('alternatives%1'(XXV1906),XO,XO),
    ocall('in%2'(XA, XXV1906),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.options@one64'(XXV1906, XXV1905, XA, XSpecs, XO):- 'lo.list@listEl'(XO, XSpecs),
    'lo.options@or17'(XXV1906, XXV1905, XO, XA),
    !.
'lo.options^processOption'('_call%5'(XV18293, XV18294, XV18295, XV18296, XV18297), 'lo.options^processOption', _):- 'lo.options@processOption'(XV18293, XV18294, XV18295, XV18296, XV18297).
'lo.options^processAll'('_call%4'(XV18298, XV18299, XV18300, XV18301), 'lo.options^processAll', _):- 'lo.options@processAll'(XV18298, XV18299, XV18300, XV18301).
'lo.options^processOptions'('_call%4'(XV18302, XV18303, XV18304, XV18305), 'lo.options^processOptions', _):- 'lo.options@processOptions'(XV18302, XV18303, XV18304, XV18305).
