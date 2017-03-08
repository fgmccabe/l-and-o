'#pkg'("n7o7'()7'n2o2'pkg's'test.format's'0.0.2'n2o2'()2'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.unit'e'*'s'I0's'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'test.format@show'():- ocall('frmt%1'(XXV2072),'lo.core$format$lo.core*float','lo.core$format$lo.core*float'),
    ocall('_call%3'(34.567, "99.99", XXe2050),XXV2072,XXV2072),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe2050, 'lo.core#[]')), XXd9591),
    '_logmsg'(XXd9591),
    ocall('frmt%1'(XXV2073),'lo.core$format$lo.core*float','lo.core$format$lo.core*float'),
    ocall('_call%3'(34.567, "00000.99", XXe2051),XXV2073,XXV2073),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe2051, 'lo.core#[]')), XXd9594),
    '_logmsg'(XXd9594),
    ocall('frmt%1'(XXV2074),'lo.core$format$lo.core*integer','lo.core$format$lo.core*integer'),
    ocall('_call%3'(-2345, "P999.99P", XXe2052),XXV2074,XXV2074),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe2052, 'lo.core#,..'('lo.core#ss'("%"), 'lo.core#[]'))), XXd9599),
    '_logmsg'(XXd9599),
    ocall('frmt%1'(XXV2075),'lo.core$format$lo.core*integer','lo.core$format$lo.core*integer'),
    ocall('_call%3'(2345, "P999.99P", XXe2053),XXV2075,XXV2075),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe2053, 'lo.core#,..'('lo.core#ss'("%"), 'lo.core#[]'))), XXd9604),
    '_logmsg'(XXd9604).
'test.format@init'():- 'test.format@show'(),
    !.
