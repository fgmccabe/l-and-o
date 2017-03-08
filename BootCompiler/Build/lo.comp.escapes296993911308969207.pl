'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.escapes'e'*'n11o11'()11'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I3'escapeType'FT1St'lo.comp.types*tipe''isEscape'PT1S'escCode'FT1Si\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.escapes@init'() :- !.
'lo.comp.escapes@escapeType'("_exit", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_command_line", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_command_opts", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_unify", 'lo.comp.types#univType'('lo.comp.types#kVar'("t"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#kVar'("t"), 'lo.core#,..'('lo.comp.types#kVar'("t"), 'lo.core#[]')))))) :- !.
'lo.comp.escapes@escapeType'("_identical", 'lo.comp.types#univType'('lo.comp.types#kVar'("t"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#kVar'("t"), 'lo.core#,..'('lo.comp.types#kVar'("t"), 'lo.core#[]')))))) :- !.
'lo.comp.escapes@escapeType'("var", 'lo.comp.types#univType'('lo.comp.types#kVar'("t"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#kVar'("t"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("ground", 'lo.comp.types#univType'('lo.comp.types#kVar'("t"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#kVar'("t"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_call", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.core#[]')))))) :- !.
'lo.comp.escapes@escapeType'("_defined", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_int_plus", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_int_minus", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_int_times", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_int_div", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_int_mod", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_flt_plus", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_flt_minus", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_flt_times", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_flt_div", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_flt_mod", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_int_abs", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_flt_abs", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_int_lt", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_int_ge", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_flt_lt", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_flt_ge", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_int2flt", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_flt2int", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_flt_hash", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_pwr", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("sqrt", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("exp", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("log", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("log10", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("pi", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("sin", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("cos", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("tan", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("asin", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("acos", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("atan", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("trunc", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("floor", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("ceil", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("integral", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("srand", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("rand", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("irand", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_ldexp", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_frexp", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))))) :- !.
'lo.comp.escapes@escapeType'("_modf", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')))))) :- !.
'lo.comp.escapes@escapeType'("_band", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_bor", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_bxor", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_blsl", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_blsr", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_basr", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_bnot", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_nthb", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_suspend", 'lo.comp.types#univType'('lo.comp.types#kVar'("u"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#kVar'("u"), 'lo.core#,..'('lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#[]')), 'lo.core#[]')))))) :- !.
'lo.comp.escapes@escapeType'("_get_file", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_put_file", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_cwd", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_cd", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_rm", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_mv", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_mkdir", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_rmdir", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isdir", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_chmod", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_ls", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_file_mode", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_file_present", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_file_type", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_file_size", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_file_modified", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_file_date", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))))) :- !.
'lo.comp.escapes@escapeType'("_openInFile", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.io*fileHandle"))) :- !.
'lo.comp.escapes@escapeType'("_openOutFile", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.io*fileHandle"))) :- !.
'lo.comp.escapes@escapeType'("_openAppendFile", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.io*fileHandle"))) :- !.
'lo.comp.escapes@escapeType'("_openAppendIOFile", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.io*fileHandle"))) :- !.
'lo.comp.escapes@escapeType'("_popen", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.core#,..'('lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.core#[]')), 'lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]'))))))))) :- !.
'lo.comp.escapes@escapeType'("_close", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_end_of_file", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_ready", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_inchars", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_inbytes", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_inchar", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_inbyte", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_inline", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_intext", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_outch", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_outbyte", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_outbytes", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_outtext", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_stdfile", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.io*fileHandle"))) :- !.
'lo.comp.escapes@escapeType'("_fposition", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_fseek", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_flush", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_flushall", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#[]'))) :- !.
'lo.comp.escapes@escapeType'("_setfileencoding", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_install_pkg", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_pkg_is_present", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))))) :- !.
'lo.comp.escapes@escapeType'("_logmsg", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_connect", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]')))))))) :- !.
'lo.comp.escapes@escapeType'("_listen", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_accept", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.io*fileHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))))))) :- !.
'lo.comp.escapes@escapeType'("_udpPort", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.io*udpHandle"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_udpGet", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*udpHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))))) :- !.
'lo.comp.escapes@escapeType'("_udpSend", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*udpHandle"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))))) :- !.
'lo.comp.escapes@escapeType'("_udpClose", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.io*udpHandle"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("hosttoip", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("iptohost", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("delay", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("sleep", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("now", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("today", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("ticks", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_time2date", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))))))))))) :- !.
'lo.comp.escapes@escapeType'("_time2utc", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))))))))))) :- !.
'lo.comp.escapes@escapeType'("_date2time", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))))))), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_utc2time", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))))))), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_isCcChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isCfChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isCnChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isCoChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isCsChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isLlChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isLmChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isLoChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isLtChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isLuChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isMcChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isMeChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isMnChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isNdChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isNlChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isNoChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isPcChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isPdChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isPeChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isPfChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isPiChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isPoChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isPsChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isScChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isSkChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isSmChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isSoChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isZlChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isZpChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isZsChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_isLetterChar", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_digitCode", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_int2str", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_flt2str", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*logical"), 'lo.core#[]')))))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_int_format", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_flt_format", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_str2flt", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*float"))) :- !.
'lo.comp.escapes@escapeType'("_str_lt", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_str_ge", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_str_hash", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_str_len", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_str_gen", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_stringOf", 'lo.comp.types#univType'('lo.comp.types#kVar'("t"), 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#kVar'("t"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))), 'lo.comp.types#tipe'("lo.core*string")))) :- !.
'lo.comp.escapes@escapeType'("_trim", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("explode", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("implode", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_str_find", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]'))))))) :- !.
'lo.comp.escapes@escapeType'("_sub_str", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#[]')))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_str_split", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*integer"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))))) :- !.
'lo.comp.escapes@escapeType'("_str_concat", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_str_start", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_str_multicat", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("getenv", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("setenv", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("envir", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("getlogin", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#tipe'("lo.core*string"))) :- !.
'lo.comp.escapes@escapeType'("_fork", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#[]')), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.thread*thread"))) :- !.
'lo.comp.escapes@escapeType'("_thread", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#tipe'("lo.thread*thread"))) :- !.
'lo.comp.escapes@escapeType'("kill", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.thread*thread"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("thread_state", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.thread*thread"), 'lo.core#[]')), 'lo.comp.types#tipe'("lo.thread*processState"))) :- !.
'lo.comp.escapes@escapeType'("waitfor", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.thread*thread"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_shell", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]')), 'lo.core#,..'('lo.comp.types#typeExp'('lo.comp.types#tpFun'("lo.core*list", 1), 'lo.core#,..'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*string"), 'lo.core#[]'))), 'lo.core#[]')), 'lo.core#[]')))), 'lo.comp.types#tipe'("lo.core*integer"))) :- !.
'lo.comp.escapes@escapeType'("_newLock", 'lo.comp.types#funType'('lo.comp.types#tupleType'('lo.core#[]'), 'lo.comp.types#tipe'("lo.thread*lock"))) :- !.
'lo.comp.escapes@escapeType'("_acquireLock", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.thread*lock"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_waitLock", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.thread*lock"), 'lo.core#,..'('lo.comp.types#tipe'("lo.core*float"), 'lo.core#[]'))))) :- !.
'lo.comp.escapes@escapeType'("_releaseLock", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'('lo.comp.types#tipe'("lo.thread*lock"), 'lo.core#[]')))) :- !.
'lo.comp.escapes@escapeType'("_ins_debug", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#[]'))) :- !.
'lo.comp.escapes@escapeType'("_stackTrace", 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#[]'))) :- !.
'lo.comp.escapes@escapeType'(_, _) :- raise_exception('error'("escapeType", 9, 3, 69)).
'lo.comp.escapes@isEscape'("_exit").
'lo.comp.escapes@isEscape'("_command_line").
'lo.comp.escapes@isEscape'("_command_opts").
'lo.comp.escapes@isEscape'("_unify").
'lo.comp.escapes@isEscape'("_identical").
'lo.comp.escapes@isEscape'("var").
'lo.comp.escapes@isEscape'("ground").
'lo.comp.escapes@isEscape'("_call").
'lo.comp.escapes@isEscape'("_defined").
'lo.comp.escapes@isEscape'("_int_plus").
'lo.comp.escapes@isEscape'("_int_minus").
'lo.comp.escapes@isEscape'("_int_times").
'lo.comp.escapes@isEscape'("_int_div").
'lo.comp.escapes@isEscape'("_int_mod").
'lo.comp.escapes@isEscape'("_flt_plus").
'lo.comp.escapes@isEscape'("_flt_minus").
'lo.comp.escapes@isEscape'("_flt_times").
'lo.comp.escapes@isEscape'("_flt_div").
'lo.comp.escapes@isEscape'("_flt_mod").
'lo.comp.escapes@isEscape'("_int_abs").
'lo.comp.escapes@isEscape'("_flt_abs").
'lo.comp.escapes@isEscape'("_int_lt").
'lo.comp.escapes@isEscape'("_int_ge").
'lo.comp.escapes@isEscape'("_flt_lt").
'lo.comp.escapes@isEscape'("_flt_ge").
'lo.comp.escapes@isEscape'("_int2flt").
'lo.comp.escapes@isEscape'("_flt2int").
'lo.comp.escapes@isEscape'("_flt_hash").
'lo.comp.escapes@isEscape'("_pwr").
'lo.comp.escapes@isEscape'("sqrt").
'lo.comp.escapes@isEscape'("exp").
'lo.comp.escapes@isEscape'("log").
'lo.comp.escapes@isEscape'("log10").
'lo.comp.escapes@isEscape'("pi").
'lo.comp.escapes@isEscape'("sin").
'lo.comp.escapes@isEscape'("cos").
'lo.comp.escapes@isEscape'("tan").
'lo.comp.escapes@isEscape'("asin").
'lo.comp.escapes@isEscape'("acos").
'lo.comp.escapes@isEscape'("atan").
'lo.comp.escapes@isEscape'("trunc").
'lo.comp.escapes@isEscape'("floor").
'lo.comp.escapes@isEscape'("ceil").
'lo.comp.escapes@isEscape'("integral").
'lo.comp.escapes@isEscape'("srand").
'lo.comp.escapes@isEscape'("rand").
'lo.comp.escapes@isEscape'("irand").
'lo.comp.escapes@isEscape'("_ldexp").
'lo.comp.escapes@isEscape'("_frexp").
'lo.comp.escapes@isEscape'("_modf").
'lo.comp.escapes@isEscape'("_band").
'lo.comp.escapes@isEscape'("_bor").
'lo.comp.escapes@isEscape'("_bxor").
'lo.comp.escapes@isEscape'("_blsl").
'lo.comp.escapes@isEscape'("_blsr").
'lo.comp.escapes@isEscape'("_basr").
'lo.comp.escapes@isEscape'("_bnot").
'lo.comp.escapes@isEscape'("_nthb").
'lo.comp.escapes@isEscape'("_suspend").
'lo.comp.escapes@isEscape'("_get_file").
'lo.comp.escapes@isEscape'("_put_file").
'lo.comp.escapes@isEscape'("_cwd").
'lo.comp.escapes@isEscape'("_cd").
'lo.comp.escapes@isEscape'("_rm").
'lo.comp.escapes@isEscape'("_mv").
'lo.comp.escapes@isEscape'("_mkdir").
'lo.comp.escapes@isEscape'("_rmdir").
'lo.comp.escapes@isEscape'("_isdir").
'lo.comp.escapes@isEscape'("_chmod").
'lo.comp.escapes@isEscape'("_ls").
'lo.comp.escapes@isEscape'("_file_mode").
'lo.comp.escapes@isEscape'("_file_present").
'lo.comp.escapes@isEscape'("_file_type").
'lo.comp.escapes@isEscape'("_file_size").
'lo.comp.escapes@isEscape'("_file_modified").
'lo.comp.escapes@isEscape'("_file_date").
'lo.comp.escapes@isEscape'("_openInFile").
'lo.comp.escapes@isEscape'("_openOutFile").
'lo.comp.escapes@isEscape'("_openAppendFile").
'lo.comp.escapes@isEscape'("_openAppendIOFile").
'lo.comp.escapes@isEscape'("_popen").
'lo.comp.escapes@isEscape'("_close").
'lo.comp.escapes@isEscape'("_end_of_file").
'lo.comp.escapes@isEscape'("_ready").
'lo.comp.escapes@isEscape'("_inchars").
'lo.comp.escapes@isEscape'("_inbytes").
'lo.comp.escapes@isEscape'("_inchar").
'lo.comp.escapes@isEscape'("_inbyte").
'lo.comp.escapes@isEscape'("_inline").
'lo.comp.escapes@isEscape'("_intext").
'lo.comp.escapes@isEscape'("_outch").
'lo.comp.escapes@isEscape'("_outbyte").
'lo.comp.escapes@isEscape'("_outbytes").
'lo.comp.escapes@isEscape'("_outtext").
'lo.comp.escapes@isEscape'("_stdfile").
'lo.comp.escapes@isEscape'("_fposition").
'lo.comp.escapes@isEscape'("_fseek").
'lo.comp.escapes@isEscape'("_flush").
'lo.comp.escapes@isEscape'("_flushall").
'lo.comp.escapes@isEscape'("_setfileencoding").
'lo.comp.escapes@isEscape'("_install_pkg").
'lo.comp.escapes@isEscape'("_pkg_is_present").
'lo.comp.escapes@isEscape'("_logmsg").
'lo.comp.escapes@isEscape'("_connect").
'lo.comp.escapes@isEscape'("_listen").
'lo.comp.escapes@isEscape'("_accept").
'lo.comp.escapes@isEscape'("_udpPort").
'lo.comp.escapes@isEscape'("_udpGet").
'lo.comp.escapes@isEscape'("_udpSend").
'lo.comp.escapes@isEscape'("_udpClose").
'lo.comp.escapes@isEscape'("hosttoip").
'lo.comp.escapes@isEscape'("iptohost").
'lo.comp.escapes@isEscape'("delay").
'lo.comp.escapes@isEscape'("sleep").
'lo.comp.escapes@isEscape'("now").
'lo.comp.escapes@isEscape'("today").
'lo.comp.escapes@isEscape'("ticks").
'lo.comp.escapes@isEscape'("_time2date").
'lo.comp.escapes@isEscape'("_time2utc").
'lo.comp.escapes@isEscape'("_date2time").
'lo.comp.escapes@isEscape'("_utc2time").
'lo.comp.escapes@isEscape'("_isCcChar").
'lo.comp.escapes@isEscape'("_isCfChar").
'lo.comp.escapes@isEscape'("_isCnChar").
'lo.comp.escapes@isEscape'("_isCoChar").
'lo.comp.escapes@isEscape'("_isCsChar").
'lo.comp.escapes@isEscape'("_isLlChar").
'lo.comp.escapes@isEscape'("_isLmChar").
'lo.comp.escapes@isEscape'("_isLoChar").
'lo.comp.escapes@isEscape'("_isLtChar").
'lo.comp.escapes@isEscape'("_isLuChar").
'lo.comp.escapes@isEscape'("_isMcChar").
'lo.comp.escapes@isEscape'("_isMeChar").
'lo.comp.escapes@isEscape'("_isMnChar").
'lo.comp.escapes@isEscape'("_isNdChar").
'lo.comp.escapes@isEscape'("_isNlChar").
'lo.comp.escapes@isEscape'("_isNoChar").
'lo.comp.escapes@isEscape'("_isPcChar").
'lo.comp.escapes@isEscape'("_isPdChar").
'lo.comp.escapes@isEscape'("_isPeChar").
'lo.comp.escapes@isEscape'("_isPfChar").
'lo.comp.escapes@isEscape'("_isPiChar").
'lo.comp.escapes@isEscape'("_isPoChar").
'lo.comp.escapes@isEscape'("_isPsChar").
'lo.comp.escapes@isEscape'("_isScChar").
'lo.comp.escapes@isEscape'("_isSkChar").
'lo.comp.escapes@isEscape'("_isSmChar").
'lo.comp.escapes@isEscape'("_isSoChar").
'lo.comp.escapes@isEscape'("_isZlChar").
'lo.comp.escapes@isEscape'("_isZpChar").
'lo.comp.escapes@isEscape'("_isZsChar").
'lo.comp.escapes@isEscape'("_isLetterChar").
'lo.comp.escapes@isEscape'("_digitCode").
'lo.comp.escapes@isEscape'("_int2str").
'lo.comp.escapes@isEscape'("_flt2str").
'lo.comp.escapes@isEscape'("_int_format").
'lo.comp.escapes@isEscape'("_flt_format").
'lo.comp.escapes@isEscape'("_str2flt").
'lo.comp.escapes@isEscape'("_str_lt").
'lo.comp.escapes@isEscape'("_str_ge").
'lo.comp.escapes@isEscape'("_str_hash").
'lo.comp.escapes@isEscape'("_str_len").
'lo.comp.escapes@isEscape'("_str_gen").
'lo.comp.escapes@isEscape'("_stringOf").
'lo.comp.escapes@isEscape'("_trim").
'lo.comp.escapes@isEscape'("explode").
'lo.comp.escapes@isEscape'("implode").
'lo.comp.escapes@isEscape'("_str_find").
'lo.comp.escapes@isEscape'("_sub_str").
'lo.comp.escapes@isEscape'("_str_split").
'lo.comp.escapes@isEscape'("_str_concat").
'lo.comp.escapes@isEscape'("_str_start").
'lo.comp.escapes@isEscape'("_str_multicat").
'lo.comp.escapes@isEscape'("getenv").
'lo.comp.escapes@isEscape'("setenv").
'lo.comp.escapes@isEscape'("envir").
'lo.comp.escapes@isEscape'("getlogin").
'lo.comp.escapes@isEscape'("_fork").
'lo.comp.escapes@isEscape'("_thread").
'lo.comp.escapes@isEscape'("kill").
'lo.comp.escapes@isEscape'("thread_state").
'lo.comp.escapes@isEscape'("waitfor").
'lo.comp.escapes@isEscape'("_shell").
'lo.comp.escapes@isEscape'("_newLock").
'lo.comp.escapes@isEscape'("_acquireLock").
'lo.comp.escapes@isEscape'("_waitLock").
'lo.comp.escapes@isEscape'("_releaseLock").
'lo.comp.escapes@isEscape'("_ins_debug").
'lo.comp.escapes@isEscape'("_stackTrace").
'lo.comp.escapes@escCode'("_exit", 0) :- !.
'lo.comp.escapes@escCode'("_command_line", 1) :- !.
'lo.comp.escapes@escCode'("_command_opts", 2) :- !.
'lo.comp.escapes@escCode'("_unify", 3) :- !.
'lo.comp.escapes@escCode'("_identical", 4) :- !.
'lo.comp.escapes@escCode'("var", 5) :- !.
'lo.comp.escapes@escCode'("ground", 6) :- !.
'lo.comp.escapes@escCode'("_call", 7) :- !.
'lo.comp.escapes@escCode'("_defined", 8) :- !.
'lo.comp.escapes@escCode'("_int_plus", 9) :- !.
'lo.comp.escapes@escCode'("_int_minus", 10) :- !.
'lo.comp.escapes@escCode'("_int_times", 11) :- !.
'lo.comp.escapes@escCode'("_int_div", 12) :- !.
'lo.comp.escapes@escCode'("_int_mod", 13) :- !.
'lo.comp.escapes@escCode'("_flt_plus", 14) :- !.
'lo.comp.escapes@escCode'("_flt_minus", 15) :- !.
'lo.comp.escapes@escCode'("_flt_times", 16) :- !.
'lo.comp.escapes@escCode'("_flt_div", 17) :- !.
'lo.comp.escapes@escCode'("_flt_mod", 18) :- !.
'lo.comp.escapes@escCode'("_int_abs", 19) :- !.
'lo.comp.escapes@escCode'("_flt_abs", 20) :- !.
'lo.comp.escapes@escCode'("_int_lt", 21) :- !.
'lo.comp.escapes@escCode'("_int_ge", 22) :- !.
'lo.comp.escapes@escCode'("_flt_lt", 23) :- !.
'lo.comp.escapes@escCode'("_flt_ge", 24) :- !.
'lo.comp.escapes@escCode'("_int2flt", 25) :- !.
'lo.comp.escapes@escCode'("_flt2int", 26) :- !.
'lo.comp.escapes@escCode'("_flt_hash", 27) :- !.
'lo.comp.escapes@escCode'("_pwr", 28) :- !.
'lo.comp.escapes@escCode'("sqrt", 29) :- !.
'lo.comp.escapes@escCode'("exp", 30) :- !.
'lo.comp.escapes@escCode'("log", 31) :- !.
'lo.comp.escapes@escCode'("log10", 32) :- !.
'lo.comp.escapes@escCode'("pi", 33) :- !.
'lo.comp.escapes@escCode'("sin", 34) :- !.
'lo.comp.escapes@escCode'("cos", 35) :- !.
'lo.comp.escapes@escCode'("tan", 36) :- !.
'lo.comp.escapes@escCode'("asin", 37) :- !.
'lo.comp.escapes@escCode'("acos", 38) :- !.
'lo.comp.escapes@escCode'("atan", 39) :- !.
'lo.comp.escapes@escCode'("trunc", 40) :- !.
'lo.comp.escapes@escCode'("floor", 41) :- !.
'lo.comp.escapes@escCode'("ceil", 42) :- !.
'lo.comp.escapes@escCode'("integral", 43) :- !.
'lo.comp.escapes@escCode'("srand", 44) :- !.
'lo.comp.escapes@escCode'("rand", 45) :- !.
'lo.comp.escapes@escCode'("irand", 46) :- !.
'lo.comp.escapes@escCode'("_ldexp", 47) :- !.
'lo.comp.escapes@escCode'("_frexp", 48) :- !.
'lo.comp.escapes@escCode'("_modf", 49) :- !.
'lo.comp.escapes@escCode'("_band", 50) :- !.
'lo.comp.escapes@escCode'("_bor", 51) :- !.
'lo.comp.escapes@escCode'("_bxor", 52) :- !.
'lo.comp.escapes@escCode'("_blsl", 53) :- !.
'lo.comp.escapes@escCode'("_blsr", 54) :- !.
'lo.comp.escapes@escCode'("_basr", 55) :- !.
'lo.comp.escapes@escCode'("_bnot", 56) :- !.
'lo.comp.escapes@escCode'("_nthb", 57) :- !.
'lo.comp.escapes@escCode'("_suspend", 58) :- !.
'lo.comp.escapes@escCode'("_get_file", 59) :- !.
'lo.comp.escapes@escCode'("_put_file", 60) :- !.
'lo.comp.escapes@escCode'("_cwd", 61) :- !.
'lo.comp.escapes@escCode'("_cd", 62) :- !.
'lo.comp.escapes@escCode'("_rm", 63) :- !.
'lo.comp.escapes@escCode'("_mv", 64) :- !.
'lo.comp.escapes@escCode'("_mkdir", 65) :- !.
'lo.comp.escapes@escCode'("_rmdir", 66) :- !.
'lo.comp.escapes@escCode'("_isdir", 67) :- !.
'lo.comp.escapes@escCode'("_chmod", 68) :- !.
'lo.comp.escapes@escCode'("_ls", 69) :- !.
'lo.comp.escapes@escCode'("_file_mode", 70) :- !.
'lo.comp.escapes@escCode'("_file_present", 71) :- !.
'lo.comp.escapes@escCode'("_file_type", 72) :- !.
'lo.comp.escapes@escCode'("_file_size", 73) :- !.
'lo.comp.escapes@escCode'("_file_modified", 74) :- !.
'lo.comp.escapes@escCode'("_file_date", 75) :- !.
'lo.comp.escapes@escCode'("_openInFile", 76) :- !.
'lo.comp.escapes@escCode'("_openOutFile", 77) :- !.
'lo.comp.escapes@escCode'("_openAppendFile", 78) :- !.
'lo.comp.escapes@escCode'("_openAppendIOFile", 79) :- !.
'lo.comp.escapes@escCode'("_popen", 80) :- !.
'lo.comp.escapes@escCode'("_close", 81) :- !.
'lo.comp.escapes@escCode'("_end_of_file", 82) :- !.
'lo.comp.escapes@escCode'("_ready", 83) :- !.
'lo.comp.escapes@escCode'("_inchars", 84) :- !.
'lo.comp.escapes@escCode'("_inbytes", 85) :- !.
'lo.comp.escapes@escCode'("_inchar", 86) :- !.
'lo.comp.escapes@escCode'("_inbyte", 87) :- !.
'lo.comp.escapes@escCode'("_inline", 88) :- !.
'lo.comp.escapes@escCode'("_intext", 89) :- !.
'lo.comp.escapes@escCode'("_outch", 90) :- !.
'lo.comp.escapes@escCode'("_outbyte", 91) :- !.
'lo.comp.escapes@escCode'("_outbytes", 92) :- !.
'lo.comp.escapes@escCode'("_outtext", 93) :- !.
'lo.comp.escapes@escCode'("_stdfile", 94) :- !.
'lo.comp.escapes@escCode'("_fposition", 95) :- !.
'lo.comp.escapes@escCode'("_fseek", 96) :- !.
'lo.comp.escapes@escCode'("_flush", 97) :- !.
'lo.comp.escapes@escCode'("_flushall", 98) :- !.
'lo.comp.escapes@escCode'("_setfileencoding", 99) :- !.
'lo.comp.escapes@escCode'("_install_pkg", 100) :- !.
'lo.comp.escapes@escCode'("_pkg_is_present", 101) :- !.
'lo.comp.escapes@escCode'("_logmsg", 102) :- !.
'lo.comp.escapes@escCode'("_connect", 103) :- !.
'lo.comp.escapes@escCode'("_listen", 104) :- !.
'lo.comp.escapes@escCode'("_accept", 105) :- !.
'lo.comp.escapes@escCode'("_udpPort", 106) :- !.
'lo.comp.escapes@escCode'("_udpGet", 107) :- !.
'lo.comp.escapes@escCode'("_udpSend", 108) :- !.
'lo.comp.escapes@escCode'("_udpClose", 109) :- !.
'lo.comp.escapes@escCode'("hosttoip", 110) :- !.
'lo.comp.escapes@escCode'("iptohost", 111) :- !.
'lo.comp.escapes@escCode'("delay", 112) :- !.
'lo.comp.escapes@escCode'("sleep", 113) :- !.
'lo.comp.escapes@escCode'("now", 114) :- !.
'lo.comp.escapes@escCode'("today", 115) :- !.
'lo.comp.escapes@escCode'("ticks", 116) :- !.
'lo.comp.escapes@escCode'("_time2date", 117) :- !.
'lo.comp.escapes@escCode'("_time2utc", 118) :- !.
'lo.comp.escapes@escCode'("_date2time", 119) :- !.
'lo.comp.escapes@escCode'("_utc2time", 120) :- !.
'lo.comp.escapes@escCode'("_isCcChar", 121) :- !.
'lo.comp.escapes@escCode'("_isCfChar", 122) :- !.
'lo.comp.escapes@escCode'("_isCnChar", 123) :- !.
'lo.comp.escapes@escCode'("_isCoChar", 124) :- !.
'lo.comp.escapes@escCode'("_isCsChar", 125) :- !.
'lo.comp.escapes@escCode'("_isLlChar", 126) :- !.
'lo.comp.escapes@escCode'("_isLmChar", 127) :- !.
'lo.comp.escapes@escCode'("_isLoChar", 128) :- !.
'lo.comp.escapes@escCode'("_isLtChar", 129) :- !.
'lo.comp.escapes@escCode'("_isLuChar", 130) :- !.
'lo.comp.escapes@escCode'("_isMcChar", 131) :- !.
'lo.comp.escapes@escCode'("_isMeChar", 132) :- !.
'lo.comp.escapes@escCode'("_isMnChar", 133) :- !.
'lo.comp.escapes@escCode'("_isNdChar", 134) :- !.
'lo.comp.escapes@escCode'("_isNlChar", 135) :- !.
'lo.comp.escapes@escCode'("_isNoChar", 136) :- !.
'lo.comp.escapes@escCode'("_isPcChar", 137) :- !.
'lo.comp.escapes@escCode'("_isPdChar", 138) :- !.
'lo.comp.escapes@escCode'("_isPeChar", 139) :- !.
'lo.comp.escapes@escCode'("_isPfChar", 140) :- !.
'lo.comp.escapes@escCode'("_isPiChar", 141) :- !.
'lo.comp.escapes@escCode'("_isPoChar", 142) :- !.
'lo.comp.escapes@escCode'("_isPsChar", 143) :- !.
'lo.comp.escapes@escCode'("_isScChar", 144) :- !.
'lo.comp.escapes@escCode'("_isSkChar", 145) :- !.
'lo.comp.escapes@escCode'("_isSmChar", 146) :- !.
'lo.comp.escapes@escCode'("_isSoChar", 147) :- !.
'lo.comp.escapes@escCode'("_isZlChar", 148) :- !.
'lo.comp.escapes@escCode'("_isZpChar", 149) :- !.
'lo.comp.escapes@escCode'("_isZsChar", 150) :- !.
'lo.comp.escapes@escCode'("_isLetterChar", 151) :- !.
'lo.comp.escapes@escCode'("_digitCode", 152) :- !.
'lo.comp.escapes@escCode'("_int2str", 153) :- !.
'lo.comp.escapes@escCode'("_flt2str", 154) :- !.
'lo.comp.escapes@escCode'("_int_format", 155) :- !.
'lo.comp.escapes@escCode'("_flt_format", 156) :- !.
'lo.comp.escapes@escCode'("_str2flt", 157) :- !.
'lo.comp.escapes@escCode'("_str_lt", 158) :- !.
'lo.comp.escapes@escCode'("_str_ge", 159) :- !.
'lo.comp.escapes@escCode'("_str_hash", 160) :- !.
'lo.comp.escapes@escCode'("_str_len", 161) :- !.
'lo.comp.escapes@escCode'("_str_gen", 162) :- !.
'lo.comp.escapes@escCode'("_stringOf", 163) :- !.
'lo.comp.escapes@escCode'("_trim", 164) :- !.
'lo.comp.escapes@escCode'("explode", 165) :- !.
'lo.comp.escapes@escCode'("implode", 166) :- !.
'lo.comp.escapes@escCode'("_str_find", 167) :- !.
'lo.comp.escapes@escCode'("_sub_str", 168) :- !.
'lo.comp.escapes@escCode'("_str_split", 169) :- !.
'lo.comp.escapes@escCode'("_str_concat", 170) :- !.
'lo.comp.escapes@escCode'("_str_start", 171) :- !.
'lo.comp.escapes@escCode'("_str_multicat", 172) :- !.
'lo.comp.escapes@escCode'("getenv", 173) :- !.
'lo.comp.escapes@escCode'("setenv", 174) :- !.
'lo.comp.escapes@escCode'("envir", 175) :- !.
'lo.comp.escapes@escCode'("getlogin", 176) :- !.
'lo.comp.escapes@escCode'("_fork", 177) :- !.
'lo.comp.escapes@escCode'("_thread", 178) :- !.
'lo.comp.escapes@escCode'("kill", 179) :- !.
'lo.comp.escapes@escCode'("thread_state", 180) :- !.
'lo.comp.escapes@escCode'("waitfor", 181) :- !.
'lo.comp.escapes@escCode'("_shell", 182) :- !.
'lo.comp.escapes@escCode'("_newLock", 183) :- !.
'lo.comp.escapes@escCode'("_acquireLock", 184) :- !.
'lo.comp.escapes@escCode'("_waitLock", 185) :- !.
'lo.comp.escapes@escCode'("_releaseLock", 186) :- !.
'lo.comp.escapes@escCode'("_ins_debug", 187) :- !.
'lo.comp.escapes@escCode'("_stackTrace", 188) :- !.
'lo.comp.escapes@escCode'(_, _) :- raise_exception('error'("escCode", 391, 3, 21)).
'lo.comp.escapes^escapeType'('_call%2'(XV1399, XV1400), 'lo.comp.escapes^escapeType', _) :- 'lo.comp.escapes@escapeType'(XV1399, XV1400).
'lo.comp.escapes^isEscape'('_call%1'(XV1401), 'lo.comp.escapes^isEscape', _) :- 'lo.comp.escapes@isEscape'(XV1401).
'lo.comp.escapes^escCode'('_call%2'(XV1402, XV1403), 'lo.comp.escapes^escCode', _) :- 'lo.comp.escapes@escCode'(XV1402, XV1403).
