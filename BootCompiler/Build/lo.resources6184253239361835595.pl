'#pkg'("n7o7'()7'n2o2'pkg's'lo.resources'e'*'n11o11'()11'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'s\"I3'getResource'FT1t'lo.uri*uri'S'putResource'PT2t'lo.uri*uri'S'resourcePresent'PT1t'lo.uri*uri'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.resources@init'() :- !.
'lo.resources@getResource'(XU, XX6514) :- !,
    'lo.uri@getUriPath'(XU, XX6513),
    '_get_file'(XX6513, XX6514).
'lo.resources@getResource'(_, _) :- raise_exception('error'("getResource", 7, 3, 42)).
'lo.resources@putResource'(XU, XContent) :- 'lo.uri@getUriPath'(XU, XX6518),
    '_put_file'(XX6518, XContent).
'lo.resources@resourcePresent'(XU) :- 'lo.uri@getUriPath'(XU, XX6522),
    '_file_present'(XX6522).
'lo.resources^getResource'('_call%2'(XV1188, XV1189), 'lo.resources^getResource', _) :- 'lo.resources@getResource'(XV1188, XV1189).
'lo.resources^putResource'('_call%2'(XV1190, XV1191), 'lo.resources^putResource', _) :- 'lo.resources@putResource'(XV1190, XV1191).
'lo.resources^resourcePresent'('_call%1'(XV1192), 'lo.resources^resourcePresent', _) :- 'lo.resources@resourcePresent'(XV1192).
