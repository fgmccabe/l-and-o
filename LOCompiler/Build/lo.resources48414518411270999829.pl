'#pkg'("n7o7'()7'n2o2'pkg's'lo.resources's'1.0.0'n2o2'()2'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I3'getResource'FT1t'lo.uri*uri'S'putResource'PT2t'lo.uri*uri'S'resourcePresent'PT1t'lo.uri*uri'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.resources@init'():- !.
'lo.resources@getResource'(XU, XXc423):- !,
    'lo.uri@getUriPath'(XU, XXd28197),
    '_get_file'(XXd28197, XXc423).
'lo.resources@getResource'(_, _):- raise_exception('error'("lo.resources@getResource", 7, 3, 42)).
'lo.resources@putResource'(XU, XContent):- 'lo.uri@getUriPath'(XU, XXd28198),
    '_put_file'(XXd28198, XContent).
'lo.resources@resourcePresent'(XU):- 'lo.uri@getUriPath'(XU, XXd28199),
    '_file_present'(XXd28199).
'lo.resources^getResource'('_call%2'(XV21862, XV21863), 'lo.resources^getResource', _):- 'lo.resources@getResource'(XV21862, XV21863).
'lo.resources^putResource'('_call%2'(XV21864, XV21865), 'lo.resources^putResource', _):- 'lo.resources@putResource'(XV21864, XV21865).
'lo.resources^resourcePresent'('_call%1'(XV21866), 'lo.resources^resourcePresent', _):- 'lo.resources@resourcePresent'(XV21866).
