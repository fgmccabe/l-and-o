'#pkg'("n7o7'()7'n2o2'pkg's'lo.resources's'1.0.0'n2o2'()2'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I3'getResource'FT1t'lo.uri*uri'S'putResource'PT2t'lo.uri*uri'S'resourcePresent'PT1t'lo.uri*uri'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.resources@init'():- !.
'lo.resources@getResource'(XU, XXc832):- !,
    'lo.uri@getUriPath'(XU, XXd8988),
    '_get_file'(XXd8988, XXc832).
'lo.resources@getResource'(_, _):- raise_exception('error'("lo.resources@getResource", 7, 3, 42)).
'lo.resources@putResource'(XU, XContent):- 'lo.uri@getUriPath'(XU, XXd8989),
    '_put_file'(XXd8989, XContent).
'lo.resources@resourcePresent'(XU):- 'lo.uri@getUriPath'(XU, XXd8990),
    '_file_present'(XXd8990).
'lo.resources^getResource'('_call%2'(XV18686, XV18687), 'lo.resources^getResource', _):- 'lo.resources@getResource'(XV18686, XV18687).
'lo.resources^putResource'('_call%2'(XV18688, XV18689), 'lo.resources^putResource', _):- 'lo.resources@putResource'(XV18688, XV18689).
'lo.resources^resourcePresent'('_call%1'(XV18690), 'lo.resources^resourcePresent', _):- 'lo.resources@resourcePresent'(XV18690).
