'#pkg'("n7o7'()7'n2o2'pkg's'lo.sort's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I1'sort':k'e'FT2Lk'e'PT2k'e'k'e'Lk'e'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.sort@init'():- !.
'lo.sort@merge'('lo.core#[]', XL, X_25163, XL):- !.
'lo.sort@merge'(XL, 'lo.core#[]', X_25164, XL):- !.
'lo.sort@merge'('lo.core#,..'(Xd, XL1), 'lo.core#,..'(Xe, XL2), XP, 'lo.core#,..'(Xd, XXd29593)):- ocall('_call%2'(Xd, Xe),XP,XP),
    !,
    'lo.sort@merge'(XL1, 'lo.core#,..'(Xe, XL2), XP, XXd29593).
'lo.sort@merge'(XL1, 'lo.core#,..'(Xe, XL2), XP, 'lo.core#,..'(Xe, XXd29595)):- !,
    'lo.sort@merge'(XL1, XL2, XP, XXd29595).
'lo.sort@merge'(_, _, _, _):- raise_exception('error'("lo.sort@merge", 20, 3, 18)).
'lo.sort@split'('lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.sort@split'('lo.core#,..'(Xe, 'lo.core#[]'), 'lo.core#[]', 'lo.core#,..'(Xe, 'lo.core#[]')).
'lo.sort@split'('lo.core#,..'(Xe1, 'lo.core#,..'(Xe2, XL)), 'lo.core#,..'(Xe1, XL1), 'lo.core#,..'(Xe2, XL2)):- 'lo.sort@split'(XL, XL1, XL2).
'lo.sort@mergeSort'('lo.core#[]', X_25177, 'lo.core#[]'):- !.
'lo.sort@mergeSort'('lo.core#,..'(Xe, 'lo.core#[]'), X_25179, 'lo.core#,..'(Xe, 'lo.core#[]')):- !.
'lo.sort@mergeSort'(XL, XP, XXd29600):- 'lo.sort@split'(XL, XL1, XL2),
    !,
    'lo.sort@mergeSort'(XL1, XP, XXd29598),
    'lo.sort@mergeSort'(XL2, XP, XXd29599),
    'lo.sort@merge'(XXd29598, XXd29599, XP, XXd29600).
'lo.sort@mergeSort'(_, _, _):- raise_exception('error'("lo.sort@mergeSort", 10, 3, 21)).
'lo.sort@sort'(XL, XP, XXd29601):- !,
    'lo.sort@mergeSort'(XL, XP, XXd29601).
'lo.sort@sort'(_, _, _):- raise_exception('error'("lo.sort@sort", 7, 3, 27)).
'lo.sort^merge'('_call%4'(XV24250, XV24251, XV24252, XV24253), 'lo.sort^merge', _):- 'lo.sort@merge'(XV24250, XV24251, XV24252, XV24253).
'lo.sort^split'('_call%3'(XV24254, XV24255, XV24256), 'lo.sort^split', _):- 'lo.sort@split'(XV24254, XV24255, XV24256).
'lo.sort^mergeSort'('_call%3'(XV24257, XV24258, XV24259), 'lo.sort^mergeSort', _):- 'lo.sort@mergeSort'(XV24257, XV24258, XV24259).
'lo.sort^sort'('_call%3'(XV24260, XV24261, XV24262), 'lo.sort^sort', _):- 'lo.sort@sort'(XV24260, XV24261, XV24262).
