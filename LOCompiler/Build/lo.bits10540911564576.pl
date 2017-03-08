'#pkg'("n7o7'()7'n2o2'pkg's'lo.bits's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I8'.|.'FT2iii'.&.'FT2iii'.^.'FT2iii'.<<.'FT2iii'.>>.'FT2iii'.>>>.'FT2iii'.~.'FT1ii'.#.'PT2ii\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.bits@init'():- !.
'lo.bits@.|.'(XX, XY, XXc370):- !,
    '_bor'(XX, XY, XXc370).
'lo.bits@.|.'(_, _, _):- raise_exception('error'("lo.bits@.|.", 5, 3, 20)).
'lo.bits@.&.'(XX, XY, XXc371):- !,
    '_band'(XX, XY, XXc371).
'lo.bits@.&.'(_, _, _):- raise_exception('error'("lo.bits@.&.", 8, 3, 21)).
'lo.bits@.^.'(XX, XY, XXc372):- !,
    '_bxor'(XX, XY, XXc372).
'lo.bits@.^.'(_, _, _):- raise_exception('error'("lo.bits@.^.", 11, 3, 21)).
'lo.bits@.<<.'(XX, XY, XXc373):- !,
    '_blsl'(XX, XY, XXc373).
'lo.bits@.<<.'(_, _, _):- raise_exception('error'("lo.bits@.<<.", 14, 3, 22)).
'lo.bits@.>>.'(XX, XY, XXc374):- !,
    '_blsr'(XX, XY, XXc374).
'lo.bits@.>>.'(_, _, _):- raise_exception('error'("lo.bits@.>>.", 17, 3, 22)).
'lo.bits@.>>>.'(XX, XY, XXc375):- !,
    '_basr'(XX, XY, XXc375).
'lo.bits@.>>>.'(_, _, _):- raise_exception('error'("lo.bits@.>>>.", 20, 3, 23)).
'lo.bits@.~.'(XX, XXc376):- !,
    '_bnot'(XX, XXc376).
'lo.bits@.~.'(_, _):- raise_exception('error'("lo.bits@.~.", 23, 3, 17)).
'lo.bits@.#.'(XX, XY):- '_nthb'(XX, XY).
'lo.bits^.|.'('_call%3'(XV19430, XV19431, XV19432), 'lo.bits^.|.', _):- 'lo.bits@.|.'(XV19430, XV19431, XV19432).
'lo.bits^.&.'('_call%3'(XV19433, XV19434, XV19435), 'lo.bits^.&.', _):- 'lo.bits@.&.'(XV19433, XV19434, XV19435).
'lo.bits^.^.'('_call%3'(XV19436, XV19437, XV19438), 'lo.bits^.^.', _):- 'lo.bits@.^.'(XV19436, XV19437, XV19438).
'lo.bits^.<<.'('_call%3'(XV19439, XV19440, XV19441), 'lo.bits^.<<.', _):- 'lo.bits@.<<.'(XV19439, XV19440, XV19441).
'lo.bits^.>>.'('_call%3'(XV19442, XV19443, XV19444), 'lo.bits^.>>.', _):- 'lo.bits@.>>.'(XV19442, XV19443, XV19444).
'lo.bits^.>>>.'('_call%3'(XV19445, XV19446, XV19447), 'lo.bits^.>>>.', _):- 'lo.bits@.>>>.'(XV19445, XV19446, XV19447).
'lo.bits^.~.'('_call%2'(XV19448, XV19449), 'lo.bits^.~.', _):- 'lo.bits@.~.'(XV19448, XV19449).
'lo.bits^.#.'('_call%2'(XV19450, XV19451), 'lo.bits^.#.', _):- 'lo.bits@.#.'(XV19450, XV19451).
