'#pkg'("n7o7'()7'n2o2'pkg's'lo.bits's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I8'.|.'FT2iii'.&.'FT2iii'.^.'FT2iii'.<<.'FT2iii'.>>.'FT2iii'.>>>.'FT2iii'.~.'FT1ii'.#.'PT2ii\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.bits@init'():- !.
'lo.bits@.|.'(XX, XY, XXc855):- !,
    '_bor'(XX, XY, XXc855).
'lo.bits@.|.'(_, _, _):- raise_exception('error'("lo.bits@.|.", 5, 3, 20)).
'lo.bits@.&.'(XX, XY, XXc856):- !,
    '_band'(XX, XY, XXc856).
'lo.bits@.&.'(_, _, _):- raise_exception('error'("lo.bits@.&.", 8, 3, 21)).
'lo.bits@.^.'(XX, XY, XXc857):- !,
    '_bxor'(XX, XY, XXc857).
'lo.bits@.^.'(_, _, _):- raise_exception('error'("lo.bits@.^.", 11, 3, 21)).
'lo.bits@.<<.'(XX, XY, XXc858):- !,
    '_blsl'(XX, XY, XXc858).
'lo.bits@.<<.'(_, _, _):- raise_exception('error'("lo.bits@.<<.", 14, 3, 22)).
'lo.bits@.>>.'(XX, XY, XXc859):- !,
    '_blsr'(XX, XY, XXc859).
'lo.bits@.>>.'(_, _, _):- raise_exception('error'("lo.bits@.>>.", 17, 3, 22)).
'lo.bits@.>>>.'(XX, XY, XXc860):- !,
    '_basr'(XX, XY, XXc860).
'lo.bits@.>>>.'(_, _, _):- raise_exception('error'("lo.bits@.>>>.", 20, 3, 23)).
'lo.bits@.~.'(XX, XXc861):- !,
    '_bnot'(XX, XXc861).
'lo.bits@.~.'(_, _):- raise_exception('error'("lo.bits@.~.", 23, 3, 17)).
'lo.bits@.#.'(XX, XY):- '_nthb'(XX, XY).
'lo.bits^.|.'('_call%3'(XV19136, XV19137, XV19138), 'lo.bits^.|.', _):- 'lo.bits@.|.'(XV19136, XV19137, XV19138).
'lo.bits^.&.'('_call%3'(XV19139, XV19140, XV19141), 'lo.bits^.&.', _):- 'lo.bits@.&.'(XV19139, XV19140, XV19141).
'lo.bits^.^.'('_call%3'(XV19142, XV19143, XV19144), 'lo.bits^.^.', _):- 'lo.bits@.^.'(XV19142, XV19143, XV19144).
'lo.bits^.<<.'('_call%3'(XV19145, XV19146, XV19147), 'lo.bits^.<<.', _):- 'lo.bits@.<<.'(XV19145, XV19146, XV19147).
'lo.bits^.>>.'('_call%3'(XV19148, XV19149, XV19150), 'lo.bits^.>>.', _):- 'lo.bits@.>>.'(XV19148, XV19149, XV19150).
'lo.bits^.>>>.'('_call%3'(XV19151, XV19152, XV19153), 'lo.bits^.>>>.', _):- 'lo.bits@.>>>.'(XV19151, XV19152, XV19153).
'lo.bits^.~.'('_call%2'(XV19154, XV19155), 'lo.bits^.~.', _):- 'lo.bits@.~.'(XV19154, XV19155).
'lo.bits^.#.'('_call%2'(XV19156, XV19157), 'lo.bits^.#.', _):- 'lo.bits@.#.'(XV19156, XV19157).
