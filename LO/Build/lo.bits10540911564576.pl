'#pkg'("n7o7'()7'n2o2'pkg's'lo.bits's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I8'.|.'FT2iii'.&.'FT2iii'.^.'FT2iii'.<<.'FT2iii'.>>.'FT2iii'.>>>.'FT2iii'.~.'FT1ii'.#.'PT2ii\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.bits@init'():- !.
'lo.bits@.|.'(XX, XY, XXc798):- !,
    '_bor'(XX, XY, XXc798).
'lo.bits@.|.'(_, _, _):- raise_exception('error'("lo.bits@.|.", 5, 3, 20)).
'lo.bits@.&.'(XX, XY, XXc799):- !,
    '_band'(XX, XY, XXc799).
'lo.bits@.&.'(_, _, _):- raise_exception('error'("lo.bits@.&.", 8, 3, 21)).
'lo.bits@.^.'(XX, XY, XXc800):- !,
    '_bxor'(XX, XY, XXc800).
'lo.bits@.^.'(_, _, _):- raise_exception('error'("lo.bits@.^.", 11, 3, 21)).
'lo.bits@.<<.'(XX, XY, XXc801):- !,
    '_blsl'(XX, XY, XXc801).
'lo.bits@.<<.'(_, _, _):- raise_exception('error'("lo.bits@.<<.", 14, 3, 22)).
'lo.bits@.>>.'(XX, XY, XXc802):- !,
    '_blsr'(XX, XY, XXc802).
'lo.bits@.>>.'(_, _, _):- raise_exception('error'("lo.bits@.>>.", 17, 3, 22)).
'lo.bits@.>>>.'(XX, XY, XXc803):- !,
    '_basr'(XX, XY, XXc803).
'lo.bits@.>>>.'(_, _, _):- raise_exception('error'("lo.bits@.>>>.", 20, 3, 23)).
'lo.bits@.~.'(XX, XXc804):- !,
    '_bnot'(XX, XXc804).
'lo.bits@.~.'(_, _):- raise_exception('error'("lo.bits@.~.", 23, 3, 17)).
'lo.bits@.#.'(XX, XY):- '_nthb'(XX, XY).
'lo.bits^.|.'('_call%3'(XV18141, XV18142, XV18143), 'lo.bits^.|.', _):- 'lo.bits@.|.'(XV18141, XV18142, XV18143).
'lo.bits^.&.'('_call%3'(XV18144, XV18145, XV18146), 'lo.bits^.&.', _):- 'lo.bits@.&.'(XV18144, XV18145, XV18146).
'lo.bits^.^.'('_call%3'(XV18147, XV18148, XV18149), 'lo.bits^.^.', _):- 'lo.bits@.^.'(XV18147, XV18148, XV18149).
'lo.bits^.<<.'('_call%3'(XV18150, XV18151, XV18152), 'lo.bits^.<<.', _):- 'lo.bits@.<<.'(XV18150, XV18151, XV18152).
'lo.bits^.>>.'('_call%3'(XV18153, XV18154, XV18155), 'lo.bits^.>>.', _):- 'lo.bits@.>>.'(XV18153, XV18154, XV18155).
'lo.bits^.>>>.'('_call%3'(XV18156, XV18157, XV18158), 'lo.bits^.>>>.', _):- 'lo.bits@.>>>.'(XV18156, XV18157, XV18158).
'lo.bits^.~.'('_call%2'(XV18159, XV18160), 'lo.bits^.~.', _):- 'lo.bits@.~.'(XV18159, XV18160).
'lo.bits^.#.'('_call%2'(XV18161, XV18162), 'lo.bits^.#.', _):- 'lo.bits@.#.'(XV18161, XV18162).
