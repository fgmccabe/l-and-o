'#pkg'("n7o7'()7'n2o2'pkg's'lo.bits'e'*'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I8'.|.'FT2iii'.&.'FT2iii'.^.'FT2iii'.<<.'FT2iii'.>>.'FT2iii'.>>>.'FT2iii'.~.'FT1ii'.#.'PT2ii\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.bits@init'() :- !.
'lo.bits@.|.'(XX, XY, XX2166) :- !,
    '_bor'(XX, XY, XX2166).
'lo.bits@.|.'(_, _, _) :- raise_exception('error'(".|.", 5, 3, 20)).
'lo.bits@.&.'(XX, XY, XX2171) :- !,
    '_band'(XX, XY, XX2171).
'lo.bits@.&.'(_, _, _) :- raise_exception('error'(".&.", 8, 3, 21)).
'lo.bits@.^.'(XX, XY, XX2176) :- !,
    '_bxor'(XX, XY, XX2176).
'lo.bits@.^.'(_, _, _) :- raise_exception('error'(".^.", 11, 3, 21)).
'lo.bits@.<<.'(XX, XY, XX2181) :- !,
    '_blsl'(XX, XY, XX2181).
'lo.bits@.<<.'(_, _, _) :- raise_exception('error'(".<<.", 14, 3, 22)).
'lo.bits@.>>.'(XX, XY, XX2186) :- !,
    '_blsr'(XX, XY, XX2186).
'lo.bits@.>>.'(_, _, _) :- raise_exception('error'(".>>.", 17, 3, 22)).
'lo.bits@.>>>.'(XX, XY, XX2191) :- !,
    '_basr'(XX, XY, XX2191).
'lo.bits@.>>>.'(_, _, _) :- raise_exception('error'(".>>>.", 20, 3, 23)).
'lo.bits@.~.'(XX, XX2194) :- !,
    '_bnot'(XX, XX2194).
'lo.bits@.~.'(_, _) :- raise_exception('error'(".~.", 23, 3, 17)).
'lo.bits@.#.'(XX, XY) :- '_nthb'(XX, XY).
'lo.bits^.|.'('_call%3'(XV821, XV822, XV823), 'lo.bits^.|.', _) :- 'lo.bits@.|.'(XV821, XV822, XV823).
'lo.bits^.&.'('_call%3'(XV824, XV825, XV826), 'lo.bits^.&.', _) :- 'lo.bits@.&.'(XV824, XV825, XV826).
'.^.'('_call%3'(XV827, XV828, XV829), '.^.', _) :- 'lo.bits@.^.'(XV827, XV828, XV829).
'lo.bits^.<<.'('_call%3'(XV830, XV831, XV832), 'lo.bits^.<<.', _) :- 'lo.bits@.<<.'(XV830, XV831, XV832).
'lo.bits^.>>.'('_call%3'(XV833, XV834, XV835), 'lo.bits^.>>.', _) :- 'lo.bits@.>>.'(XV833, XV834, XV835).
'lo.bits^.>>>.'('_call%3'(XV836, XV837, XV838), 'lo.bits^.>>>.', _) :- 'lo.bits@.>>>.'(XV836, XV837, XV838).
'lo.bits^.~.'('_call%2'(XV839, XV840), 'lo.bits^.~.', _) :- 'lo.bits@.~.'(XV839, XV840).
'lo.bits^.#.'('_call%2'(XV841, XV842), 'lo.bits^.#.', _) :- 'lo.bits@.#.'(XV841, XV842).
