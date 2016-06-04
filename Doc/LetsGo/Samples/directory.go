/*
 * A sample directory package, as described in Chapter 2.
 */
directory{
  import go.dynamic.
  import go.datelib.

  -- the dirVal type is defined here,
  dirVal <~ thing.

  -- other implementations are possible ...
  when:[date]@=dirVal.
  when(D)..{
    show()=>D.show().
  }.

  name:[symbol]@=dirVal.
  name(N)..{
    show()=>explode(N).
  }.

  skills:[list[symbol]]@=dirVal.
  skills(L)..{
    show()=>L.show().
  }.

  attribute ::= attr(symbol,dirVal).

  directory <~ { 
	register:[list[attribute]]*.
	find:[list[attribute],list[symbol]]=>list[list[attribute]]}.

  directory:[]@>directory.
  directory()..{
    descriptions:dynamic[list[attribute]] = dynamic([]).

    register(D) -> sync{
          descriptions.add(D)
        }.

    find(D,S) => valof{sync{valis locate(D,S) } }.

    locate:[list[attribute],list[symbol]]=>list[list[attribute]].
    locate(D,Q) => { extract(E,Q)..(E::match(D,E)) in descriptions.ext() }.

    match:[list[attribute],list[attribute]]{}.
    match(D,E) :- 
        attr(A,V) in D *> attr(A,V) in E.

    extract:[list[attribute],list[symbol]]=>list[attribute].
    extract(E,[]) => E.    -- the whole entry
    extract(E,Q) =>
        { attr(K,V)..(attr(K,V)::K in Q) in E }.
  }.

  dir:directory = directory().
}
