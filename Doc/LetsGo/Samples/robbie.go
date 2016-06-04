/*
 * A sample world for the STRIPS planner to work on
 */

robbie{
  import go.io.
  import go.dynamic.
  import strips.

  /* The blocks world */
  blockPred ::= clear(symbol)
    | holding(symbol)
    | ontable(symbol)
    | on(symbol,symbol)
    | handempty.
  
  blockActions ::= stack(symbol,symbol)
    | unstack(symbol,symbol)
    | pickup(symbol)
    | putdown(symbol).

  robbie:[list[blockPred]]@>stripsWorld[blockActions,blockPred].
  robbie(State)..{
    onTable:dynamic[symbol] = dynamic({X..ontable(X) in State}).
    onTop:dynamic[(symbol,symbol)] = dynamic({(X,Y) .. on(X,Y) in State}).
    inHand:dynamic[symbol] = dynamic({X..holding(X) in State}).
    isClear:dynamic[symbol] = dynamic({X..clear(X) in State}).

    holds(on(X,Y)):--onTop.mem((X,Y)).
    holds(clear(X)) :-- isClear.mem(X).
    holds(handempty) :-- \+inHand.mem(_).
    holds(holding(X)) :-- inHand.mem(X).
    holds(ontable(X)) :-- onTable.mem(X).
        
    -- basic operators
    strips_rule(stack(X,Y),[clear(Y),holding(X)],
                [on(X,Y),clear(X),handempty],
                [clear(Y),holding(X)]).
    strips_rule(unstack(X,Y),
                [on(X,Y),clear(X),handempty],
                [clear(Y),holding(X)],
                [on(X,Y),clear(X),handempty]).
    strips_rule(pickup(X),
                [ontable(X),clear(X),handempty],
                [holding(X)],
                [ontable(X),clear(X),handempty]).
    strips_rule(putdown(X),
                [holding(X)],
                [ontable(X),clear(X),handempty],
                [holding(X)]).
                    
    -- We actually perform the actions of the blocks robbie
    
    do(stack(X,Y))::onTop.mem((X,Y)) -> {}.
    do(stack(X,Y)) -> 
	stdout.outLine("Stack "<>X.show()<>" onto "<>Y.show());
	onTop.add((X,Y));
	isClear.del(Y);
	isClear.add(X);
	inHand.del(X).
    do(pickup(X))::inHand.mem(X)->{}.
    do(pickup(X)) -> 
	stdout.outLine("Pick up "<>X.show());
	inHand.add(X); onTable.del(X).
    do(unstack(X,Y)) -> 
	stdout.outLine("Unstack "<>X.show()<>" from "<>Y.show());
	onTop.del((X,Y));
	inHand.add(X);
	isClear.add(Y).
    do(putdown(X))::onTable.mem(X)->{}.
    do(putdown(X))->
	stdout.outLine("Put "<>X.show()<>" down");
	onTable.add(X);
	inHand.del(X).
  }.
}
