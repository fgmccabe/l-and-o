/*
 * A simulation of a world of blocks
 */
 
(world)..{
  include "sys:go/dynamic.gof".
  include "sys:go/stdlib.gof".
  include "blocks.gh".
  include "action.gh".
  include "meta.gh".
  
  world[]<~clauses[blockPred].
  world[](T,O,H,C){
    onTable = $dynamic[blockPred](T).
    onTop = $dynamic[blockPred](O).
    inHand = $dynamic[blockPred](H).
    isClear = $dynamic[blockPred](C).
    
    do(stack(X,Y))::(isClear.mem(clear(Y)),inHand.mem(holding(X))) -> 
      onTop.add(on(X,Y));
      isClear.del(clear(Y));
      isClear.add(clear(X));
      inHand.del(holding(X)).

    do(pickup(X))::(onTable.mem(ontable(X)),isClear.mem(clear(X)),\+inHand.mem(holding(_))) -> 
      inHand.add(holding(X)); onTable.del(ontable(X)).
      
    do(unstack(X,Y))::(onTop.mem(on(X,Y)),isClear.mem(clear(X)),\+inHand.mem(holding(_))) -> 
      onTop.del(on(X,Y));
      inHand.add(holding(X));
      isClear.add(clear(Y)).

    do(putdown(X))::inHand.mem(holding(X))->
      onTable.add(ontable(X));
      inHand.del(holding(X)).

    cls(clear(S),TRUE):--isClear.mem(clear(S)).
    cls(holding(S),TRUE) :-- inHand.mem(holding(S)).
    cls(ontable(S),TRUE) :-- onTable.mem(ontable(S)).
    cls(on(X,Y),TRUE) :-- onTop.mem(on(X,Y)).
    cls(handempty,TRUE) :-- \+inHand.mem(holding(_)).
  }.
}
