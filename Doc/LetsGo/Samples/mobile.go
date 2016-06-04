/*
 * A mobile agent class
 */

mobile..{
  include "sys:go/stdlib.gof".
  include "sys:go/io.gof".
  include "sys:go/stack.gof".
  include "sys:go/cell.gof".
  include "mobile.gh".

  mobile[]<~mbAgent[].            -- We implement the mobile agent interface
  mobile(l,home){
    Places = $stack[handle](l).   -- List of places to go
    Counter = $cell[number](0).   -- How many palces have we been to?

    visit(P) ->                   -- We might get a request to visit somewhere
        Places.push(P).

    exec(_)::(Places.depth()==0) -> this>>home.
    exec(O:outFile) ->
        O.outLine("I am "<>this^0<>" at my "<>Counter.get()^0<>"th place");
        Places.pop(Nx);
        O.outLine("Going to "<>Nx^0);
        Counter.set(Counter.get()+1);
        this >> Nx.
  }
}
      
    