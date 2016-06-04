rwStack..{
  include "sys:go/cell.gof".
  include "sys:go/stdlib.gof".
  
  rwStack(I){
    __stack = $cell(I).       -- stack itself is private
    push(E) -> __stack.set([E,..__stack.get()]).
    pop()=>valof{
      [E,..S]=__stack.get();
      __stack.set(S);
      valis E
    }.
    len()=>listlen(__stack.get()).
  }.
}

