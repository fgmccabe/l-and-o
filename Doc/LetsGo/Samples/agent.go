/*
 * A simple mobile agent
 */

mobile .. {
  include "sys:go/stdlib.gof".
  include "sys:go/io.gof".
  include "sys:go/cell.gof".
  include "m.gh".

  Counter = $cell[number](0).

  mobile([]) -> {}.
  mobile([P,..laces]) ->
      exec(((O:outFile[])->O.outLine("I am "<>self^0<>" on my "<>Counter.get()^0<>"th visit");
                Counter.set(Counter.get()+1);mobile(laces)))>>P.
}