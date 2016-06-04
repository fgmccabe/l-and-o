main..{
  include "sys:go/stdlib.gof".
  include "sys:go/io.gof".
  include "linda.gof".
  
  rel=$linda([3]).
  
  main() -> 
  		H=spawn {stdout.outLine("Spawn waiting"); 
  		                {rel.notw(_)} ; 
  		                stdout.outLine("rel is now empty");
  		                stdout.outLine("Spawn waiting2"); 
  		                {rel.memw(X)};
  		                stdout.outLine(X^0);
  		                delay(2)};
  		delay(2);
  		rel.del(_);
  		delay(2);
  		rel.add(5);
  		waitfor(H).
  }