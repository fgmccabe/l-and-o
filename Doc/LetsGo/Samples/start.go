/* Start an agent
   % go start.goc
*/
main..{
  include "sys:go/scomms.gof".
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "m.gh".
  include "agent.gof".

  agent(S) ->
      mobile([hdl('a','a'),hdl('b','b'),hdl('c','c'),S]);
      ( exec(_) << H1 ->
	    stdout.outLine(H1^0<>" has come home")
      ); agent(S).

  main() -> scsConnect((()->agent(self)),"localhost",4545).
}
