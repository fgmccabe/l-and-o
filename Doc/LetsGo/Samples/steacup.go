/*
 * Test the mobile agent, within a single engine
 */

main .. {
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "m.gh".
  include "base.gof".
  include "agent.gof".

  main() ->
      B1 = spawn { base() };
      B2 = spawn { base() };
      B3 = spawn { base() };
      B4 = spawn { base() };
      S = self;
      spawn { mobile([B1,B2,B3,B4,B1,B2,B3,B4,S]) };
      spawn { mobile([B4,B3,B2,B1,B4,B3,B2,B1,S]) };
      ( exec(_) << H1 ->
	    stdout.outLine(H1^0<>" has come home")
      );
      ( exec(_) << H2 ->
	    stdout.outLine(H2^0<>" has come home")
      ).
}