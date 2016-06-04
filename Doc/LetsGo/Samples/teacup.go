/*
 * Test the mobile agent, within a single engine
 */

main .. {
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "mobile.gh".
  include "mobile.gof".
  include "mbase.gof".

  main() ->
      B1 = spawn { base() };
      B2 = spawn { base() };
      B3 = spawn { base() };
      B4 = spawn { base() };
      A1 = $mobile[]([B1,B2,B3,B4,B1,B2,B3,B4],self);
      A2 = $mobile[]([B1,B2,B3,B4,B1,B2,B3,B4],self);
      spawn { A1.exec(stdout) };
      spawn { A2.exec(stdout) };
      ( AA<~mbAgent[] << _ ->
	    stdout.outLine(AA^0<>" has come home")
      );
      ( BB<~mbAgent[] << _ ->
	    stdout.outLine(BB^0<>" has come home")
      );
}