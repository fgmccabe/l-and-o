/* Start an agent, and keep it running
   % go start.goc
*/
main..{
  include "sys:go/scomms.gof".
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "mobile.gh".
  include "mobile.gof".

  start() ->
      A = $mobile([hdl('a','a'),hdl('b','b'),hdl('c','c')],self);
      A.exec(stdout);
      loop() .. {
        loop() -> (
             XX<~mbAgent[] << From ->
                 stdout.outLine(XX^0<>" has come home, from "<>From^0);
                 XX.visit(hdl('a','a'));
                 XX.visit(hdl('b','b'));
                 XX.visit(hdl('c','c'));
                 XX.exec(stdout)
            ); loop()
      }.

  main() -> scsConnect(start,"localhost",4545).
}
