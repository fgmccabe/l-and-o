/* The directory server, designed to be executed as a top-level program.
   execute as:
   % go -N directory -n server
*/
main..{
  include "sys:go/dynamic.gof".
  include "sys:go/scomms.gof".
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "directory.gof".

  main() -> waitfor(
             spawn{ scsConnect(directory_server,"localhost",5050)}
               as hdl('server','directory')).
}
  

