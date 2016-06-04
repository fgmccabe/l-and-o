/* Base station ...
   execute as:
   % go -N S -n s
  where S is root name you want to give to base station '<main>' thread
*/
main..{
  include "sys:go/scomms.gof".
  include "mbase.gof".

  main() -> scsConnect(base,"localhost",4545).
}
