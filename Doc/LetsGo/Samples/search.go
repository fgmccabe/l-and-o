-- Search the directory server
main .. {
  include "sys:go/dynamic.gof".
  include "sys:go/scomms.gof".
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  
  attribute ::= attr(symbol,any).
  DSmessage ::= register(attribute[])
      | search(attribute[],symbol[])
      | inform(attribute[]).
      
  gender ::= male | female.

  main() -> scsConnect(searchDir,"localhost",5050).
  
  dir = hdl('server','directory').
  
  searchDir() ->
    search([attr('role',??('dancer'))],['name','gender']) >> dir;
    read_responses() .. {
      read_responses() ->
        inform(Atts) << dir;
        stdout.outLine("Result = "<>Atts^0);
        read_responses()
    }.
}
