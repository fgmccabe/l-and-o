-- Simple test of the directory server
main..{
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "directory.gof".
  
  attribute ::= attr(symbol,any).
  DSmessage ::= register(attribute[])
      | search(attribute[],symbol[])
      | inform(attribute[]).
      
  gender ::= male | female.

  dir = spawn{ directory_server() }.
  
  main() ->
    register([attr('name',??(self)),
              attr('gender',??(male)),
              attr('role',??('dancer'))]) >> dir;
    register([attr('name',??(dir)),
              attr('gender',??(female)),
              attr('role',??('directory'))]) >> dir;
    register([attr('name',??(nullhandle)),
              attr('gender',??(male)),
              attr('role',??('dancer'))]) >> dir;
    search([attr('role',??('dancer'))],['name','gender']) >> dir;
    read_responses() .. {
      read_responses() ->
        inform(Atts) << dir;
        stdout.outLine("Result = "<>Atts^0);
        read_responses()
    }.
}