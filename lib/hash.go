/*
  A hashtable class, supports a more efficient lookup than standard dynamic relations
  (c) 2001-2006 F.G. McCabe

  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

go.hash{
  import go.io.

  hash[Ky,Vl] <~ { 
	insert:[Ky,Vl]*. 
	find:[Ky]=>Vl. 
	present:[Ky+,Vl]{}. 
	delete:[Ky]*.
	ext:[]=>list[(Ky,Vl)]. 
	keys:[]=>list[Ky]. 
	values:[]=>list[Vl]. 
	count:[]=>integer
      }.

  hash:[list[(Ktp,Vtp)],integer]@>hash[Ktp,Vtp].
  hash(I,Size)..{
    table:opaque := __newhash(max(Size,listlen(I)*2)).

    ${
      ((H,V) in I *> table:=__hashinsert(table,H,V));
    }.
      
    insert(Ky,Vl) -> sync{ table:=__hashinsert(table,Ky,Vl)}
        onerror(
         error(_,Code) ->
             raise error("insert",Code)
        ).
    
    find(Ky) => valof{
                  sync{
                    __hashsearch(table,Ky,Value) ?
                      valis Value
                   | raise error("find",'eNOTFND')
                  }
                }.
    
    present(Ky,Value) :-
        action{ sync{ __hashsearch(table,Ky,Value) ? valis true | valis false}}.
    
    count() => valof{
      sync{
        valis __hashcount(table);
      }
    }.
    
    delete(Ky) -> sync{ 
          __hashdelete(table,Ky)
      } onerror(
        error(_,Code) ->
         raise error("delete",Code)
      ).
    
    ext() => valof{
      sync{
        valis __hashcontents(table)
      } onerror(
        error(_,Code) ->
         raise error("ext",Code)
      )
    }.

    keys() => valof{
      sync{
        valis __hashkeys(table)
      } onerror(
        error(_,Code) ->
         raise error("keys",Code)
      )
    }.

    values() => valof{
      sync{
        valis __hashvalues(table)
      } onerror(
        error(_,Code) ->
         raise error("ext",Code)
      )
    }.
  }.

}
