/*
   Directory service
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
 
directory{
  import go.io.
  import go.hash.

  directory = $hash([],32).

  desc[] ::=
          address(string)
        | att(symbol,string).

  description[] ::= entry(symbol,list[desc[]]).

  register(Owner,Desc) ->
      sync(directory){
        if directory.present(Owner,_) 
            raise error("duplicate",'fail')
       | _ -> 

  

 