/*
   Implements a simple mail box facility for use between local threads.
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.

*/
go.mbox{
  import go.hash.
  import go.queue.
  import go.dynamic.
  import go.io.

  -- Types that support inter-process communication
  dropbox[M] <~ { post:[M]* }.

  mailbox[M] <~ {
	next:[]=>M. 
	nextW:[number]=>M.
	pending:[]{}.
	msg:[M]*.
	msgW:[M,number]*.
	dropbox:[]=>dropbox[M] }.
  
  nullhandle:[]@=dropbox[_].
  nullhandle..{
    show()=>"nullbox".
    post(_) -> {}.                   -- We discard messages to the nullhandle
  }.
	
  timedout:[]@=exception.
  timedout..{
    cause() => "timedout".
    code() => 'timedout'.
    show() => "timeout".
  }.
			
  private mhandle:[dynamic[M]]@=dropbox[M].
  mhandle(Q)..{
    post(Msg) -> 
	sync(Q){Q.add(Msg)}.
  }.
        
  mailbox:[]@>mailbox[_].
  mailbox():mailbox[m]..{
    Msgs:dynamic[m] = dynamic([]).

    myH:dropbox[m] = mhandle(Msgs).

    pending() :- Msgs.mem(_).
    
    next() => valof{
                sync(Msgs){
                  Msgs.mem(E) ->
                      Msgs.del(E);
		      valis E
                }
              }.
   
    nextW(T) => valof{
		  sync(Msgs){
                    Msgs.mem(E) ->
			Msgs.del(E);
			valis E
                  }
                  timeout (T+now() -> raise timedout)
                }.
    
    msg(P) ->
        sync(Msgs){
          Msgs.mem(P) ->
              Msgs.del(P);
        }.

    msgW(P,T) -> sync(Msgs){
          Msgs.mem(P) ->
              Msgs.del(P);
        }
        timeout (T+now() -> raise timedout).

    dropbox() => myH.
  }.
}