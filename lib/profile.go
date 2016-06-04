/*
  Interface to the profile generator of the compiler
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

go.profile{
  import go.io.

  profiler <~ { event:[string,integer]* }.

  profiler:[] @> profiler.
  profiler() .. {
    profOut:outChannel = openOutFile("goProfile.out",utf8Encoding).

    event(File,Line) ->
	profOut.outLine("<event tId = \""<>__thread().show()<>"\" File=\""<>
			File<>"\" Line=\""<>Line.show()<>"\" timeStamp=\""<>
			ticks().show()<>"\"/>").
  }.

  currProfiler:profiler = profiler().

  event:[string,integer]*.
  event(File,Line) ->
      currProfiler.event(File,Line).
}.
      

  


