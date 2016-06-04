/*
  A memoized class. This allows one to recover instances from labels
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
memo{
  import go.hash.

  memo[] <~ { instance:((this)=>this)}.

  instances = $hash([],32).

  memo(X<~memo[]):memo[]..{
    ${
      instances.insert(??(X),this)
    }.

    instance(Lbl) => I :-
        instances.present(??(Lbl),I).
  }.
}
    