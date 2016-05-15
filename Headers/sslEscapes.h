/* 
  This defines the escapes that access the SSL interface
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.

 */

  escape("__connectSSL",s_connectSSL,600,True,False,"F\1+OO","connect securely to remote host")

  escape("__acceptSSL",s_acceptSSL,601,True,False,"F\1+OO","accept secure connection")


