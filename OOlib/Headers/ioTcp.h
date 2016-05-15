/* 
   I/O handling library, TCP/IP header
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/ 

#ifndef _IO_TCP_LIB_H_
#define _IO_TCP_LIB_H_

#include "config.h"
#include "file.h"

typedef struct _sock_object_ *sockPo;

sockPo listeningPort(string name,int port);
retCode acceptConnection(sockPo listen,ioEncoding encoding,
			 ioPo *inC,ioPo *outC);

string peerName(sockPo stream,int *port);
string peerIP(sockPo stream,int *port,string buff,long len);
retCode connectRemote(string where,int port,
		      ioEncoding encoding,logical waitForMe,
		      ioPo *inC,ioPo *outC);
sockPo udpPort(string name,int port);
retCode udpRead(sockPo f,byte *msg,long *blen,string peer,long len,int *port);
retCode udpSend(sockPo f,byte *msg,long blen,string peer,int port);

extern classPo sockClass;

#ifdef VERIFY_OBJECT
extern objectPo checkCast(void *c,classPo class);

#define O_SOCK(c) ((sockPo)(checkCast((c),sockClass)))
#else
#define O_SOCK(c) ((sockPo)(c))
#endif


#endif

