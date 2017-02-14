/* 
  Socket and TCP handling functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <string.h>

#include "lo.h"
#include "fileio.h"
#include "ioTcp.h"
#include "hosts.h"

/* Open up a socket for listening to */

retCode g__listen(processPo P, ptrPo a) {
  ptrI Port = deRefI(&a[1]);

  if (isvar(Port) || !isInteger(objV(Port)))
    return liberror(P, "__listen", eINTNEEDD);
  else if (!isvar(deRefI(&a[2])))
    return liberror(P, "__listen", eVARNEEDD);
  else {
    integer port = integerVal(intV(Port));
    byte nBuff[MAX_MSG_LEN];
    ioPo listen;

    strMsg(nBuff, NumberOf(nBuff), "listen@%ld", port);
    switchProcessState(P, wait_io);
    listen = O_IO(listeningPort(nBuff, (int)port));
    setProcessRunnable(P);

    if (listen == NULL)
      return liberror(P, "_listen", eNOPERM);
    else {
      ptrI t2 = allocFilePtr(listen); /* return open file descriptor */
      ptrPo res = deRef(&a[2]);

      bindVar(P, res, t2);
      return Ok;
    }
  }
}

/* accept allows a connection from a connect socket and returns the
   socket number of the connection and information about the connecting
   host */
retCode g__accept(processPo P, ptrPo a) {
  ptrI Port = deRefI(&a[1]);
  objPo o1 = objV(Port);

  if (!hasClass(o1, filePtrClass))
    return liberror(P, "__accept", eINVAL);
  else {
    ioPo listen = filePtr(Port);
    ioPo inC, outC;

    switchProcessState(P, wait_io);
    retCode ret = acceptConnection(O_SOCK(listen), pickEncoding(deRefI(&a[7])),
                                   &inC, &outC);

    setProcessRunnable(P);

    switch (ret) {
      case Ok: {
        int port;
        byte pBuff[MAX_MSG_LEN];
        string peerN = peerName(O_SOCK(inC), &port);
        string peerI = peerIP(O_SOCK(inC), &port, &pBuff[0], NumberOf(pBuff));
        ptrI txtList;

        if (peerN == NULL || peerI == NULL) {
          closeFile(inC);
          closeFile(outC);
          return liberror(P, "__accept", eNOTFND);
        }

        ptrI tI = allocFilePtr(inC); /* return open file descriptor */
        bindVar(P, deRef(&a[2]), tI);
        ptrI tO = allocFilePtr(outC);
        bindVar(P, deRef(&a[3]), tO);

        txtList = allocateString(&P->proc.heap, peerN, uniStrLen(peerN));

        bindVar(P, deRef(&a[4]), txtList); /* Bind the peername of the connection */

        ptrI pt = allocateInteger(&P->proc.heap, port);
        bindVar(P, deRef(&a[6]), pt);   /* Bind the port number of the connection */

        txtList = allocateString(&P->proc.heap, peerI, uniStrLen(peerI));
        bindVar(P, deRef(&a[5]), txtList);      /* Bind the IP address of the connection */

        return Ok;
      }
      default:
        return liberror(P, "__accept", eIOERROR);
    }
  }
}

/* Attempt a connection with a server
   specified as a pair: hostname(or ip address)/port
*/
retCode g__connect(processPo P, ptrPo a) {
  switchProcessState(P, wait_io);

  ptrI Host = deRefI(&a[1]);
  ptrI Port = deRefI(&a[2]);

  if (!IsString(Host))
    return liberror(P, "__connect", eSTRNEEDD);
  else if (isvar(Port) || !isInteger(objV(Port)))
    return liberror(P, "__connect", eINTNEEDD);
  else if (isvar(deRefI(&a[3])))
    return liberror(P, "__connect", eINVAL);
  else {
    short port = (short) integerVal(intV(Port));
    retCode ret;

    if (port == 0 || !IsString(Host))
      return liberror(P, "__connect", eINVAL);

    string host = stringVal(stringV(Host));
    ioPo inC, outC;
    ret = connectRemote(host, port, pickEncoding(deRefI(&a[3])), True, &inC, &outC);

    setProcessRunnable(P);

    switch (ret) {
      case Ok: {
        ptrI RemIn = allocFilePtr(inC);
        ptrI RemOut = allocFilePtr(outC);
        equal(P, &a[4], &RemIn);
        return equal(P, &a[5], &RemOut);
      }
      default:
        logMsg(logFile, "Failed to establish connection: %U", host);
        return liberror(P, "__connect", eCONNECT);
    }
  }
}

#if 0
** *  Fix me later ** *

/* Open up a UDP socket for listening to */

retCode g_udpPort(processPo P,ptrPo a)
{
  ptrI Port = deRefI(&a[1]);
  
  if(isvar(Port)||!isInteger(objV(Port)))
    return liberror(P,"__udpPort",eINTNEEDD);
  else if(!isvar(deRefI(&a[2])))
    return liberror(P,"__udpPort",eVARNEEDD);
  else{
    long portNo = integerVal(intV(Port));
    byte nBuff[MAX_MSG_LEN];
    ioPo sock;
    
    strMsg(nBuff,NumberOf(nBuff),"udpPort:%d",portNo);
    sock = udpPort(nBuff,portNo);

    if(sock==NULL)
      return liberror(P,"__udpPort",eNOPERM);
    else{
      ptrI t2 = allocFilePtr(sock); /* return open file descriptor */
      ptrPo res = deRef(&a[2]);

      bindVar(P,res,t2);
      return Ok;
    }
  }
}

/* Send a block of text down a UDP port */
retCode g_udpSend(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__udpSend",eINVAL);
  else{
    ioPo file = filePtr(t1);

    if(isUDPport(file)!=Ok)
      return liberror(P,"__udpSend",eINVAL);
    else if(isGroundString(&a[2])!=Ok||isGroundString(&a[3])!=Ok)
      return liberror(P,"__udpSend",eSTRNEEDD);
    else if(isvar(t1=deRefI(&a[4])) || !isInteger(objV(t1)))
      return liberror(P,"__udpSend",eINTNEEDD);
    else{
      long len = StringLen(&a[2])+1;
      byte buff[len];
      long plen = StringLen(&a[3])+1;
      byte peer[plen];
      int port = integerVal(intV(t1));
      
      if(String2Uni(&a[2],buff,len)!=Ok)
        return liberror(P,"__udpSend",eSTRNEEDD);
      else if(String2Uni(&a[3],peer,plen)!=Ok)
        return liberror(P,"__udpSend",eSTRNEEDD);
        
      switch(udpSend(file,buff,len-1,peer,port)){
      case Ok:
        return Ok;

      default:
  return liberror(P,"__udpSend",eIOERROR);
      }
    }
  }
}

/* read a message from a UDP port */

retCode g_udpGet(processPo P,ptrPo a)
{
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if(!hasClass(o1,filePtrClass))
    return liberror(P,"__udpGet",eINVAL);
  else{
    ioPo file = filePtr(t1);
    byte txt[16384];
    long len = NumberOf(txt);
    byte peer[1024];
    long plen = NumberOf(peer);
    int port = 0;

    if(isUDPport(file)!=Ok)
      return liberror(P,"__udpGet",eNOPERM);

    switch(udpRead(file,txt,&len,peer,plen,&port)){
      case Eof:
        return liberror(P,"__udpGet",eEOF);
      case Ok:{
        ptrI el = allocateString(&P->proc.heap,txt,len);

        if(equal(P,&el,&a[2])!=Ok)
          return Fail;

        el = allocateString(&P->proc.heap,peer,uniStrLen(peer));

        if(equal(P,&el,&a[3])!=Ok)
          return Fail;

        el = allocateInteger(&P->proc.heap,port);

        return equal(P,&el,&a[4]);
      }
      default:
  return liberror(P,"__intext",eIOERROR);
    }
  }
}
#endif

/* Access host name functions */
/* return IP addresses of a host */
retCode g_hosttoip(processPo P, ptrPo a) {
  ptrI Host = deRefI(&a[1]);

  if (!IsString(Host))
    return liberror(P, "hosttoip", eSTRNEEDD);
  else {
    long i;
    byte ip[MAX_MSG_LEN];
    ptrI l = emptyList;
    ptrI el = kvoid;
    rootPo root = gcAddRoot(&P->proc.heap, &l);
    string host = stringVal(stringV(Host));

    gcAddRoot(&P->proc.heap, &el);

    for (i = 0; getNthHostIP(host, (unsigned)i, ip, NumberOf(ip)) != NULL; i++) {
      el = allocateString(&P->proc.heap, ip, uniStrLen(ip));

      l = consLsPair(&P->proc.heap, el, l);
    }

    gcRemoveRoot(&P->proc.heap, root);
    return equal(P, &a[2], &l);
  }
}

/* Access host name from IP address */
retCode g_iptohost(processPo P, ptrPo a) {
  ptrI IP = deRefI(&a[1]);

  if (!IsString(IP))
    return liberror(P, "iptohost", eSTRNEEDD);
  else {
    string ip = stringVal(stringV(IP));
    string host = getHostname(ip);

    if (host != NULL) {
      ptrI Host = allocateString(&P->proc.heap, host, uniStrLen(host));
      return equal(P, &Host, &a[2]);
    } else
      return liberror(P, "iptohost", eNOTFND);
  }
}
