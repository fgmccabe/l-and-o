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
#include "udp.h"

/* Open up a socket for listening to */

retCode g__listen(processPo P, ptrPo a) {
  ptrI Port = deRefI(&a[1]);

  if (isvar(Port) || !isInteger(objV(Port)))
    return liberror(P, "_listen", eINTNEEDD);
  else if (!isvar(deRefI(&a[2])))
    return liberror(P, "_listen", eVARNEEDD);
  else {
    integer port = integerVal(intV(Port));
    byte nBuff[MAX_MSG_LEN];
    ioPo listen;

    strMsg(nBuff, NumberOf(nBuff), "listen@%ld", port);
    switchProcessState(P, wait_io);
    listen = O_IO(listeningPort(nBuff, (unsigned short) port));
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
    return liberror(P, "_accept", eINVAL);
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
        ptrI peerNme;

        if (peerN == NULL || peerI == NULL) {
          closeFile(inC);
          closeFile(outC);
          return liberror(P, "_accept", eNOTFND);
        }

        ptrI tI = allocFilePtr(inC); /* return open file descriptor */
        bindVar(P, deRef(&a[2]), tI);
        ptrI tO = allocFilePtr(outC);
        bindVar(P, deRef(&a[3]), tO);

        peerNme = allocateString(&P->proc.heap, peerN, uniStrLen(peerN));

        bindVar(P, deRef(&a[4]), peerNme); /* Bind the peername of the connection */

        ptrI pt = allocateInteger(&P->proc.heap, port);
        bindVar(P, deRef(&a[6]), pt);   /* Bind the port number of the connection */

        peerNme = allocateString(&P->proc.heap, peerI, uniStrLen(peerI));
        bindVar(P, deRef(&a[5]), peerNme);      /* Bind the IP address of the connection */

        return Ok;
      }
      default:return liberror(P, "_accept", eIOERROR);
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
  ptrI Enc = deRefI(&a[3]);

  if (!IsString(Host))
    return liberror(P, "_connect", eSTRNEEDD);
  else if (isvar(Port) || !isInteger(objV(Port)))
    return liberror(P, "_connect", eINTNEEDD);
  else if (isvar(Enc) || !isInteger(objV(Enc)))
    return liberror(P, "_connect", eINVAL);
  else {
    int16 port = (int16) integerVal(intV(Port));
    retCode ret;

    if (port == 0)
      return liberror(P, "_connect", eINVAL);

    byte host[MAX_MSG_LEN];

    copyString2Buff(host, NumberOf(host), stringV(Host));

    ioPo inC, outC;
    ret = connectRemote(host, port, pickEncoding(Enc), True, &inC, &outC);

    setProcessRunnable(P);

    switch (ret) {
      case Ok: {
        ptrI RemIn = allocFilePtr(inC);
        ptrI RemOut = allocFilePtr(outC);
        equal(P, &a[4], &RemIn);
        return equal(P, &a[5], &RemOut);
      }
      default:logMsg(logFile, "Failed to establish connection: %U", host);
        return liberror(P, "_connect", eCONNECT);
    }
  }
}

ptrI udpPtrClass;

static long udpSizeFun(specialClassPo class, objPo o);
static comparison udpCompFun(specialClassPo class, objPo o1, objPo o2);
static retCode udpOutFun(specialClassPo class, ioPo out, objPo o);
static retCode udpScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o);
static objPo udpCopyFun(specialClassPo class, objPo dst, objPo src);
static uinteger udpHashFun(specialClassPo class, objPo o);

static udpPo udpPtr(ptrI p);
static ptrI allocUDPPtr(udpPo udp);
static void clearUDPPointer(ptrI p);
static logical isUdpPtr(ptrI p);

typedef struct {
  ptrI class; // == udpPtrClass
  udpPo udp;
} udpRec, *uPo;

void initUdp(void) {
  udpPtrClass = newSpecialClass("lo.io#udp", udpSizeFun, udpCompFun,
                                udpOutFun, udpCopyFun, udpScanFun, udpHashFun);
}

static long udpSizeFun(specialClassPo class, objPo o) {
  return CellCount(sizeof(udpRec));
}

static comparison udpCompFun(specialClassPo class, objPo o1, objPo o2) {
  if (o1->class == udpPtrClass && o2->class == udpPtrClass) {
    if (o1 == o2)
      return same;
    else
      return incomparible;
  } else
    return incomparible;
}

static retCode udpOutFun(specialClassPo class, ioPo out, objPo o) {
  assert(o->class == filePtrClass);

  uPo f = (uPo) o;

  return outMsg(out, "%s[%x]", udpName(f->udp), udpPortNo(f->udp));
}

static retCode udpScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o) {
  return Ok;
}

static objPo udpCopyFun(specialClassPo class, objPo dst, objPo src) {
  long size = udpSizeFun(class, src);
  memmove((void *) dst, (void *) src, size * sizeof(ptrI));

  return (objPo) (((ptrPo) dst) + size);
}

static uinteger udpHashFun(specialClassPo class, objPo o) {
  assert(o->class == filePtrClass);

  return (uinteger) ((PTRINT) (((uPo) o)->udp));
}

/* Open up a UDP socket for listening to */

retCode g__udpPort(processPo P, ptrPo a) {
  ptrI Port = deRefI(&a[1]);

  if (isvar(Port) || !isInteger(objV(Port)))
    return liberror(P, "_udpPort", eINTNEEDD);
  else if (!isvar(deRefI(&a[2])))
    return liberror(P, "_udpPort", eVARNEEDD);
  else {
    int portNo = (int) integerVal(intV(Port));
    byte nBuff[MAX_MSG_LEN];

    strMsg(nBuff, NumberOf(nBuff), "udpPort:%d", portNo);
    udpPo sock = newUDPPort(nBuff, portNo, ioREAD | ioWRITE);

    if (sock == NULL)
      return liberror(P, "_udpPort", eNOPERM);
    else {
      ptrI t2 = allocFilePtr(O_IO(sock)); /* return open file descriptor */
      ptrPo res = deRef(&a[2]);

      bindVar(P, res, t2);
      return Ok;
    }
  }
}

retCode g__udpClose(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);
  objPo o1 = objV(t1);

  if (!hasClass(o1, udpPtrClass))
    return liberror(P, "_udpClose", eINVAL);
  else {
    udpPo file = udpPtr(t1);

    if (file != NULL) {
      clearUDPPointer(t1);
      switchProcessState(P, wait_io);
      retCode ret = closeUDP(file);
      setProcessRunnable(P);
      return ret;
    } else
      return Ok;
  }
}

/* Send a block of text down a UDP port */
retCode g__udpSend(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!isUdpPtr(t1))
    return liberror(P, "_udpSend", eINVAL);
  else {
    udpPo file = udpPtr(t1);
    ptrI a2 = deRefI(&a[2]);

    if (isvar(a2))
      return liberror(P, "_udpSend", eINSUFARG);
    else if (!IsString(a2))
      return liberror(P, "_udpSend", eSTRNEEDD);
    else {
      stringPo str = stringV(a2);
      string text = stringVal(str);
      long tLen = stringLen(str);

      ptrI a3 = deRefI(&a[3]);

      if (isvar(a3))
        return liberror(P, "_udpSend", eINSUFARG);
      else if (!IsString(a3))
        return liberror(P, "_udpSend", eSTRNEEDD);
      else {
        str = stringV(a3);
        long peerLn = stringLen(str);

        byte peer[peerLn + 1];
        copyString2Buff(peer, NumberOf(peer), str);

        if (isvar(t1 = deRefI(&a[4])) || !isInteger(objV(t1)))
          return liberror(P, "_udpSend", eINTNEEDD);
        else {
          uint16 port = (uint16) integerVal(intV(t1));

          switch (udpSend(file, text, tLen, peer, port)) {
            case Ok:return Ok;

            default:return liberror(P, "__udpSend", eIOERROR);
          }
        }
      }
    }
  }
}

/* read a message from a UDP port */

retCode g__udpGet(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!isUdpPtr(t1))
    return liberror(P, "_udpGet", eINVAL);
  else {
    udpPo file = udpPtr(t1);
    byte txt[16384];                 // Maximum size of
    long len = NumberOf(txt);
    byte peer[1024];
    long plen = NumberOf(peer);
    int port = 0;

    switch (udpRead(file, txt, &len, peer, plen, &port)) {
      case Eof:return liberror(P, "__udpGet", eEOF);
      case Ok: {
        ptrI el = allocateString(&P->proc.heap, txt, len);

        if (equal(P, &el, &a[2]) != Ok)
          return Fail;

        el = allocateString(&P->proc.heap, peer, uniStrLen(peer));

        if (equal(P, &el, &a[3]) != Ok)
          return Fail;

        el = allocateInteger(&P->proc.heap, port);

        return equal(P, &el, &a[4]);
      }
      default:return liberror(P, "__intext", eIOERROR);
    }
  }
}

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

    for (i = 0; getNthHostIP(host, (unsigned) i, ip, NumberOf(ip)) != NULL; i++) {
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

ptrI allocUDPPtr(udpPo udp) {
  uPo f = (uPo) allocateSpecial(&globalHeap, udpPtrClass);

  f->udp = udp;
  return objP(f);
}

udpPo udpPtr(ptrI p) {
  objPo o = objV(p);
  assert(hasClass(o, udpPtrClass));
  return ((uPo) o)->udp;
}

static void clearUDPPointer(ptrI p) {
  objPo o = objV(p);
  assert(hasClass(o, udpPtrClass));

  ((uPo) o)->udp = NULL;
}

logical isUdpPtr(ptrI p) {
  objPo o = objV(p);
  return hasClass(o, udpPtrClass);
}
