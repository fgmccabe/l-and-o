//
// Created by Francis McCabe on 2/19/17.
//

#ifndef LANDO_UDP_H
#define LANDO_UDP_H

#include "config.h"
#include "file.h"

typedef struct _udp_object_ *udpPo;

udpPo udpPort(byte *name, int port, ioDirection dir);
retCode udpRead(udpPo f,byte *msg,long *blen,string peer,long len,int *port);
retCode udpSend(udpPo f,byte *msg,long blen,string peer,int port);
string udpName(udpPo f);
uint16 udpPortNo(udpPo u);
retCode closeUDP(udpPo u);

extern classPo udpClass;

#ifdef VERIFY_OBJECT
extern objectPo checkCast(void *c,classPo class);

#define O_UDP(c) ((udpPo)(checkCast((c),udpClass)))
#else
#define O_UDP(c) ((udpPo)(c))
#endif


#endif //LANDO_UDP_H
