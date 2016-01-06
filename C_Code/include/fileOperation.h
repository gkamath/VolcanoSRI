/*
 * fileOperation.h
 *
 *  Created on: Feb 15, 2014
 *      Author: danhuang
 */

#ifndef FILEOPERATION_H_
#define FILEOPERATION_H_

#include <iostream>
#include <sys/inotify.h>
#include <sys/socket.h> 
#include <netinet/in.h>
#include <arpa/inet.h> 
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
//#include <string>

using namespace std;

#define EVENT_SIZE  ( sizeof (struct inotify_event) )
#define BUF_LEN     ( 32 * ( EVENT_SIZE + 16 ) )

#define MAIN_SERVER_PORT 19998

extern bool isVerbose;

string readvector();
int writetofile(const char* filename, const char* str);

int initvector();

char* readroute();
void *watchpwdfile(void *arg);
int sendmsgtoip(char* str, char* ip);

#endif /* FILEOPERATION_H_ */
