#ifndef SMPI_H
#define SMPI_H

#include <vector>
#include <map>
#include <string>
#include <iostream>
#include <fstream>
#include <pthread.h>
#include <ctime>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <ctime>
#include <sstream>
#include <vector>
#include <iostream>
#include <iterator>
#include <pthread.h>
#include <networkutils.h>

#include "readdatafrommatlab.h"
#include "InitDaemon.h"
#include "LogUtil.h"
#include "fileOperation.h"
#include "MulticastThread.h"
#include "TomoParams.h"
#include "GossipSolicitSend.h"
#include "GossipUnsolicitSend.h"
#include "SlownessCompute.h"
#include "GossipRecv.h"
#include "GossipCompute.h"

#define DEF_WAIT_TIME 5
#define DEF_MAX_SLEEP_TIME 5
#define DEF_MAX_GOSSIP_ROUNDS 20
#define DEF_MAX_GOSSIP_TRIES 20


#define NULL_DESC "/dev/null"
#define RUN_DIR "./"
#define BUFFER 10000
//#define STATION_NUM 100

//#define HALF_SECOND 500000
//#define DEFAULT_SENSING_PORT 4444
//#define DEFAULT_PORT 6666
#define IP_ADD_LEN 4

//#define SIDE_LEN 10.0
#define INF 9999.9999

#define TOP_CONFIG_FILE "tomo.config"
#define SLOWNESS_FILE "vector"
#define EXE_PATH "./"
#define BUNDLE_PATH "./"
#define MODEL_PATH "./slowness/"
#define MODEL_TEMP_PATH "./"
#define MODEL_RECV_PATH "./bnin/"


extern bool recv_window;
extern bool sending_flag;

extern pthread_mutex_t mutex_sending_flag;
extern pthread_cond_t cv_sending_flag;
/*extern pthread_mutex_t mutex_rx_done;
extern pthread_mutex_t mutex_tx_done;
extern pthread_mutex_t mutex_tx_progress;
extern vector<float> model_transfer_progress;*/

//data variables
extern int dim;

//latest available value
extern int* latestvalind;
extern int neighborcount;;
extern char **neighborips;

//ip during random gossip
extern char chosen_ip[20];

//specified number of gossip updations
extern int specified_rounds;

//The SMPI Functions

void * SMPI_ALLREDUCE(void*);
void SMPI_INIT(char*,params_t*);
int SMPI_SEND(char*,char*,int,int);
int SMPI_RECIEVE(int,char*,int,char*);
int SMPI_BIND();



#endif
