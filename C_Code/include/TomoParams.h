#ifndef TOMOPARAMS_H
#define TOMOPARAMS_H

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

enum SYSTEM_STATES {IDLE, SOLICIT_SEND, UNSOLICIT_SEND, SOLICIT_RECV, UNSOLICIT_RECV, GOSSIP_AVG, BAYESIAN_ART};

struct thread_params {
    pthread_mutex_t mutex_system_state;
    SYSTEM_STATES system_state;
    vector<double> slow_self;
    vector< vector<double> > slowalltobeaveraged;
    int gossip_tries;
    char *chosen_ip;
    int gossip_rounds;    
};

typedef struct thread_params params_t;

//the wait times and sleep times for gossip
extern double WAIT_TIME;
extern int MAX_SLEEP_TIME;
extern int MAX_GOSSIP_TRIES;
extern int MAX_GOSSIP_ROUNDS;

void change_system_state(params_t *gossip_params, SYSTEM_STATES system_state);

#endif