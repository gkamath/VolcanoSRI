/*
 * tomography.cpp
 *
 *  Created on: Jun 22, 2014
 *      Author: Paritosh Ramanan
 */


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
#include <time.h>
#include <sys/time.h>
#include "networkutils.h"
#include "Tomography.h"

#include "InitDaemon.h"
#include "LogUtil.h"
#include "fileOperation.h"

#define AIDX_FILE "Aidx"
#define B_FILE "b"
#define GOSSIP_CONFIG_FILE "gossip.config"
#define TOP_CONFIG_FILE "tomo.config"

using namespace std;

int res_levels = 0;					//resolution levels
vector<string> config_files_name;	//config files name on different resolution levels
char ipaddr[100];
vector<double> a_ls;
vector<int> id_ls;
vector< vector<double> > A_l;
vector< vector<int> > idx_l;
vector<double> b_l;
vector<double> x;

//int dim;


double rho;
double lambda;
double tolerance;
int rounds;
int dim;
double update;
int global_rounds = 0;
int max_rounds = 20;
int node_id = 0;
bool runwatch = false;
int source = 100;

vector< vector<double> > xFinal;

double get_wall_time(){
    struct timeval time;
    if (gettimeofday(&time,NULL)){
        //  Handle error
        return 0;
    }
    return (double)time.tv_sec + (double)time.tv_usec * .000001;
}

double get_cpu_time(){
    return (double)clock() / CLOCKS_PER_SEC;
}


int main(int argc, char *argv[])
{
#ifdef BROADCAST
	//info(LOG_APPL,"Broadcast mode\n");
#else
	//info(LOG_APPL,"Broadcast mode off\n");
#endif

	// initialize the program as a daemon
	if(init_daemon() == -1) 
	{
		error(LOG_APPL, "INIT_DAEMON ERROR can't fork self");
		exit(1);
	}
	info(LOG_APPL,"Tomosysd started");


	// Read all the config file to generate Ax = b from matlab
	read_top_config_gout((char*)TOP_CONFIG_FILE,dim,rho,lambda,tolerance,max_rounds);
	//read_top_config_gout((char*)TOP_CONFIG_FILE,&dim,&rho,&lambda,&tolerance,&rounds,&max_rounds);
	vector_len = dim*dim;

	//every node I will create a cache where I have stored the initial slowness
	info(LOG_APPL,"Read dim : %d rho :%f lambda : %f tolerance : %f rounds : %f\n",dim,rho,lambda,tolerance,max_rounds);
	//rounds=10; //number of times of BART	

    params_t gossip_params;
    pthread_mutex_init (&gossip_params.mutex_system_state, NULL);
    gossip_params.system_state = IDLE;
    gossip_params.slow_self.assign(vector_len,0.0);
    gossip_params.gossip_tries=0;
    gossip_params.gossip_rounds=0;
    gossip_params.chosen_ip=NULL;
    gossip_params.slowalltobeaveraged.clear();

	read_slowness_model_gout("slow_self",gossip_params.slow_self);
	info(LOG_APPL,"Slow_self created of size %d %lf",gossip_params.slow_self.size(),gossip_params.slow_self[0]);

	//TODO: Remove comment to enable bayesian art
	read_A_idx((char*)AIDX_FILE, (char*) B_FILE, A_l,idx_l,b_l,dim);

	xFinal.clear();
	double wall1 = get_wall_time();
	SMPI_INIT((char*)GOSSIP_CONFIG_FILE, &gossip_params);

// make -f Makefile.linux clean; make -f Makefile.linux
	
	while(gossip_params.gossip_tries < MAX_GOSSIP_TRIES && gossip_params.gossip_rounds < MAX_GOSSIP_ROUNDS)
	{
		sleep(1);	
	}
	double wall2 = get_wall_time();	
	write_slowness_final("xFinal",xFinal);			
	return 0;
}

