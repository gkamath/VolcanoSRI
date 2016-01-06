#include <stdio.h>
#include <string.h>
#include <string>
#include <sstream>
#include <time.h>
#include <pthread.h>

#include "Tomography.h"
#include "LogUtil.h"
#include "BayesianArt.h"
#include "readdatafrommatlab.h"

void *slowness_compute(void *arg) {
	params_t *gossip_params = (params_t*)(arg);

	while(1) {
		if(gossip_params->system_state == BAYESIAN_ART) {
			int rounds = 1;
			bayesian_art(A_l, idx_l, b_l, gossip_params->slow_self, vector_len, rho, lambda, tolerance, rounds);
			info(LOG_APPL,"Bayesian ART DONE. Putting System State back to IDLE mode");
			xFinal.push_back(gossip_params->slow_self);
			
			pthread_mutex_lock(&gossip_params->mutex_system_state);
			gossip_params->system_state = IDLE;
			pthread_mutex_unlock(&gossip_params->mutex_system_state);

		} else {
			usleep(5000);	
		}
	}
}
	
