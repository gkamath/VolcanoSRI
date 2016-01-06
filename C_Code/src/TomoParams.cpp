#include "TomoParams.h"

void change_system_state(params_t *gossip_params, SYSTEM_STATES system_state) {
	pthread_mutex_lock(&gossip_params->mutex_system_state);
	gossip_params->system_state = system_state;	
	pthread_mutex_unlock(&gossip_params->mutex_system_state);
}

