#include "networkutils.h"
#include "LogUtil.h"
#include "Serialization.h"
#include "GossipUnsolicitSend.h"
#include "GossipSolicitSend.h"

void *gossip_unsolicit_send(void *arg) {
	params_t *gossip_params = (params_t*)(arg);

	while(1) {
		if (gossip_params->system_state == UNSOLICIT_SEND) {
			int status = gossip_to_neighbor(gossip_params->slow_self, gossip_params->chosen_ip, 0);
			gossip_params->chosen_ip = NULL;
			change_system_state(gossip_params,GOSSIP_AVG);
			info(LOG_APPL,"Completed unsolicit sending mode and status: %d", status);
		} else {
			usleep(5000);
		}
	}
}


  
