#include "networkutils.h"
#include "LogUtil.h"
#include "Serialization.h"
#include "GossipSolicitSend.h"

void *gossip_solicit_send(void *arg) {
	params_t *gossip_params = (params_t*)(arg);

	while(1) {
		if(gossip_params->system_state == IDLE) {
			pthread_mutex_lock(&gossip_params->mutex_system_state);
			if(gossip_params->system_state == IDLE) {
				gossip_params->system_state = SOLICIT_SEND;
			} 
			pthread_mutex_unlock(&gossip_params->mutex_system_state);
			if(gossip_params->system_state != SOLICIT_SEND) {
				continue;
			}
			info(LOG_APPL,"===============================================");
			info(LOG_APPL,"Starting solicit sending mode");
			
			char neighbor_ip[20];
			int neighbor_count = get_random_neighbor(neighbor_ip);
			if (neighbor_count < 1) {
				change_system_state(gossip_params,IDLE);
				info(LOG_APPL,"Found no neighbors, went back to IDLE state");
				continue;
			}
			gossip_params->gossip_tries++;
			int status = gossip_to_neighbor(gossip_params->slow_self, neighbor_ip, 1);
			
			if (status > 0 ) {
				handle_solicit_receive(gossip_params,neighbor_ip);
		
			} else {
				change_system_state(gossip_params,IDLE);
			}

			info(LOG_APPL,"Completed solicit sending mode and status: %d", status);
			usleep(500000);
		} else {
			usleep(5000);
		}
	}
}

void handle_solicit_receive(params_t *gossip_params, char* neighbor_ip) {
	gossip_params->chosen_ip = neighbor_ip;
	change_system_state(gossip_params, SOLICIT_RECV);

	usleep(500000);

	if(gossip_params->system_state == SOLICIT_RECV) {
		//We waited for max wait time and we did not receive anything,
		// so lets put back the system state to idle.
		pthread_mutex_lock(&gossip_params->mutex_system_state);
		if(gossip_params->system_state == SOLICIT_RECV) {
			gossip_params->system_state = IDLE;
			info(LOG_APPL,"Solicit receieve did not come back, so putting system back to Idle");
		} 
		pthread_mutex_unlock(&gossip_params->mutex_system_state);
	}		
}

int get_random_neighbor(char* neighbor_ip) {
	int neighbor_count;
	char **neighbor_ips = recv_list(&neighbor_count);
				
	if(neighbor_count==0)
	{	
		info(LOG_APPL,"No neighbors found");
		return -1;
	} else {
		strcpy(neighbor_ip,neighbor_ips[0]);
		return neighbor_count;
	}
}

int gossip_to_neighbor(std::vector<double> slow_self, char* destination_ip, int solicit) {
	info(LOG_APPL,"Sending msg to neighbor:%s, slow %lf",destination_ip,slow_self[0]);
	char buffer[BUFFER];

	int length = packing(slow_self,buffer, solicit);
	int status=SMPI_SEND(destination_ip,buffer,length,1);									

	return status;
}

  
