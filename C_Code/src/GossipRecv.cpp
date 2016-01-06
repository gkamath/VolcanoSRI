#include "networkutils.h"
#include "LogUtil.h"
#include "Serialization.h"
#include "GossipRecv.h"


void *gossip_recv(void *arg) {
	params_t *gossip_params = (params_t*)(arg);
	int binded = 0;
	int s;

	while(1) {
		if (binded == 0) {
			s = SMPI_BIND();
			if(s >= 0) {
				binded = 1;
			} else {
				continue;
			}
		}

		char buffer[BUFFER];
  		char src_ip[20];  		

		int status = SMPI_RECIEVE(s, buffer,BUFFER,src_ip);
		//info(LOG_APPL,"Message received from %s", src_ip);

		if(status <= 0) {
			continue;
		}		
		if(gossip_params->system_state == IDLE) {
			vector<double> slow_v;
			int solicit_status = unpacking(slow_v,buffer);	
			if(solicit_status == 0) {
				info(LOG_APPL,"Discarded: A solicited message was not received from %s when in %d mode", src_ip, gossip_params->system_state);
				continue;
			}
			pthread_mutex_lock(&gossip_params->mutex_system_state);
			if(gossip_params->system_state == IDLE) {
				gossip_params->system_state = UNSOLICIT_RECV;
			} 
			pthread_mutex_unlock(&gossip_params->mutex_system_state);
			if(gossip_params->system_state != UNSOLICIT_RECV) {
				continue;
			}
			info(LOG_APPL,"===============================================");
			info(LOG_APPL,"Starting unsolicit receive mode");
			gossip_params->gossip_tries++;
			/*vector<double> slow_v;
			unpacking(slow_v,buffer);	     			*/
    		gossip_params->slowalltobeaveraged.push_back(slow_v);	     			
   			info(LOG_APPL, "RECORDED:unsolicited message received from: %s, slow_v[0]: %lf", src_ip, slow_v[0]);
   			gossip_params->chosen_ip = src_ip;
   			change_system_state(gossip_params, UNSOLICIT_SEND);
   			info(LOG_APPL, "Changed system mode to UNSOLICITED SEND");
		} else if (gossip_params->system_state == SOLICIT_RECV) {
			if (strcmp(src_ip, gossip_params->chosen_ip) == 0) {
				vector<double> slow_v;
				int solicit_status = unpacking(slow_v,buffer);	     			
				if(solicit_status == 1) {
					info(LOG_APPL,"Discarded: A unsolicited message was not received from %s when in %d mode", src_ip, gossip_params->system_state);
					continue;
				}
				pthread_mutex_lock(&gossip_params->mutex_system_state);								
     			gossip_params->slowalltobeaveraged.push_back(slow_v);    			
     			info(LOG_APPL, "RECORDED:solicited message received from: %s, slow_v[0]: %lf", src_ip, slow_v[0]);
     			gossip_params->system_state = GOSSIP_AVG;
     			gossip_params->chosen_ip = NULL;
     			pthread_mutex_unlock(&gossip_params->mutex_system_state);
			} else {
				info(LOG_APPL, "DISCARDED:received from: %s when in SOLICIT_RECV mode", src_ip);
			}
		} else {
			info(LOG_APPL,"Discarded: Message received from %s when in %d mode", src_ip, gossip_params->system_state);
			//usleep(10000);
		}
	}
}



  
