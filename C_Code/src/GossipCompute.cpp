#include "networkutils.h"
#include "LogUtil.h"
#include "Serialization.h"
#include "SMPI.h"
#include "Tomography.h"
#include "GossipCompute.h"

void *gossip_compute(void *arg) {
	params_t *gossip_params = (params_t*)(arg);

	while(1) {
		if(gossip_params->system_state == GOSSIP_AVG) {
			compute_avg(gossip_params);
			gossip_params->gossip_rounds++;
			info(LOG_APPL,"Average complete. Solicited_tries is %d Successful Rounds %d\n", gossip_params->gossip_tries, gossip_params->gossip_rounds);
			change_system_state(gossip_params, BAYESIAN_ART);
		} else {
			usleep(5000);
		}
	}
}

void compute_avg(params_t *gossip_params) {
	//Add self so that we can do average
	gossip_params->slowalltobeaveraged.push_back(gossip_params->slow_self);
	double rows = gossip_params->slowalltobeaveraged.size();
	int cols = gossip_params->slowalltobeaveraged[0].size();
	double sum = 0;
		
	if(cols <= 0 || rows < 2) {
		error(LOG_APPL,"No value to average");
		return;
	}

	for(int i = 0; i < cols; i++)
	{
		for(int j = 0; j < rows; j++) 
		{
			sum = sum + gossip_params->slowalltobeaveraged[j][i];
		}
		gossip_params->slow_self[i]=(sum/rows); 		
		sum = 0;		
	}
	gossip_params->slowalltobeaveraged.clear();
}
