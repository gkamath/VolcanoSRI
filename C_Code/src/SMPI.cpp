#include<SMPI.h>				
#include<errno.h>

int * latestvalind;
int neighborcount;
char** neighborips;

//lock for random gossip
pthread_mutex_t wait_recv;

//variables related to data
int vector_len;

double WAIT_TIME;
int MAX_SLEEP_TIME;
int MAX_GOSSIP_TRIES;
int MAX_GOSSIP_ROUNDS;

int SMPI_SEND(char *DESTINATION,char *buffer, int buffer_len,int scope)
{
	struct sockaddr_in si_other;
	int s, i, slen=sizeof(si_other);
	//char buf[BUFLEN];
	//char message[BUFLEN];

	if ( (s=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1)
	{
		error(LOG_APPL,"SMPI_SEND:unable to initiate socket Error: %s", strerror(errno));
		return 0;
		//die("socket");
	}

	memset((char *) &si_other, 0, sizeof(si_other));
	si_other.sin_family = AF_INET;
	si_other.sin_port = htons(19999);
	inet_aton(DESTINATION, &si_other.sin_addr);
	int status= sendto(s, buffer,buffer_len , 0 , (struct sockaddr *) &si_other, slen);
	close(s);
	return status;

}

int SMPI_BIND() {
	struct sockaddr_in si_me;
	int s, i , recv_len;	

	//create a UDP socket
	if ((s=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1)
	{
		error(LOG_APPL,"SMPI_BIND:error creating socket! Error: %s", strerror(errno));
        return -1;		
	}

	// zero out the structure
	memset((char *) &si_me, 0, sizeof(si_me));

	si_me.sin_family = AF_INET;
	si_me.sin_port = htons(19999);
	si_me.sin_addr.s_addr = htonl(INADDR_ANY);

	//bind socket to port
	if( bind(s , (struct sockaddr*)&si_me, sizeof(si_me) ) == -1)
	{
		error(LOG_APPL,"SMPI_BIND:error binding socket! Error: %s", strerror(errno));
		return -1;		
	}  
	return s;
}

int SMPI_RECIEVE(int s, char *buffer, int buffer_max_len, char *src_ip)
{
	struct sockaddr_in si_other;
	int i , recv_len;
	socklen_t slen = sizeof(si_other);
	int status = recvfrom(s, buffer, buffer_max_len, 0, (struct sockaddr *) &si_other, &slen);
	info(LOG_APPL,"recvfrom status : %d",status);
	strcpy(src_ip,inet_ntoa(si_other.sin_addr));

	//unbind socket
	const int       optVal = 1;
	const socklen_t optLen = sizeof(optVal);

	//int rtn = setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (void*) &optVal, optLen);	
	return status;	
}

void SMPI_INIT(char* GOSSIP_CONFIG_FILE, params_t* gossip_params)
{	
	//load neighbors, create map 
	neighborips=recv_list(&neighborcount);
	latestvalind=(int*)malloc(neighborcount*sizeof(int));	
	info(LOG_APPL,"No of Neighbors %d",neighborcount);
	for(int i=0;i<neighborcount;i++)
	{
		latestvalind[i]=-1;
	}
	
	pthread_t gossip_send_thread_id;
	int gossip_send_thread_status;

	gossip_send_thread_status = pthread_create(&gossip_send_thread_id, NULL, &gossip_solicit_send, gossip_params);
	if(gossip_send_thread_status != 0)
	{
		error(LOG_APPL, "Gossip Send Thread creation failed.");
		exit(1);
	}

	pthread_t gossip_unsolicit_send_thread_id;
	int gossip_unsolicit_send_thread_status;

	gossip_unsolicit_send_thread_status = pthread_create(&gossip_unsolicit_send_thread_id, NULL, &gossip_unsolicit_send, gossip_params);
	if(gossip_unsolicit_send_thread_status != 0)
	{
		error(LOG_APPL, "Gossip Send Thread creation failed.");
		exit(1);
	}

	pthread_t gossip_recv_thread_id;
	int gossip_recv_thread_status;
	
	gossip_recv_thread_status = pthread_create(&gossip_recv_thread_id, NULL, &gossip_recv, gossip_params);
	if(gossip_recv_thread_status != 0)
	{
		error(LOG_APPL, "Gossip Recv Thread creation failed.");
		exit(1);
	}

	pthread_t gossip_compute_thread_id;
	int gossip_compute_thread_status;
	
	gossip_compute_thread_status = pthread_create(&gossip_compute_thread_id, NULL, &gossip_compute, gossip_params);
	if(gossip_compute_thread_status != 0)
	{
		error(LOG_APPL, "Gossip Compute Thread creation failed.");
		exit(1);
	}

	pthread_t slowness_compute_thread_id;
	int slowness_compute_thread_status;
	
	slowness_compute_thread_status = pthread_create(&slowness_compute_thread_id, NULL, &slowness_compute, gossip_params);
	if(slowness_compute_thread_status != 0)
	{
		error(LOG_APPL, "Bayesian Art Thread creation failed.");
		exit(1);
	}

	info(LOG_APPL,"All four threads created");

	//read gossip details
	if( access((const char*) GOSSIP_CONFIG_FILE, F_OK ) != -1 ) {
		// file exists
		read_gossip_config((char*)GOSSIP_CONFIG_FILE,WAIT_TIME,MAX_SLEEP_TIME,MAX_GOSSIP_ROUNDS,MAX_GOSSIP_TRIES);
		info(LOG_APPL,"Read WAIT_TIME : %f MAX_SLEEP_TIME :%d MAX_GOSSIP_ROUNDS : %d MAX_GOSSIP_TRIES : %d\n",WAIT_TIME,MAX_SLEEP_TIME,MAX_GOSSIP_ROUNDS,MAX_GOSSIP_TRIES);
	}
	else{
		
		WAIT_TIME=DEF_WAIT_TIME;
		info(LOG_APPL,"default wait time %f",WAIT_TIME);
		MAX_SLEEP_TIME = DEF_MAX_SLEEP_TIME;
		info(LOG_APPL,"default max_sleep time %f",MAX_SLEEP_TIME);
		MAX_GOSSIP_ROUNDS = DEF_MAX_GOSSIP_ROUNDS;
		info(LOG_APPL,"default MAX_GOSSIP_ROUNDS %d",MAX_GOSSIP_ROUNDS);
		MAX_GOSSIP_TRIES = DEF_MAX_GOSSIP_TRIES;
		info(LOG_APPL,"default solicit_tries %d",MAX_GOSSIP_TRIES);
	}		
}
