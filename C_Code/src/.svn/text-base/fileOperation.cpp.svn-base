/*
 * fileOperation.cpp
 *
 *  Created on: Feb 16, 2014
 *      Author: danhuang
 */


#include "fileOperation.h"
#include "LogUtil.h"

using namespace std;

int count;

string readvector() {
	char * buffer = NULL;
	string str = "";
	size_t len = 0;
	FILE *fp = fopen("vector", "r");
	size_t read = 0;
	//read = getline(&buffer, &len, fp);
	if (NULL == fp) {
		printf("Error: You have not created file vector, exit! \n");
		writetofile("vector", "1\n0\n");
		return str;
	}
	bool isSize = true;
	while ((read = getline(&buffer, &len, fp)) != -1) {
		if (isSize == false) {
			if (strlen(buffer) > 0 && buffer[strlen(buffer) - 1] == '\n') {
				str[str.length() - 1] = ' ';
			}
			str.append(buffer);
			//strcat(str, " ");
		} else {
			isSize = false;
		}
	}
	if ((str.length()) > 0 && str[(str.length()) - 1] == ' ') {
		str[(str.length()) - 1] = '\0';
	}
	fclose(fp);
	return str;
}


int writetofile(const char* filename, const char* str) {

	FILE *f;
	f = fopen(filename, "w");

	fprintf(f, "%s", str);
	fclose(f);
}

int initvector() {
	//checking for file//

	/*struct stat st;
	int result = stat("vector", &st); // check for file
	//FILE *fp = fopen("vector", "r");
	if(result != 0)
	{
	//end//
		char s[20];
		sprintf(s, "1\n0");
		writetofile("vector", s);
	}
	//return 1;*/
	FILE *fp = fopen("vector", "r");
	if(NULL == fp)
	{
	char source[9000];
	char destination[9000];
	sprintf(source,"%d\n",64);
	strcat(destination,source);
	for (int i = 1; i < 64;i++)
	{
		sprintf(source,"%d\n",0);//initialize to zero
		strcat(destination,source);
	}
	writetofile("vector", destination);
	}

}

char* readroute() {
//return ips
	char* buffer = NULL;
	char * ips = new char[2000];
	strcpy(ips, "");
	size_t len = 0;
	size_t read = 0;
	system("route -n > routeinfo");
	FILE *fp = fopen("routeinfo", "r");
	while ((read = getline(&buffer, &len, fp)) != -1) {

		//printf("Retrieved line of length %zu :\n", read);
		//printf("%s \n", buffer);
		char * pch;
		pch = strtok(buffer, " ");

		if (strlen(pch) > 6 && strchr(pch, '.')
				&& !(pch[strlen(pch) - 2] == '.' && pch[strlen(pch) - 1] == '0')) {
			strcat(ips, pch);
			strcat(ips, ":");
		}
	}
	if (strlen(ips) > 0 && ips[strlen(ips) - 1] == ':') {
		ips[strlen(ips) - 1] = '\0';
	}

	//printf("%s \n", ips);
	fclose(fp);

	// test on local labtop
	//strcpy(ips, "127.0.0.1");

	return ips;
}


// a new thread used for watching file

void *watchpwdfile(void *arg) {
	int length, i = 0;
	int fd;
	int wd;
	char buffer[BUF_LEN];
	fd = inotify_init();
	//string curvector = readvector();
	//string newvector = "";
	if (fd < 0) {
		perror("inotify_init");
	}
	char cwd[1024];
	if (getcwd(cwd, sizeof(cwd)) != NULL)
		info(LOG_APPL, "Current working dir: %s\n", cwd);
	else
		perror("getcwd() error");
	wd = inotify_add_watch(fd, cwd,
			IN_MODIFY | IN_CREATE | IN_DELETE | IN_MOVED_TO);
	bool ismodified = false;
	while (!ismodified) {
		ismodified = false;
		length = read(fd, buffer, BUF_LEN);
		if (length < 0) {
			perror("read");
		}
		i = 0;
		while (i < length) {
			struct inotify_event *event = (struct inotify_event *) &buffer[i];
			//printf(" %s event.\n", event->name);
			if (event->len) {
				if (event->mask & IN_CREATE) {
					if (event->mask & IN_ISDIR) {
						//info(LOG_APPL,"The directory %s was created.\n", event->name);
					} else {
						//info(LOG_APPL,"The file %s was created.\n", event->name);
					}
				} else if (event->mask & IN_CLOSE_WRITE) {
					if (event->mask & IN_ISDIR) {
						//info(LOG_APPL,"The directory %s was deleted.\n", event->name);
					} else {
						//printf("The file %s was deleted.\n", event->name);
						//info(LOG_APPL,"The file %s was N_CLOSE_WRITE.\n", event->name);
						if (strstr(event->name, "summation") != NULL) {
							// vector file is modified then send message to main

						}
					}
				} else if (event->mask & IN_MODIFY) {
					if (event->mask & IN_ISDIR) {
						//info(LOG_APPL,"The file %s was modified.\n", event->name);

					} else {
						if (strcmp("summation", event->name) == 0) {
							//info(LOG_APPL,"The file %s was modified.\n", event->name);
							ismodified = true;
						}
					}
				} else if (event->mask & IN_MOVED_TO) {
					if (event->mask & IN_ISDIR) {
						//	printf("The directory %s was move to.\n", event->name);
					} else {
						//	printf("The file %s was move to.\n", event->name);
						if (strcmp("summation", event->name) == 0) {
							info(LOG_APPL,"The file %s was moved to.\n", event->name);
							ismodified = true;
						}
					}
				}
			}
			if (ismodified) {
				info(LOG_APPL,"Summation File New");
				break;
				/*	newvector = readvector();
				if ((curvector.compare(newvector)) != 0) {
					char msg[] = "vector change";
					printf("updated vector is : %s \n", newvector.c_str());
					printf("start to send vector change to main: %s \n", addr);
					sendmsgtoip(msg, addr);
					printf("finish to send vector change to main: %s \n", addr);

					curvector = newvector;
			} else {

			}*/
			}
			//ismodified = false;
			i += EVENT_SIZE + event->len;

		}

	}
	inotify_rm_watch(fd, wd);
	close(fd);

	sleep(1);
	pthread_exit(0);


}




// send msg to the machine of ip
/*
int sendmsgtoip(char* str, char* ip) {
	if (NULL == ip || strlen(ip) == 0) {
		printf("the ip of sendmsgtoip is empty! \n");
		return 0;
	}

	char logMsg[1024];
	struct sockaddr_in echoServAddr;  Echo server address
	int echoServPort = MAIN_SERVER_PORT;  Echo server port

	char *echoString;  String to send to echo server

	unsigned int echoStringLen;  Length of string to echo
	int bytesRcvd, totalBytesRcvd;

	int sock;

	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		sprintf(logMsg, "socket failed \n");
		printf("%s\n", logMsg);

		return -1;

	}
	memset(&echoServAddr, 0, sizeof(echoServAddr));  Zero out structure
	echoServAddr.sin_family = AF_INET;  Internet address family
	echoServAddr.sin_addr.s_addr = inet_addr(ip);  Server IP address
	echoServAddr.sin_port = htons(echoServPort);  Server port

	 Establish the connection to the echo server


	 Send the string to the server
	if (connect(sock, (struct sockaddr *) &echoServAddr, sizeof(echoServAddr))
			< 0) {
		sprintf(logMsg, "node connect to main failed %s \n", ip);
		printf("%s\n", logMsg);
		usleep(50000);
		return -1;

	}

	int buffer_len = strlen(str);


	if (send(sock, str, buffer_len, 0) != buffer_len) {
		sprintf(logMsg,
				"main node status: fail for sending request vector.!!!! ");
		if (isVerbose) {
			printf("%s\n", logMsg);

		}

		sleep(1);
	} else {
		sprintf(logMsg, " Main Node status: succeed for sending msg to %s. \n",
				ip);
		if (isVerbose) {

		}


	}


	 if ((bytesRcvd = read(*(int *) sock, buffer, MAX_BUNDLE_PAYLOAD_LEN + 1))
	 > 0) {

	 } else {

	 }
	close(sock);

	return 1;
}

*/
