#include<networkutils.h>
#include "LogUtil.h"
			
void my_ip_address(char *raw_serv)
	{		FILE *op;
			//op=fopen("log","w");
			int len;
			FILE* fp= popen("ip address | grep inet | grep eth0 | awk -F \" \" '{print $2}'| awk -F \"/\" '{print $1}'","r");
			fgets(raw_serv,20,fp);
			pclose(fp);
			len=strlen(raw_serv)-1;
			raw_serv[len]='\0';
			//fprintf(op,"%s has len %d\n",raw_serv,strlen(raw_serv));
			//fclose(op);	
	}

/*
void choose_random_neighbor(char* raw_serv)
	{	int len;
		FILE * fp = popen("ip route ls table 66 | awk -F\" \" '{ if($3 ==\"eth0\") print $1 }'|sort -R | head -n 1", "r");
		fgets(raw_serv,20,fp);
		pclose(fp);
		len=strlen(raw_serv)-1;
		raw_serv[len]='\0';
	}
*/
char** recv_list(int *p)
{
		char raw_serv[20];
		char **neighbor_ips;
		//char *temp;
		int len;
		FILE *fp;
		//if(broadcast==true)
			//{
			#ifdef BROADCAST
				fp = popen("ip route | awk -F\" \" '{ if($3 ==\"eth0\") print $1 }'|sort -R | grep -v \"10.4.33.1\" ", "r");
			//}	
			#else
			//{	
				//#ifdef GEO_CONSTR
					fp = popen("ip route  | awk -F\" \" '{ if($3 ==\"eth0\") print $1 }'|sort -R | grep -v \"10.4.33.1\"| head -n 1", "r");
				//#else				
				//	fp = popen("ip route ls table 66 | awk -F\" \" '{ print $1 }'|sort -R | head -n 1", "r");
				//#endif	
			//}	
			#endif
		int count=0;	
		while(fgets(raw_serv,20,fp)!=NULL)
			{
				if(count==0)
					{
						neighbor_ips=(char**)malloc(sizeof(char*));
					}
				else
					{
						neighbor_ips=(char**)realloc(neighbor_ips,sizeof(char*)*(count+1));
					}
				
				neighbor_ips[count]=(char*)malloc(sizeof(char)*20);	
				len=strlen(raw_serv)-1;
				raw_serv[len]='\0';
				strcpy(neighbor_ips[count],raw_serv);
				count++;				
			}
		*p=count;
		return neighbor_ips;
		pclose(fp);
}		
