from pylab import *
import sys
import os
from topology import topology
import threading
import time

################################################################################
# This program is used to setup BBB Cluster with different topology such as:
# 'Ring','RGG','Star','Kn','Grid','Chain'
# ipAddress of the host is present in ipAddress.txt
#
################################################################################

topolog = 'Grid' #
exitFlag = 0
DEFAULT = '10.4.33.1'

file = open('./allIpAddress.txt', 'r')
f = open('./ipAddress.txt', 'w')
Station=0
ipAddress=[]
exitFlag = 0
str = '#'
for line in file:
	if((line.find(str))!=0):
		f.write(line)
		ipAddress.append(line.rstrip())
		Station = Station + 1
f.close()
ret = os.system("python healthCheck.py")
if ret == 0:
    print 'All IS WELL'
else:
    print 'NODES DOWN'
    os._exit(1)

class myThread (threading.Thread):
    def __init__(self, threadID, ipAddress):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.name = ipAddress        
    def run(self):        
        copyFiles(self.threadID, self.name)
        

# Define a function for the thread
def copyFiles( threadID,ipAddress):
    
    if exitFlag:
        thread.exit()
    else:
        cmd = 'scp ./ipFlush.sh root@'+ipAddress+':~/BBBCluster/'
    	os.system(cmd)

    	#print "Flushing IP's in %s" %(ipAddress)
    	cmd	 = 'ssh -n -f root@'+ipAddress+ ' "sh -c \'cd /home/root/BBBCluster; nohup ./ipFlush.sh > tomo.log 2>&1 < /dev/null &\'\"'
    	os.system(cmd)

threads = []

for i in range(0,Station):
    thread = myThread(i, ipAddress[i])
    thread.start()

    threads.append(thread)

print "Flushing the route tables...."

for thread in threads:
    thread.join()

print "Flushing Complete."

print "Preparing to Create a "+topolog+" topology"

if((topolog == 'Grid') and round(sqrt(Station))!=sqrt(Station)):
    print('For grid topology, the number of nodes has to be a square number! Choose a new n.')

adjMatrix=topology(topolog,Station)
(row,col) = adjMatrix.shape
print adjMatrix

for i in range(0,row):
    ipsplit = ipAddress[i].split('.')
    j=ipsplit[2]
    k=ipsplit[3]

    routeFilename = './Data/n-'+j+'-'+k+'/routeTable'
    routeFile = open(routeFilename,'wa')

    nonrouteFilename = './Data/n-'+j+'-'+k+'/nonrouteTable'
    nonrouteFile = open(nonrouteFilename,'wa')


    for j in range(0,col):
        if(i!=j):
            if((adjMatrix[i,j] == 1)):
                routeFile.write(ipAddress[j]+'\n')
            elif(adjMatrix[i,j] == 0):
                nonrouteFile.write(ipAddress[j]+'\n')
nonrouteFile.close()
routeFile.close()

class myThreadRouting (threading.Thread):
    def __init__(self, threadID, ipAddress):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.name = ipAddress        
    def run(self):        
        copyRoutingTables(self.threadID, self.name)
        

# Define a function for the thread
def copyRoutingTables( threadID,ipAddress):
    
    if exitFlag:
        thread.exit()
    else:
        ipsplit = ipAddress.split('.')
        j=ipsplit[2]
        k=ipsplit[3]

        cmd = 'scp ./Data/n-'+j+'-'+k+'/*routeTable ./setRoute.py root@'+ipAddress+':~/BBBCluster/'
        os.system(cmd)

        cmd = 'ssh -n -f root@'+ipAddress+' \"cd /home/root/BBBCluster; python setRoute.py > tomo.log 2>&1 < /dev/null &\"'
        os.system(cmd)

routingThreads = []

for i in range(0,Station):
    thread = myThreadRouting(i, ipAddress[i])
    thread.start()

    routingThreads.append(thread)

print "Creating the route tables...."

for thread in routingThreads:
    thread.join()

print "Route Table Complete."
