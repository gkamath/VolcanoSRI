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

class myThread (threading.Thread):
    def __init__(self, threadID, ipAddress):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.name = ipAddress        
    def run(self):
        #print "Starting " + self.name
        copyFiles(self.threadID, self.name)
        #print "Exiting " + self.name

# Define a function for the thread
def copyFiles( threadID,ipAddress):
    
    if exitFlag:
        thread.exit()
    else:
        ipsplit = ipAddress.split('.')
        j=ipsplit[2]
        k=ipsplit[3]

        print "Running Thread-%d %s" % (threadID,ipAddress)
        # cmd = 'ssh root@'+ipAddress+ ' mkdir /home/root/BBBCluster'
        # os.system(cmd)
        cmd = 'ssh root@'+ipAddress+ ' mkdir /home/root/BBBCluster/Results'    
        os.system(cmd)
        
        #copy all files
        cmd = 'scp ./Data/n-'+j+'-'+k+'/run* root@'+ipAddress+':~/BBBCluster/'
        os.system(cmd)

        #copy binary files also
        cmd = 'scp ./tomosysd root@'+ipAddress+':~/BBBCluster/'
        os.system(cmd)

        #Execute All the files
        print "Executing tomosysd in %s" %(ipAddress)
        cmd = 'ssh -n -f root@'+ipAddress+ ' "sh -c \'cd /home/root/BBBCluster; nohup ./run.sh >> tomo.log 2>&1 < /dev/null &\'\"'
        os.system(cmd)

ret = os.system("python healthCheck.py")
if ret == 0:
    print 'All IS GOOD'
else:
    print 'NODES DOWN'
    os._exit(1)

file = open('./ipAddress.txt', 'r')
Station=0
ipAddress=[]
for line in file:
    ipAddress.append(line.rstrip())
    Station=Station+1                           

for i in range(0,Station):
    thread = myThread(i, ipAddress[i])
    thread.start()

print "Exiting the Main thread"
