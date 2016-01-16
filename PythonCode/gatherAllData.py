import datetime
import time
import os
import threading
import thread

exitFlag = 0

class myThread (threading.Thread):
    def __init__(self, threadID, ipAddress,st):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.name = ipAddress        
    def run(self):
        #print "Starting " + self.name
        copyFiles(self.threadID, self.name,st)
        #print "Exiting " + self.name

# Define a function for the thread
def copyFiles( threadID,ipAddress,st):
    if exitFlag:
        thread.exit()
    else:
	cmd = 'ssh root@'+ipAddress+' killall -9 tomosysd'
	os.system(cmd)

	cmd = 'mkdir ./Results/'+st+'/'+ipAddress
	os.system(cmd)

	directory = './Results/'+st+'/'+ipAddress+'/'

	cmd = 'scp -r root@'+ipAddress+':~/BBBCluster/Results '+directory
	os.system(cmd)

	cmd = 'scp -r root@'+ipAddress+':~/BBBCluster/tomo.log '+directory
	os.system(cmd)

    cmd = 'scp -r root@'+ipAddress+':~/BBBCluster/xFinal '+directory
    os.system(cmd)

    cmd = 'ssh root@'+ipAddress+' \"rm ~/BBBCluster/tomo.log\"'
    os.system(cmd)

    cmd = 'ssh root@'+ipAddress+' \"rm ~/BBBCluster/xFinal\"'
    os.system(cmd)

    cmd = 'ssh root@'+ipAddress+' \"rm -r ~/BBBCluster/Results\"'
    os.system(cmd)
	
ts = time.time()
st = datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d-%H-%M-%S')
file = open('./ipAddress.txt', 'r')
Station=0
ipAddress=[]
for line in file:
    ipAddress.append(line.rstrip())
    Station=Station+1

cmd = 'mkdir ./Results/'+st
os.system(cmd)

for i in range(0,Station):
    thread = myThread(i, ipAddress[i],st)
    thread.start()