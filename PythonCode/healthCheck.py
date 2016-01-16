import os
import threading
import thread
import sys

exitFlag = 0

class myThread (threading.Thread):
    def __init__(self, threadID, ipAddress):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.name = ipAddress        
    def run(self):        
        function(self.threadID, self.name)        

# Define a function for the thread
def function( threadID,ipAddress):
    
    if exitFlag:
        thread.exit()
    else:
        cmd = 'ping -c 1 -w2 '+ipAddress+' > /dev/null 2>&1'
        response = os.system(cmd)
        if response == 0:
            print ipAddress, 'is UP!'
        else:
            print ipAddress, 'is DOWN!'                        
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
