import random
import sys
import os

percentFail = 8

file = open('./ipAddress.txt', 'r')
Station=0
ipAddress=[]
for line in file:
    ipAddress.append(line.rstrip())
    Station=Station+1

for i in range(0,Station):
	ipsplit = ipAddress[i].split('.')
	j=ipsplit[2]
	k=ipsplit[3]
	cmd = 'cp run.sh ./Data/n-'+j+'-'+k+'/'
	os.system(cmd)

if(Station > percentFail):
	nodeList =  random.sample(range(1, Station), percentFail)
	print nodeList
	for nodes in nodeList:
		print ipAddress[nodes]
		ipsplit = ipAddress[nodes].split('.')
		j=ipsplit[2]
		k=ipsplit[3]

		cmd = 'cp runRobust.sh ./Data/n-'+j+'-'+k+'/run.sh'
		os.system(cmd)
else:
	print 'Not Enough Station to Drop'