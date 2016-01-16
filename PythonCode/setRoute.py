import sys
import os

DEFAULT = '10.4.33.1'
routeFile = open('./routeTable', 'r')
Neighbor=0
neighborIpAddress=[]
for line in routeFile:
    neighborIpAddress.append(line.rstrip())
    Neighbor=Neighbor+1
routeFile.close()

nonrouteFile = open('./nonrouteTable', 'r')
NotNeighbor=0
NotNeighborIpAddress=[]
for line in nonrouteFile:
    NotNeighborIpAddress.append(line.rstrip())
    NotNeighbor=NotNeighbor+1
nonrouteFile.close()

# print "Flushing Iptables and Creating Topology"
cmd = '/usr/sbin/iptables --flush'
print cmd
os.system(cmd)
cmd = '/sbin/route add -host '+DEFAULT+ ' eth0'
print cmd
os.system(cmd)

cmd = '/sbin/route del -net 10.4.0.0 netmask 255.255.0.0 eth0'
print cmd
os.system(cmd)
for j in range(0,Neighbor):        
    cmd = '/sbin/route add -host '+neighborIpAddress[j]+ ' eth0'
    print cmd
    os.system(cmd)

for j in range(0,NotNeighbor):       
    cmd = '/usr/sbin/iptables -I INPUT -s '+NotNeighborIpAddress[j]+ ' -j DROP'
    #print cmd
    os.system(cmd)

    cmd = '/usr/sbin/iptables -I OUTPUT -s '+NotNeighborIpAddress[j]+ ' -j DROP'
    #print cmd
    os.system(cmd)

