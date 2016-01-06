#!/usr/bin/python

###############################################################
import os, datetime, time
from math import sqrt
from core import pycore
from core.misc import ipaddr
import WlanTopology
import utils
import signal, sys
#from core.constants import QUAGGA_STATE_DIR
###############################################################

# number of nodes

numnodes_x = 10
numnodes_y = 10

# program and configs location
testdir = os.path.expanduser('~')+"/code/tomosyscode/trunk"
sensordir = os.path.expanduser('~')+"/code/tomosyscode/trunk/tools/datagen"
configsdir = "/tools/configsgen/configs-core"

eventclidir = os.path.expanduser('~')+"/code/tomosyscode/trunk/tools/eventclient"
eventlocdir = os.path.expanduser('~')+"/code/tomosyscode/trunk/tools/eventlocation"

# IP subnet
prefix  = ipaddr.IPv4Prefix("172.16.0.0/16") # class B address

# logger in gloal space
logger = utils.Logger("test_tomo.log")
#EMANE_ENABLER = True
EMANE_ENABLER = False

node_list = [];

def signal_handler(signal, frame):
    print 'You pressed Ctrl+C!'
    #for idx in range(0, numnodes_x*numnodes_y):
    #    node_list[idx].term("killall ipaudit");
    #    node_list[idx].term("killall batmand");
    #    node_list[idx].term("killall olsrd");
    #    node_list[idx].term("killall BndlAdmin");
    #    node_list[idx].term("killall tomosysd");
    #sys.exit(0)

def main():
    logger.logbegin();
    try:
        signal.signal(signal.SIGINT, signal_handler)

        start = datetime.datetime.now()

        # construct and configure the session and topology
        session = pycore.Session(persistent = True)
        node_list = WlanTopology.createGridTopology(session, prefix,
                                     numnodes_x, numnodes_y, EMANE_ENABLER)

        routing_convergence_time = 2 * numnodes_x * numnodes_y #s
        #routing_convergence_time = 10
        print("waiting %s sec (routing table converges)" % routing_convergence_time)
        #time.sleep(routing_convergence_time)

        # log kernel routing table
        logger.info("Routing Tables of All Nodes")
        utils.logKernelRoutingTables(logger, node_list) # log routing tables10
        print("elapsed time for startup: %s" % (datetime.datetime.now() - start))
        logger.flush()

        landlord_list = [node_list[0], node_list[numnodes_y-1], node_list[(numnodes_x-1)*numnodes_y], node_list[numnodes_x*numnodes_y-1]];

        #### copy the config files and startup the daemons
        # node_list[0].term("bash");
        # node_list[4].term("bash");
        # node_list[18].term("bash");
        #for idx in range(1, numnodes_x*numnodes_y + 1):
        #    node_list[idx-1].term("bash");

        for y in range(0, numnodes_y):
            for x in range(0, numnodes_x):
                print("starting event client services on node (%d, %d)" % (x, y));
        utils.launchCoreDaemon(session)
        logger.logend();
    except Exception as e:
        logger.warn("catch exception: %s" % e)
        logger.logend()
        session.shutdown()
        #utils.killCoreProc()
    else:
        logger.logend()
        session.shutdown()
        #utils.killCoreProc()

def runIpPerfTests(session, n_src, n_dst):
    logger.info("=========Network Throughput Test Set==========")

    pos_src = n_src.getposition()
    pos_dst = n_dst.getposition()
    xdiff = pos_src[0] - pos_dst[0]
    ydiff = pos_src[1] - pos_dst[1]
    distance = sqrt(xdiff*xdiff + ydiff*ydiff)
    distance = distance / 100.0 * session.location.refscale
    logger.info("distance: %.03f" % distance)

    # start a shell on node 1
    #n_src.term("bash")
    #n_dst.term("bash")

    # test latency by ping command
    ttl = pingtest(n_src, n_dst, 10)
    logger.flush()

    # test bandwidth by iperf command
    iperftest(n_src, n_dst, ttl, 15)
    logger.flush()

    time.sleep(3); # reduce the interference between the two bandwidth testing

    # test dtn bandwidth
    bndltest(n_src, n_dst);
    logger.flush()

def pingtest(n_src, n_dst, count=50, verbose=False):
    ''' Ping through a chain of nodes and report the average latency.
    '''
    src_addr = n_src.netif(0).addrlist[0].split('/')[0]
    dst_addr = n_dst.netif(0).addrlist[0].split('/')[0]
    logger.info("ping from %s to %s..." % (src_addr, dst_addr))
    p = utils.PingCmd(n_src, verbose, dst_addr, count=count, interval=0.1).run()
    (latency, mdev, ttl) = p
    logger.info("get end-to-end latency (ms): avg-%.03f, mdev-%.03f, ttl-%d" % (latency, mdev, ttl))
    return ttl

def runBroadcastTest(source_bandwidth, root_list, node_list):
    logger.info("=========Broadcast Throughput Test==========")

    root_addr_list = []
    for root in root_list:
        root_addr_list.append(root.netif(0).addrlist[0].split('/')[0])

    logger.info("source bandwidth of broadcasting nodes is %d bps ..." % (source_bandwidth))
    logger.info("broadcasting test for root node list %s ..." % (" ".join(root_addr_list)))
    logger.flush()

#    collection_bps = 0
    union_broadcast_bandwidth = utils.BroadcastCmd(source_bandwidth, root_addr_list, node_list).run()

    logger.info("get broadcast throughput (bps): %.01f" % union_broadcast_bandwidth );
    logger.flush()

def runCollectionTest(numnodes_x, numnodes_y, source_bandwidth, root_list, node_list):
    logger.info("=========Collection Throughput Test==========")

    root_addr_list = []
    for root in root_list:
        root_addr_list.append(root.netif(0).addrlist[0].split('/')[0])

    logger.info("source bandwidth of normal nodes is %d bps ..." % (source_bandwidth))
    logger.info("collection test for root node list %s ..." % (" ".join(root_addr_list)))
    logger.flush()

#    collection_bps = 0
    union_collection_bandwidth = utils.ColletionCmd(numnodes_x, numnodes_y, source_bandwidth, root_addr_list, node_list).run()

    logger.info("get collection throughput (bps): %.01f" % union_collection_bandwidth );
    logger.flush()

def iperftest(n_client, n_server, ttl, time=20):
    ''' Run iperf through a chain of nodes and report the maximum
        throughput.
    '''
    server_addr = n_server.netif(0).addrlist[0].split('/')[0]
    client_addr = n_client.netif(0).addrlist[0].split('/')[0]
    logger.info("iperf from %s to %s ..." % (client_addr, server_addr))

    bps = 0
    if ttl is not 0:
        bps = utils.IperfCmd(n_server, n_client, verbose=False,
                         addr=server_addr, time=time).run()

    logger.info("get end-to-end throughput (bps): %s" % bps)
    return bps

def bndltest(n_client, n_server):
    server_addr = n_server.netif(0).addrlist[0].split('/')[0]
    client_addr = n_client.netif(0).addrlist[0].split('/')[0]
    logger.info("bundle message test from %s to %s ..." % (client_addr, server_addr))

    bndl_bps = utils.BndlPerfCmd(n_server, n_client, verbose=False,
                             addr=server_addr, time=time).run()
    logger.info("get hop-by-hop throughput (bps): %s" % bndl_bps );

if __name__ == "__main__":
    main()
