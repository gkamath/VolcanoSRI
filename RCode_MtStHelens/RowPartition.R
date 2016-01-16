RowPartition <- function(eqnsys,rows,stationNum,eventNum)
{
  #get stationnumber and eventnum
#   clusterSize = 10
  Node = array(list(array(list(),eventNum)),stationNum)
  for(i in 1:stationNum)
  {
    for(j in 1:eventNum)
    {
      Node[[i]][[j]]$A = eqnsys[[i+(j-1)*stationNum]]$A
      Node[[i]][[j]]$b = eqnsys[[i+(j-1)*stationNum]]$b
      Node[[i]][[j]]$idx = eqnsys[[i+(j-1)*stationNum]]$idx
    }
  }
  return(Node)
}

RowPartitionCluster <- function(eqnsys,rows,stationNum,eventNum,clusterSize)
{
  #get stationnumber and eventnum
  #   clusterSize = 10
  NC = (stationNum/clusterSize)
  cluster = array(list(array(list(),eventNum*NC)),clusterSize)
  Node = array(list(array(list(),eventNum)),stationNum)
  for(i in 1:stationNum)
  {
    for(j in 1:eventNum)
    {
      Node[[i]][[j]]$A = eqnsys[[i+(j-1)*stationNum]]$A
      Node[[i]][[j]]$b = eqnsys[[i+(j-1)*stationNum]]$b
      Node[[i]][[j]]$idx = eqnsys[[i+(j-1)*stationNum]]$idx
    }
  }
 
  print(paste("Initial Partition Done",NC))
  for(i in 1:clusterSize)
  {
    ii = 1
#     print(paste("i=",i))
    temp1 = ((i-1)*NC)+1
    temp2 = (i*NC)
    for(j in temp1:temp2)
    {
#       print(paste("j=",j))
      for(k in 1:eventNum)
      {
#         print(paste("k=",k))
        cluster[[i]][[(ii-1)*eventNum+k]]$A = Node[[j]][[k]]$A
        cluster[[i]][[(ii-1)*eventNum+k]]$b = Node[[j]][[k]]$b
        cluster[[i]][[(ii-1)*eventNum+k]]$idx = Node[[j]][[k]]$idx
      }
      ii = ii + 1
      
    }
   
  }
  
#   i = 0
#   j = 0
#   for(i in 1:5)
#   { print(paste("i = ",i))
#     for(j in ((i-1)*5+1):i*5)
#     {
#       print(paste("j = ",j))
#     }
#   }
    
  print("Cluster Partition Done")
  return(cluster)
}

RowShuffle <-function(eqnsys, rows,stationNum,eventNum,NoR)
{
  
  Node = array(list(array(list(),eventNum+NoR*stationNum)),stationNum)

  for(i in 1:stationNum)
  {
    for(j in 1:eventNum)
    {
      Node[[i]][[j]]$A = eqnsys[[i+(j-1)*stationNum]]$A
      Node[[i]][[j]]$b = eqnsys[[i+(j-1)*stationNum]]$b
      Node[[i]][[j]]$idx = eqnsys[[i+(j-1)*stationNum]]$idx
    }
  }
  
  for(i in 1:stationNum)
  {
   index1 = sample(1,rows,NoR*stationNum)
   k = 1
    for(j in (eventNum+1):(eventNum+NoR*stationNum))
    {
     
      Node[[i]][[j]]$A = eqnsys[[index1[k]]]$A
      Node[[i]][[j]]$b = eqnsys[[index1[k]]]$b
      Node[[i]][[j]]$idx = eqnsys[[index1[k]]]$idx
      k = k +1
    }
    
  }
  
  return(Node)
}

