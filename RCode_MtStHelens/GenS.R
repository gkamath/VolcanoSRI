GenS <- function(partitionNode,stationNum,No_of_col)
{
#   print("clusterSize = ",clusterSize)
  ssidx = rep(0,No_of_col)
  for(kk in 1:stationNum)
  {
    sindex = rep(0,No_of_col)
    
    Genssys = partitionNode[[kk]]
    rr = length(partitionNode[[kk]])
    for(i in 1:rr)
    {
      index = Genssys[[i]]$idx
      for(j in 1:length(index))
      {
        sindex[index[j]] = 1
      } 
      rm(index)
      
    }
      for(i in 1:No_of_col)
      {
        if(sindex[i])
        {
          ssidx[i] = ssidx[i] + 1
          
        }
      }
      
  }
    return(ssidx)
  }


GenSCluster <- function(partitionNode,stationNum,No_of_col,clusterSize)
{
  #   print("clusterSize = ",clusterSize)
  ssidx = rep(0,No_of_col)
  for(kk in 1:clusterSize)
  {
    sindex = rep(0,No_of_col)
    
    Genssys = partitionNode[[kk]]
    rr = length(partitionNode[[kk]])
    for(i in 1:rr)
    {
      index = Genssys[[i]]$idx
      for(j in 1:length(index))
      {
        sindex[index[j]] = 1
      } 
      rm(index)
      
    }
    for(i in 1:No_of_col)
    {
      if(sindex[i])
      {
        ssidx[i] = ssidx[i] + 1
        
      }
    }
    
  }
  return(ssidx)
}