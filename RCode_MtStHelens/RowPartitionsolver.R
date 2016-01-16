RowPartitionsolver <- function (eqnsys,rows,partitionNode,stationNum,resolutionDim,slowness,rho, lambda,tolerance, inner_loop,flag,slownum,eventNum)
{
  jjj = 1
  maxRounds = 200
  update = rep(0,maxRounds)
  sError = rep(0,maxRounds)
  update[jjj] = Inf
  sError[jjj] = Inf
  flag = 0;
  No_of_col = resolutionDim^3
  slowness_pert = array(0.0, No_of_col)
  while(jjj < maxRounds && update[jjj] > tolerance)
  {
    estx = matrix(0,No_of_col,stationNum)
    sumX = matrix(0,No_of_col,1)
    x0 = slowness_pert
    for (jj in 1:stationNum)
    { 
      sidx = matrix(0,No_of_col)
      sidx = NoUpdate(partitionNode[[jj]],No_of_col)
      for (aa in 1:No_of_col)
      {
        if(sidx[aa]==0)
        {
          slowness_pert[aa]=0
        }
      }
      
      flag = 1
      estx[,jj] = SolveCarpART(partitionNode[[jj]],length(partitionNode[[jj]]),slowness_pert,rho, tolerance, 4,flag)
#       estx[,jj] = SolveLsqr(partitionNode[[jj]], slowness_pert, length(partitionNode[[jj]]))
#       estx[,jj] = SolveBayesianART(partitionNode[[jj]], length(partitionNode[[jj]]), resolutionDim, slowness_pert, rho, lambda, tolerance, 4)
     for (aa in 1:No_of_col)
      {
        if(sidx[aa]==0)
        {
          estx[aa,jj]=0
        }
      }     
    }
    
      sumX = rowSums(estx)
      for (ii in 1:No_of_col)
      {
        if(slownum[ii]>=1)
        {
          slowness_pert[ii] = sumX[ii]/slownum[ii] #there is a problem here, when you divide by one
          #       slowness_pert_cav[ii] = slowness_pert_cav[ii]+ sumS[ii]*(rho)/stationNum
          #the error starts increasing. but when u divide by stationumer it works
        }
        else
        {
          slowness_pert[ii]=0
          #       slowness_pert_cav[ii] = 0
        }
      }
    
    
    
    jjj = jjj +1
    x = slowness_pert
    sError[jjj] = SquareError(eqnsys,eventNum*stationNum,slowness_pert)
    deltaX = x-x0
    deltaXNorm = sqrt(deltaX %*% deltaX)
    x0Norm = sqrt(x0 %*% x0)
    update[jjj] = deltaXNorm/x0Norm 
    print(paste("++++ update:", update[jjj],sError[jjj],tolerance,jjj))
    
  }
  
  slowness = slowness + slowness_pert
  vecLength = length(slowness)  
  output = array(0, vecLength+1)
  output[1] = resolutionDim
  output[2:(vecLength+1)] = slowness[1:vecLength]
  filename = paste("slowness_approx_", partitionDim, "_", resolutionDim, sep="")
  write(output, file = filename, sep = "\n")
  
  updateLength = length(update)
  updateOut = array(0,updateLength)
  updateOut[1:(updateLength)] = update[1:updateLength]
  filename1 = paste("relative_update", partitionDim, "_", resolutionDim, sep="")
  write(updateOut, file = filename1, sep = "\n")
  
  SELength = length(sError)
  sErrorOut = array(0,SELength)
  sErrorOut[1:(SELength)] = update[1:SELength]
  
  filename2 = paste("square_error", partitionDim, "_", resolutionDim, sep="")
  write(sErrorOut, file = filename2, sep = "\n")
#   SliceResGen(filename)
  result = list(slowness = slowness, filename = filename,iteration = jjj)
  return(result)
  
}




RowPartitionsolverCluster <- function (eqnsys,rows,partitionNode,stationNum,resolutionDim,slowness,rho, lambda,tolerance, inner_loop,flag,slownum,clusterSize,eventNum)
{
  
  jjj = 1
  maxRounds = 150
  update = rep(0,maxRounds)
  sError = rep(0,maxRounds)
  update[jjj] = Inf
  sError[jjj] = Inf
  flag = 0;
  No_of_col = resolutionDim^3
  slowness_pert = array(0.0, No_of_col)
  while(jjj < maxRounds && update[jjj] > tolerance)
  {
    estx = matrix(0,No_of_col,clusterSize)
    sumX = matrix(0,No_of_col,1)
    x0 = slowness_pert
    for (jj in 1:clusterSize)
    { 
      sidx = matrix(0,No_of_col)
      sidx = NoUpdate(partitionNode[[jj]],No_of_col)
      for (aa in 1:No_of_col)
      {
        if(sidx[aa]==0)
        {
          slowness_pert[aa]=0
        }
      }
      
      flag = 0 #flag = 1 Bi-CAV flag = 0 CARP
        estx[,jj] = SolveCarpART(partitionNode[[jj]],length(partitionNode[[jj]]),slowness_pert,rho, tolerance, 4,flag)
#       estx[,jj] = SolveCAV(partitionNode[[jj]],length(partitionNode[[jj]]),slowness_pert,rho, tolerance, 4,flag)
#       estx[,jj] = SolveBayesianART(partitionNode[[jj]], length(partitionNode[[jj]]), resolutionDim, slowness_pert, rho, lambda, tolerance, 4)
      for (aa in 1:No_of_col)
      {
        if(sidx[aa]==0)
        {
          estx[aa,jj]=0
        }
      }     
    }
#     randomno = runif(1, min=0, max=1)
#     if(randomno < 0.4)
#     {
#       packetloss = sample(1:clusterSize, 1)
#       estx[,packetloss] = rep(0,No_of_col)
#     }
    
  
   
#     print(paste("++++ Link Failure in the Cluster",packetloss))
    sumX = rowSums(estx)
    for (ii in 1:No_of_col)
    {
      if(slownum[ii]>=1)
      {
        slowness_pert[ii] = sumX[ii]/slownum[ii] #there is a problem here, when you divide by one
        #       slowness_pert_cav[ii] = slowness_pert_cav[ii]+ sumS[ii]*(rho)/stationNum
        #the error starts increasing. but when u divide by stationumer it works
      }
      else
      {
        slowness_pert[ii]=0
        #       slowness_pert_cav[ii] = 0
      }
    }
    
    
    
    jjj = jjj +1;
    x = slowness_pert
    sError[jjj] = SquareError(eqnsys,eventNum*stationNum,slowness_pert)
    #     sError = SquareError(systems$originSystem, systems$originRows,slowness_pert)
    deltaX = x-x0
    deltaXNorm = sqrt(deltaX %*% deltaX)
    x0Norm = sqrt(x0 %*% x0)
    update[jjj] = deltaXNorm/x0Norm
#     print(paste("++++ deltaXNorm X0Norm",deltaXNorm,x0Norm))
    print(paste("++++ Test update:", update[jjj],sError[jjj],tolerance,jjj))
  }
  
  slowness = slowness + slowness_pert
  vecLength = length(slowness)  
  output = array(0, vecLength+1)
  output[1] = resolutionDim
  output[2:(vecLength+1)] = slowness[1:vecLength]
  filename = paste("Test_slowness_approx", partitionDim, "_", resolutionDim, sep="")
  write(output, file = filename, sep = "\n")
  
  updateLength = length(update)
  updateOut = array(0,updateLength)
  updateOut[1:(updateLength)] = update[1:updateLength]
  filename1 = paste("Test_relative_update_rho", rho, "_", resolutionDim, sep="")
  write(updateOut, file = filename1, sep = "\n")
  
  SELength = length(sError)
  sErrorOut = array(0,SELength)
  sErrorOut[1:(SELength)] = sError[1:SELength]
  
  filename2 = paste("Test_square_error_rho", rho, "_", resolutionDim, sep="")
  write(sErrorOut, file = filename2, sep = "\n")
  
  #   SliceResGen(filename)
  result = list(slowness = slowness, filename = filename,iteration = jjj)
  return(result)
  
}

RowPartitionsolverClusterBaysian <- function (eqnsys,originrows,partitionNode,stationNum,resolutionDim,slowness,rho, lambda,tolerance, inner_loop,flag,slownum,clusterSize,eventNum)
{
  jjj = 1
  maxRounds = 200
  update = rep(0,maxRounds)
  updater = rep(0,maxRounds)
  dError1 = rep(0,maxRounds)
  dError2 = rep(0,maxRounds)
  dError3 = rep(0,maxRounds)
  sError = rep(0,maxRounds)

  update[jjj] = Inf
  sError[jjj] = Inf
  flag = 0;
  No_of_col = resolutionDim^3
  tempSlness = matrix(0,No_of_col,maxRounds)
  slowness_pert = array(0.0, No_of_col)
  rows = length(partitionNode[[1]])
  regular_vec = array(0.0,rows)
  while(jjj < maxRounds && update[jjj] > tolerance)
  {
    estx = matrix(0,No_of_col,clusterSize)
    extr = matrix(0,rows,clusterSize)
    sumX = matrix(0,No_of_col,1)
    x0 = slowness_pert
    r0 = regular_vec
    for (jj in 1:clusterSize)
    { 
      sidx = matrix(0,No_of_col)
      sidx = NoUpdate(partitionNode[[jj]],No_of_col)
      for (aa in 1:No_of_col)
      {
        if(sidx[aa]==0)
        {
          slowness_pert[aa]=0
        }
      }
      
      flag = 0
#       estx[,jj] = SolveCarpART(partitionNode[[jj]],length(partitionNode[[jj]]),slowness_pert,rho, tolerance, 4,flag)
       ans = SolveBayesianART(partitionNode[[jj]], length(partitionNode[[jj]]), resolutionDim, slowness_pert,regular_vec, rho, lambda, tolerance, 4)
      estx[,jj] = ans$x
      extr[,jj] = ans$r
      for (aa in 1:No_of_col)
      {
        if(sidx[aa]==0)
        {
          estx[aa,jj]=0
        }
      }     
    }
    
    sumX = rowSums(estx)
    sumr = rowSums(extr)
    for(aj in 1:rows)
    {
      regular_vec[aj] = sumr[aj]/clusterSize
    }
    
    for (ii in 1:No_of_col)
    {
      if(slownum[ii]>=1)
      {
        slowness_pert[ii] = sumX[ii]/slownum[ii] #there is a problem here, when you divide by one
        #       slowness_pert_cav[ii] = slowness_pert_cav[ii]+ sumS[ii]*(rho)/stationNum
        #the error starts increasing. but when u divide by stationumer it works
      }
      else
      {
        slowness_pert[ii]=0
        #       slowness_pert_cav[ii] = 0
      }
    }
    
    
    
    jjj = jjj +1;
    x = slowness_pert
    r = regular_vec
#     print(paste("++++ length of origin eqn",length(eqnsys),originrows))
    sError[jjj] = SquareError(eqnsys,eventNum*stationNum,slowness_pert)
    #     sError = SquareError(systems$originSystem, systems$originRows,slowness_pert)
    deltar = r-r0
    deltaX = x-x0
    deltaXNorm = sqrt(deltaX %*% deltaX)
    deltarNorm = sqrt(deltar %*% deltar)
    x0Norm = sqrt(x0 %*% x0)
    r0Norm = sqrt(r0 %*% r0)
#     updater[jjj]= deltarNorm/r0Norm
    update[jjj] = deltaXNorm/x0Norm
#     tempSlness[,jjj] = slowness + slowness_pert    
#     errorD = dError(cube, tempSlness)
    #     print(paste("++++ deltaXNorm X0Norm",deltaXNorm,x0Norm))
    print(paste("++++ update:", update[jjj],sError[jjj],updater[jjj],jjj))
#     print(paste("++++ update:",jjj))
  }
#   for(ab in 1:jjj)
#   {
#     errorD = dError(cube, tempSlness[,ab])
#     dError1[ab] = errorD$d1
#     dError2[ab] = errorD$d2
#     dError3[ab] = errorD$d3
# #     print(paste("++++ update:", update[ab],errorD$d1,errorD$d2,errorD$d3,jjj))
#   }
  
  slowness = slowness + slowness_pert
  vecLength = length(slowness)  
  output = array(0, vecLength+1)
  output[1] = resolutionDim
  output[2:(vecLength+1)] = slowness[1:vecLength]
  filename = paste("BART_slowness_approx_", clusterSize, "_", resolutionDim, sep="")
  write(output, file = filename, sep = "\n")
  
  updateLength = length(update)
  updateOut = array(0,updateLength)
  updateOut[1:(updateLength)] = update[1:updateLength]
  filename1 = paste("BART_relative_update", clusterSize, "_", resolutionDim, sep="")
  write(updateOut, file = filename1, sep = "\n")
  
  SELength = length(sError)
  sErrorOut = array(0,SELength)
  sErrorOut[1:(SELength)] = sError[1:SELength]  
  filename2 = paste("BART_square_error", clusterSize, "_", resolutionDim, sep="")
  write(sErrorOut, file = filename2, sep = "\n")
  
  dELength = length(dError1)
  dErrorOut = array(0,dELength)
  dErrorOut[1:(dELength)] = dError1[1:dELength]  
  filename3 = paste("BART_d_error_1_", clusterSize, "_", resolutionDim, sep="")
  write(dErrorOut, file = filename3, sep = "\n")
  
  dELength = length(dError2)
  dErrorOut = array(0,dELength)
  dErrorOut[1:(dELength)] = dError2[1:dELength]  
  filename4 = paste("BART_d_error_2_", clusterSize, "_", resolutionDim, sep="")
  write(dErrorOut, file = filename4, sep = "\n")
  
  dELength = length(dError3)
  dErrorOut = array(0,dELength)
  dErrorOut[1:(dELength)] = dError3[1:dELength]  
  filename5 = paste("BART_d_error_3_", clusterSize, "_", resolutionDim, sep="")
  write(dErrorOut, file = filename5, sep = "\n")
  
  #   SliceResGen(filename)
  result = list(slowness = slowness, filename = filename,iteration = jjj)
  return(result)
  
}


RowPartitionsolverClusterCAV <- function (eqnsys,rows,partitionNode,stationNum,resolutionDim,slowness,rho, lambda,tolerance, inner_loop,flag,slownum,clusterSize,eventNum)
{
  jjj = 1
  maxRounds = 150
  update = rep(0,maxRounds)
  sError = rep(0,maxRounds)
  update[jjj] = Inf
  sError[jjj] = Inf
  flag = 0;
  No_of_col = resolutionDim^3
  slowness_pert = array(0.0, No_of_col)
  while(jjj < maxRounds && update[jjj] > tolerance)
  {
    estx = matrix(0,No_of_col,clusterSize)
    sumX = matrix(0,No_of_col,1)
    x0 = slowness_pert
    for (jj in 1:clusterSize)
    { 
      sidx = matrix(0,No_of_col)
      sidx = NoUpdate(partitionNode[[jj]],No_of_col)
      for (aa in 1:No_of_col)
      {
        if(sidx[aa]==0)
        {
          slowness_pert[aa]=0
        }
      }
      
      flag = 0 #flag = 1 Bi-CAV flag = 0 CARP
#       estx[,jj] = SolveCarpART(partitionNode[[jj]],length(partitionNode[[jj]]),slowness_pert,rho, tolerance, 4,flag)
    estx[,jj] = SolveCAV(partitionNode[[jj]],length(partitionNode[[jj]]),slowness_pert,rho, tolerance, 1,flag)
      #       estx[,jj] = SolveBayesianART(partitionNode[[jj]], length(partitionNode[[jj]]), resolutionDim, slowness_pert, rho, lambda, tolerance, 4)
      for (aa in 1:No_of_col)
      {
        if(sidx[aa]==0)
        {
          estx[aa,jj]=0
        }
      }     
    }
    
    sumX = rowSums(estx)
    sumX = sumX*(rho)
    
#     for (ii in 1:No_of_col)
#     {
#       if(slownum[ii]>=1)
#       {
#         sumX[ii] = sumX[ii]/slownum[ii] #there is a problem here, when you divide by one
#         #       slowness_pert_cav[ii] = slowness_pert_cav[ii]+ sumS[ii]*(rho)/stationNum
#         #the error starts increasing. but when u divide by stationumer it works
#       }
#       else
#       {
#        sumX[ii]=0
#         #       slowness_pert_cav[ii] = 0
#       }
#     }
#     
    slowness_pert = slowness_pert + sumX
#   rho = rho/1.01
    jjj = jjj +1;
    x = slowness_pert
    sError[jjj] = SquareError(eqnsys,eventNum*stationNum,slowness_pert)
    #     sError = SquareError(systems$originSystem, systems$originRows,slowness_pert)
    deltaX = x-x0
    deltaXNorm = sqrt(deltaX %*% deltaX)
    x0Norm = sqrt(x0 %*% x0)
    update[jjj] = deltaXNorm/x0Norm
    #     print(paste("++++ deltaXNorm X0Norm",deltaXNorm,x0Norm))
    print(paste("++++ update:", update[jjj],sError[jjj],tolerance,jjj))
  }
  
  slowness = slowness + slowness_pert
  vecLength = length(slowness)  
  output = array(0, vecLength+1)
  output[1] = resolutionDim
  output[2:(vecLength+1)] = slowness[1:vecLength]
  filename = paste("CAV_slowness_approx", partitionDim, "_", resolutionDim, sep="")
  write(output, file = filename, sep = "\n")
  
  updateLength = length(update)
  updateOut = array(0,updateLength)
  updateOut[1:(updateLength)] = update[1:updateLength]
  filename1 = paste("CAV_relative_update", rho, "_", resolutionDim, sep="")
  write(updateOut, file = filename1, sep = "\n")
  
  SELength = length(sError)
  sErrorOut = array(0,SELength)
  sErrorOut[1:(SELength)] = sError[1:SELength]
  
  filename2 = paste("CAV_square_error", rho, "_", resolutionDim, sep="")
  write(sErrorOut, file = filename2, sep = "\n")
  
  #   SliceResGen(filename)
  result = list(slowness = slowness, filename = filename,iteration = jjj)
  return(result)
  
}


step1 <- function (eqnsys,rows,partitionNode,stationNum,resolutionDim,slowness,rho, lambda,tolerance, inner_loop,flag,slownum,clusterSize,eventNum)
{
  
  jjj = 1
  maxRounds = 2
  update = rep(0,maxRounds)
  sError = rep(0,maxRounds)
  update[jjj] = Inf
  sError[jjj] = Inf
  flag = 0;
  No_of_col = resolutionDim^3
  slowness_pert = array(0.0, No_of_col)
  slowness_init = matrix(0,No_of_col,clusterSize)
  while(jjj < maxRounds)
  {
    estx = matrix(0,No_of_col,clusterSize)
    sumX = matrix(0,No_of_col,1)
    x0 = slowness_pert
    for (jj in 1:clusterSize)
    { 
      sidx = matrix(0,No_of_col)
      sidx = NoUpdate(partitionNode[[jj]],No_of_col)
      for (aa in 1:No_of_col)
      {
        if(sidx[aa]==0)
        {
          slowness_pert[aa]=0
        }
      }
      
      flag = 0 #flag = 1 Bi-CAV flag = 0 CARP
      estx[,jj] = SolveLsqr(partitionNode[[jj]], slowness_pert, length(partitionNode[[jj]]))
      #       sErrortest = SquareError(partitionNode[[jj]],length(partitionNode[[jj]]),slowness_pert)
      #       print(paste("++++ LSQR update:",sErrortest))
    }    
    #     print(paste("++++ Link Failure in the Cluster",packetloss))
    sumX = rowSums(estx)
    for (ii in 1:No_of_col)
    {
      if(slownum[ii]>=1)
      {
        slowness_pert[ii] = sumX[ii]/slownum[ii] #there is a problem here, when you divide by one
        #       slowness_pert_cav[ii] = slowness_pert_cav[ii]+ sumS[ii]*(rho)/stationNum
        #the error starts increasing. but when u divide by stationumer it works
      }
      else
      {
        slowness_pert[ii]=0
        #       slowness_pert_cav[ii] = 0
      }
    }
    
    
    
    jjj = jjj +1;
    x = slowness_pert
    sError[jjj] = SquareError(eqnsys,eventNum*stationNum,slowness_pert)
    #     sError = SquareError(systems$originSystem, systems$originRows,slowness_pert)
    deltaX = x-x0
    deltaXNorm = sqrt(deltaX %*% deltaX)
    x0Norm = sqrt(x0 %*% x0)
    update[jjj] = deltaXNorm/x0Norm
    #     print(paste("++++ deltaXNorm X0Norm",deltaXNorm,x0Norm))
    #     print(paste("++++ Test update:", update[jjj],sError[jjj],tolerance,jjj))
  }
  
  slowness = slowness + slowness_pert
  
  vecLength = length(slowness)  
  output = array(0, vecLength+1)
  output[1] = resolutionDim
  output[2:(vecLength+1)] = slowness[1:vecLength]
  filename = paste("Step1", partitionDim, "_", resolutionDim, sep="")
  write(output, file = filename, sep = "\n")
  result = list(slowness = slowness, slowness_pert = slowness_pert,filename = filename,iteration = jjj)
  return(result)
  
}


step4 <- function (partitionNodefiner,partitionNodecoarser,resolutionDim,slowness_pert_finer,clusterSize,slownum)
{
  No_of_col = resolutionDim^3
  No_of_col_coarse = (resolutionDim/2)^3
  #   m = length(partitionNodefiner[[1]])
  estd = matrix(0,length(partitionNodefiner[[1]]),clusterSize) 
  estv = matrix(0,No_of_col_coarse,clusterSize)
  slowness_pert = array(0.0, No_of_col_coarse)
  x = slowness_pert_finer
  
  
  for (jj in 1:clusterSize)
  {       
    eqnsys = partitionNodefiner[[jj]]
    estd[,jj] = residualError(eqnsys,length(partitionNodefiner[[jj]]),x)
    m = length(estd[,jj])
    partnodelength =  length(partitionNodecoarser[[jj]])
    #     m =  min(m,partnodelength)
    for(iii in 1:partnodelength)
    {
      partitionNodecoarser[[jj]][[iii]]$b = estd[iii,jj]
    }
    #      print(paste("Residual Error Done"))
    #      
    estv[,jj] = SolveLsqr(partitionNodecoarser[[jj]], slowness_pert, length(partitionNodecoarser[[jj]]))
    #      print(paste("LSQR Done"))
  }
  sumv = rowSums(estv)
  for (ii in 1:No_of_col_coarse)
  {
    if(slownum[ii]>=1)
    {
      slowness_pert[ii] = sumv[ii]/slownum[ii] #there is a problem here, when you divide by one
      #       slowness_pert_cav[ii] = slowness_pert_cav[ii]+ sumS[ii]*(rho)/stationNum
      #the error starts increasing. but when u divide by stationumer it works
    }
    else
    {
      slowness_pert[ii]=0
      #       slowness_pert_cav[ii] = 0
    }
  }
  
  return(slowness_pert)
  
}
