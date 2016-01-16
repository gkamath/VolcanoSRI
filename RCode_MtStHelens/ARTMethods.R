#### SolveART function definition
#### solve Ax = b by ART method
####
#### input:
####   eqnSys - an array of lists contains the information of equation system
####     eqnSys[[i]]$A - the non-zero values in row i of matrix A
####     eqnSys[[i]]$idx - the indexes of the corresponding non-zero values in row i of matrix A
####     eqnSys[[i]]$b - b[i] of column vector b
####   rows - rows of eqnSys
####   initGuess - initial guess of the estimation
####   maxRounds - max iterations to perform on eqnSys
#### output:
####   estimation of x
####

SolveART <- function(eqnSys, rows, initGuess, rho, tolerance, maxRounds = 1000)
{
	x = initGuess
	print(paste(">>>> ART Method >>>>", rows))
# 	write(paste(">>>> ART Method >>>>", rows), file="logs", append=TRUE)

	update = Inf
	j = 0
	
	while(j < maxRounds && update > tolerance)
	{
		j = j+1
		print(paste("    ART Iteration ====", j))
# 		write(paste("    ART Iteration ====", j), file="logs", append=TRUE)

		x0 = x

		for(i in 1:rows)
		{
			ai = eqnSys[[i]]$A
			bi = eqnSys[[i]]$b
			index = eqnSys[[i]]$idx

# 			m = length(ai)
			
			#as = 0.0
			#for(k in 1:m) {
			#	as = as + ai[k] * x[index[k]]
			#}

			as = ai %*% x[index]

			as = bi - as
			aa = ai %*% ai

			if(aa == 0.0)
			{
				next
			}

			num = as
			denom = aa

			gamma = (rho*num) / denom
				
			#for(k in 1:m) {
			#	x[index[k]] = x[index[k]] + gamma*ai[k]
			#}

			x[index] = x[index] + gamma*ai
		}
		rho = rho / 1.01

		deltaX = x-x0
		deltaXNorm = sqrt(deltaX %*% deltaX)
		x0Norm = sqrt(x0 %*% x0)
		update = deltaXNorm/x0Norm
		print(paste("    **** relative update: ", update))
	}
	
	print("<<<< End of ART <<<<")
	
	return(x)
}

#### SolveCarpART function definition
#### solve Ax = b by CARP method
####
#### input:
####   eqnSys - an array of lists contains the information of equation system
####     eqnSys[[i]]$A - the non-zero values in row i of matrix A
####     eqnSys[[i]]$idx - the indexes of the corresponding non-zero values in row i of matrix A
####     eqnSys[[i]]$b - b[i] of column vector b
####   rows - rows of eqnSys
####   initGuess - initial guess of the estimation
####   maxRounds - max iterations to perform on eqnSys
#### output:
####   estimation of x
####

SolveCarpART <- function(eqnSys, rows, initGuess, rho, tolerance, maxRounds =4,flag)
{
	x = initGuess
# 	print(paste(">>>> ART Method >>>>", rows))
# 	write(paste(">>>> ART Method >>>>", rows), file="logs", append=TRUE)
# 	rho = 2
	sindex = rep(0,No_of_col)
	for(i in 1:rows)
	{
	  index = eqnSys[[i]]$idx
	  for(j in 1:length(index))
	  {
	    sindex[index[j]] = sindex[index[j]]+1
	  } 
	  rm(index)
	  
	}

	update = Inf
	j = 0
	
	while(j < maxRounds)# && update > tolerance)
	{
		j = j+1
# 		print(paste("    ART Iteration ====", j))
# 		write(paste("    ART Iteration ====", j), file="logs", append=TRUE)

		x0 = x
		
		for(i in 1:rows)
		{
			ai = eqnSys[[i]]$A
			bi = eqnSys[[i]]$b
			index = eqnSys[[i]]$idx

			m = length(ai)
			
			as = 0.0
			for(k in 1:m) {
				as = as + ai[k] * x[index[k]]
			}
			as = bi - as
			if(flag == 1)
			{
			  aa = 0
			  for(k in 1:m) #BICAV
			  { 
			    aa = aa + sindex[index[k]]*ai[k]*ai[k]
			    
			  }
			}
			  else{
# 			    print(paste(">>>> Bayesian ART Method >>>>", ai,i)) 
			    aa = ai %*% ai
			  }
			
      
			num = as
			denom = aa

			gamma = (rho*num) / denom
				
			for(k in 1:m) {
				x[index[k]] = x[index[k]] + gamma*ai[k]
			}
		}
# 		rho = rho / 1.01

		deltaX = x-x0
		deltaXNorm = sqrt(deltaX %*% deltaX)
		x0Norm = sqrt(x0 %*% x0)
		update = deltaXNorm/x0Norm
# 		print(paste("    **** relative update: ", update,tolerance,maxRounds))
	}
	
# 	print("<<<< End of ART <<<<")
	
	return(x)
}

SolveCAV <- function(eqnSys, rows, initGuess, rho, tolerance, maxRounds =1,flag)
{
  x = initGuess
  # 	print(paste(">>>> ART Method >>>>", rows))
  # 	write(paste(">>>> ART Method >>>>", rows), file="logs", append=TRUE)
  # 	rho = 2
  sindex = rep(0,No_of_col)
  d = rep(0,No_of_col)
  for(i in 1:rows)
  {
    index = eqnSys[[i]]$idx
    for(j in 1:length(index))
    {
      sindex[index[j]] = sindex[index[j]]+1
    } 
    rm(index)
    
  }
  
  update = Inf
  j = 0
  
  while(j < maxRounds)# && update > tolerance)
  {
    j = j+1
    # 		print(paste("    ART Iteration ====", j))
    # 		write(paste("    ART Iteration ====", j), file="logs", append=TRUE)
    
#     x = d
    
    for(i in 1:rows)
    {
      ai = eqnSys[[i]]$A
      bi = eqnSys[[i]]$b
      index = eqnSys[[i]]$idx
      
      m = length(ai)
      
      as = 0.0
      for(k in 1:m) {
        as = as + ai[k] * x[index[k]]
      }
      as = bi - as
      
      aa = 0.0
      aa = ai %*% ai
      
#       for(k in 1:m) #CAV
#       { 
#         aa = aa + 10*sindex[index[k]]*ai[k]*ai[k]
#         
#       }
      
      
      
      num = as
      denom = aa
      
      gamma = (num) / denom
      
      for(k in 1:m) {
        d[index[k]] = d[index[k]] + gamma*ai[k]
      }
    }
    # 		rho = rho / 1.01
    
    # 		print(paste("    **** relative update: ", update,tolerance,maxRounds))
  }
  for(j in 1:length(d))
  {
    d[j]= d[j]/rows
  }
  # 	print("<<<< End of ART <<<<")
  
  return(d)
}


#### SolveBayesianART function definition
#### solve Ax = b by Bayesian ART method
####
#### input:
####   eqnSys - an array of lists contains the information of equation system
####     eqnSys[[i]]$A - the non-zero values in row i of matrix A
####     eqnSys[[i]]$idx - the indexes of the corresponding non-zero values in row i of matrix A
####     eqnSys[[i]]$b - b[i] of column vector b
####   rows - rows of eqnSys
####   initGuess - initial guess of the estimation
####   maxRounds - max iterations to perform on eqnSys
#### output:
####   estimation of x
####

SolveBayesianART <- function(eqnSys, rows, resDim, initGuess, rho, lambda, tolerance, maxRounds = 1000)
{
	x = initGuess
	xsmooth = initGuess
	print(paste(">>>> Bayesian ART Method >>>>", rows))
# 	write(paste(">>>> Bayesian ART Method >>>>", rows), file="logs", append=TRUE)
# 	print(rows)
# 	print(rho)
# 	print(lambda)
	update = Inf
	
	r = array(0, rows)
	
	totalUpdate = array(0, length(initGuess))
	
	j = 0

	while(j < maxRounds && update > tolerance)
	{
		j = j+1
		print(paste("    Bayesian ART Iteration ====", j, "iterations"))
# 		write(paste("    Bayesian ART Iteration ====", j), file="logs", append=TRUE)

		x0 = x
		
# 		flops = 0
		for(i in 1:rows)
		{
			ai = eqnSys[[i]]$A
			bi = eqnSys[[i]]$b
			index = eqnSys[[i]]$idx

			if(bi == 0.0)
			{
				next
			}
			
# 			print(paste("---- ", i))
# 			print(ai)
# 			print(bi)
# 			print(index)

# 			m = length(ai)
# 			flops = flops + m
			
			#as = 0.0
			#for(k in 1:m) {
			#	as = as + ai[k] * x[index[k]]
			#}

			as = ai %*% x[index]

			aa = ai %*% ai

			num = bi - lambda*r[i] - as
			denom = lambda*lambda + aa

			gamma = (rho*num) / denom
			
# 			print("++++++++")
# 			print(num)
# 			print(denom)
# 			print(length(r))
# 			print(gamma)
# 			print(lambda)
# 			print("++++++++")
			r[i] = r[i] + gamma*lambda
			
			#for(k in 1:m) {
			#	x[index[k]] = x[index[k]] + gamma*ai[k]
			#}

			x[index] = x[index] + gamma*ai
		}

		rho = rho / 1.01
		
		deltaX = x-x0
		deltaXNorm = sqrt(deltaX %*% deltaX)
		x0Norm = sqrt(x0 %*% x0)
		update = deltaXNorm/x0Norm
# 		print(paste("    **** deltaX, deltaXNorm, x0Norm: ", deltaX, deltaXNorm, x0Norm))
		print(paste("    **** relative update: ", update))
# 		write(paste("    **** relative update: ", update, " -- ", flops), file="logs", append=TRUE)
	}
	
	print(paste("<<<< End of Bayesian ART <<<<", j))
# 	write("<<<< End of Bayesian ART <<<<", file="logs", append=TRUE)
	
	return(x)
}

SolveBayesianARTPara <- function(eqnSys, rows, resDim, initGuess, rho, lambda, tolerance, maxRounds = 1000)
{
	x = initGuess
	xsmooth = initGuess
	print(paste(">>>> Bayesian ART Method >>>>", rows))
# 	write(paste(">>>> Bayesian ART Method >>>>", rows), file="logs", append=TRUE)
# 	print(rows)
# 	print(rho)
# 	print(lambda)
	update = Inf
	
	r = array(0, rows)
	
	totalUpdate = array(0, length(initGuess))
	
	j = 0

	while(j < maxRounds && update > tolerance)
	{
		j = j+1
		print(paste("    Bayesian ART Iteration ====", j, "iterations"))
# 		write(paste("    Bayesian ART Iteration ====", j), file="logs", append=TRUE)

		x0 = x
		
# 		flops = 0
		for(i in 1:rows)
		{
			ai = eqnSys[[i]]$A
			bi = eqnSys[[i]]$b
			index = eqnSys[[i]]$idx

			if(bi == 0.0)
			{
				next
			}
			
# 			print(paste("---- ", i))
# 			print(ai)
# 			print(bi)
# 			print(index)

			m = length(ai)
# 			flops = flops + m
			
			#as = 0.0
			#for(k in 1:m) {
			#	as = as + ai[k] * x[index[k]]
			#}

            as = ai %*% x[index]

			aa = ai %*% ai

			num = bi - lambda*r[i] - as
			denom = lambda*lambda + aa

			gamma = (rho*num) / denom
			
# 			print("++++++++")
# 			print(num)
# 			print(denom)
# 			print(length(r))
# 			print(gamma)
# 			print(lambda)
# 			print("++++++++")
			r[i] = r[i] + gamma*lambda
			
			#for(k in 1:m) {
			#	x[index[k]] = x[index[k]] + gamma*ai[k]
			#}

            x[index] = x[index] + gamma*ai
		}

		rho = rho / 1.01
		
		deltaX = x-x0
		deltaXNorm = sqrt(deltaX %*% deltaX)
		x0Norm = sqrt(x0 %*% x0)
		update = deltaXNorm/x0Norm
# 		print(paste("    **** deltaX, deltaXNorm, x0Norm: ", deltaX, deltaXNorm, x0Norm))
		print(paste("    **** relative update: ", update))
# 		write(paste("    **** relative update: ", update, " -- ", flops), file="logs", append=TRUE)
	}
	
	print(paste("<<<< End of Bayesian ART <<<<", j))
# 	write("<<<< End of Bayesian ART <<<<", file="logs", append=TRUE)

	result = list()
	result$x = x
	result$itrs = j
	
# 	return(x)
	return(result)
}

SolveBayesianARTSmooth <- function(eqnSys, rows, xRes, yRes, zRes, initGuess, rho, lambda, tolerance, maxRounds = 1000, smoothing_rate)
{
	x = initGuess
	xsmooth = initGuess
	print(paste(">>>> Bayesian ART Method >>>>", rows))
# 	write(paste(">>>> Bayesian ART Method >>>>", rows), file="logs", append=TRUE)
# 	print(rows)
# 	print(rho)
# 	print(lambda)
	update = Inf
	
	r = array(0, rows)
	
	totalUpdate = array(0, length(initGuess))
	
	j = 0

	while(j < maxRounds && update > tolerance)
	{
		j = j+1
		print(paste("    Bayesian ART Iteration ====", j, "iterations"))
# 		write(paste("    Bayesian ART Iteration ====", j), file="logs", append=TRUE)

		x0 = x
		
# 		flops = 0
		for(i in 1:rows)
		{
			ai = eqnSys[[i]]$A
			bi = eqnSys[[i]]$b
			index = eqnSys[[i]]$idx

			if(bi == 0.0)
			{
				next
			}
			
# 			print(paste("---- ", i))
# 			print(ai)
# 			print(bi)
# 			print(index)

			m = length(ai)
# 			flops = flops + m
			
			#as = 0.0
			#for(k in 1:m) {
			#	as = as + ai[k] * x[index[k]]
			#}

            as = ai %*% x[index]

			aa = ai %*% ai

			num = bi - lambda*r[i] - as
			denom = lambda*lambda + aa

			gamma = (rho*num) / denom
			
# 			print("++++++++")
# 			print(num)
# 			print(denom)
# 			print(length(r))
# 			print(gamma)
# 			print(lambda)
# 			print("++++++++")
			r[i] = r[i] + gamma*lambda
			
			#for(k in 1:m) {
			#	x[index[k]] = x[index[k]] + gamma*ai[k]
			#}

            x[index] = x[index] + gamma*ai
		}

		# Laplacian Smoothing Function Begin
		# 1. construct the smoothing matrix first
		idxCell = 0
		for(s_i in 1:xRes) {
			for(s_j in 1:yRes) {
				for(s_k in 1:zRes) {
					idxCell = idxCell + 1
					numAdjCell = 0
					xsmooth[idxCell] = 0
					if(s_k == 1) {
						numAdjCell = numAdjCell + 1
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell+1]
					}
					else if(s_k == zRes) {
						numAdjCell = numAdjCell + 1
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell-1]
					}
					else {
						numAdjCell = numAdjCell + 2
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell-1]
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell+1]
					}
					
					if(s_j == 1) {
						numAdjCell = numAdjCell + 1
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell+zRes]
					}
					else if(s_j == yRes) {
						numAdjCell = numAdjCell + 1
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell-zRes]
					}
					else {
						numAdjCell = numAdjCell + 2
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell-zRes]				
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell+zRes]
					}
					
					if(s_i == 1) {
						numAdjCell = numAdjCell + 1
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell+yRes*zRes]
					}
					else if(s_i == xRes) {
						numAdjCell = numAdjCell + 1
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell-yRes*zRes]
					}
					else {
						numAdjCell = numAdjCell + 2
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell-yRes*zRes]
						xsmooth[idxCell] =  xsmooth[idxCell] + x[idxCell+yRes*zRes]
					}
					xsmooth[idxCell] = xsmooth[idxCell] / numAdjCell
				}
			}
		}
		# 2. linearly combine the smoothing matrix with the original matrix
		x = (1 - smoothing_rate * rho) * x + smoothing_rate * rho * xsmooth

		rho = rho / 1.01
# 		rho = rho - 0.001
		
		deltaX = x-x0
		deltaXNorm = sqrt(deltaX %*% deltaX)
		x0Norm = sqrt(x0 %*% x0)
		update = deltaXNorm/x0Norm
# 		print(paste("    **** deltaX, deltaXNorm, x0Norm: ", deltaX, deltaXNorm, x0Norm))
		print(paste("    **** relative update: ", update))
# 		write(paste("    **** relative update: ", update, " -- ", flops), file="logs", append=TRUE)
	}
	
	print(paste("<<<< End of Bayesian ART <<<<", j))
# 	write("<<<< End of Bayesian ART <<<<", file="logs", append=TRUE)

	result = list()
	result$x = x
	result$itrs = j
	
# 	return(x)
	return(result)
}
