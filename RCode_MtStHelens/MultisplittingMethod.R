source("DirectMethods.R")
source("ARTMethods.R")
source("Lsqr.R")
source("ErrorAnalysis.R")

MultiSplit <- function(eqnSys, rows, cols, parts, rounds, ref = TRUE, refx = array(0, cols))
{
	if(cols %% parts != 0)
	{
		stop(paste("The system of", cols, "columns can not be partitioned exactly into", parts, "parts"))
	}

	subitem = array(list(), rows)
	subsystem = array(subitem, parts)

	subcols = cols/parts
	index = c(1:subcols)

	for(j in 1:parts)
	{
		a = (j-1)*subcols + 1
		b = j*subcols
		for(i in 1:rows)
		{
			subitem[[i]]$A = eqnSys[[i]]$A[a:b]
			subitem[[i]]$idx = index
			subitem[[i]]$b = eqnSys[[i]]$b
		}
		subsystem[[j]] = subitem
	}

	alpha = array(1/parts, parts)
	
	xx = matrix(0, parts, subcols)
	yy = matrix(0, parts, subcols)
	delta = matrix(0, parts, subcols)

	B = matrix(0, rows, parts)
	
	delta = yy-xx

	for(r in 1:rounds)
	{
		for(j in 1:parts)
		{
			for(i in 1:rows)
			{
				B[i,j] = 0
				for(k in 1:subcols)
				{
					B[i,j] = B[i,j] + subsystem[[j]][[i]]$A[k]*delta[j,k]
				}
			}
		}

# 		print(B)

		for(i in 1:parts)
		{
			for(j in 1:parts)
			{
				# deltaB = 0
				for(k in 1:rows)
				{
					if(j != i)
					{
						subsystem[[i]][[k]]$b = subsystem[[i]][[k]]$b - alpha[j]*B[k,j]
					}
				}
			}
		}

# 		for(i in 1:p)
# 		{
# 			for(j in 1:p)
# 			{
# 				if(j != i)
# 				{
# 					bb[,i] = bb[,i] - alpha[j]*B[,j]
# 				}
# 			}
# 		}

# 		print("&&&&&&&&&&&&&&&&&&")
# 		print(subsystem)
# 		print("&&&&&&&&&&&&&&&&&&")

		for(j in 1:parts)
		{
			yy[j,] = SolveQR(subsystem[[j]], m, n/parts)
		}

# 		print(yy)

		delta = yy-xx

# 		print(paste("----", delta))

		for(j in 1:parts)
		{
			xx[j,] = xx[j,] + delta[j,]*alpha[j]
		}

# 		print(r)
		if(ref)
		{
			normError = errorNorm(as.vector(t(xx)), refx)
			print(paste("== Iter", r, ", error:", normError))
		}
	}

	result = list()
	result$xx = xx
	result$subsystem = subsystem

	return(result)
}

MultiSplitART <- function(eqnSys, rows, cols, parts, rounds,ref = TRUE, refx = array(0, cols))
{
	if(cols %% parts != 0)
	{
		stop(paste("The system of", cols, "columns can not be partitioned exactly into", parts, "parts"))
	}

	subitem = array(list(), rows)
	subsystem = array(subitem, parts)

	subcols = cols/parts
	index = c(1:subcols)

	for(j in 1:parts)
	{
		a = (j-1)*subcols + 1
		b = j*subcols
		for(i in 1:rows)
		{
			subitem[[i]]$A = eqnSys[[i]]$A[a:b]
			subitem[[i]]$idx = index
			subitem[[i]]$b = eqnSys[[i]]$b
		}
		subsystem[[j]] = subitem
	}

	alpha = array(1/parts, parts)
	
	xx = matrix(0, parts, subcols)
	yy = matrix(0, parts, subcols)
	delta = matrix(0, parts, subcols)

	B = matrix(0, rows, parts)
	
	delta = yy-xx

	for(r in 1:rounds)	
	{
		for(j in 1:parts)
		{
			for(i in 1:rows)
			{
				B[i,j] = 0
				for(k in 1:subcols)
				{
					B[i,j] = B[i,j] + subsystem[[j]][[i]]$A[k]*delta[j,k]
				}
			}
		}

# 		print(B)

		for(i in 1:parts)
		{
			for(j in 1:parts)
			{
				# deltaB = 0
				for(k in 1:rows)
				{
					if(j != i)
					{
						subsystem[[i]][[k]]$b = subsystem[[i]][[k]]$b - alpha[j]*B[k,j]
					}
				}
			}
		}

		for(j in 1:parts)
		{
# 			y[j,] = SolveQR(subsystem[[j]], m, n/parts)
			initGuess = array(0, n/parts)
			rho = 1.0
			lambda = 1.0
			tolerance = 0.001
# 			yy[j,] = SolveBayesianART(subsystem[[j]], rows, 0, initGuess, rho, lambda, tolerance)
# 			yy[j,] = SolveART(subsystem[[j]], rows, initGuess, rho, tolerance)
			yy[j,] = SolveLsqr(subsystem[[j]], initGuess, rows)
		}

# 		print(y)

		delta = yy-xx

# 		print(paste("----", delta))

		for(j in 1:parts)
		{
			xx[j,] = xx[j,] + delta[j,]*alpha[j]
		}

# 		print(r)

		if(ref)
		{
			normError = errorNorm(as.vector(t(xx)), refx)
			print(paste("== Iter", r, ", error:", normError))
		}
	}

	result = list()
	result$xx = xx
	result$subsystem = subsystem

	return(result)
}