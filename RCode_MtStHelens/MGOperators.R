#### Restriction Operator
Restriction <- function(vector, resDim)
{
	NN = resDim
	#### increase the resolution of the estimation
	N = NN/2
	vectorlow = array(0, N*N*N)
	for(i in 1:N)
	{
		for(j in 1:N)
		{
			for(k in 1:N)
			{
				vecsum = 0
				for (a in 1:2)
				{
					for (b in 1:2)
					{
						for (c in 0:1)
						{
							vecsum = vecsum + vector[(i*2-a)*NN*NN + (j*2-b)*NN + k*2-c]
						}
					}
				}
				vectorlow[(i-1)*N*N + (j-1)*N + k] = vecsum/8
			}
		}
	}

	return(vectorlow)
}

#### Prolongation Operator
Prolongation <- function(vector, resDim)
{
	N = resDim
	#### increase the resolution of the estimation
	NN = N*2
	vectorhi = array(0, NN*NN*NN)
	for(i in 1:N)
	{
		for(j in 1:N)
		{
			for(k in 1:N)
			{
				for (a in 1:2)
				{
					for (b in 1:2)
					{
						for (c in 0:1)
						{
							vectorhi[(i*2-a)*NN*NN + (j*2-b)*NN + k*2-c] = vector[(i-1)*N*N + (j-1)*N + k]
						}
					}
				}
			}
		}
	}

	return(vectorhi)
}

#### compute residuals
SubResiduals<- function(subSystem, slowness)
{
	rows = subSystem$rayNum
	residuals = array(0, rows)

	for(i in 1:rows)
	{
		residuals[i] = subSystem[[i]]$b - (subSystem[[i]]$A %*% slowness[subSystem[[i]]$idx])
	}

	return(residuals)
}