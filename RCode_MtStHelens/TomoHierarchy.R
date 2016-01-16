library(sp)
library(splancs)
library(RTOMO)

source("./TomoPartition.R")
source("./ARTMethods.R")
source("./Lsqr.R")

#### IncreaseResolution function
#### input:
####   slowness - the original slowness model
####   resDim - resolution dimension
#### output:
####   slowness - slowness model with higher resolution

IncreaseResolution <- function(slowness, resDim)
{
	N = resDim
	resDim = resDim*2
		
	#### increase the resolution of the estimation
	NN = N*2
	slownesshi = array(0, NN*NN*NN)
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
							slownesshi[(i*2-a)*NN*NN + (j*2-b)*NN + k*2-c] = slowness[(i-1)*N*N + (j-1)*N + k]
						}
					}
				}
			}
		}
	}
	slowness = slownesshi

	return(slowness)
}

#### HierarchyTomoSolver function
#### input: 
####   levels - levels of hierarchies
####   startPartitionDim - first partition dimension
####   startResolutionDim - first resolution simension
####   stationNum - number of stations
####   eventNumList - a vector of number of events in each level
####   rounds - Bayesian ART iterations
####   rho - relaxiation parameter
####   lambda - regularization parameter
####   flag - which solver should be used (0-ApproxPartitionTomoSolver, 1-ExactPartitionTomoSolver)
####
#### output:
####   hierarchyResult - a list of the results
####     hierarchyResult[[i]]$slowness - the slowness model for level i
####     hierarchyResult[[i]]$filename - name of the slowness model log file for level i

HierarchyTomoSolver <- function(levels, cube, side, cubeDim, slowness, startPartitionDim, startResolutionDim, stationNum, eventNumList, rho, lambda, flag, tolerance, rounds, fullRank = FALSE, velMod = NULL)
{	
	hierarchyResult = array(list(), levels)
	
	partitionDim = startPartitionDim
	resolutionDim = startResolutionDim
	
	eventStart = 1

	for (lv in 1:levels)
	{
		vecLength = resolutionDim^3
		eventNum = eventNumList[lv]

		print("generating the systems")
		#### generate the systems
		if(is.null(velMod))
		{
			systems = GenerateAxb(cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventStart, eventNum, flag, fullRank)
		}else
		{
			systems = GenerateLayerAxb(cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventStart, eventNum, flag, velMod)
		}
		
		print(paste("zero columns in the system: ", length(which(systems$blocks == 0))))
		print(paste("system rows: ", systems$originRows))
		
		#### solve the tomography with conrresponding solver
		result = PartitionTomoSolver(systems, slowness, partitionDim, resolutionDim, rho, lambda, flag, tolerance, rounds)

		result$originSystem = systems$originSystem
		result$originRows = systems$originRows
		hierarchyResult[[lv]] = result

		#### update the initial guess
		slowness = result$slowness
		#### increase the velocity model resolution
		slowness = IncreaseResolution(slowness, resolutionDim)
		
		partitionDim = partitionDim*2
		resolutionDim = resolutionDim*2
		eventStart = eventStart + eventNum
	}
	
	return(hierarchyResult)
}
