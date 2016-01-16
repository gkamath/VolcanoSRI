#### Example 

library(RTOMO)
source("./TomoGenerate.R")

# definition of ray tracing models
TRACING_MODEL_3D_LINE = 0 # raytracing through straight lines
TRACING_MODEL_1D_LAYER = 1 # raytracing with 1D layer model 

# definition of ray distributions
STATION_ONTOP_DISTRIBUTION =0   # stations on the top of cube only
STATION_RANDOM_DISTRIBUTION = 1 # station can be anywhere in the cube

LAYER_MODEL = TRACING_MODEL_3D_LINE
RAY_DISTRIBUTION = STATION_RANDOM_DISTRIBUTION

STATIONS_LOG = "./stationslog"
EVENTS_LOG = "./eventslog"
SLOWNESS_TRUTH = "./slowness_truth"

cubeDim = 10
velocity = 4.5 # velocity: non-megma volcano area velocity for generating artifical volcano model

data(VELMOD1D)
velMod = VELMOD1D # velMod: 1D velocity model used for layer model

side = 10.0 # side: cubic area side length/width/height

#####################################
#### Generate artificial 3D volcano model
#####################################
print(">>>> Generating synthetic 3D volcano model")
if(LAYER_MODEL == TRACING_MODEL_3D_LINE)
{
	cube = GenerateCube(side, cubeDim, velocity)
}else if(LAYER_MODEL == TRACING_MODEL_1D_LAYER)
{
	cube = GenerateLayeredCube(side, cubeDim, velMod)
}
print("<<<< DONE Generating synthetic 3D volcano model")

#####################################
#### Generate event and station locations
#####################################
stationNum = 100

#eventNumList = c(startingNumOfEvent, startingNumOfEvent*8, startingNumOfEvent*8*8) 
levels= 2 # number of levels if hierchical methods apply. So levels=1 means one resolution only.

# set the starting resolution and partition dimensions
resolutionDim = 10 # the starting resolution dimension for calculation, thus the starting calculated cube model has size partitionDim*partitionDim*partitionDim. When we set $resolutionDim = cubeDim$, the synthetic cube model can be considered as the ground truth for comparison, since the synthetic cube model has same resolution of calculation.
partitionDim = 1 # the starting number of partitions of the cube on X and Y axis. That is to say, the volcano cube will be partitioned to partitionDim*partitionDim partitions. If eventNumList has only one element, it will only partition once (e.g., centralized calculation if partitionDim = 1); otherwise it will run multi-resolution calculation for total $levels = length(eventNumList)$ resolutions where the $x$-th resolution will be partitioned to partitionDim*2^x partitions.

eventNumList = array (0, levels)
for(l in 1:levels) 
{
	eventNumList[l] = floor((1+1/l)*resolutionDim^3/stationNum*8^(l-1)) # event number in each resolution level
}

# random seed for generating station and event locations
stationSeed = 80 # seed to generate random numbers which can be any number. Different seed will generate different station coordinates
eventSeed = 81 # seed to generate random numbers which can be any number. Different seed will generate different event coordinates

print(">>>> Generating event and station locations")
print("Event number for each level")
print(eventNumList)
GenerateStationAndEventCoors(side, stationNum, sum(eventNumList), stationSeed, eventSeed, RAY_DISTRIBUTION)
print("<<<< DONE Generating event and station locations")

#####################################
#### Set computation parameters
#####################################

rounds = 1 # the round to perform exact solver (1 ART and 1 Bayesian ART per round)
tolerance = 0.0001 # ART stops whenever |x(k+1)-x(k)|/|x(k)| < tolerance
rho = 1.0 # rho: relaxiation parameter or step size of ART. In this program, after each round, rho=rho/1.01. See page 7 equation (2.3) of paper "Beyasian ART versus conjugate gradient methods in tomographic seismic imaging: An application at Mount St. Helens, Washington"  
lambda = 1.0 # lamba: regualarization parameter. See page 6 equation (1.3) of paper "Beyasian ART versus conjugate gradient methods in tomographic seismic imaging: An application at Mount St. Helens, Washington" 
flag = 0 # solve the tomography with approximation solver (flag = 0) or exact solver (flag = 1) or lsqr solver in each partition (flag = 2)

#####################################
#### Generate initial slowness guess
#####################################

print(">>>> Generating initial slowness guess")
if(LAYER_MODEL == TRACING_MODEL_3D_LINE)
{
	slowness = array(1/velocity, resolutionDim^3)
}else if(LAYER_MODEL == TRACING_MODEL_1D_LAYER)
{
	layerBound = velMod$zp
	velLayers = velMod$vp
	slness = 1/velLayers
	numVel = length(layerBound)
	segLen = side/resolutionDim

	slowness = array(0.0, resolutionDim^3) # initial guess of slowness model
	for(i in 1:resolutionDim)
	{
		for(j in 1:resolutionDim)
		{
			for(k in 1:resolutionDim)
			{
				dist = k*segLen-(segLen/2)
				for(l in 1:(numVel-1))
				{
					if(dist >= layerBound[l] & dist < layerBound[l+1])
					{
						slowness[(i-1)*resolutionDim*resolutionDim + (j-1)*resolutionDim + k] = slness[l]
					}
				}
			}
		}
	}
}
print("<<<< DONE Generating initial slowness guess")

#### Load the functions defined in related modules
source("./TomoHierarchy.R")
source("./ErrorAnalysis.R")
# source("./SliceResultGen.R")
source("./RLSMethod.R")
source("./ARTMethods.R")
source("./Lsqr.R")

#### compute initial guess to partition b_i
eventStart = 1
eventNum = eventNumList[1]
systems = GenerateAxb(cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventStart, eventNum, flag)
vecLength = resolutionDim^3
print("%%%%%%%%%%%%%%%%")
print(systems$originRows)
print(length(which(systems$blocks==1)))
print(length(which(systems$blocks==0)))

sptb = array(0, vecLength)
# slslowptb = SolveBayesianART(systems$originSystem, systems$originRows, resolutionDim, sptb, rho, lambda, tolerance)
slslowptb = SolveRLS(systems$originSystem, systems$originRows, vecLength, sptb, lambda)
slslow = slowness + slslowptb

output = array(0, vecLength+1)
output[1] = resolutionDim
output[2:(vecLength+1)] = slslow[1:vecLength]
filename = paste("slowness_rls_", partitionDim, "_", resolutionDim, sep="")
write(output, file = filename, sep = "\n")

sError = SquareError(systems$originSystem, slslowptb)
nResidual = NormalEqnResidual(systems$originSystem, slslowptb)
print(paste("++++ Square Error: ", sError))
print(paste("++++ Normal Eqn Residual: ", nResidual))

# slshigh = IncreaseResolution(slslow, resolutionDim)
# print(paste("error analysis at the resolution dimension: ", resolutionDim))
# errorD = dError(cube, slshigh)
# normError = errorNorm(cube, slshigh)
# print(paste("d1 = sum((cube-tempSlness)^2)/sum((tempSlness-mean(tempSlness))^2): ", errorD$d1))
# print(paste("d2 = sum(abs(cube-tempSlness))/sum(abs(tempSlness)): ", errorD$d2))
# print(paste("d3 = max(abs(cube-tempSlness)): ", errorD$d3))
# print(paste("error norm = ||cube-tempSlness||: ", normError))
# 
# ######################
# 
# #### Multigrid test
# partLow = partitionDim
# partHigh = partitionDim*2
# resLow = resolutionDim
# resHigh = resolutionDim*2
# 
# systemsLow = GenerateAxb(cube, side, cubeDim, slslow, partLow, resLow, stationNum, eventStart, eventNum, flag)
# print("%%%%%%%%%%%%%%%%")
# print(systemsLow$originRows)
# print(length(which(systemsLow$blocks==1)))
# print(length(which(systemsLow$blocks==0)))

# eventStart = eventStart + eventNum
# eventNum = eventNumList[2]
# 
# systemsHigh = GenerateAxb(cube, side, cubeDim, slshigh, partHigh, resHigh, stationNum, eventStart, eventNum, flag)
# print("%%%%%%%%%%%%%%%%")
# for(i in 1:parts)
# {
# 	print(systemsHigh$subSystems[[i]]$rayNum)
# }

# sptb = array(0, (resolutionDim*2)^3)
# parts = (partitionDim*2)^2
# sptb_init = array(0, (resolutionDim*2)^3)
# #### perform Bayesian ART on each subsystem
# for(i in 1:parts)
# {
# 	print(paste("==== Solving Partition ", i, " ====",sep=""))
# 	print(systems$subSystems[[i]]$rayNum)
# 	sptb_temp = SolveBayesianART(systems$subSystems[[i]], systems$subSystems[[i]]$rayNum, resolutionDim*2, sptb_init, rho, lambda, tolerance)
# 	sptb = sptb + sptb_temp
# }
# 
# slshigh = slshigh + sptb
# vecLength = length(slshigh)
# 
# output = array(0, vecLength+1)
# output[1] = resolutionDim*2
# output[2:(vecLength+1)] = slshigh[1:vecLength]
# filename = paste("slowness_approx_", partitionDim*2, "_", resolutionDim*2, sep="")
# write(output, file = filename, sep = "\n")
# 
# sError = SquareError(systems$originSystem, slslowptb)
# nResidual = NormalEqnResidual(systems$originSystem, slslowptb)
# print(paste("++++ Square Error: ", sError))
# print(paste("++++ Normal Eqn Residual: ", nResidual))
# 
# slshigh = IncreaseResolution(slslow, resolutionDim)
# print(paste("error analysis at the resolution dimension: ", resolutionDim))
# errorD = dError(cube, slshigh)
# normError = errorNorm(cube, slshigh)
# print(paste("d1 = sum((cube-tempSlness)^2)/sum((tempSlness-mean(tempSlness))^2): ", errorD$d1))
# print(paste("d2 = sum(abs(cube-tempSlness))/sum(abs(tempSlness)): ", errorD$d2))
# print(paste("d3 = max(abs(cube-tempSlness)): ", errorD$d3))
# print(paste("error norm = ||cube-tempSlness||: ", normError))