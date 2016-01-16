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
levels= 1 # number of levels if hierchical methods apply. So levels=1 means one resolution only.

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
lambda = 0 # lamba: regualarization parameter. See page 6 equation (1.3) of paper "Beyasian ART versus conjugate gradient methods in tomographic seismic imaging: An application at Mount St. Helens, Washington" 
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
source("./DirectMethods.R")
source("./ARTMethods.R")
source("./Lsqr.R")

#### compute initial guess to partition b_i
noiseList = c(0.0, 0.01, 0.02, 0.04, 0.08, 0.1)
nRound = length(noiseList)

eventStart = 1
eventNum = eventNumList[1]
vecLength = resolutionDim^3

lambdaStep = 2
mRound = 11

RNDM = matrix(0, mRound, nRound)
SEM = matrix(0, mRound, nRound)
NERM = matrix(0, mRound, nRound)
D1M = matrix(0, mRound, nRound)
D2M = matrix(0, mRound, nRound)
D3M = matrix(0, mRound, nRound)
ENM = matrix(0, mRound, nRound)

for(nround in 1:nRound)
{
	systems = GenerateAxb(cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventStart, eventNum, flag, noise = noiseList[nround])	

	lambda = 1
	for(mround in 1:mRound)
	{
		print(paste("====================== Error Analysis of Noise: ", noiseList[nround], ", Lambda: ", lambda))
		sptb = array(0, vecLength)
		rho = 1.0
		slsRes = SolveBayesianARTPara(systems$originSystem, systems$originRows, resolutionDim, sptb, rho, lambda, tolerance)
		RNDM[mround, nround] = slsRes$itrs
		print(paste("    BART iterations:     ", slsRes$itrs))
		
		slslowptb = slsRes$x
		slslow = slowness + slslowptb

		output = array(0, vecLength+1)
		output[1] = resolutionDim
		output[2:(vecLength+1)] = slslow[1:vecLength]
		filename = paste("./result/slowness_bart_", resolutionDim, "_noise_", nround, "_mround_", mround, sep="")
		write(output, file = filename, sep = "\n")

		sError = SquareError(systems$originSystem, systems$originRows, slslowptb)
		nResidual = NormalEqnResidual(systems$originSystem, systems$originRows, slslowptb)
		SEM[mround, nround] = sError
		print(paste("    Square Error:        ", sError))
		NERM[mround, nround] = nResidual
		print(paste("    Normal Eqn Residual: ", nResidual))

		errorD = dError(cube, slslow)
		normError = errorNorm(cube, slslow)
		D1M[mround, nround] = errorD$d1
		print(paste("    d1:                  ", errorD$d1))
		D2M[mround, nround] = errorD$d2
		print(paste("    d2:                  ", errorD$d2))
		D3M[mround, nround] = errorD$d3
		print(paste("    d3:                  ", errorD$d3))
		ENM[mround, nround] = normError
		print(paste("    error norm:          ", normError))

		lambda = lambda * lambdaStep
	}
}

write.table(RNDM, file = "./data/RNDM2.txt", row.names = FALSE, col.names = FALSE)
write.table(SEM, file = "./data/SEM2.txt", row.names = FALSE, col.names = FALSE)
write.table(NERM, file = "./data/NERM2.txt", row.names = FALSE, col.names = FALSE)
write.table(D1M, file = "./data/D1M2.txt", row.names = FALSE, col.names = FALSE)
write.table(D2M, file = "./data/D2M2.txt", row.names = FALSE, col.names = FALSE)
write.table(D3M, file = "./data/D3M2.txt", row.names = FALSE, col.names = FALSE)
write.table(ENM, file = "./data/ENM2.txt", row.names = FALSE, col.names = FALSE)
