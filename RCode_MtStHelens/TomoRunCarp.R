#### Trying to understand and improve the existing algorithm

rm(list = ls())  #clearing the workspace.

library(RTOMO) 
# Description: Created mainly for use with seismic tomography, this
# program plots tomographic images, and allows one to interact
# and query three-dimensional tomographic models. Vertical
# cross-sectional cuts can be extracted by mouse click.
# Geographic information can be added easily.



source('./TomoGenerate.R') #Generate the artificial magma area as the ground-truth
#Contains Function TomoGenerates which generates cube containing true slowness model
#Also contains function which creates Station and Events.

# definition of ray tracing models
TRACING_MODEL_3D_LINE = 0   # raytracing through straight lines
TRACING_MODEL_1D_LAYER = 1  # raytracing with 1D layer model 

# definition of ray distributions
STATION_ONTOP_DISTRIBUTION =0   # stations on the top of cube only
STATION_RANDOM_DISTRIBUTION = 1 # station can be anywhere in the cube

FULL_RANK = TRUE  #Let it be a Full Rank Matrix

#Define Layer Model and Ray Distribution criteria
LAYER_MODEL = TRACING_MODEL_3D_LINE
RAY_DISTRIBUTION = STATION_ONTOP_DISTRIBUTION

#Files to save data
STATIONS_LOG = "./stationslog"
EVENTS_LOG = "./eventslog"
SLOWNESS_TRUTH = "./slowness_truth"

#Dimension of the cube
cubeDim = 16
# set the starting resolution and partition dimensions
resolutionDim = 8 # the starting resolution dimension for calculation
#thus the starting calculated cube model has size 
#partitionDim*partitionDim*partitionDim. When we set $resolutionDim = cubeDim$
#the synthetic cube model can be considered as the ground truth for comparison, 
#since the synthetic cube model has same resolution of calculation.

# velocity: non-megma volcano area velocity for generating artifical volcano model
velocity = 4.5 

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
#Number of station
stationNum = 100
clusterSize = 10

levels= 2 # number of levels if hierchical methods apply. So levels=1 means one resolution only.



partitionDim = 1 # the starting number of partitions of the cube on X and Y axis. 
#That is to say, the volcano cube will be partitioned to partitionDim*partitionDim 
#partitions. If eventNumList has only one element, it will only partition once 
#(e.g., centralized calculation if partitionDim = 1); otherwise it will run 
#multi-resolution calculation for total $levels = length(eventNumList)$ resolutions 
#where the $x$-th resolution will be partitioned to partitionDim*2^x partitions.

eventNumList = array (0, levels) #To store the even number in each resolution level
resdim = resolutionDim
for(l in 1:levels) 
{
#   eventNumList[l] = floor((1+1/l)*resolutionDim^3/stationNum*8^(l-1)) # event number in each resolution level
#     eventNumList[l] = floor((resdim^3)/(l*3))
# #     eventNumList[l] = 200
#     resdim = resdim*2
}
eventNumList[1]= 100
eventNumList[2]= 300
# eventNumList[3]= 500


# eventNumList[l] = 10000
# eventNum = eventNumList[1] #considering just one resolution

eventStart = 1 #No idea what this is

# random seed for generating station and event locations
stationSeed = 80 # seed to generate random numbers which can be any number.
#Different seed will generate different station coordinates

eventSeed = 81 # seed to generate random numbers which can be any number. 
#Different seed will generate different event coordinates

print(">>>> Generating event and station locations")
#Calling the function from TomoGenerate to generate event and station Location
GenerateStationAndEventCoors(side, stationNum, sum(eventNumList), stationSeed, eventSeed, RAY_DISTRIBUTION)
print("<<<< DONE Generating event and station locations")

rounds = 1 # the round to perform exact solver (1 ART and 1 Bayesian ART per round)
#Looks like rounds are not used but a threshold and maxrounds is used to stop the ART

tolerance = 0.001 # ART stops whenever |x(k+1)-x(k)|/|x(k)| < tolerance

rho = 1 # rho: relaxiation parameter or step size of ART. 
#In this program, after each round, rho=rho/1.01. See page 7 equation (2.3) of paper 
#"Beyasian ART versus conjugate gradient methods in tomographic seismic imaging: 
#An application at Mount St. Helens, Washington" 
#Rho played an important role in determining the convergence. Have to study more on this one

lambda = 1.0 # lamba: regualarization parameter. See page 6 equation (1.3) of paper 
#"Beyasian ART versus conjugate gradient methods in tomographic seismic imaging: 
#An application at Mount St. Helens, Washington" 
#Used for Bayesian ART Have to study the effect of lambda

flag = 2 # solve the tomography with approximation solver (flag = 0) or 
#exact solver (flag = 1) or lsqr solver in each partition (flag = 2)

#####################################
#### Generate initial slowness guess
#####################################

print(">>>> Generating initial slowness guess")
if(LAYER_MODEL == TRACING_MODEL_3D_LINE)
{
  slowness = array(1/velocity, resolutionDim^3)
}else if(LAYER_MODEL == TRACING_MODEL_1D_LAYER)
{
  #if 1DLayer then we need to convert actualy Slowness to fit the 1D dimension  
  
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
slowness_init = slowness
No_of_col = length(slowness)
print(paste("<<<< Length of initial slowness guess",No_of_col))
slowness_pert = array(0.0, No_of_col)
slowness_pert_cav = array(0.0, No_of_col)
#### Load the functions defined in related modules
# source("./TomoHierarchy.R")
source("./ErrorAnalysis.R")
source("./SliceResultGen.R")
source("./DirectMethods.R")
source("./TomoPartition.R")
source("./ARTMethods.R")
source("./RowPartition.R")
source("./GenS.R")
source("./NoUpdate.R")
source("./HierarchyTomoSolver.R")
source("./RowPartitionsolver.R")



#The structure of hierarchyResult is array(list(), levels)
#the first list() gives info abt layer 1 and so on.
#GenerateAxB is used to generate the system and and solved using PartitionTomoSolver.

# rholist = array (0, 10)
# rholist[1]= 0.006
# rholist[2]= 0.008
# rholist[3]= 0.01
# rholist[4]= 0.04
# rholist[5]= 0.06
# rholist[6]= 0.1
# rholist[7]= 0.14
# rholist[8] = 0.004
# rholist[9]= 0.002
# rholist[10]= 0.18

# for(k in 1:10)
# {
#   rho = rholist[k]

  print(paste("Simulation Rho: ", rho))
if(LAYER_MODEL == TRACING_MODEL_3D_LINE)
{
#   hierarchyResult = HierarchyTomoSolver(levels, cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventNumList, rho, lambda, flag, tolerance, rounds, fullRank = FULL_RANK)
  hierarchyResult = HierarchyTomoSolverCluster(levels, cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventNumList, rho, lambda, flag, tolerance, rounds,clusterSize, fullRank = FULL_RANK,clusterSize)
#   hierarchyResult = HierarchyTomoSolverClusterBaysian(levels, cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventNumList, rho, lambda, flag, tolerance, rounds,clusterSize, fullRank = FULL_RANK,clusterSize)
}


for(i in 1:levels)
  {
  print(paste("Iteration ",i, hierarchyResult[[i]]$iteration))
}

filename = paste("Test_slowness_approx","1", "_",16, sep="")
SliceResGen(filename)

SliceResGen("slowness_truth")
# SliceResGen("~/svn/android/Tomography/TomoPartition/rsim_baseline/slowness_truth")

# for(i in 1:levels)
#   {
# tempSlness = hierarchyResult[[i]]$slowness
# tempRes = 128
# tempSlness = IncreaseResolution(tempSlness, 128)
# errorD = dError(cube, tempSlness)
#   normError = errorNorm(cube, tempSlness)
#   print(paste("d1 = sum((cube-tempSlness)^2)/sum((tempSlness-mean(tempSlness))^2): ", errorD$d1))
#   print(paste("d2 = sum(abs(cube-tempSlness))/sum(abs(tempSlness)): ", errorD$d2))
#   print(paste("d3 = max(abs(cube-tempSlness)): ", errorD$d3))
#   print(paste("error norm = ||cube-tempSlness||: ", normError))
# }

# print(">>>>> Start error analysis")
# for(i in 1:levels)
# {
#   resLevel = log(cubeDim/(resolutionDim*(2^(i-1))), 2)
#   # 	print(paste("initial resolution: ", resolutionDim, " i: ", i))
#   # 	print(paste("resLevel: ", resLevel))
#   tempRes = resolutionDim*(2^(i-1))
#   tempSlness = hierarchyResult[[i]]$slowness
#   slowness_init = IncreaseResolution(slowness_init, (tempRes)/2)
#    sError = SquareError(hierarchyResult[[i]]$originSystem, hierarchyResult[[i]]$originRows, tempSlness - slowness_init)
#   nResidual = NormalEqnResidual(hierarchyResult[[i]]$originSystem, hierarchyResult[[i]]$originRows, tempSlness - slowness_init)
#   print(paste("++++ Square Error: ", sError))
#   print(paste("++++ Normal Eqn Residual: ", nResidual))
#   
#   #### test for qr/lsqr solver:*
#   
#   vecLength = tempRes^3
#   print(paste("slowness length: ", length(tempSlness)))
#   if(resLevel > 0)
#   {
#     print("!!!increased resolution to groud truth for comparison - is it better to shrink ground truth for comparison???")
#     for(j in 1:resLevel)
#     {
#       tempSlness = IncreaseResolution(tempSlness, tempRes)
#     }
#   }
#   # 	print(paste("increased slowness length: ", length(tempSlness)))
#   # 	print(paste("truth slowness length: ", length(cube)))
#   
#   print(paste("error analysis at the resolution dimension: ", resolutionDim*(2^(i-1))))
#   errorD = dError(cube, tempSlness)
#   normError = errorNorm(cube, tempSlness)
#   print(paste("d1 = sum((cube-tempSlness)^2)/sum((tempSlness-mean(tempSlness))^2): ", errorD$d1))
#   print(paste("d2 = sum(abs(cube-tempSlness))/sum(abs(tempSlness)): ", errorD$d2))
#   print(paste("d3 = max(abs(cube-tempSlness)): ", errorD$d3))
#   print(paste("error norm = ||cube-tempSlness||: ", normError))
  



