#### Example 2
library(RTOMO)
library(rgl)

source("LoadRealData.R")
zOffset = 3.1
realData = LoadRealData(zOffset)

partitionDim = 1 # the number of partitions of the cube on X and Y axis. That is to say, the volcano cube will be partitioned to partitionDim*partitionDim partitions
rounds = 1 # the round to perform exact solver (1 ART and 1 Bayesian ART per round)
tolerance = 0.01 # ART stops whenever |x(k+1)-x(k)|/|x(k)| < tolerance
rho = 1.0 # rho: relaxiation parameter or step size of ART. In this program, after each round, rho=rho/1.01. See page 7 equation (2.3) of paper "Beyasian ART versus conjugate gradient methods in tomographic seismic imaging: An application at Mount St. Helens, Washington"  
lambda = 1.0 # lamba: regualarization parameter. See page 6 equation (1.3) of paper "Beyasian ART versus conjugate gradient methods in tomographic seismic imaging: An application at Mount St. Helens, Washington" 

#### Load the functions defined in related modules
source("./TomoPartition.R")
source("./ARTMethods.R")

# data(HEL1D)
# velMod = HEL1D
# numVel = length(velMod$zp)
# print(HEL1D)

HEL1D = read.table("../MSH1DVel.mod", header=TRUE)
HEL1D[,1] = HEL1D[,1]+zOffset
print(HEL1D)

velMod = list()
velMod$zp = HEL1D[3:14,1]
velMod$vp = HEL1D[3:14,2]
numVel = length(velMod$zp)

print(velMod)
#stop()

layerBound = velMod$zp
velLayers = velMod$vp
slness = 1/velLayers


xLen = 160
yLen = 200
zLen = 24

xInt = 1
yInt = 1
zInt = 1

xRes = xLen/xInt
yRes = yLen/yInt
zRes = zLen/zInt

vecLength = xRes*yRes*zRes

slowness = array(0.0, vecLength) # initial guess of slowness model
for(i in 1:xRes)
{
	for(j in 1:yRes)
	{
		for(k in 1:zRes)
		{
			dist = k*zInt-(zInt/2)
			for(l in 1:(numVel-1))
			{
				if(dist >= layerBound[l] & dist < layerBound[l+1])
				{
					slowness[(i-1)*yRes*zRes + (j-1)*zRes + k] = slness[l]
				}
			}
		}
	}
	print(paste("construct initial slowness: ", i))
}

print(paste("resolution: ", xRes, "x", yRes, "x", zRes))
print(vecLength)

# test for slowness model initial
write(slowness, file="Rout", sep="\n")
#stop()

smoothing_factor = c(0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9)
smi = 6

output = array(0, vecLength+3)
output[1] = xRes
output[2] = yRes
output[3] = zRes
output[4:(vecLength+3)] = slowness[1:vecLength]
filename = paste("slowness_1dslsref_z1_", smi, "_", xRes, "x", yRes, "x", zRes, sep="")
write(output, file = filename, sep = "\n")

velocity = 1/slowness
output = array(0, vecLength+3)
output[1] = xRes
output[2] = yRes
output[3] = zRes
output[4:(vecLength+3)] = velocity[1:vecLength]
filename = paste("slowness_1dvelref_z1_", smi, "_", xRes, "x", yRes, "x", zRes, sep="")
write(output, file = filename, sep = "\n")

print(paste("realdatarays: ", realData$rays))

systems = GenerateRealLayerAxbCarp(xLen, yLen, zLen, xInt, yInt, zInt, velMod, slowness, realData$events, realData$stations, realData$rays, length(realData$events), lsqrSmooth = TRUE)
print(paste("    systems rows:        ", systems$originRows))

for(i in 1:systems$originRows) {
    write(systems$originSystem[[i]]$A, file = "Amatrix", append=TRUE, ncolumns = 100)
    write(systems$originSystem[[i]]$idx, file = "Amatrix", append=TRUE, ncolumns = 100)
    write(systems$originSystem[[i]]$b, file = "bvector", append=TRUE)
}

stop()

sptb = array(0, vecLength)
slsRes = SolveBayesianARTSmooth(systems$originSystem, systems$originRows, xRes, yRes, zRes, sptb, rho, lambda, tolerance, smoothing_rate = smoothing_factor[smi])
 
print(systems$originRows)
print(paste("    BART iterations:     ", slsRes$itrs))

slsperturb = slsRes$x

timestamp = format(Sys.time(), "%m%d_%H%M")

# slsperturb = SolveLsqrReal(systems$originSystem, vecLength, systems$originRows)
output = array(0, vecLength+3)
output[1] = xRes
output[2] = yRes
output[3] = zRes
output[4:(vecLength+3)] = slsperturb[1:vecLength]
filename = paste("slowness_slsperturb_z1_", smi, "_", xRes, "x", yRes, "x", zRes, "_", timestamp, sep="")
write(output, file = filename, sep = "\n")

slsreal = slsperturb + slowness
output = array(0, vecLength+3)
output[1] = xRes
output[2] = yRes
output[3] = zRes
output[4:(vecLength+3)] = slsreal[1:vecLength]
filename = paste("slowness_slsreal_z1_", smi, "_", xRes, "x", yRes, "x", zRes, "_", timestamp, sep="")
write(output, file = filename, sep = "\n")

velreal = 1/slsreal
output = array(0, vecLength+3)
output[1] = xRes
output[2] = yRes
output[3] = zRes
output[4:(vecLength+3)] = velreal[1:vecLength]
filename = paste("slowness_velreal_z1_", smi, "_", xRes, "x", yRes, "x", zRes, "_", timestamp, sep="")
write(output, file = filename, sep = "\n")

velperturb = velreal-velocity
output = array(0, vecLength+3)
output[1] = xRes
output[2] = yRes
output[3] = zRes
output[4:(vecLength+3)] = velperturb[1:vecLength]
filename = paste("slowness_velperturb_z1_", smi, "_", xRes, "x", yRes, "x", zRes, "_", timestamp, sep="")
write(output, file = filename, sep = "\n")

velrperturb = velperturb/velocity
output = array(0, vecLength+3)
output[1] = xRes
output[2] = yRes
output[3] = zRes
output[4:(vecLength+3)] = velrperturb[1:vecLength]
filename = paste("slowness_velrperturb_z1_", smi, "_", xRes, "x", yRes, "x", zRes, "_", timestamp, sep="")
write(output, file = filename, sep = "\n")
