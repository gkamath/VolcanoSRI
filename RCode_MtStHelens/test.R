#### testing script

#### test for SolveQR in DirectMethods.R
# source("DirectMethods.R")
# 
# m = 4
# n = 2
# 
# eqn = array(list(), 4)
# 
# eqn[[1]]$A = c(-3, 1)
# eqn[[1]]$idx = c(1, 2)
# eqn[[1]]$b = 70
# 
# eqn[[2]]$A = c(1, 1)
# eqn[[2]]$idx = c(1, 2)
# eqn[[2]]$b = 21
# 
# eqn[[3]]$A = c(-7, 1)
# eqn[[3]]$idx = c(1, 2)
# eqn[[3]]$b = 110
# 
# eqn[[4]]$A = c(5, 1)
# eqn[[4]]$idx = c(1, 2)
# eqn[[4]]$b = -35
# 
# solution = SolveQR(eqn, 4, 2)
# print(solution)
# solution = SolveSVD(eqn, 4, 2)
# print(solution)

#### test norm error computation
# source("ErrorAnalysis.R")
# 
# SLOWNESS_MODEL_T = "slowness_truth"
# SLOWNESS_MODEL_E = "slowness_approx_1_20"
# 
# dataset = read.table(SLOWNESS_MODEL_T, sep = "\n")
# 
# len = length(dataset[,1])
# N = dataset[1,1]
# 
# res = N*N*N
# slowness_truth = array(0, res)
# slowness_truth[1:res] = dataset[2:len,1]
# 
# ###############
# dataset = read.table(SLOWNESS_MODEL_E, sep = "\n")
# 
# len = length(dataset[,1])
# N = dataset[1,1]
# 
# res = N*N*N
# slowness_est = array(0, res)
# slowness_est[1:res] = dataset[2:len,1]
# 
# norm = errorNorm(slowness_truth, slowness_est)
# print(norm)

#### test restriction operator in multigrid
# source("TomoGenerate.R")
# source("MGOperators.R")
# 
# side = 10.0
# cubeDim = 80
# velocity = 4.5
# 
# cube = GenerateCube(side, cubeDim, velocity)
# 
# vecLength = cubeDim^3
# output = array(0, vecLength+1)
# output[1] = cubeDim
# output[2:(vecLength+1)] = cube[1:vecLength]
# filename = paste("slowness_high_truth", sep="")
# write(output, file = filename, sep = "\n")
# 
# newcube = Restriction(cube, cubeDim)
# vecLength = (cubeDim/2)^3
# output = array(0, vecLength+1)
# output[1] = cubeDim/2
# output[2:(vecLength+1)] = newcube[1:vecLength]
# filename = paste("slowness_low_truth", sep="")
# write(output, file = filename, sep = "\n")

#### test 1D model read
# library(RTOMO)
# zOffset = 3.1
# velModel = read.table("MSH1DVel.mod", header = TRUE)
# velModel[,1] = velModel[,1] + zOffset
# data(HEL1D)
# velMod = HEL1D
# velMod$zp[2:numVel] = velMod$zp[2:numVel] + zOffset
# 
# print(velModel[3:14,1])
# print(velModel[3:14,2])
# print(velMod$zp[1:12])
# print(velMod$vp[1:12])
# i = 1000
# iz = i %% resolutionDim
# if(iz == 0)
# {
# 	iz = 10
# }
# iy = ((i - iz) %% (resolutionDim*resolutionDim))/resolutionDim + 1
# ix = (i - iz - (iy-1)*resolutionDim)/(resolutionDim*resolutionDim) + 1
# 
# # p1 = c(ix*seg-(seg/2), iy*seg-(seg/2), iz*seg-(seg/2))
# # rx = c(p1[i], stax[j])
# # ry = c(evy[i], stay[j])
# # rz = c(evz[i], staz[j])
# 
# print(paste("index: ", i, ix, iy, iz))

#### test model partition index
# RAP = list()
# RAP$r = array(0, 512)
# dim = 8
# part = 2
# base = dim/part
# 
# for(i in 1:8)
# {
# 	arr = array(i, dim*dim)
# 	RAP$ix = c(RAP$ix, arr)
# }
# 
# for(i in 1:8)
# {
# 	for(j in 1:8)
# 	{
# 		arr = array(j, dim)
# 		RAP$iy = c(RAP$iy, arr)
# 	}
# }
# 
# for(i in 1:8)
# {
# 	for(j in 1:8)
# 	{
# 		RAP$iz = c(RAP$iz, 1:8)
# 	}
# }
# 
# print("%%%%%%%%")
# print(RAP$ix)
# print(RAP$iy)
# print(RAP$iz)
# 
# m = length(RAP$r)
# idxcount = 0
# 
# for(k in 1:m)
# {
# 	ix = RAP$ix[k]
# 	iy = RAP$iy[k]
# 	
# 	secidx = 0
# 	for(a in 1:part)
# 	{
# 		for(b in 1:part)
# 		{
# 			secidx = secidx + 1
# 			xscale_a = (a-1)*base + 1
# 			xscale_b = a*base
# 			yscale_a = (b-1)*base + 1
# 			yscale_b = b*base
# 			if(ix >= xscale_a && ix <= xscale_b && iy >= yscale_a && iy <= yscale_b)
# 			{
# 				RAP$ib[k] = secidx
# 				idxcount = idxcount + 1
# 			}
# 		}
# 	}
# 	
# 	RAP$idx[k] = (RAP$ix[k]-1)*dim*dim + (RAP$iy[k]-1)*dim + RAP$iz[k]
# }
# 
# print("=========================")
# print(RAP$idx)
# print(RAP$ib)

#### test for partition idx map function
# source("TomoPartition.R")
# resDim = 8
# partDim = 2
# map = PartIdxMap(partDim, resDim)
# 
# print("========================")
# print(map$idx)
# print("========================")
# print(map$ib)
# print(which(map$ib == 1))

#### test Bayesian ART method
# source("ARTMethods.R")
# eqnSys = array(list(), 3)
# 
# len = sqrt(2)
# 
# eqnSys[[1]]$A = c(len)
# eqnSys[[1]]$idx = c(1)
# eqnSys[[1]]$b = len*1
# 
# eqnSys[[2]]$A = c(len, len)
# eqnSys[[2]]$idx = c(2, 3)
# eqnSys[[2]]$b = len*2+len*3
# 
# eqnSys[[3]]$A = c(len)
# eqnSys[[3]]$idx = c(4)
# eqnSys[[3]]$b = len*4
# 
# eqnSys[[4]]$A = c(len)
# eqnSys[[4]]$idx = c(2)
# eqnSys[[4]]$b = len*2
# 
# eqnSys[[5]]$A = c(len, len)
# eqnSys[[5]]$idx = c(1, 4)
# eqnSys[[5]]$b = len*1+len*4
# 
# eqnSys[[6]]$A = c(len)
# eqnSys[[6]]$idx = c(3)
# eqnSys[[6]]$b = len*3
# 
#### test solving the system row partition
# eqnSys[[1]]$A = c(1, 1)
# eqnSys[[1]]$idx = c(1, 2)
# eqnSys[[1]]$b = 3
# 
# sptb = array(0, 4)
# result = SolveBayesianART(eqnSys, 1, 8, sptb, 1.0, 0.0, 0.00000001)
# print(result$itrs)
# print(result$x)
# 
# eqnSys[[1]]$A = c(1, 1)
# eqnSys[[1]]$idx = c(3, 4)
# eqnSys[[1]]$b = 7
# 
# sptb = array(0, 4)
# result = SolveBayesianART(eqnSys, 1, 8, sptb, 1.0, 0.0, 0.00000001)
# print(result$itrs)
# print(result$x)
# 
# eqnSys[[1]]$A = c(1, 1)
# eqnSys[[1]]$idx = c(2, 4)
# eqnSys[[1]]$b = 6
# 
# sptb = array(0, 4)
# result = SolveBayesianART(eqnSys, 1, 8, sptb, 1.0, 0.0, 0.00000001)
# print(result$itrs)
# print(result$x)
# 
# eqnSys[[1]]$A = c(1, 1)
# eqnSys[[1]]$idx = c(1, 2)
# eqnSys[[1]]$b = 3
# 
# eqnSys[[2]]$A = c(1, 1)
# eqnSys[[2]]$idx = c(3, 4)
# eqnSys[[2]]$b = 7
# 
# eqnSys[[3]]$A = c(1, 1)
# eqnSys[[3]]$idx = c(2, 4)
# eqnSys[[3]]$b = 6
# 
# sptb = array(0, 4)
# result = SolveBayesianART(eqnSys, 3, 8, sptb, 1.0, 0.0, 0.00000001)
# print(result$itrs)
# print(result$x)

#### test random system
# source("ARTMethods.R")
# 
# Fnorm <- function(eqnSys, rows)
# {
# 	norm = 0.0
# 	for(i in 1:rows)
# 	{
# 		norm = norm + eqnSys[[i]]$A %*% eqnSys[[i]]$A
# 	}
# 	return(sqrt(norm))
# }
# 
# ColNorm <- function(eqnSys, rows, col)
# {
# 	norm = 0.0
# 	for(i in 1:rows)
# 	{
# 		index = which(eqnSys[[i]]$idx == col)
# # 		print(index)
# 		if(length(index) != 0)
# 		{
# # 			print(eqnSys[[i]])
# 			norm = norm + eqnSys[[i]]$A[index] * eqnSys[[i]]$A[index]
# 		}
# 	}
# 
# 	return(sqrt(norm))
# }
# 
# seed = 2
# eqns = 20
# eqnSys = array(list(), eqns)
# x = array(1, 10)
# for(i in 1:eqns)
# {
# 	set.seed(i*seed)
# 	row = runif(10, 0, 1)
# 	for(j in 1:10)
# 	{
# 		if(row[j] < 0.8)
# 		{
# 			row[j] = 0
# 		}
# 	}
# 	eqnSys[[i]]$A = row
# 	eqnSys[[i]]$idx = c(1:10)
# 	eqnSys[[i]]$b = row %*% x
# }
# 
# print(Fnorm(eqnSys, eqns))
# 
# for(i in 1:10)
# {
# 	print(ColNorm(eqnSys, eqns, i))
# }

# print(eqnSys)

# sptb = array(0, 10)
# result = SolveBayesianART(eqnSys, eqns, 8, sptb, 1.0, 0.0, 0.000001)
# print(result$itrs)
# print(result$x)

#### test for Ray.time1D function with stations of non-zero elevation
# library(RTOMO)
# source("TomoPartition.R")
# 
# # zOffset = 3.1
# zOffset = 3.1
# 
# HEL1D = read.table("MSH1DVel.mod", header=TRUE)
# print(HEL1D)
# HEL1D[,1] = HEL1D[,1] + zOffset
# print(HEL1D)
# 
# velocity = list()
# velocity$zp = HEL1D[3:14,1]
# velocity$vp = HEL1D[3:14,2]
# numVel = length(velocity$zp)
# 
# layerBound = velocity$zp
# velLayers = velocity$vp
# 
# print("===================")
# print(velocity)
# 
# slness = 1/velLayers
# 
# xLen = 160
# yLen = 200
# zLen = 24
# 
# xInt = 5
# yInt = 5
# zInt = 2
# 
# xRes = xLen/xInt
# yRes = yLen/yInt
# zRes = zLen/zInt
# 
# vecLength = xRes*yRes*zRes
# 
# slowness = array(0.0, vecLength) # initial guess of slowness model
# count = 0
# for(i in 1:xRes)
# {
# 	for(j in 1:yRes)
# 	{
# 		for(k in 1:zRes)
# 		{
# 			dist = k*zInt-(zInt/2)
# 			for(l in 1:(numVel-1))
# 			{
# 				if(dist >= layerBound[l] & dist < layerBound[l+1])
# 				{
# 					slowness[(i-1)*yRes*zRes + (j-1)*zRes + k] = slness[l]
# 					count = count + 1
# 				}
# 			}
# 		}
# 	}
# 	print(paste("construct initial slowness: ", i))
# }
# 
# source("LoadRealData.R")
# zOffset = 3.1
# realData = LoadRealData(zOffset)
# 
# stas = realData$stations
# 
# rx = c(10.0, 10.0001)
# ry = c(10.0, 10.0)
# rz = c(6.33, 1.7)
# 
# print(velocity)
# 
# print(which(velocity$zp > rz[2])[1]-1)
# 
# velAdj = list()
# velLen = length(velocity$zp)
# velStart = which(velocity$zp > rz[2])[1]-1
# 
# velAdj$zp = (velocity$zp - rz[2])[velStart:velLen]
# velAdj$zp[1] = 0
# velAdj$vp = velocity$vp[velStart:velLen]
# 
# print(velAdj)
# ########
# # rz = rz - localOffset
# ########

# print(velocity)
# # velocity$zp = velocity$zp - localOffset
# print(paste("$$$$$$$$$$$$$$", rz[1], rz[2]))
# print(velocity)


# use 1D velocity model to trace the ray
# dee = sqrt((rx[2]-rx[1])^2 + (ry[2]-ry[1])^2)
# raypath = Ray.time1D(dee, rz[1] - rz[2], 0, length(velAdj$zp), velAdj$zp, 1/velAdj$vp)
# 
# print(raypath)
# 
# az = atan2(ry[2]-ry[1], rx[2]-rx[1])
# zx=cos(az)*raypath$rnod[1:raypath$nnod] + rx[1]
# zy=sin(az)*raypath$rnod[1:raypath$nnod] + ry[1]
# zz = raypath$znod[1:raypath$nnod] + rz[2]
# 
# print(zx)
# print(zy)
# print(zz)
# 
# xLen = 160
# yLen = 200
# zLen = 24
# 
# xInt = 5
# yInt = 5
# zInt = 2
# 
# xo = seq(0, xLen, xInt)
# yo = seq(0, yLen, yInt)
# zo = seq(0, zLen, zInt)
# 
# get the ray blocks
# raysegs = raypath$nnod - 1
# RAP = Get1DRayblox(raysegs, zx, zy, zz, xo, yo, zo)
# print(RAP)

#### test get3DRayblox() function on negtive numbers
# 
# library(RTOMO)
# source("TomoPartition.R")
# 
# xLen = 160
# yLen = 200
# zLen = 24
# 
# xInt = 5
# yInt = 5
# zInt = 2
# 
# offset = 3.1
# 
# xo = seq(0, xLen, xInt)
# yo = seq(0, yLen, yInt)
# zo = seq(0, zLen, zInt)
# print(zo)
# 
# zx = c(0, 5)
# zy = c(0, 5)
# zz = c(5, 0)
# 
# RAP = GetRayblox(zx, zy, zz, xo, yo, zo)
# print(RAP)
# print("-------------------------------")
# zo = seq(0-offset, zLen-offset, zInt)
# print(zo)
# 
# zx = c(0, 5)
# zy = c(0, 5)
# zz = c(5-offset, 0-offset)
# RAP = GetRayblox(zx, zy, zz, xo, yo, zo)
# print(RAP)

#### test Ray.time1D function
# library(RTOMO)
# source("TomoPartition.R")
# 
# zOffset = 3.1
# 
# HEL1D = read.table("MSH1DVel.mod", header=TRUE)
# print(HEL1D)
# HEL1D[,1] = HEL1D[,1] + zOffset
# print(HEL1D)
# 
# velocity = list()
# velocity$zp = HEL1D[3:14,1]
# velocity$vp = HEL1D[3:14,2]
# numVel = length(velocity$zp)
# 
# layerBound = velocity$zp
# velLayers = velocity$vp
# 
# rx = c(0, 0)
# ry = c(0, 0)
# rz = c(6.33, 2.247)
# 
# velAdj = list()
# 
# velStart = (which(velocity$zp > rz[2])[1] - 1)
# velAdj$zp = (velocity$zp - rz[2])[velStart:numVel]
# velAdj$zp[1] = 0
# velAdj$vp = velocity$vp[velStart:numVel]
# 
# use 1D velocity model to trace the ray
# dee = sqrt((rx[2]-rx[1])^2 + (ry[2]-ry[1])^2)
# dee = 54.9720462748862
# print("&&&&&&&&&&&&&&&&&&&&&&&&&&&")
# print(paste("    ====", dee, rz[1], rz[2], rz[1]-rz[2]))
# print(velAdj)
# print(velocity)
# raypath = Ray.time1D(dee, rz[1] - rz[2], 0, length(velAdj$zp), velAdj$zp, 1/velAdj$vp)
# print(raypath)
# 
# source("TomoPartition.R")

### render real data
# source("TomoRender.R")
# xLen = 160
# yLen = 200
# zLen = 24
# 
# xInt = 5
# yInt = 5
# zInt = 2
# 
# xRes = xLen/xInt
# yRes = yLen/yInt
# zRes = zLen/zInt
# Slice2DRenderZxes("slowness_velrperturb_32x40x12", xLen, yLen, zLen, xInt, yInt, zInt, 1)

### real data rays
source("TomoPartition.R")

xLen = 160
yLen = 200
zLen = 24

xInt = 5
yInt = 5
zInt = 2

xo = seq(0, xLen, xInt)
yo = seq(0, yLen, yInt)
zo = seq(0, zLen, zInt)

temp = 12.123423

xx = c(83.7478202188252, 97.7992158806929)
yy = c(123.660546699075, 144.780186616161)
zz = c(8.0002, 8.0001)

Ray = GetRayblox(xx, yy, zz, xo, yo, zo)
d1 = sqrt((xx[2]-xx[1])^2 + (yy[2]-yy[1])^2 + (zz[2]-zz[1])^2)
d2 = sum(Ray$r)

#print(d1)
#print(d2)


