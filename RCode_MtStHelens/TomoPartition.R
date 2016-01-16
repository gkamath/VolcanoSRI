library(sp)
library(splancs)
library(RTOMO)

#############################################
#### Load station and event location log file
#############################################
LoadStationEventLog <- function(stationNum, eventStart, eventNum)
{
	#### read station location log file 
	stationslog = read.table(STATIONS_LOG)
	totalStas = dim(stationslog)[1]/3

	stax = stationslog[1:stationNum, 1]
	stay = stationslog[(1+totalStas):(totalStas+stationNum), 1]
	staz = stationslog[(1+2*totalStas):(1+2*totalStas+stationNum-1),1]

	#### read event location log file
	eventslog = read.table(EVENTS_LOG)
	totalEvts = dim(eventslog)[1]/3

	evx = eventslog[eventStart:(eventStart+eventNum-1),1]
	evy = eventslog[(eventStart+totalEvts):(eventStart+totalEvts+eventNum-1),1]
	evz = eventslog[(eventStart+2*totalEvts):(eventStart+2*totalEvts+eventNum-1),1]

	stationEventLog = list(stax = stax, stay = stay, staz = staz, evx = evx, evy = evy, evz = evz)

	return(stationEventLog)
}

#################################################
#### Get 3D ray path blocks using get3Drayblox
#################################################
GetRayblox <- function(xCoor, yCoor, zCoor, xo, yo, zo)
{
	#### check if the line is orthogonal with z axes
	#### test for real data
	if(zCoor[2] == zCoor[1])
	{
		zCoor[2] = zCoor[2] + 0.0001
	}
	
	#### If the ray is from top to bottom
	#### change the direction of the ray path
	#### get3Drayblox assumes that ray path is from bottom to top
	if(zCoor[1] < zCoor[2])
	{
	    xCoor = c(xCoor[2], xCoor[1])
	    yCoor = c(yCoor[2], yCoor[1])
	    zCoor = c(zCoor[2], zCoor[1])
	}

	dee = sqrt((xCoor[2]-xCoor[1])^2 + (yCoor[2]-yCoor[1])^2 + (zCoor[2]-zCoor[1])^2)
	deexy = sqrt((xCoor[2]-xCoor[1])^2 + (yCoor[2]-yCoor[1])^2)

	fi  = findInterval(zCoor, zo)

	#### List the interval boundary along z axis where the ray penetrates
	#### include the starting and ending point
	if(fi[1] == fi[2])
	{
		zNod = c(zCoor[1], zCoor[2])
	}else
	{
		zNod = c(zo[fi[1]:(fi[2]+1)], zCoor[2])
		if(zCoor[1] != zo[fi[1]])
		{
			zNod = c(zCoor[1], zNod)
		}
	}	

	alpha = asin(deexy/dee)
	RN = deexy-(zNod-zCoor[2])*tan(alpha)

	#### Mapping the interval boundary along z axis to x and y axises
	xNod = xCoor[1]+RN*(xCoor[2]-xCoor[1])/deexy
	yNod = yCoor[1]+RN*(yCoor[2]-yCoor[1])/deexy

	#### Get 3D ray path blocks
	rayPath = get3Drayblox(xNod, yNod, zNod, xo, yo, zo)

	resX = length(xo) - 1
	resY = length(yo) - 1
	resZ = length(zo) - 1
	rayPath$idx = (rayPath$ix-1)*resY*resZ + (rayPath$iy-1)*resZ + rayPath$iz

	return(rayPath)
}

#################################################
#### Get 3D ray path blocks using get3Drayblox
#################################################
Get1DRayblox <- function(raysegs, xCoor, yCoor, zCoor, xo, yo, zo)
{
	# extract a_i in A from the raypath
	#print(paste(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", raysegs))
	#print(xCoor)
	#print(yCoor)
	#print(zCoor)
	RAP = GetRayblox(c(xCoor[1], xCoor[2]), c(yCoor[1], yCoor[2]), c(zCoor[1], zCoor[2]), xo, yo, zo)
	#print("==========================================================")
	#print(raysegs)
	#print(xCoor[1])
	#print(xCoor[2])
	#print(yCoor[1])
	#print(yCoor[2])
	#print(zCoor[1])
	#print(zCoor[2])
	#print(length(RAP$ix))
	#print(RAP)
	#print("==========================================================")
	
# 	print(paste("    ---- ray: ", length(RAP$idx)))
# 	print(RAP)

	if(raysegs > 2)
	{
		for(rseg in 2:raysegs)
		{
# 			print(paste("  **** ray segment X: ", xCoor[rseg], xCoor[rseg+1]))
# 			print(paste("  **** ray segment Y: ", yCoor[rseg], yCoor[rseg+1]))	
# 			print(paste("  **** ray segment Z: ", zCoor[rseg], zCoor[rseg+1]))
			RAPseg = GetRayblox(c(xCoor[rseg], xCoor[rseg+1]), c(yCoor[rseg], yCoor[rseg+1]), c(zCoor[rseg], zCoor[rseg+1]), xo, yo, zo)
# 			print(paste("**************************************************"))
# 			print(RAPseg)
			if(is.null(RAPseg$ix))
			{
				RAP = NULL
				return(RAP)
			}
			rayLen = length(RAP$idx)
			raySegLen = length(RAPseg$idx)
# 			print(paste("  ==== ray segment: ", rseg, raySegLen))
# 			print(RAPseg)
			if(RAP$idx[rayLen] == RAPseg$idx[1])
			{
				RAP$r[rayLen] = RAP$r[rayLen] + RAPseg$r[1]
				if(raySegLen > 1)
				{
					RAP$ix = c(RAP$ix, RAPseg$ix[2:raySegLen])
					RAP$iy = c(RAP$iy, RAPseg$iy[2:raySegLen])
					RAP$iz = c(RAP$iz, RAPseg$iz[2:raySegLen])
					RAP$r = c(RAP$r, RAPseg$r[2:raySegLen])
					RAP$idx = c(RAP$idx, RAPseg$idx[2:raySegLen])
				}
			}
			else
			{
				RAP$ix = c(RAP$ix, RAPseg$ix[1:raySegLen])
				RAP$iy = c(RAP$iy, RAPseg$iy[1:raySegLen])
				RAP$iz = c(RAP$iz, RAPseg$iz[1:raySegLen])
				RAP$r = c(RAP$r, RAPseg$r[1:raySegLen])
				RAP$idx = c(RAP$idx, RAPseg$idx[1:raySegLen])
			}
# 			print(paste("    ++++ ray: ", length(RAP$idx)))
# 			print(RAP)
		}
	}

	return(RAP)
}

#### Generate the equation systems Ax=b and partition Ax=b to {A1x1=b1; A2x2=b2; ...; Amxm=bm}
#### input: 
#### 	Cube: the inputed ground truth tomography cube, which will be used to generate the EQ events, arrival timing and rays for our tomography computation.
####	partitionDim: the dimension of partitions (e.g., the region will be partitioned to partitionDim*partitionDim), where each partition will be assigned to a node to compute.
####	resolutionDim: the resolution dimension that we will compute. It is typically 4 times smaller than cubeDimension.
#### output: list systems

#### Data structure of systems
####  
####  For the approximation method (flag = 0):
####  systems$subSystems[[i]] contains the sub system in partition i
####    systems$subSystems[[i]][[j]]$A is a vector of the non-zero values of j-th subray in partition i
####    systems$subSystems[[i]][[j]]$idx is a vector of the non-zero values' index of j-th subray in partition i
####    systems$subSystems[[i]][[j]]$b is the travel time residual of j-th subray in partition i
####  
####  For the exact method (flag = 1):
####  ttSystem[[i]]$A is a vector of 1s with length of how many parts ray i is partitioned
####  ttSystem[[i]]$idx is a vector of the indexes of the partial travel time residual
####  ttSystem[[i]]$b is the travel time residual of ray i
####  
####  systems$subSystems[[i]] contains the sub system in partition i 
####    systems$subSystems[[i]][[j]]$A is a vector of the non-zero values of j-th subray in partition i, and a -1 to the corresponding partial travel time residual 
####    systems$subSystems[[i]][[j]]$idx is a vector of the non-zero value indexes of j-th subray in partition i, and index of the partial travel time residual
####    systems$subSystems[[i]][[j]]$b is 0
####
GenerateAxb <- function(cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventStart, eventNum, flag, fullRank = FALSE, noise = 0.0)
{
	vecLength = resolutionDim^3
	#### indicate the cells penetrated
	blocks = array(0, vecLength)

	#### cube resolution blocks partitioin for generating observations
	seghi = side/cubeDim
	xohi = seq(0, side, seghi)
	yohi = seq(0, side, seghi)
	zohi = seq(0, side, seghi)

	#### blocks partition with required resolution for getting ray path blocks
	seg = side/resolutionDim
	xo = seq(0, side, seg)
	yo = seq(0, side, seg)
	zo = seq(0, side, seg)

	#### load the station and events coordinates

	stationEventLog = LoadStationEventLog(stationNum, eventStart, eventNum)
	stax = stationEventLog$stax
	stay = stationEventLog$stay
	staz = stationEventLog$staz
	evx = stationEventLog$evx
	evy = stationEventLog$evy
	evz = stationEventLog$evz

	#### initialize partition parameter
	parts = partitionDim*partitionDim
	part = sqrt(parts)
	base = resolutionDim/part

	#### total ray number
	raylimit = stationNum*eventNum
	
	#### initialize the systems to generate
	systems = list()

	rays = 0
	originSystem = array(list(), raylimit+vecLength)

	numPartRays = array(0, parts)
	subSystems = array(list(), parts)
	for(i in 1:parts)
	{
		subSystems[[i]] = array(list(), raylimit)
	}
	
	ttSystem = array(list(), raylimit)
	#### initial guess of the travel time residual partition
	ttInit = array()
	
	#### number of partial time residuals
	numSubt = 0
	firstWrite = TRUE

	#### generating noise
	set.seed(14)
	wgn = runif(raylimit+vecLength, 0, 1)

	#### ray info lost ratio
	set.seed(1111)
	lostRatio = runif(raylimit*4, 0, 10)
	lostRatioTh = 0
        
	for(i in 1:eventNum)
	{
		ptm = proc.time()
		for(j in 1:stationNum)
		{
			rays = rays + 1

			rx = c(evx[i], stax[j])
			ry = c(evy[i], stay[j])
			rz = c(evz[i], staz[j])
			
			# print(paste("----> rays: ", rays))
			
			
			RAPhi = GetRayblox(rx, ry, rz, xohi, yohi, zohi)
			mhi = length(RAPhi$r)
			
# 			ttobs = 0.0
# 			for(k in 1:mhi)
# 			{
# 				ttobs = ttobs + RAPhi$r[k] * cube[RAPhi$idx[k]]
# 			}
			ttobs = (RAPhi$r %*% cube[RAPhi$idx])[1]

			#### adding noise
			ttobs = ttobs + wgn[rays]*noise

			RAP = GetRayblox(rx, ry, rz, xo, yo, zo)

			#### indicate which partition the blocks belong to
			m = length(RAP$r)
			idxcount = 0
			for(k in 1:m)
			{
				ix = RAP$ix[k]
				iy = RAP$iy[k]
				
				secidx = 0
				for(a in 1:part)
				{
					for(b in 1:part)
					{
						secidx = secidx + 1
						xscale_a = (a-1)*base + 1
						xscale_b = a*base
						yscale_a = (b-1)*base + 1
						yscale_b = b*base
						if(ix >= xscale_a && ix <= xscale_b && iy >= yscale_a && iy <= yscale_b)
						{
							RAP$ib[k] = secidx
							idxcount = idxcount + 1
						}
					}
				}
				
# 				RAP$idx[k] = (RAP$ix[k]-1)*resolutionDim*resolutionDim + (RAP$iy[k]-1)*resolutionDim + RAP$iz[k]
			}

			if(idxcount != m || length(which(RAP$idx == 0)))
			{
				print("Rayblox error!!!!")
				stop()
			}

#  			ttpdt = 0.0
#  			for(k in 1:m)
#  			{
#  				ttpdt = ttpdt + RAP$r[k] * slowness[RAP$idx[k]]
#  			}

			blocks[RAP$idx] = 1
  
			ttpdt = (RAP$r %*% slowness[RAP$idx])[1]
			tt = ttobs - ttpdt
			
			#### initial total travel time residual
			if(flag == 1)
			{
				ttSystem[[rays]]$b = tt
			}

			originSystem[[rays]]$A = RAP$r
			originSystem[[rays]]$idx = RAP$idx
			originSystem[[rays]]$b = tt

			#### partition index
			rayparts = 0

			for(partidx in 1:parts)
			{
				m = length(RAP$ib[RAP$ib==partidx])
				if(m > 0)
				{
					numSubt = numSubt + 1
					rayparts = rayparts + 1
					
					ix = RAP$ix[RAP$ib==partidx]
					iy = RAP$iy[RAP$ib==partidx]
					iz = RAP$iz[RAP$ib==partidx]
					lr = RAP$r[RAP$ib==partidx]
					ib = RAP$ib[RAP$ib==partidx]
					idx = RAP$idx[RAP$ib==partidx]
					ray = list(ix=ix, iy=iy, iz=iz, r=lr, ib=ib, idx=idx)
					
# 					ttpp = 0.0
# 					for(k in 1:m)
# 					{
# 						ttpp = ttpp + ray$r[k] * slowness[ray$idx[k]]
# 					}
					ttpp = (ray$r %*% slowness[ray$idx])[1]
					
					ttp = (ttpp/ttpdt)*tt

					if(lostRatio[numSubt] > lostRatioTh)
					{
						numPartRays[partidx] = numPartRays[partidx]+1
						subSystems[[partidx]][[numPartRays[partidx]]]$A = ray$r
						subSystems[[partidx]][[numPartRays[partidx]]]$idx = ray$idx
						
						#### initialize the sub equation systems
						if(flag == 0 || flag == 2)
						{
							subSystems[[partidx]][[numPartRays[partidx]]]$b = ttp
						}
						else if(flag == 1)
						{
							ttSystem[[rays]]$A[rayparts] = 1
							ttSystem[[rays]]$idx[rayparts] = numSubt
							ttInit[numSubt] = ttp
							
							subSystems[[partidx]][[numPartRays[partidx]]]$A[m+1] = -1
							subSystems[[partidx]][[numPartRays[partidx]]]$idx[m+1] = vecLength+numSubt
							subSystems[[partidx]][[numPartRays[partidx]]]$b = 0
						}
					}
				}else
				{
					numSubt = numSubt + 1
					rayparts = rayparts + 1

					if(lostRatio[numSubt] > lostRatioTh)
					{
						numPartRays[partidx] = numPartRays[partidx]+1
						subSystems[[partidx]][[numPartRays[partidx]]]$A = 0
						subSystems[[partidx]][[numPartRays[partidx]]]$idx = 1
						
						#### initialize the sub equation systems
						if(flag == 0 || flag == 2)
						{
							subSystems[[partidx]][[numPartRays[partidx]]]$b = 0.0
						}
						else if(flag == 1)
						{
							ttSystem[[rays]]$A[rayparts] = 1
							ttSystem[[rays]]$idx[rayparts] = numSubt
							ttInit[numSubt] = 0.0
							
							subSystems[[partidx]][[numPartRays[partidx]]]$A[m+1] = -1
							subSystems[[partidx]][[numPartRays[partidx]]]$idx[m+1] = vecLength+numSubt
							subSystems[[partidx]][[numPartRays[partidx]]]$b = 0.0
						}
					}
				}
			}
		}
		etime = proc.time() - ptm
		print(paste("Ray generation done, event ----", i))
		print(etime)
# 		if(i >= 1)
# 		{
# 			stop()
# 		}
	}

	#### test for full rank
	if(fullRank)
	{
		set.seed(22)
		zeroCols = which(blocks==0)
		frandom = runif(length(zeroCols)*3, -1, 1)
# 		print(length(zeroCols))
# 		stop()
		numBlocks = 0
		for(i in zeroCols)
		{
			iz = i %% resolutionDim
			if(iz == 0)
			{
				iz = 10
			}
			iy = ((i - iz) %% (resolutionDim*resolutionDim))/resolutionDim + 1
			ix = (i - iz - (iy-1)*resolutionDim)/(resolutionDim*resolutionDim) + 1

			p1 = c(ix*seg-(seg/2), iy*seg-(seg/2), iz*seg-(seg/2))

			rays = rays + 1

			rx = c(p1[1], p1[1]+frandom[numBlocks*3+1]*(seg/2))
			ry = c(p1[2], p1[2]+frandom[numBlocks*3+2]*(seg/2))
			rz = c(p1[3], p1[3]+frandom[numBlocks*3+3]*(seg/2))
			
# 			print("^^^^^^^^^^^^^^^^^^^^^^^^^^^")
# 			print(paste(length(frandom), numBlocks, numBlocks*3+1, numBlocks*3+2, numBlocks*3+3))
# 			print(rx)
# 			print(ry)
# 			print(rz)

			RAPhi = GetRayblox(rx, ry, rz, xohi, yohi, zohi)
			mhi = length(RAPhi$r)
			ttobs = (RAPhi$r %*% cube[RAPhi$idx])[1]

			RAP = GetRayblox(rx, ry, rz, xo, yo, zo)
			blocks[RAP$idx] = 1
  
			ttpdt = (RAP$r %*% slowness[RAP$idx])[1]
			tt = ttobs - ttpdt

			originSystem[[rays]]$A = RAP$r
			originSystem[[rays]]$idx = RAP$idx
			originSystem[[rays]]$b = tt
			
			numBlocks = numBlocks + 1
		}
	}

	for(i in 1:parts)
	{
		subSystems[[i]]$rayNum = numPartRays[i]
	}
		
	if(flag == 0 || flag == 2)
	{
		systems = list(subSystems = subSystems)
	}
	else if(flag == 1)
	{
		systems = list(subSystems = subSystems, ttSystem = ttSystem, ttInit = ttInit)
	}

	systems$originSystem = originSystem
	systems$originRows = rays
	systems$blocks = blocks

	return(systems)
}

GenerateAxbCore <- function(cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventStart, eventNum, flag, fullRank = FALSE, noise = 0.0)
{
	vecLength = resolutionDim^3
	#### indicate the cells penetrated
	blocks = array(0, vecLength)

	#### cube resolution blocks partitioin for generating observations
	seghi = side/cubeDim
	xohi = seq(0, side, seghi)
	yohi = seq(0, side, seghi)
	zohi = seq(0, side, seghi)

	#### blocks partition with required resolution for getting ray path blocks
	seg = side/resolutionDim
	xo = seq(0, side, seg)
	yo = seq(0, side, seg)
	zo = seq(0, side, seg)

	#### load the station and events coordinates

	stationEventLog = LoadStationEventLog(stationNum, eventStart, eventNum)
	stax = stationEventLog$stax
	stay = stationEventLog$stay
	staz = stationEventLog$staz
	evx = stationEventLog$evx
	evy = stationEventLog$evy
	evz = stationEventLog$evz

	#### initialize partition parameter
	parts = partitionDim*partitionDim
	part = sqrt(parts)
	base = resolutionDim/part

	#### total ray number
	raylimit = stationNum*eventNum
	
	#### initialize the systems to generate
	systems = list()

	rays = 0
	originSystem = array(list(), raylimit+vecLength)

	numPartRays = array(0, parts)
	subSystems = array(list(), parts)
	for(i in 1:parts)
	{
		subSystems[[i]] = array(list(), raylimit)
	}
	
	ttSystem = array(list(), raylimit)
	#### initial guess of the travel time residual partition
	ttInit = array()
	
	#### number of partial time residuals
	numSubt = 0
	firstWrite = TRUE

	#### generating noise
	set.seed(14)
	wgn = runif(raylimit+vecLength, 0, 1)

	#### ray info lost ratio
	set.seed(1111)
	lostRatio = runif(raylimit*4, 0, 10)
	lostRatioTh = 0
        
	for(i in 1:eventNum)
	{
		ptm = proc.time()
		for(j in 1:stationNum)
		{
			rays = rays + 1

			rx = c(evx[i], stax[j])
			ry = c(evy[i], stay[j])
			rz = c(evz[i], staz[j])
			
			# print(paste("----> rays: ", rays))
			
			
			RAPhi = GetRayblox(rx, ry, rz, xohi, yohi, zohi)
			mhi = length(RAPhi$r)
			
			ttobs = (RAPhi$r %*% cube[RAPhi$idx])[1]

			#### adding noise
			ttobs = ttobs + wgn[rays]*noise

			RAP = GetRayblox(rx, ry, rz, xo, yo, zo)

			ttpdt = (RAP$r %*% slowness[RAP$idx])[1]
			tt = ttobs - ttpdt
			#### log the rays and observations
			if(firstWrite)
			{
				write(raylimit, file=paste("./TTObserved", "_", resolutionDim, "_", eventNum, sep=""), sep="\n")
				write(formatC(tt, digits=32, format="f"), file=paste("./TTObserved", "_", resolutionDim, "_", eventNum, sep=""), sep="\n", append=TRUE)
				write(raylimit, file=paste("./ResRays", "_", resolutionDim, "_", eventNum, sep=""), sep="\n")
				write(length(RAP$r), file=paste("./ResRays", "_", resolutionDim, "_", eventNum, sep=""), append=TRUE)
				firstWrite = FALSE
			}
			else
			{
				write(formatC(tt, digits=32, format="f"), file=paste("./TTObserved", "_", resolutionDim, "_", eventNum, sep=""), sep="\n", append=TRUE)
				write(length(RAP$r), file=paste("./ResRays", "_", resolutionDim, "_", eventNum, sep=""), sep="\n", append=TRUE)
			}

			write(RAP$ix, file=paste("./ResRays", "_", resolutionDim, "_", eventNum, sep=""), sep="\n", append=TRUE)
			write(RAP$iy, file=paste("./ResRays", "_", resolutionDim, "_", eventNum, sep=""), sep="\n", append=TRUE)
			write(RAP$iz, file=paste("./ResRays", "_", resolutionDim, "_", eventNum, sep=""), sep="\n", append=TRUE)
			write(formatC(RAP$r, digits=32, format="f"), file=paste("./ResRays", "_", resolutionDim, "_", eventNum, sep=""), sep="\n", append=TRUE)
		}
		etime = proc.time() - ptm
		print(paste("Ray generation done, event ----", i))
		print(etime)
	}

	systems$originSystem = originSystem
	systems$originRows = rays

	return(systems)
}


#### generate partition index map table
PartIdxMap <- function(part, res)
{
	idxMap = list()
	dim = res
	part = part
	base = dim/part

	for(i in 1:dim)
	{
		arr = array(i, dim*dim)
		idxMap$ix = c(idxMap$ix, arr)
	}

	for(i in 1:dim)
	{
		for(j in 1:dim)
		{
			arr = array(j, dim)
			idxMap$iy = c(idxMap$iy, arr)
		}
	}

	for(i in 1:dim)
	{
		for(j in 1:dim)
		{
			idxMap$iz = c(idxMap$iz, 1:dim)
		}
	}

# 	print("%%%%%%%%")
# 	print(idxMap$ix)
# 	print(idxMap$iy)
# 	print(idxMap$iz)

	m = dim^3
	for(k in 1:m)
	{
		ix = idxMap$ix[k]
		iy = idxMap$iy[k]
		
		secidx = 0
		for(a in 1:part)
		{
			for(b in 1:part)
			{
				secidx = secidx + 1
				xscale_a = (a-1)*base + 1
				xscale_b = a*base
				yscale_a = (b-1)*base + 1
				yscale_b = b*base
				if(ix >= xscale_a && ix <= xscale_b && iy >= yscale_a && iy <= yscale_b)
				{
					idxMap$ib[k] = secidx
				}
			}
		}
		
		idxMap$idx[k] = (idxMap$ix[k]-1)*dim*dim + (idxMap$iy[k]-1)*dim + idxMap$iz[k]
	}

# 	print("=========================")
# 	print(idxMap$idx)
# 	print(idxMap$ib)

	return(idxMap)
}

#### test for system splitting
GenerateAxbSplit <- function(cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventStart, eventNum, flag, fullRank = FALSE)
{
	vecLength = resolutionDim^3
	#### indicate the cells penetrated
	blocks = array(0, vecLength)

	#### cube resolution blocks partitioin for generating observations
	seghi = side/cubeDim
	xohi = seq(0, side, seghi)
	yohi = seq(0, side, seghi)
	zohi = seq(0, side, seghi)

	#### blocks partition with required resolution for getting ray path blocks
	seg = side/resolutionDim
	xo = seq(0, side, seg)
	yo = seq(0, side, seg)
	zo = seq(0, side, seg)

	#### load the station and events coordinates
	stationEventLog = LoadStationEventLog(stationNum, eventStart, eventNum)
	stax = stationEventLog$stax
	stay = stationEventLog$stay
	staz = stationEventLog$staz
	evx = stationEventLog$evx
	evy = stationEventLog$evy
	evz = stationEventLog$evz

	#### initialize partition parameter
	parts = partitionDim*partitionDim
	part = sqrt(parts)
	base = resolutionDim/part

	#### total ray number
	raylimit = stationNum*eventNum
	
	#### initialize the systems to generate
	systems = list()

	rays = 0
	originSystem = array(list(), raylimit+vecLength)

	numPartRays = array(0, parts)
	subSystems = array(list(), parts)
	for(i in 1:parts)
	{
		subSystems[[i]] = array(list(), raylimit)
	}
	
	ttSystem = array(list(), raylimit)
	#### initial guess of the travel time residual partition
	ttInit = array()
	
	#### number of partial time residuals
	numSubt = 0
	firstWrite = TRUE

	#### generating noise
	set.seed(14)
	wgn = runif(raylimit, -1, 1)

	#### ray info lost ratio
	set.seed(1111)
	lostRatio = runif(raylimit*4, 0, 10)
	lostRatioTh = 0

	#### generate a partition idx map table
	map = PartIdxMap(part, resolutionDim)
        
	for(i in 1:eventNum)
	{
		ptm = proc.time()
		for(j in 1:stationNum)
		{
			rays = rays + 1

			rx = c(evx[i], stax[j])
			ry = c(evy[i], stay[j])
			rz = c(evz[i], staz[j])
			
			# print(paste("----> rays: ", rays))
			
			
			RAPhi = GetRayblox(rx, ry, rz, xohi, yohi, zohi)
			mhi = length(RAPhi$r)
			
# 			ttobs = 0.0
# 			for(k in 1:mhi)
# 			{
# 				ttobs = ttobs + RAPhi$r[k] * cube[RAPhi$idx[k]]
# 			}
			ttobs = (RAPhi$r %*% cube[RAPhi$idx])[1]

			#### adding noise
# 			ttobs = ttobs + wgn[rays]*0.04

			RAP = GetRayblox(rx, ry, rz, xo, yo, zo)

			#### indicate which partition the blocks belong to
			RAP$ib = map$ib[RAP$idx]

			blocks[RAP$idx] = 1
  
			ttpdt = (RAP$r %*% slowness[RAP$idx])[1]
			tt = ttobs - ttpdt
			
			#### initial total travel time residual
			if(flag == 1)
			{
				ttSystem[[rays]]$b = tt
			}

			originSystem[[rays]]$A = RAP$r
			originSystem[[rays]]$idx = RAP$idx
			originSystem[[rays]]$b = tt

			#### partition index
			rayparts = 0

			for(partidx in 1:parts)
			{
				m = length(RAP$ib[RAP$ib==partidx])
				if(m > 0)
				{
					numSubt = numSubt + 1
					rayparts = rayparts + 1
					
					ix = RAP$ix[RAP$ib==partidx]
					iy = RAP$iy[RAP$ib==partidx]
					iz = RAP$iz[RAP$ib==partidx]
					lr = RAP$r[RAP$ib==partidx]
					ib = RAP$ib[RAP$ib==partidx]
					idx = RAP$idx[RAP$ib==partidx]
					ray = list(ix=ix, iy=iy, iz=iz, r=lr, ib=ib, idx=idx)
					
# 					ttpp = 0.0
# 					for(k in 1:m)
# 					{
# 						ttpp = ttpp + ray$r[k] * slowness[ray$idx[k]]
# 					}
					ttpp = (ray$r %*% slowness[ray$idx])[1]
					
					ttp = (ttpp/ttpdt)*tt

					if(lostRatio[numSubt] > lostRatioTh)
					{
						numPartRays[partidx] = numPartRays[partidx]+1
						subSystems[[partidx]][[numPartRays[partidx]]]$A = ray$r
# 						subSystems[[partidx]][[numPartRays[partidx]]]$idx = ray$idx
						subSysIdxMap = which(map$ib == partidx)
						subSystems[[partidx]][[numPartRays[partidx]]]$idx = which(subSysIdxMap %in% ray$idx)
						
						#### initialize the sub equation systems
						if(flag == 0 || flag == 2)
						{
							subSystems[[partidx]][[numPartRays[partidx]]]$b = ttp
						}
						else if(flag == 1)
						{
							ttSystem[[rays]]$A[rayparts] = 1
							ttSystem[[rays]]$idx[rayparts] = numSubt
							ttInit[numSubt] = ttp
							
							subSystems[[partidx]][[numPartRays[partidx]]]$A[m+1] = -1
							subSystems[[partidx]][[numPartRays[partidx]]]$idx[m+1] = vecLength+numSubt
							subSystems[[partidx]][[numPartRays[partidx]]]$b = 0
						}
					}
				}else
				{
					numSubt = numSubt + 1
					rayparts = rayparts + 1

					if(lostRatio[numSubt] > lostRatioTh)
					{
						numPartRays[partidx] = numPartRays[partidx]+1
						subSystems[[partidx]][[numPartRays[partidx]]]$A = 0
						subSystems[[partidx]][[numPartRays[partidx]]]$idx = 1
						
						#### initialize the sub equation systems
						if(flag == 0 || flag == 2)
						{
							subSystems[[partidx]][[numPartRays[partidx]]]$b = 0.0
						}
						else if(flag == 1)
						{
							ttSystem[[rays]]$A[rayparts] = 1
							ttSystem[[rays]]$idx[rayparts] = numSubt
							ttInit[numSubt] = 0.0
							
							subSystems[[partidx]][[numPartRays[partidx]]]$A[m+1] = -1
							subSystems[[partidx]][[numPartRays[partidx]]]$idx[m+1] = vecLength+numSubt
							subSystems[[partidx]][[numPartRays[partidx]]]$b = 0.0
						}
					}
				}
			}
		}
		etime = proc.time() - ptm
		print(paste("Ray generation done, event ----", i))
		print(etime)
# 		if(i >= 1)
# 		{
# 			stop()
# 		}
	}

	#### test for full rank
	if(fullRank)
	{
		set.seed(22)
		zeroCols = which(blocks==0)
		frandom = runif(length(zeroCols)*3, -1, 1)
# 		print(length(zeroCols))
# 		stop()
		numBlocks = 0
		for(i in zeroCols)
		{
			iz = i %% resolutionDim
			if(iz == 0)
			{
				iz = 10
			}
			iy = ((i - iz) %% (resolutionDim*resolutionDim))/resolutionDim + 1
			ix = (i - iz - (iy-1)*resolutionDim)/(resolutionDim*resolutionDim) + 1

			p1 = c(ix*seg-(seg/2), iy*seg-(seg/2), iz*seg-(seg/2))

			rays = rays + 1

			rx = c(p1[1], p1[1]+frandom[numBlocks*3+1]*(seg/2))
			ry = c(p1[2], p1[2]+frandom[numBlocks*3+2]*(seg/2))
			rz = c(p1[3], p1[3]+frandom[numBlocks*3+3]*(seg/2))
			
# 			print("^^^^^^^^^^^^^^^^^^^^^^^^^^^")
# 			print(paste(length(frandom), numBlocks, numBlocks*3+1, numBlocks*3+2, numBlocks*3+3))
# 			print(rx)
# 			print(ry)
# 			print(rz)

			RAPhi = GetRayblox(rx, ry, rz, xohi, yohi, zohi)
			mhi = length(RAPhi$r)
			ttobs = (RAPhi$r %*% cube[RAPhi$idx])[1]

			RAP = GetRayblox(rx, ry, rz, xo, yo, zo)
			blocks[RAP$idx] = 1
  
			ttpdt = (RAP$r %*% slowness[RAP$idx])[1]
			tt = ttobs - ttpdt

			originSystem[[rays]]$A = RAP$r
			originSystem[[rays]]$idx = RAP$idx
			originSystem[[rays]]$b = tt
			
			numBlocks = numBlocks + 1
		}
	}

	for(i in 1:parts)
	{
		subSystems[[i]]$rayNum = numPartRays[i]
	}
		
	if(flag == 0 || flag == 2)
	{
		systems = list(subSystems = subSystems)
	}
	else if(flag == 1)
	{
		systems = list(subSystems = subSystems, ttSystem = ttSystem, ttInit = ttInit)
	}

	systems$map = map
	systems$originSystem = originSystem
	systems$originRows = rays
	systems$blocks = blocks

	return(systems)
}

#### Generate the equation systems Ax=b (considering 1D raytracing)
#### input: 
#### 	Cube: the inputed ground truth tomography cube, which will be used to generate the EQ events, arrival timing and rays for our tomography computation.
####	partitionDim: the dimension of partitions (e.g., the region will be partitioned to partitionDim*partitionDim), where each partition will be assigned to a node to compute.
####	resolutionDim: the resolution dimension that we will compute. It is typically 4 times smaller than cubeDimension.
#### output: originSystem = array(list(), raylimit), raylimit is the number of rays

GenerateLayerAxb <- function(cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventStart, eventNum, flag, velocity)
{
	vecLength = resolutionDim^3

	#### cube resolution blocks partitioin for generating observations
	seghi = side/cubeDim
	xohi = seq(0, side, seghi)
	yohi = seq(0, side, seghi)
	zohi = seq(0, side, seghi)

	#### blocks partition with required resolution for getting ray path blocks
	seg = side/resolutionDim
	xo = seq(0, side, seg)
	yo = seq(0, side, seg)
	zo = seq(0, side, seg)

	#### load the station and events coordinates

	stationEventLog = LoadStationEventLog(stationNum, eventStart, eventNum)
	stax = stationEventLog$stax
	stay = stationEventLog$stay
	staz = stationEventLog$staz
	evx = stationEventLog$evx
	evy = stationEventLog$evy
	evz = stationEventLog$evz

	#### initialize partition parameter
	parts = partitionDim*partitionDim
	part = sqrt(parts)
	base = resolutionDim/part

	#### total ray number
	raylimit = stationNum*eventNum
	
	#### initialize the systems to generate
	systems = list()

	rays = 0
# 	originSystem = array(list(), raylimit+vecLength)    #### for smoothing
	originSystem = array(list(), raylimit)

	numPartRays = array(0, parts)
	subSystems = array(list(), parts)
	for(i in 1:parts)
	{
		subSystems[[i]] = array(list(), raylimit)
	}

	numSubt = 0
	firstWrite = TRUE

	#### generating noise
	set.seed(14)
	wgn = runif(raylimit, 0, 1)

	# render the ray paths for testing 1D raytracing
	rex = range(c(stax, 0) )
	rey = range(c(stay, 0) )
	
	### visulization for 1D raytracing test
# 	rgl.open()
# 	view3d( theta = 0, phi = -75)
# 	quads3d(c(rex[1],rex[2], rex[2],rex[1]), y = c(rey[1], rey[1], rey[2], rey[2]), z = rep(0, 4) , col='white', alpha=0.4)

	### how many bad rays
	badRays = 0
        
	for(i in 1:eventNum)
	{
		for(j in 1:stationNum)
		{
# 			print(paste("----> rays:", rays))

			rx = c(evx[i], stax[j])
			ry = c(evy[i], stay[j])
			rz = c(evz[i], staz[j])
			
			# print(paste("----> rays: ", rays))

			# use 1D velocity model to trace the ray
			dee = sqrt((rx[2]-rx[1])^2 + (ry[2]-ry[1])^2)

			sink(file="/dev/null")
			raypath = Ray.time1D(dee, rz[1], rz[2], length(velocity$zp), velocity$zp, 1/velocity$vp)
			sink()

			az = atan2(ry[2]-ry[1], rx[2]-rx[1])
			zx=cos(az)*raypath$rnod[1:raypath$nnod] + rx[1]
			zy=sin(az)*raypath$rnod[1:raypath$nnod] + ry[1]
			zz = raypath$znod[1:raypath$nnod]
# 			lines3d(cbind(zx, zy, zz) , col='red')
# 			points3d(cbind(zx[raypath$nnod] ,zy[raypath$nnod], zz[raypath$nnod]), col=rgb(.6,.6,1))

			# get the ray blocks
			raysegs = raypath$nnod - 1
			RAPhi = Get1DRayblox(raysegs, zx, zy, zz, xohi, yohi, zohi)
			if(is.null(RAPhi))
			{
				print("bad ray plus 1")
				badRays = badRays + 1
				next
			}

			mhi = length(RAPhi$r)
			ttobs = 0.0
			for(k in 1:mhi)
			{
				ttobs = ttobs + RAPhi$r[k] * cube[RAPhi$idx[k]]
			}

			RAP = Get1DRayblox(raysegs, zx, zy, zz, xo, yo, zo)

			#### indicate which partition the blocks belong to
			m = length(RAP$r)
			idxcount = 0
			for(k in 1:m)
			{
				ix = RAP$ix[k]
				iy = RAP$iy[k]
				
				secidx = 0
				for(a in 1:part)
				{
					for(b in 1:part)
					{
						secidx = secidx + 1
						xscale_a = (a-1)*base + 1
						xscale_b = a*base
						yscale_a = (b-1)*base + 1
						yscale_b = b*base
						if(ix >= xscale_a && ix <= xscale_b && iy >= yscale_a && iy <= yscale_b)
						{
							RAP$ib[k] = secidx
							idxcount = idxcount + 1
						}
					}
				}
			}

			if(idxcount != m || length(which(RAP$idx == 0)))
			{
				print("Rayblox error!!!!")
				stop()
			}

			rays = rays + 1

			#### adding noise
# 			ttobs = ttobs + wgn[rays]*0.02

 			ttpdt = 0.0
 			for(k in 1:m)
 			{
 				ttpdt = ttpdt + RAP$r[k] * slowness[RAP$idx[k]]
 			}
			tt = ttobs - ttpdt

			originSystem[[rays]]$A = RAP$r
			originSystem[[rays]]$idx = RAP$idx
			originSystem[[rays]]$b = tt

			#### partition index
			rayparts = 0
			for(partidx in 1:parts)
			{
				m = length(RAP$ib[RAP$ib==partidx])
				if(m > 0)
				{
					numSubt = numSubt + 1
					rayparts = rayparts + 1
					
					ix = RAP$ix[RAP$ib==partidx]
					iy = RAP$iy[RAP$ib==partidx]
					iz = RAP$iz[RAP$ib==partidx]
					lr = RAP$r[RAP$ib==partidx]
					ib = RAP$ib[RAP$ib==partidx]
					idx = RAP$idx[RAP$ib==partidx]
					ray = list(ix=ix, iy=iy, iz=iz, r=lr, ib=ib, idx=idx)
					
					ttpp = 0.0
					for(k in 1:m)
					{
						ttpp = ttpp + ray$r[k] * slowness[ray$idx[k]]
					}
					
					ttp = (ttpp/ttpdt)*tt

					numPartRays[partidx] = numPartRays[partidx]+1
					subSystems[[partidx]][[numPartRays[partidx]]]$A = ray$r
					subSystems[[partidx]][[numPartRays[partidx]]]$idx = ray$idx
					subSystems[[partidx]][[numPartRays[partidx]]]$b = ttp
				}
			}
		}
		print(paste("Ray generation done, event ----", i))
	}

	for(i in 1:parts)
	{
		subSystems[[i]]$rayNum = numPartRays[i]
	}
		
	systems = list(subSystems = subSystems)

	systems$originSystem = originSystem
	systems$originRows = rays

	return(systems)
}

GenerateRealLayerAxbCarp <- function(xLen, yLen, zLen, xInt, yInt, zInt, velocity, slowness, events, stations, raylimit, eventNum, lsqrSmooth = FALSE)
{
	xo = seq(0, xLen, xInt)
	yo = seq(0, yLen, yInt)
	zo = seq(0, zLen, zInt)

# 	print(xo)
# 	print(yo)
# 	print(zo)
	xRes = xLen/xInt
	yRes = yLen/yInt
	zRes = zLen/zInt

	vecLength = xRes*yRes*zRes	
	#### initialize the systems to generate
	systems = list()

	rays = 0
    if(lsqrSmooth) {
	    originSystem = array(list(), raylimit+vecLength)
    } else {
        originSystem = array(list(), raylimit)
    }

    num_stn = length(stations$name)
    numPartRays = array(0, num_stn)
    subSystems = array(list(), num_stn)
	for(i in 1:num_stn)
	{
		subSystems[[i]] = array(list(), raylimit)
	}
	
    # render the ray paths for testing 1D raytracing
	rex = range(c(stations$x, 0) )
	rey = range(c(stations$y, 0) )
	print(rex)
	print(rey)
	# rgl.open()

	#### how many bad rays
	badRays = 0
	fuzzyRays = 0
	posPerturb = 0

	#### reorganize the velocity model for Ray.time1D
	velAdj = list()
	velLen = length(velocity$zp)

	#### test for counting perturbation on tt
	ttpert = array(0, raylimit)
        
	print(eventNum)
	for(i in 1:eventNum)
	{
		evx = events[[i]]$x
		evy = events[[i]]$y
		evz = events[[i]]$depth
		staNum = length(events[[i]]$station)
# 		print(paste(i, evx, evy, evz, staNum))
		for(j in 1:staNum)
		{
			stnIdx = which(stations$name == events[[i]]$station[j])
			numPartRays[stnIdx] = numPartRays[stnIdx] + 1
			stax = stations$x[stnIdx]
			stay = stations$y[stnIdx]
			staz = stations$alt[stnIdx]
		
            # test
            #evx = 78.249670
            #evy = 115.543840
            #evz = 5.311567

			rx = c(evx, stax)
			ry = c(evy, stay)
			rz = c(evz, staz)

            #print("=================================================")
            #print(rx)
            #print(ry)
            #print(rz)
            #print(zp)
            #print()
            #print("=================================================")

			velStart = (which(velocity$zp > rz[2])[1] - 1)

            #print("&&&&&&&&&&&&&&&&")
            #print(velocity$zp)
            #print(rz[2])
            #print(which(velocity$zp > rz[2]))
            #print(velStart)


			velAdj$zp = (velocity$zp - rz[2])[velStart:velLen]
			#print(velAdj$zp)
            velAdj$zp[1] = 0
            #print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
            #print(stations$name[stnIdx])
            #print(rx)
            #print(ry)
            #print(rz)
            #print(velAdj$zp)
            velAdj$vp = velocity$vp[velStart:velLen]
            #print(velAdj$vp)
			
			# use 1D velocity model to trace the ray
			dee = sqrt((rx[2]-rx[1])^2 + (ry[2]-ry[1])^2)
			#print("========>>>>>>>>")
			raypath = Ray.time1D(dee, rz[1] - rz[2], 0, length(velAdj$zp), velAdj$zp, 1/velAdj$vp)
			#print("<<<<<<<<========")

			az = atan2(ry[2]-ry[1], rx[2]-rx[1])
			zx=cos(az)*raypath$rnod[1:raypath$nnod] + rx[1]
			zy=sin(az)*raypath$rnod[1:raypath$nnod] + ry[1]
			# plus 0.001 for BAD ray test
			zz = raypath$znod[1:raypath$nnod] + rz[2]

            #print("<<<<<<<<<<<<<<<<<<")
            #print(raypath)

			# lines3d(cbind(zx, zy, zz) , col='red')
			# points3d(cbind(zx[raypath$nnod] ,zy[raypath$nnod], zz[raypath$nnod]), col=rgb(.6,.6,1))

# 			if(raypath$znod[1] <= raypath$znod[2])
# 			{
# 				badRays = badRays + 1
# 				print(paste("BAD rays: ", badRays))
# 				RAP = Get1DRayblox(raysegs, zx, zy, zz, xo, yo, zo)
# 				print("%%%%%%%%%%%%%%%%%%%%%%%%%%")
# 				print(zx)
# 				print(zy)
# 				print(zz)
# 				print(RAP)
# 				print("%%%%%%%%%%%%%%%%%%%%%%%%%%")
# 				stop()
# 			}
			### get the ray blocks
			raysegs = raypath$nnod - 1

			### two same starting points (removes one)
			if(zx[1]==zx[2] && zy[1]==zy[2] && zz[1]==zz[2])
			{
				zx = zx[2:length(zx)]
				zy = zy[2:length(zy)]
				zz = zz[2:length(zz)]
				raysegs = raysegs - 1
				#print(paste(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", raysegs))
				#print(zx)
				#print(zy)
				#print(zz)
				fuzzyRays = fuzzyRays + 1
			}

			RAP = Get1DRayblox(raysegs, zx, zy, zz, xo, yo, zo)

            #print("--------------------------------------")
            #print(RAP)
			if(is.null(RAP))
			{
				badRays = badRays + 1
				print(paste("BAD rays: ", badRays))
				print("&&&&&&&&&&&&&&&&&&&&&&&&&&&")
				print(paste("    ====", dee, rz[1], rz[2], rz[1]-rz[2]))
				print(velAdj)
				print(velocity)
				print(raypath)
				print("===========================")
# 				print(raysegs)
# 				print(rx)
# 				print(ry)
# 				print(rz)
# 				print(zx)
# 				print(zy)
# 				print(zz)
# 				print(events[[i]]$traveltime[j])
				stop()
			}

			rays = rays + 1
			print(paste("good rays: ", rays))
			m = length(RAP$r)
			
			#print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
			#print(RAP)
			#stop()

			ttobs = events[[i]]$traveltime[j]
 			ttpdt = 0.0
 			for(k in 1:m)
 			{
 				ttpdt = ttpdt + RAP$r[k] * slowness[RAP$idx[k]]
 			}
			tt = ttobs - ttpdt

			#print("=====================")
			#print(paste("origin: ", events[[i]]$origin))
			#print(paste("tt: ", ttobs, ttpdt, tt))

			ttpert[rays] = tt/ttobs

			if(tt > 0)
			{
				posPerturb = posPerturb + 1
			}
# 			print(paste("event coor: ", rx[1], ry[1], rz[1]))
# 			print(paste("station coor: ", rx[2], ry[2], rz[2]))
# 			print(paste("distance: ", sqrt((rx[1]-rx[2])^2+(ry[1]-ry[2])^2+(rz[1]-rz[2])^2)))
# 			print(raysegs)
# 			print(rx)
# 			print(ry)
# 			print(rz)
# 			print(zx)
# 			print(zy)
# 			print(zz)
			
			# if(rays == 4)
			# {
			# 	stop()
			# }

# 			bias = events[[i]]$origin %% 1

# 			if(bias < 0.4)
# 			{
# 				print(ttobs)
# 				print(bias)
# 				print(ttobs+bias)
# 				print(ttpdt)
# 				print(tt)
# 				print(tt+bias)
# 			}

			originSystem[[rays]]$A = RAP$r
			originSystem[[rays]]$idx = RAP$idx
			originSystem[[rays]]$b = tt

            subSystems[[stnIdx]][[numPartRays[stnIdx]]]$A = RAP$r
			subSystems[[stnIdx]][[numPartRays[stnIdx]]]$idx = RAP$idx
            subSystems[[stnIdx]][[numPartRays[stnIdx]]]$b = tt
		}
	}

	write(ttpert, file = "ttpert.log", sep = "\n")

	print(paste("~~~~~~~~~~~~~~~~~~~~~~~~~~~~", posPerturb))
	print(paste("  good   >>>>", rays))
	print(paste("  bad    >>>>", badRays))
	print(paste("  fuzzy  >>>>", fuzzyRays))

	#### Adding smoothness constraints to the original system

	if(lsqrSmooth)
	{
		numCell = 0
		for(i in 1:xRes)
		{
			for(j in 1:yRes)
			{
				for(k in 1:zRes)
				{
					numCell = numCell + 1
					numAdjCell = 0
					if(k == 1)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + (j-1)*zRes + (k+1)
					}
					else if(k == zRes)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + (j-1)*zRes + (k-1)
					}
					else
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + (j-1)*zRes + (k-1)

						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + (j-1)*zRes + (k+1)
					}
					
					if(j == 1)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + ((j+1)-1)*zRes + k
					}
					else if(j == yRes)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + ((j-1)-1)*zRes + k
					}
					else
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + ((j-1)-1)*zRes + k

						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + ((j+1)-1)*zRes + k
					}
					
					if(i == 1)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = ((i+1)-1)*yRes*zRes + (j-1)*zRes + k
					}
					else if(i == xRes)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = ((i-1)-1)*yRes*zRes + (j-1)*zRes + k
					}
					else
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = ((i-1)-1)*yRes*zRes + (j-1)*zRes + k

						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = ((i+1)-1)*yRes*zRes + (j-1)*zRes + k
					}
					
					originSystem[[numCell+raylimit]]$A[numAdjCell+1] = numAdjCell
					originSystem[[numCell+raylimit]]$idx[numAdjCell+1] = (i-1)*yRes*zRes + (j-1)*zRes + k
					originSystem[[numCell+raylimit]]$b = 0
				}
			}
			print(paste("Adding smooth constraints, xRes", i))
		}
	}
    
    for(i in 1:length(stations$name))
	{
		subSystems[[i]]$rayNum = numPartRays[i]
		subSystems[[i]]$stnName = stations$name[i]
	}
		
	systems$subSystems = subSystems
	systems$originSystem = originSystem
	
    if(lsqrSmooth) {
        systems$originRows = rays + vecLength
    } else {
        systems$originRows = rays
    }

	return(systems)
}

GenerateRealLayerAxb <- function(xLen, yLen, zLen, xInt, yInt, zInt, velocity, slowness, events, stations, raylimit, eventNum, lsqrSmooth = FALSE)
{
	xo = seq(0, xLen, xInt)
	yo = seq(0, yLen, yInt)
	zo = seq(0, zLen, zInt)

# 	print(xo)
# 	print(yo)
# 	print(zo)
	xRes = xLen/xInt
	yRes = yLen/yInt
	zRes = zLen/zInt

	vecLength = xRes*yRes*zRes	
	#### initialize the systems to generate
	systems = list()

	rays = 0
    if(lsqrSmooth) {
	    originSystem = array(list(), raylimit+vecLength)
    } else {
        originSystem = array(list(), raylimit)
    }

	# render the ray paths for testing 1D raytracing
	rex = range(c(stations$x, 0) )
	rey = range(c(stations$y, 0) )
	print(rex)
	print(rey)
	# rgl.open()

	#### how many bad rays
	badRays = 0
	fuzzyRays = 0
	posPerturb = 0

	#### reorganize the velocity model for Ray.time1D
	velAdj = list()
	velLen = length(velocity$zp)

	#### test for counting perturbation on tt
	ttpert = array(0, raylimit)
        
	print(eventNum)
	for(i in 1:eventNum)
	{
		evx = events[[i]]$x
		evy = events[[i]]$y
		evz = events[[i]]$depth
		staNum = length(events[[i]]$station)
# 		print(paste(i, evx, evy, evz, staNum))
		for(j in 1:staNum)
		{
			stnIdx = which(stations$name == events[[i]]$station[j])
			
			stax = stations$x[stnIdx]
			stay = stations$y[stnIdx]
			staz = stations$alt[stnIdx]
		
            # test
            #evx = 78.249670
            #evy = 115.543840
            #evz = 5.311567

			rx = c(evx, stax)
			ry = c(evy, stay)
			rz = c(evz, staz)

            #print("=================================================")
            #print(rx)
            #print(ry)
            #print(rz)
            #print(zp)
            #print()
            #print("=================================================")

			velStart = (which(velocity$zp > rz[2])[1] - 1)

            #print("&&&&&&&&&&&&&&&&")
            #print(velocity$zp)
            #print(rz[2])
            #print(which(velocity$zp > rz[2]))
            #print(velStart)


			velAdj$zp = (velocity$zp - rz[2])[velStart:velLen]
			#print(velAdj$zp)
            velAdj$zp[1] = 0
            #print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
            #print(stations$name[stnIdx])
            #print(rx)
            #print(ry)
            #print(rz)
            #print(velAdj$zp)
            velAdj$vp = velocity$vp[velStart:velLen]
            #print(velAdj$vp)
			
			# use 1D velocity model to trace the ray
			dee = sqrt((rx[2]-rx[1])^2 + (ry[2]-ry[1])^2)
			#print("========>>>>>>>>")
			raypath = Ray.time1D(dee, rz[1] - rz[2], 0, length(velAdj$zp), velAdj$zp, 1/velAdj$vp)
			#print("<<<<<<<<========")

			az = atan2(ry[2]-ry[1], rx[2]-rx[1])
			zx=cos(az)*raypath$rnod[1:raypath$nnod] + rx[1]
			zy=sin(az)*raypath$rnod[1:raypath$nnod] + ry[1]
			# plus 0.001 for BAD ray test
			zz = raypath$znod[1:raypath$nnod] + rz[2]

            #print("<<<<<<<<<<<<<<<<<<")
            #print(raypath)

			# lines3d(cbind(zx, zy, zz) , col='red')
			# points3d(cbind(zx[raypath$nnod] ,zy[raypath$nnod], zz[raypath$nnod]), col=rgb(.6,.6,1))

# 			if(raypath$znod[1] <= raypath$znod[2])
# 			{
# 				badRays = badRays + 1
# 				print(paste("BAD rays: ", badRays))
# 				RAP = Get1DRayblox(raysegs, zx, zy, zz, xo, yo, zo)
# 				print("%%%%%%%%%%%%%%%%%%%%%%%%%%")
# 				print(zx)
# 				print(zy)
# 				print(zz)
# 				print(RAP)
# 				print("%%%%%%%%%%%%%%%%%%%%%%%%%%")
# 				stop()
# 			}
			### get the ray blocks
			raysegs = raypath$nnod - 1

			### two same starting points (removes one)
			if(zx[1]==zx[2] && zy[1]==zy[2] && zz[1]==zz[2])
			{
				zx = zx[2:length(zx)]
				zy = zy[2:length(zy)]
				zz = zz[2:length(zz)]
				raysegs = raysegs - 1
				#print(paste(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", raysegs))
				#print(zx)
				#print(zy)
				#print(zz)
				fuzzyRays = fuzzyRays + 1
			}

			RAP = Get1DRayblox(raysegs, zx, zy, zz, xo, yo, zo)

            #print("--------------------------------------")
            #print(RAP)
			if(is.null(RAP))
			{
				badRays = badRays + 1
				print(paste("BAD rays: ", badRays))
				print("&&&&&&&&&&&&&&&&&&&&&&&&&&&")
				print(paste("    ====", dee, rz[1], rz[2], rz[1]-rz[2]))
				print(velAdj)
				print(velocity)
				print(raypath)
				print("===========================")
# 				print(raysegs)
# 				print(rx)
# 				print(ry)
# 				print(rz)
# 				print(zx)
# 				print(zy)
# 				print(zz)
# 				print(events[[i]]$traveltime[j])
				stop()
			}

			rays = rays + 1
			print(paste("good rays: ", rays))
			m = length(RAP$r)
			
			#print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
			#print(RAP)
			#stop()

			ttobs = events[[i]]$traveltime[j]
 			ttpdt = 0.0
 			for(k in 1:m)
 			{
 				ttpdt = ttpdt + RAP$r[k] * slowness[RAP$idx[k]]
 			}
			tt = ttobs - ttpdt

			#print("=====================")
			#print(paste("origin: ", events[[i]]$origin))
			#print(paste("tt: ", ttobs, ttpdt, tt))

			ttpert[rays] = tt/ttobs

			if(tt > 0)
			{
				posPerturb = posPerturb + 1
			}
# 			print(paste("event coor: ", rx[1], ry[1], rz[1]))
# 			print(paste("station coor: ", rx[2], ry[2], rz[2]))
# 			print(paste("distance: ", sqrt((rx[1]-rx[2])^2+(ry[1]-ry[2])^2+(rz[1]-rz[2])^2)))
# 			print(raysegs)
# 			print(rx)
# 			print(ry)
# 			print(rz)
# 			print(zx)
# 			print(zy)
# 			print(zz)
			
			# if(rays == 4)
			# {
			# 	stop()
			# }

# 			bias = events[[i]]$origin %% 1

# 			if(bias < 0.4)
# 			{
# 				print(ttobs)
# 				print(bias)
# 				print(ttobs+bias)
# 				print(ttpdt)
# 				print(tt)
# 				print(tt+bias)
# 			}

			originSystem[[rays]]$A = RAP$r
			originSystem[[rays]]$idx = RAP$idx
			originSystem[[rays]]$b = tt
		}
	}

	write(ttpert, file = "ttpert.log", sep = "\n")

	print(paste("~~~~~~~~~~~~~~~~~~~~~~~~~~~~", posPerturb))
	print(paste("  good   >>>>", rays))
	print(paste("  bad    >>>>", badRays))
	print(paste("  fuzzy  >>>>", fuzzyRays))

	#### Adding smoothness constraints to the original system

	if(lsqrSmooth)
	{
		numCell = 0
		for(i in 1:xRes)
		{
			for(j in 1:yRes)
			{
				for(k in 1:zRes)
				{
					numCell = numCell + 1
					numAdjCell = 0
					if(k == 1)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + (j-1)*zRes + (k+1)
					}
					else if(k == zRes)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + (j-1)*zRes + (k-1)
					}
					else
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + (j-1)*zRes + (k-1)

						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + (j-1)*zRes + (k+1)
					}
					
					if(j == 1)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + ((j+1)-1)*zRes + k
					}
					else if(j == yRes)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + ((j-1)-1)*zRes + k
					}
					else
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + ((j-1)-1)*zRes + k

						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = (i-1)*yRes*zRes + ((j+1)-1)*zRes + k
					}
					
					if(i == 1)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = ((i+1)-1)*yRes*zRes + (j-1)*zRes + k
					}
					else if(i == xRes)
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = ((i-1)-1)*yRes*zRes + (j-1)*zRes + k
					}
					else
					{
						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = ((i-1)-1)*yRes*zRes + (j-1)*zRes + k

						numAdjCell = numAdjCell + 1
						originSystem[[numCell+raylimit]]$A[numAdjCell] =  -1
						originSystem[[numCell+raylimit]]$idx[numAdjCell] = ((i+1)-1)*yRes*zRes + (j-1)*zRes + k
					}
					
					originSystem[[numCell+raylimit]]$A[numAdjCell+1] = numAdjCell
					originSystem[[numCell+raylimit]]$idx[numAdjCell+1] = (i-1)*yRes*zRes + (j-1)*zRes + k
					originSystem[[numCell+raylimit]]$b = 0
				}
			}
			print(paste("Adding smooth constraints, xRes", i))
		}
	}

	systems$originSystem = originSystem
	
    if(lsqrSmooth) {
        systems$originRows = rays + vecLength
    } else {
        systems$originRows = rays
    }

	return(systems)
}

#### ApproxPartitionTomoSolve function
#### input:
####   systems - the systems generated by approximation partition
####   slowness - initial slowness guess
####   partitionDim - partition dimension
####   resolutionDim - resolution dimension
####   rounds - Bayesian ART iterations
####   rho - relaxiation parameter
####   lambda - regularization parameter
####
#### output:
####   result
####     result$slowness - slowness model
####     result$filename - name of the file logged the slowness model
ApproxPartitionTomoSolver <- function(systems, slowness, partitionDim, resolutionDim, rho, lambda, tolerance)
{
	print(">>>> Approximate Partition Solver >>>>")
	write(">>>> Approximate Partition Solver >>>>", file="logs", append=TRUE)
	#### initial guess
	sptb = array(0, resolutionDim^3)
	parts = partitionDim^2
	sptb_init = array(0, resolutionDim^3)
	
	#### perform Bayesian ART on each subsystem
	for(i in 1:parts)
	{
		print(paste("==== Solving Partition ", i, " ====",sep=""))
		
		write(paste("==== Solving Partition ", i, " ====",sep=""), file="logs", append=TRUE)
		print(systems$subSystems[[i]]$rayNum)
		sptb_temp = SolveBayesianART(systems$subSystems[[i]], systems$subSystems[[i]]$rayNum, resolutionDim, sptb_init, rho, lambda, tolerance)
		sptb = sptb + sptb_temp
	}
	
	slowness = slowness + sptb
	vecLength = length(slowness)

	output = array(0, vecLength+1)
	output[1] = resolutionDim
	output[2:(vecLength+1)] = slowness[1:vecLength]
	filename = paste("slowness_approx_", partitionDim, "_", resolutionDim, sep="")
	write(output, file = filename, sep = "\n")

	result = list(slowness = slowness, filename = filename)
	return(result)
}

#### ExactPartitionTomoSolver function
#### input:
####   systems - the systems generated by approximation partition
####   slowness - initial slowness guess
####   partitionDim - partition dimension
####   resolutionDim - resolution dimension
####   rounds - Bayesian ART iterations
####   rho - relaxiation parameter
####   lambda - regularization parameter
####
#### output:
####   result
####     result$slowness - slowness model
####     result$filename - name of the file logged the slowness model
ExactPartitionTomoSolver <- function(systems, slowness, partitionDim, resolutionDim, rho, lambda, tolerance, rounds)
{	
	print(">>>> Exact Partition Solver >>>>")
	vecLength = length(slowness)
	initGuess = array(0, vecLength)
	travelTimes = systems$ttInit

	subRays = length(travelTimes)
	estimation = c(initGuess, travelTimes)
	
	parts = partitionDim^2
	relaxParameter = rho
	
	for(j in 1:rounds)
	{
		print(paste(">>>> Exact Partition Solving round", j, ">>>>"))
		#### perform Bayesian ART on each subsystem
		for(i in 1:parts)
		{
			print(paste("==== Solving Partition ", i, " ====",sep=""))
			estimation = SolveART(systems$subSystems[[i]], systems$subSystems[[i]]$rayNum, estimation, relaxParameter, tolerance, 1)
			
			##### update the partial travel time residual in the original travel time residual system (t_i1 + ... + tij = bi)
			for(k in 1:systems$subSystems[[i]]$rayNum)
			{
				len = length(systems$subSystems[[i]][[k]]$idx)
				index = systems$subSystems[[i]][[k]]$idx[len]
				travelTimes[index-vecLength] = estimation[index]
			}
		}
		
		#### perform ART on the original travel time residual system (t_i1 + ... + tij = bi)
		travelTimes = SolveBayesianART(systems$ttSystem, length(systems$ttSystem), resolutionDim, travelTimes, relaxParameter, lambda, tolerance, 1)
		
		#### update the partial travel time residual in each subsystem
		for(i in 1:subRays)
		{
			estimation[vecLength+i] = travelTimes[i]
		}
		
		relaxParameter = relaxParameter/1.01
	}
	
	slowness = slowness + estimation[1:vecLength]
	output = array(0, vecLength+1)
	output[1] = resolutionDim
	output[2:(vecLength+1)] = slowness[1:vecLength]
	filename = paste("slowness_exact_", partitionDim, "_", resolutionDim, sep="")
	write(output, file = filename, sep = "\n")
	
	result = list(slowness = slowness, filename = filename)
	return(result)
}

#### PartitionTomoSolver function
#### input:
####   systems - the systems generated by approximation partition
####   slowness - initial slowness guess
####   partitionDim - partition dimension
####   resolutionDim - resolution dimension
####   rounds - Bayesian ART iterations
####   rho - relaxiation parameter
####   lambda - regularization parameter
####   flag - which solver should be used (0-ApproxPartitionTomoSolver, 1-ExactPartitionTomoSolver)
PartitionTomoSolver <- function(systems, slowness, partitionDim, resolutionDim, rho, lambda, flag, tolerance, rounds = 100)
{
	if(flag == 0)
	{
		result = ApproxPartitionTomoSolver(systems, slowness, partitionDim, resolutionDim, rho, lambda, tolerance)
	}
	else if(flag == 1)
	{
		result = ExactPartitionTomoSolver(systems, slowness, partitionDim, resolutionDim, rho, lambda, tolerance, rounds)
	}
	else if(flag == 2)
	{
		result = LsqrPartitionTomoSolver(systems, slowness, partitionDim, resolutionDim)
	}
	return(result)
}

LsqrPartitionTomoSolver <- function(systems, slowness, partitionDim, resolutionDim)
{
	print(">>>> Lsqr Partition Solver >>>>")
	parts = partitionDim^2
	
	#### perform Bayesian ART on each subsystem
	for(i in 1:parts)
	{
		print(paste("==== Solving Partition ", i, " ====",sep=""))
		print(systems$subSystems[[i]]$rayNum)
		slowness = SolveLsqr(systems$subSystems[[i]], slowness, systems$subSystems[[i]]$rayNum)
	}
	
	vecLength = length(slowness)

	output = array(0, vecLength+1)
	output[1] = resolutionDim
	output[2:(vecLength+1)] = slowness[1:vecLength]
	filename = paste("slowness_lsqr_part_", partitionDim, "_", resolutionDim, sep="")
	write(output, file = filename, sep = "\n")

	result = list(slowness = slowness, filename = filename)
	return(result)
}

# GenerateAxb <- function(cube, side, cubeDim, slowness, partitionDim, resolutionDim, stationNum, eventStart, eventNum, flag)
# {
# 	vecLength = resolutionDim^3
# 
# 	#### cube resolution blocks partitioin for generating observations
# 	seghi = side/cubeDim
# 	xohi = seq(0, side, seghi)
# 	yohi = seq(0, side, seghi)
# 	zohi = seq(0, side, seghi)
# 
# 	#### blocks partition with required resolution for getting ray path blocks
# 	seg = side/resolutionDim
# 	xo = seq(0, side, seg)
# 	yo = seq(0, side, seg)
# 	zo = seq(0, side, seg)
# 
# 	#### load the station and events coordinates
# 
# 	stationEventLog = LoadStationEventLog(stationNum, eventStart, eventNum)
# 	stax = stationEventLog$stax
# 	stay = stationEventLog$stay
# 	staz = stationEventLog$staz
# 	evx = stationEventLog$evx
# 	evy = stationEventLog$evy
# 	evz = stationEventLog$evz
# 
# 	#### initialize partition parameter
# 	parts = partitionDim*partitionDim
# 	part = sqrt(parts)
# 	base = resolutionDim/part
# 
# 	#### total ray number
# 	raylimit = stationNum*eventNum
# 	
# 	#### initialize the systems to generate
# 	systems = list()
# 
# 	rays = 0
# 	originSystem = array(list(), raylimit)
# 
# 	numPartRays = array(0, parts)
# 	subSystems = array(list(), parts)
# 	for(i in 1:parts)
# 	{
# 		subSystems[[i]] = array(list(), raylimit)
# 	}
# 	
# 	ttSystem = array(list(), raylimit)
# 	#### initial guess of the travel time residual partition
# 	ttInit = array()
# 	
# 	#### number of partial time residuals
# 	numSubt = 0
# 	firstWrite = TRUE
# 
# 	#### generating noise
# 	set.seed(14)
# 	wgn = runif(raylimit, 0, 1)
# 
# 	#### ray info lost ratio
# 	set.seed(1111)
# 	lostRatio = runif(raylimit*4, 0, 10)
# 	lostRatioTh = 0
#         
# 	for(i in 1:eventNum)
# 	{
# 		ptm = proc.time()
# 		for(j in 1:stationNum)
# 		{
# 			rays = rays + 1
# 
# 			rx = c(evx[i], stax[j])
# 			ry = c(evy[i], stay[j])
# 			rz = c(evz[i], staz[j])
# 			
# 			# print(paste("----> rays: ", rays))
# 			
# 			
# 			RAPhi = GetRayblox(rx, ry, rz, xohi, yohi, zohi)
# 			mhi = length(RAPhi$r)
# 			
# # 			ttobs = 0.0
# # 			for(k in 1:mhi)
# # 			{
# # 				ttobs = ttobs + RAPhi$r[k] * cube[RAPhi$idx[k]]
# # 			}
# 			ttobs = (RAPhi$r %*% cube[RAPhi$idx])[1]
# 
# 			#### adding noise
# # 			ttobs = ttobs + wgn[rays]*0.02
# 
# 			RAP = GetRayblox(rx, ry, rz, xo, yo, zo)
# 
# 			#### indicate which partition the blocks belong to
# 			m = length(RAP$r)
# 			idxcount = 0
# 			for(k in 1:m)
# 			{
# 				ix = RAP$ix[k]
# 				iy = RAP$iy[k]
# 				
# 				secidx = 0
# 				for(a in 1:part)
# 				{
# 					for(b in 1:part)
# 					{
# 						secidx = secidx + 1
# 						xscale_a = (a-1)*base + 1
# 						xscale_b = a*base
# 						yscale_a = (b-1)*base + 1
# 						yscale_b = b*base
# 						if(ix >= xscale_a && ix <= xscale_b && iy >= yscale_a && iy <= yscale_b)
# 						{
# 							RAP$ib[k] = secidx
# 							idxcount = idxcount + 1
# 						}
# 					}
# 				}
# 				
# # 				RAP$idx[k] = (RAP$ix[k]-1)*resolutionDim*resolutionDim + (RAP$iy[k]-1)*resolutionDim + RAP$iz[k]
# 			}
# 
# 			if(idxcount != m || length(which(RAP$idx == 0)))
# 			{
# 				print("Rayblox error!!!!")
# 				stop()
# 			}
# 
# #  			ttpdt = 0.0
# #  			for(k in 1:m)
# #  			{
# #  				ttpdt = ttpdt + RAP$r[k] * slowness[RAP$idx[k]]
# #  			}
#   
# 			ttpdt = (RAP$r %*% slowness[RAP$idx])[1]
# 			tt = ttobs - ttpdt
# 			
# 			#### initial total travel time residual
# 			if(flag == 1)
# 			{
# 				ttSystem[[rays]]$b = tt
# 			}
# 
# 			originSystem[[rays]]$A = RAP$r
# 			originSystem[[rays]]$idx = RAP$idx
# 			originSystem[[rays]]$b = tt
# 
# 			#### partition index
# 			rayparts = 0
# 
# 			for(partidx in 1:parts)
# 			{
# 				m = length(RAP$ib[RAP$ib==partidx])
# 				if(m > 0)
# 				{
# 					numSubt = numSubt + 1
# 					rayparts = rayparts + 1
# 					
# 					ix = RAP$ix[RAP$ib==partidx]
# 					iy = RAP$iy[RAP$ib==partidx]
# 					iz = RAP$iz[RAP$ib==partidx]
# 					lr = RAP$r[RAP$ib==partidx]
# 					ib = RAP$ib[RAP$ib==partidx]
# 					idx = RAP$idx[RAP$ib==partidx]
# 					ray = list(ix=ix, iy=iy, iz=iz, r=lr, ib=ib, idx=idx)
# 					
# # 					ttpp = 0.0
# # 					for(k in 1:m)
# # 					{
# # 						ttpp = ttpp + ray$r[k] * slowness[ray$idx[k]]
# # 					}
# 					ttpp = (ray$r %*% slowness[ray$idx])[1]
# 					
# 					ttp = (ttpp/ttpdt)*tt
# 
# 					if(lostRatio[numSubt] > lostRatioTh)
# 					{
# 						numPartRays[partidx] = numPartRays[partidx]+1
# 						subSystems[[partidx]][[numPartRays[partidx]]]$A = ray$r
# 						subSystems[[partidx]][[numPartRays[partidx]]]$idx = ray$idx
# 						
# 						#### initialize the sub equation systems
# 						if(flag == 0 || flag == 2)
# 						{
# 							subSystems[[partidx]][[numPartRays[partidx]]]$b = ttp
# 						}
# 						else if(flag == 1)
# 						{
# 							ttSystem[[rays]]$A[rayparts] = 1
# 							ttSystem[[rays]]$idx[rayparts] = numSubt
# 							ttInit[numSubt] = ttp
# 							
# 							subSystems[[partidx]][[numPartRays[partidx]]]$A[m+1] = -1
# 							subSystems[[partidx]][[numPartRays[partidx]]]$idx[m+1] = vecLength+numSubt
# 							subSystems[[partidx]][[numPartRays[partidx]]]$b = 0
# 						}
# 					}
# 				}
# 			}
# 		}
# 		etime = proc.time() - ptm
# 		print(paste("Ray generation done, event ----", i))
# 		print(etime)
# # 		if(i >= 1)
# # 		{
# # 			stop()
# # 		}
# 	}
# 
# 	for(i in 1:parts)
# 	{
# 		subSystems[[i]]$rayNum = numPartRays[i]
# 	}
# 		
# 	if(flag == 0 || flag == 2)
# 	{
# 		systems = list(subSystems = subSystems)
# 	}
# 	else if(flag == 1)
# 	{
# 		systems = list(subSystems = subSystems, ttSystem = ttSystem, ttInit = ttInit)
# 	}
# 
# 	systems$originSystem = originSystem
# 	systems$originRows = rays
# 
# 	return(systems)
# }
