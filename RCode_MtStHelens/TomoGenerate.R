#### Generate the artificial magma area as the ground-truth
#### input: 
####   cubeDim
#### output: 
####   cube - high resolution (e.g.,cubeDim*cubeDim*cubeDim) cube (e.g.,ground-truth tomography) with velocity model

GenerateCube <- function(side, cubeDim, velocity)
{
	#### velocity model
	vel = velocity
	vel1 = 0.9*vel
	slowness = 1/vel
	slowness1 = 1/vel1

	cube = array(slowness, cubeDim^3)
	segLength = side/cubeDim

	#### load data
	data(volcano)
	offset = side/2.0

	y <- volcano/2.0
	x <- (1:nrow(y))
	z <- (1:ncol(y))

	dims = dim(y)
	m = dims[1]
	n = dims[2]

	mscale = m/128
	nscale = n/128

	miny = y[which.min(y)]

	y[1,] = miny
	y[m,] = miny
	y[,1] = miny
	y[,n] = miny
	y = y - miny

	y = y*side/200.0
	y = y + offset

	md = floor(cubeDim*mscale)+1
	startx = floor((cubeDim-md)/2)+1
	endx = startx+md-1

	nd = floor(cubeDim*nscale)+1
	starty = floor((cubeDim-nd)/2)+1
	endy = starty+nd-1
	count = 0

	mstep = m/md
	nstep = n/nd

	for(i in startx:endx)
	{
		for(j in starty:endy)
		{
			for(k in 1:cubeDim)
			{
				dist = k*segLength-(segLength/2)
# 				idxx = i-startx+1
# 				idxy = j-starty+1
# 				distUp = y[idxx, idxy]

				if(i == startx)
				{
					idxx = 1
				}else if(i == endx)
				{
					idxx = m
				}else if(i > startx && i < endx)
				{
					seqx = i-startx+1
					idxx = floor(seqx*mstep)
				}

				if(j == starty)
				{
					idxy = 1
				}else if(j == endy)
				{
					idxy = n
				}else if(j > starty && j < endy)
				{
					seqy = j-starty+1
					idxy = floor(seqy*nstep)
				}

				distUp = y[idxx, idxy]
				distLow = 2*offset - distUp

				if(dist>distLow && dist<distUp)
				{
					cube[(i-1)*cubeDim*cubeDim + (j-1)*cubeDim + k] = slowness1
					count = count + 1
				}
			}
		}
	}

	vecLength = cubeDim^3
	output = array(0, vecLength+1)
	output[1] = cubeDim
	output[2:(vecLength+1)] = cube[1:vecLength]
	filename = paste("slowness_truth", sep="")
	write(output, file = filename, sep = "\n")

	return(cube)
}

#### Generate the artificial magma area as the ground-truth
#### input: 
####   side -  side length of the cube
####   cubeDim - cube dimension
####   velMod - 1D velocity model
#### output: 
####   cube - high resolution (e.g.,cubeDim*cubeDim*cubeDim) cube (e.g.,ground-truth tomography) with velocity model

GenerateLayeredCube <- function(side, cubeDim, velMod)
{

	cube = array(0.0, cubeDim^3)
	segLength = side/cubeDim

	zp = velMod$zp
	vp = velMod$vp
	slowness = 1/vp
	vlen = length(zp)

	for(i in 1:cubeDim)
	{
		for(j in 1:cubeDim)
		{
			for(k in 1:cubeDim)
			{
				dist = k*segLength-(segLength/2)
				for(l in 1:(vlen-1))
				{
					if(dist >= zp[l] & dist < zp[l+1])
					{
						cube[(i-1)*cubeDim*cubeDim + (j-1)*cubeDim + k] = slowness[l]
					}
				}
			}
		}
		print(paste("construct cube dim: ", i))
	}

	#### load data
	data(volcano)
	offset = side/2.0

	y <- volcano/2.0
	x <- (1:nrow(y))
	z <- (1:ncol(y))

	dims = dim(y)
	m = dims[1]
	n = dims[2]

	mscale = m/128
	nscale = n/128

	miny = y[which.min(y)]

	y[1,] = miny
	y[m,] = miny
	y[,1] = miny
	y[,n] = miny
	y = y - miny

	y = y*side/200.0
	y = y + offset

	md = floor(cubeDim*mscale)+1
	startx = floor((cubeDim-md)/2)+1
	endx = startx+md-1

	nd = floor(cubeDim*nscale)+1
	starty = floor((cubeDim-nd)/2)+1
	endy = starty+nd-1
	count = 0

	mstep = m/md
	nstep = n/nd

	for(i in startx:endx)
	{
		for(j in starty:endy)
		{
			for(k in 1:cubeDim)
			{
				dist = k*segLength-(segLength/2)
# 				idxx = i-startx+1
# 				idxy = j-starty+1
				
				if(i == startx)
				{
					idxx = 1
				}else if(i == endx)
				{
					idxx = m
				}else if(i > startx && i < endx)
				{
					seqx = i-startx+1
					idxx = floor(seqx*mstep)
				}

				if(j == starty)
				{
					idxy = 1
				}else if(j == endy)
				{
					idxy = n
				}else if(j > starty && j < endy)
				{
					seqy = j-starty+1
					idxy = floor(seqy*nstep)
				}

				distUp = y[idxx, idxy]
				distLow = 2*offset - distUp

				if(dist>distLow && dist<distUp)
				{
					for(l in 1:(vlen-1))
					{
						if(dist > zp[l] & dist < zp[l+1])
						{
							cube[(i-1)*cubeDim*cubeDim + (j-1)*cubeDim + k] = cube[(i-1)*cubeDim*cubeDim + (j-1)*cubeDim + k]/0.9
						}
					}
				}
			}
		}
	}

        vecLength = cubeDim^3
	output = array(0, vecLength+1)
	output[1] = cubeDim
	output[2:(vecLength+1)] = cube[1:vecLength]
	filename = paste("slowness_truth", sep="")
	write(output, file = filename, sep = "\n")

	return(cube)
}

#### GenerateStationAndEventCoors function
#### input:
####   stationNum - number of stations
####   eventNum - number of events
####
#### output:
####   two log files: stationslog and eventslog
GenerateStationAndEventCoors <- function(side, stationNum, eventNum, stationSeed, eventSeed, rayDistribution)
{
	set.seed(stationSeed)
	stax = runif(stationNum, 0, side)
	set.seed(stationSeed*2)
	stay = runif(stationNum, 0, side)
	if(rayDistribution == 0)
	{
		staz = array(0, stationNum)
	}else if(rayDistribution == 1)
	{
		set.seed(stationSeed*3)
		staz = runif(stationNum, 0, side)
	}
	stationslog = cbind(stax, stay, staz)
	write(stationslog, file = "stationslog", sep = "\n")

	set.seed(eventSeed)
	evx = runif(eventNum, 0, side)
	set.seed(eventSeed*2)
	evy = runif(eventNum, 0, side)
	set.seed(eventSeed*3)
	evz = runif(eventNum, 0, side)
	eventslog = cbind(evx, evy, evz)
	write(eventslog, file = "eventslog", sep = "\n")
}
