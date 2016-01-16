library(rgl)
#library(fields)

#####################################
#### Slice2DRender function
#### Render tomography slice by slice
#####################################

Slice2DRender <- function(modelFile, layer, side, axes="x", withBorder=TRUE, cubeDim=128, colGrad=128)
{
	dataset = read.table(modelFile, sep = "\n")
	M = cubeDim
	data(volcano)

	dims = dim(volcano)
	mm = dims[1]
	nn = dims[2]

	startx = floor((M-mm)/2)
	startz = floor((M-nn)/2)

	offsetx = startx * side / M
	offsetz = startz * side / M
	offsety = side/2.0

	len = length(dataset[,1])
	N = dataset[1,1]
	seg = side/N
	scale = seg/2

	res = N*N*N
	s = array(0, res)
	s[1:res] = dataset[2:len,1]

	y <- volcano/2.0
	x <- (1:nrow(y))
	z <- (1:ncol(y))

	dims = dim(y)
	m = dims[1]
	n = dims[2]

	miny = y[which.min(y)]

	y[1,] = miny
	y[m,] = miny
	y[,1] = miny
	y[,n] = miny

	y = y - miny

	x = x/(cubeDim/side)
	z = z/(cubeDim/side)
	y = y/20

	y2 = -y

	X = c(0, side)
	Y = c(0, side)
	Z = c(0, side)
	
	#### slice on Y axis
	px = c(x,rev(x))
	
	#### slice on X axis
	pz = c(z,rev(z))

	xo = seq(0, side, seg)
	yo = seq(0, side, seg)
	MOD = matrix(0, N, N)

	yo = seq(0, side, seg)
	if(axes == "y")
	{
		py = c(y[,layer],rev(y2[,layer]))
# 		plot(X,Z)
		ylayer = layer+startz-1
		k = ceiling(ylayer*N/cubeDim)
		for(i in 1:N)
		{
			for(j in 1:N)
			{
				MOD[i, j] = s[(i-1)*N*N + (k-1)*N + j]
			}
		}
		print(length(xo))
		print(length(yo))
		print(length(MOD))
		image(xo, yo, MOD, ylim = rev(range(yo)), col=gray.colors(colGrad), axes=FALSE, xlab="X",ylab="Z",cex.lab=1.4)
		imageTitle = paste("Model: ", modelFile, "\nLayer ", k, " of ", N, " layers along Y\nResolution: ", N, "x", N, sep="")
		title(main=imageTitle, cex.main=1.0)
		if(withBorder){
			polygon(px+offsetx,py+offsety,lwd=2)
		}
# 		axis(1, at = 0:10, labels=c(0:10), cex.axis=1.2)
# 		axis(2, at = 0:10, labels=c(10:0), cex.axis=1.2)
		fileName = paste(modelFile, "_Y_", k, "_of_", N, ".pdf", sep="")
# 		dev.copy2pdf(file=fileName)
	}else if(axes == "x")
	{
		py = c(y[layer,],rev(y2[layer,]))
# 		plot(Y,Z)
		ylayer = layer+startx-1
		k = ceiling(ylayer*N/cubeDim)
		for(i in 1:N)
		{
			for(j in 1:N)
			{
				MOD[i, j] = s[(i-1)*N*N + (k-1)*N + j]
			}
		}
		image(xo, yo, MOD, ylim = rev(range(yo)), col=gray.colors(colGrad), axes=FALSE, xlab="Y",ylab="Z",cex.lab=1.4)
		imageTitle = paste("Model: ", modelFile, "\nLayer ", k, " of ", N, " layers along X\nResolution: ", N, "x", N, sep="")
		title(main=imageTitle, cex.main=1.0)
		if(withBorder){
			polygon(pz+offsetz,py+offsety,lwd=2)
		}
# 		axis(1, at = 0:10, labels=c(0:10), cex.axis=1.2)
# 		axis(2, at = 0:10, labels=c(10:0), cex.axis=1.2)
		fileName = paste(modelFile, "_X_", k, "_of_", N, ".pdf", sep="")
# 		dev.copy2pdf(file=fileName)
	}else
	{
		print("Wrong axes!")
		stop()
	}
}

library(rgl)

#####################################
#### Slice2DRender function for real data
#### Render tomography slice by slice
#####################################

Slice2DRenderZxes <- function(modelFile, outputFile, xLen, yLen, zLen, xInt, yInt, zInt, xrange, yrange, layer, colGrad=128)
{
	xRes = xLen/xInt
	yRes = yLen/yInt
	zRes = zLen/zInt
	
	dataset = read.table(modelFile, sep = "\n")
	res = xRes*yRes*zRes
	len = length(dataset[,1])
	s = array(0, res)
	s[1:res] = dataset[4:len,1]
	
	xo = seq(0, xLen, xInt)
	yo = seq(0, yLen, yInt)
	MOD = matrix(0, xRes, yRes)
	k = layer

	for(i in 1:xRes)
	{
		for(j in 1:yRes)
		{
			MOD[i, j] = s[(i-1)*yRes*zRes + (j-1)*zRes + k]
		}
	}

	### value out of range
	# count = 0
	# for(i in 1:xRes)
	# {
		# for(j in 1:yRes)
		# {
			# if(abs(MOD[i,j]) > 0.1)
			# {
				# count = count + 1
				# MOD[i,j] = 0
			# }
		# }
	# }
	# print(count)
	# print(length(xo))
	# print(length(yo))
	# print(dim(MOD))

	# test color table
# 	color_table = c("#8B0000", "#CD0000", "#EE0000", "#FF0000", "#EE4000", "#FF4500", "#EE9A00", "#FFA500", "#8B8B00", "#F5F5F5", "#F5F5F5", "#F5F5F5", "#9ACD32", "#00CD00", "#008B00", "#1874CD", "#104E8B", "#0000FF", "#0000EE", "#0000CD", "#00008B")

# 	image(xo, yo, MOD, ylim = rev(range(yo)), zlim = c(-0.1, 0.1), col=topo.colors(colGrad), axes=FALSE, xlab="X",ylab="Y",cex.lab=1.4)
	mid = heat.colors(32)[32]
# 	color_table = c(heat.colors(32), mid)
# 	color_table = c(color_table, mid)
	color_table = c(heat.colors(32), rev(tim.colors(64))[33:64])

	color_table = tim.colors(64)
# 	color_table[29:36] = "#C1CDCDFF"

# 	print(length(color_table))

# 	MOD[which(MOD <= -0.1)] = 0.0
# 	MOD[which(MOD >= 0.1)] = 0.0

# 	image(xo, yo, MOD, xlim = xrange, ylim = yrange, col=color_table, axes=FALSE, xlab="X",ylab="Y",cex.lab=1.4)
	dev.new()
	#image.plot(xo, yo, MOD, xlim = xrange, ylim = yrange, zlim = c(-0.2, 0.2), col=rev(color_table))
	image(xo, yo, MOD, xlim = xrange, ylim = yrange, col=color_table, axes=FALSE, xlab="X",ylab="Y",cex.lab=1.4)
# 	image(xo, yo, MOD, col=heat.colors(colGrad), axes=FALSE, xlab="X",ylab="Y",cex.lab=1.4)
# 	legend(800, 600, zlim= range(MOD), col = heat.colors(colGrad), trace=TRUE)
	axis(1, at = seq(0,xLen,xInt), labels=seq(0,xLen,xInt), cex.axis=1.2)
	axis(2, at = seq(0,yLen,yInt), labels=seq(0,yLen,yInt), cex.axis=1.2)
	points(80.05613, 98.56158, pch=17, col = "black", cex=2.0)

	imageTitle = paste("Model: ", modelFile, "\nLayer ", k, " of ", zRes, " layers along Z\nResolution: ", xRes, "x", yRes, sep="")
	title(main=imageTitle, cex.main=1.0)

	dev.copy2pdf(file=outputFile)
}

Slice2DRenderYxes <- function(modelFile, outputFile, xLen, yLen, zLen, xInt, yInt, zInt, xrange, yrange, layer, colGrad=128)
{
	xRes = xLen/xInt
	yRes = yLen/yInt
	zRes = zLen/zInt
	
	dataset = read.table(modelFile, sep = "\n")
	res = xRes*yRes*zRes
	len = length(dataset[,1])
	s = array(0, res)
	s[1:res] = dataset[4:len,1]
	
	xo = seq(0, xLen, xInt)
	yo = seq(0, zLen, zInt)
	MOD = matrix(0, xRes, zRes)
	k = layer

	for(i in 1:xRes)
	{
		for(j in 1:zRes)
		{
			MOD[i, j] = s[(i-1)*yRes*zRes + (k-1)*zRes + j]
		}
	}

	### value out of range
	# count = 0
	# for(i in 1:xRes)
	# {
		# for(j in 1:zRes)
		# {
			# if(abs(MOD[i,j]) > 0.1)
			# {
				# count = count + 1
				# MOD[i,j] = 0
			# }
		# }
	# }
	# print(count)
	# print(length(xo))
	# print(length(yo))
	# print(dim(MOD))

	# test color table
# 	color_table = c("#8B0000", "#CD0000", "#EE0000", "#FF0000", "#EE4000", "#FF4500", "#EE9A00", "#FFA500", "#8B8B00", "#F5F5F5", "#F5F5F5", "#F5F5F5", "#9ACD32", "#00CD00", "#008B00", "#1874CD", "#104E8B", "#0000FF", "#0000EE", "#0000CD", "#00008B")

# 	image(xo, yo, MOD, ylim = rev(range(yo)), zlim = c(-0.1, 0.1), col=topo.colors(colGrad), axes=FALSE, xlab="X",ylab="Y",cex.lab=1.4)
	mid = heat.colors(32)[32]
	color_table = c(heat.colors(32), mid)
	color_table = c(color_table, mid)
	color_table = c(color_table, rev(tim.colors(60))[31:60])

	print(length(color_table))

	color_table = tim.colors(64)

# 	print(color_table)

# 	color_table[31] = "#F5F5DCFF"
# 	color_table[32] = "#F5F5DCFF"

# 	print(color_table)
# 	MOD[which(MOD <= -0.1)] = 0.0
# 	MOD[which(MOD >= 0.1)] = 0.0

# 	image(xo, yo, MOD, xlim = xrange, ylim = yrange, col=color_table, axes=FALSE, xlab="X",ylab="Y",cex.lab=1.4)
	image.plot(xo, yo, MOD, xlim = xrange, ylim = rev(yrange), zlim = c(-0.2, 0.2), col=rev(color_table), legend.width = 0.6, cex.main = 4.0, cex.axis = 4.0, axis.args=list(cex.axis=4.0))
# 	image.plot(xo, yo, MOD, xlim = xrange, ylim = rev(yrange), zlim = c(-0.1, 0.1), col=color_table)
#	image(xo, yo, MOD, xlim = xrange, ylim = yrange, zlim = c(-0.1, 0.1), col=color_table, axes=FALSE, xlab="X",ylab="Y",cex.lab=1.4)
# 	image(xo, yo, MOD, col=heat.colors(colGrad), axes=FALSE, xlab="X",ylab="Y",cex.lab=1.4)
# 	legend(800, 600, zlim= range(MOD), col = heat.colors(colGrad), trace=TRUE)
# 	axis(1, at = seq(60,100,1), labels=seq(60,100,1), cex.axis=4.0, col = "black")
# 	axis(2, at = seq(0,yLen,yInt), labels=seq(0,yLen,yInt), cex.main=4.0)
	imageTitle = paste("Layer ", k, " of ", zRes, " layers along Z Resolution: ", xRes, "x", yRes, sep="")
	title(main=imageTitle, cex.main=4.0)

	dev.copy2pdf(width = 40, height=15, file=outputFile)
}

#### TomoRender function
#### input: velocity model filename
#### output: tomography render in OpenGL

TomoRender <- function(filename, cubeDim=128, side=10)
{
	sec = 0

	dataset = read.table(filename, sep = "\n")
	M = cubeDim

	data(volcano)

	dims = dim(volcano)
# 	print(dims)
	mm = dims[1]
	nn = dims[2]

	startx = floor((M-mm)/2)
	startz = floor((M-nn)/2)

	offsetx = startx * side / M
	offsetz = startz * side / M
	offsety = side/2.0

	len = length(dataset[,1])
	N = dataset[1,1]
	seg = side/N
	scale = seg/2

	res = N*N*N
	s = array(0, res)
	s[1:res] = dataset[4:len,1]

# 	print(length(dataset[,1]))

	open3d(windowRect=c(100,100,900,900))
	alfa = 0.8

	if(sec == 0)
	{
		transp = 0.1
		for(i in 1:N)
		{
			for(j in 1:N)
			{
				for(k in 1:N)
				{
					x = i*seg-(seg/2)
					z = j*seg-(seg/2)
					y = k*seg-(seg/2)
					x = x - offsetx
					z = z - offsetz
					y = y - offsety

					if(s[(i-1)*N*N + (j-1)*N + k] > 0.226 && s[(i-1)*N*N + (j-1)*N + k] < 0.234)
					{
# 						shade3d(translate3d(scale3d(cube3d(col = "grey", alpha=transp), scale, scale, scale), x, y, z))
					}
					else if(s[(i-1)*N*N + (j-1)*N + k] > 0.234 && s[(i-1)*N*N + (j-1)*N + k] < 0.242) 
					{
						shade3d(translate3d(scale3d(cube3d(col = "green", alpha=transp), scale, scale, scale), x, y, z))
					} 
					if(s[(i-1)*N*N + (j-1)*N + k] > 0.242 && s[(i-1)*N*N + (j-1)*N + k] < 0.250)
					{
						shade3d(translate3d(scale3d(cube3d(col = "blue", alpha=transp), scale, scale, scale), x, y, z))
					}
					else if(s[(i-1)*N*N + (j-1)*N + k] > 0.250)
					{
						shade3d(translate3d(scale3d(cube3d(col = "brown", alpha=transp), scale, scale, scale), x, y, z))
					}
				}
			}
		}
	}

	y <- volcano/2.0
	x <- (1:nrow(y))
	z <- (1:ncol(y))

	dims = dim(y)
# 	print(dims)
	m = dims[1]
	n = dims[2]

	miny = y[which.min(y)]

	y[1,] = miny
	y[m,] = miny
	y[,1] = miny
	y[,n] = miny

	y = y - miny

	x = x/(cubeDim/side)
	z = z/(cubeDim/side)
	y = y/20

# 	rgl.surface(x, z, y, color="brown", alpha=alfa)
# 	y2 = -y
# 	rgl.surface(x, z, y2, color="brown", alpha=alfa)

	bg3d("white")
	ox = 5-offsetx
	oz = 5-offsetz

	lx = c(ox-5, ox-5)
	ly = c(-5, 5)
	lz = c(oz-5, oz-5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox-5, ox+5)
	ly = c(5, 5)
	lz = c(oz-5, oz-5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox+5, ox+5)
	ly = c(5, -5)
	lz = c(oz-5, oz-5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox+5, ox-5)
	ly = c(-5, -5)
	lz = c(oz-5, oz-5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox-5, ox-5)
	ly = c(-5, 5)
	lz = c(oz+5, oz+5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox-5, ox+5)
	ly = c(5, 5)
	lz = c(oz+5, oz+5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox+5, ox+5)
	ly = c(5, -5)
	lz = c(oz+5, oz+5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox+5, ox-5)
	ly = c(-5, -5)
	lz = c(oz+5, oz+5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox-5, ox-5)
	ly = c(-5, -5)
	lz = c(oz-5, oz+5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox+5, ox+5)
	ly = c(5, 5)
	lz = c(oz-5, oz+5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox-5, ox-5)
	ly = c(5, 5)
	lz = c(oz-5, oz+5)
	rgl.lines(lx, ly, lz, col="black")

	lx = c(ox+5, ox+5)
	ly = c(-5, -5)
	lz = c(oz-5, oz+5)
	rgl.lines(lx, ly, lz, col="black")

	title3d(main='',sub='','X','Z','Y', color="black", cex.main=4)
	view3d(theta=10, phi=15)

	# filename = paste(filename, ".png", sep="")
	# rgl.snapshot(filename, fmt="png")
}
