source("TomoRender.R")

# xSlices = c(21, 37, 53, 69)
# ySlices = c(16, 24, 32, 40)
SliceResGen <- function(slownessFile)
{
# 	xSlices = c(21, 53)
# 	ySlices = c(24, 40)
	xSlices = c(21)
	ySlices = c(40)

	numSlice = length(xSlices)
	withBorder = FALSE
	for(i in 1:numSlice)
	{
		dev.new(i)
		Slice2DRender(slownessFile, xSlices[i], 10.0, "x", withBorder)
	}

	numSlice = length(xSlices)
	for(i in 1:numSlice)
	{
		dev.new(i)
		Slice2DRender(slownessFile, ySlices[i], 10.0, "y", withBorder)
	}
}