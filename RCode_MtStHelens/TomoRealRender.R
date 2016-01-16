### render real data
source("TomoRender.R")
#filename = "data/slowness_velrperturb_z1_6_160x200x24"
filename = "slowness_velrperturb_z1_6_160x200x24_0408_1225"

xLen = 160
yLen = 200
zLen = 24

xInt = 1
yInt = 1
zInt = 1

xRes = xLen/xInt
yRes = yLen/yInt
zRes = zLen/zInt

# Rendering area
# (-122.41, -121.965)
# (46.0625, 46.375)

xr = c(60, 100)
yr = c(80, 120)

xr = c(65, 95)
yr = c(80, 120)

# xr = c(0, 160)
# yr = c(0, 200)

fileout = "test.pdf"
Slice2DRenderZxes(filename, fileout, xLen, yLen, zLen, xInt, yInt, zInt, xr, yr, 7)
# stop()

# dev.new()
# for(i in 2:7)
# {
# 	filename = paste("data/slowness_velrperturb_", i, "_", xRes, "x", yRes, "x", zRes, sep="")
# 	for(j in 1:7)
# 	{
# 		fileout = paste("data/sl_", i , "_", j, "_", xRes, "x", yRes, "x", zRes, ".pdf", sep="")
# 		Slice2DRenderZxes(filename, fileout, xLen, yLen, zLen, xInt, yInt, zInt, xr, yr, j)
# 	}
# }

# xr = c(60, 100)
# # xr = c(0, 160)
# yr = c(3, 18)
# # dev.new()
# # Slice2DRenderYxes(filename, xLen, yLen, zLen, xInt, yInt, zInt, xr, yr, 104)
# layer = c(89, 94, 99, 104, 109)
# 
# dev.new()
# for(i in 2:7)
# {
# 	filename = paste("data/slowness_velrperturb_z1_", i, "_", xRes, "x", yRes, "x", zRes, sep="")
# 	for(j in 1:5)
# 	{
# 		fileout = paste("data/sl_z1_v_", i , "_", layer[j], "_", xRes, "x", yRes, "x", zRes, ".pdf", sep="")
# 		Slice2DRenderYxes(filename, fileout, xLen, yLen, zLen, xInt, yInt, zInt, xr, yr, layer[j])
# 	}
# }
