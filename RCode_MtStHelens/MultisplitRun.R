source("MultisplittingMethod.R")

#### test with matrix structure
# #### generate random matrix A, random solution x
# #### compute b
# 
# #### m rows with n cols
# m = 2
# n = 2
# 
# #### value range of A
# a_low = 0.0
# a_high = 1.0
# 
# #### value range of x
# x_low = 0.0
# x_high = 1.0
# 
# #### random A and x
# # a_random_seed = 1
# # set.seed(a_random_seed)
# # A = matrix(runif(m*n, a_low, a_high), m, n)
# # A = ceiling(A)
# # # x = runif(n, x_low, x_high)
# # x = c(1:n)
# 
# #### random sparse A and x
# a_random_seed = 1
# set.seed(a_random_seed)
# A = matrix(runif(m*n, a_low, a_high), m, n)
# set.seed(a_random_seed*2)
# x = runif(n, x_low, x_high)
# 
# #### temp test
# A = matrix(0, 2, 2)
# A[1, 1] = 1
# A[1, 2] = 2
# A[2, 1] = 2
# A[2, 2] = 1
# 
# x = c(4, 1)
# 
# #### set sparsity
# th = 1.0
# 
# a_random_seed = 2
# set.seed(a_random_seed*3)
# rnum = matrix(runif(m*n, 0, 1), m, n)
# 
# for(i in 1:m)
# {
# 	for(j in 1:n)
# 	{
# 		if(rnum[i, j] > th)
# 		{
# 			A[i, j] = 0.0
# 		}
# 	}
# }
# 
# #### compute b
# b = A %*% x
# 
# # print(A)
# # print(b)
# # print(x)
# 
# print("========")
# result = qr.solve(A,b)
# # print(result)
# print(paste("square error : ", sqrt(crossprod(A %*% result - b))))
# print(paste("solution norm: ", sqrt(crossprod((x-result), (x-result)))))
# print("========")
# 
# #### multisplitting initialization
# p = 2                       # partitions
# sc = n/p                    # sub system columns
# 
# xx = matrix(0, p, sc)       # solution
# yy = matrix(0, p, sc)       # local solution
# delta = matrix(0, p, sc)    # delta between solution and local solution
# 
# alpha = array(1/p, p)
# bb = matrix(0, m, p)
# B = matrix(0, m, p)         # A %*% delta
# 
# for(i in 1:m)
# {
# 	for(j in 1:p)
# 	{
# 		bb[i,j] = b[i]
# 	}
# }
# 
# # print(bb)
# 
# #### algorithm
# tolerance = 0.01
# tround = 0
# 
# rounds = 2
# sol = array(0, n)
# delta = yy - xx
# 
# for(r in 1:rounds)
# {
# 	for(j in 1:p)
# 	{
# 		i1 = (j-1)*sc + 1
# 		i2 = j*sc
# 		if(sc == 1)
# 		{
# 			tempM = matrix(0, 1, 1)
# 			tempM[1,1] = delta[j,]
# 			B[,j] = A[,i1:i2] %*% tempM
# 		}
# 		else
# 		{
# 			B[,j] = A[,i1:i2] %*% delta[j,]
# 		}
# 	}
# 
# 	print(B)
# 
# 	for(i in 1:p)
# 	{
# 		for(j in 1:p)
# 		{
# 			if(j != i)
# 			{
# 				bb[,i] = bb[,i] - alpha[j]*B[,j]
# 			}
# 		}
# 	}
# 
# # 	print("&&&&&&&&&&&&&&&&&&")
# # 	print(bb)
# # 	print("&&&&&&&&&&&&&&&&&&")
# 
# 	for(j in 1:p)
# 	{
# 		i1 = (j-1)*sc + 1
# 		i2 = j*sc
# 		yy[j,] = qr.solve(A[,i1:i2],bb[,j])
# 	}
# 
# 	print(yy)
# 
# 	delta = yy - xx
# 
# 	print(paste("----", delta))
# 	
# 	for(j in 1:p)
# 	{
# 		xx[j,] = xx[j,] + alpha[j]*delta[j,]
# 	}
# 
# 	print(xx)
# 
# 	for(j in 1:p)
# 	{
# 		i1 = (j-1)*sc + 1
# 		i2 = j*sc
# 		sol[i1:i2] = xx[j,]
# 	}
# 
# 	if(sqrt(crossprod(A %*% sol - b)) < tolerance)
# 	{
# 		tround = r
# 		break
# 	}
# 
# 	if(r %% 10 == 0)
# 	{
# 		print(paste("************ rounds", r))
# 		print(paste("square error : ", sqrt(crossprod(A %*% sol - b))))
# 		print(paste("solution norm: ", sqrt(crossprod((x-sol), (x-sol)))))
# 	}
# }
# 
# print("-------------------------")
# print(x)
# print(sol)
# print(tround)
# print("-------------------------")
# print(paste("square error : ", sqrt(crossprod(A %*% result - b))))
# print(paste("solution norm: ", sqrt(crossprod((x-result), (x-result)))))
# print("-------------------------")
# print(paste("square error : ", sqrt(crossprod(A %*% sol - b))))
# print(paste("solution norm: ", sqrt(crossprod((x-sol), (x-sol)))))

#### generate random matrix A, random solution x
#### compute b

#### m rows with n cols
m = 2048
n = 1024

#### value range of A
a_low = 0.0
a_high = 1.0

#### value range of x
x_low = 0.0
x_high = 1.0

#### random A and x
# a_random_seed = 1
# set.seed(a_random_seed)
# A = matrix(runif(m*n, a_low, a_high), m, n)
# A = ceiling(A)
# # x = runif(n, x_low, x_high)
# x = c(1:n)

#### random sparse A and x
a_random_seed = 1
set.seed(a_random_seed)
A = matrix(runif(m*n, a_low, a_high), m, n)
set.seed(a_random_seed*2)
x = runif(n, x_low, x_high)

#### temp test
# A = matrix(0, 2, 2)
# A[1, 1] = 1
# A[1, 2] = 2
# A[2, 1] = 2
# A[2, 2] = 1
# 
# x = c(4, 1)

#### set sparsity
th = 0.04

a_random_seed = 2
set.seed(a_random_seed*3)
rnum = matrix(runif(m*n, 0, 1), m, n)

for(i in 1:m)
{
	for(j in 1:n)
	{
		if(rnum[i, j] > th)
		{
			A[i, j] = 0.0
		}
	}
}

#### compute b
b = A %*% x

#### adding noise to b


#### mapping to data structures using in tomopartition
system = array(list(), m)
index = c(1:n)
for(i in 1:m)
{
	system[[i]]$A = A[i,]
	system[[i]]$idx = index
	system[[i]]$b = b[i]
}

rounds = 200
p = 16

result = MultiSplitART(system, m, n, p, rounds, TRUE, x)

print(x)
print(result$x)

b_k = A %*% result$x
error = crossprod(b_k - b)
print("====")
print(error)