# matrix dimension nxm

#### generate random matrix A, random solution x
#### compute b

#### m rows with n cols
m = 40
n = 20

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

#### set sparsity
th = 1.0

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
set.seed(a_random_seed*4)
AA = matrix(runif(m*n, a_low, a_high), m, n)
b = AA %*% x
# set.seed(a_random_seed*4)
# b = runif(n, x_low, x_high)

#### mapping to data structures using in tomopartition
system = array(list(), m)
index = c(1:n)
for(i in 1:m)
{
	system[[i]]$A = A[i,]
	system[[i]]$idx = index
	system[[i]]$b = b[i]
}

SolveBayesianART <- function(eqnSys, rows, initGuess, rho, lambda, tolerance, maxRounds = 1000)
{
	x = initGuess
	xsmooth = initGuess
	print(paste(">>>> Bayesian ART Method >>>>", rows))

	update = Inf
	
	r = array(0, rows)
	totalUpdate = array(0, length(initGuess))
	
	j = 0
	while(j < maxRounds && update > tolerance)
	{
		j = j+1
		print(paste("    Bayesian ART Iteration ====", j, "iterations"))

		x0 = x
		for(i in 1:rows)
		{
			ai = eqnSys[[i]]$A
			bi = eqnSys[[i]]$b
			index = eqnSys[[i]]$idx

			if(bi == 0.0)
			{
				next
			}
			
			as = ai %*% x[index]
			aa = ai %*% ai

			num = bi - lambda*r[i] - as
			denom = lambda*lambda + aa

			gamma = (rho*num) / denom
			r[i] = r[i] + gamma*lambda

			x[index] = x[index] + gamma*ai
		}

		rho = rho / 1.01
		
		deltaX = x-x0
		deltaXNorm = sqrt(deltaX %*% deltaX)
		x0Norm = sqrt(x0 %*% x0)
		update = deltaXNorm/x0Norm
		print(paste("    **** relative update: ", update))
	}
	
	print(paste("<<<< End of Bayesian ART <<<<", j))
	
	return(x)
}

SolveQR <- function(eqnSys, rows, cols)
{
	m = rows
	n = cols
	b = array(0, m)

	A = matrix(0, m, n)

	for(i in 1:m)
	{
		A[i, eqnSys[[i]]$idx] = eqnSys[[i]]$A
		b[i] = eqnSys[[i]]$b
		#### debug
# 		if(i%%100 == 0)
# 		{
# 			print(paste("==== A ", i))
# 		}
	}

	QR = lm(formula = b ~ A - 1, model = TRUE)
	x = QR$coefficient
	return(x)
}

RLS <- function(mA, vb, m, n, lambda, w_init)
{
	P_i = diag(n)
	w_i = w_init

	for(i in 1:m)
	{
		u_i = mA[i,]
		d_i = vb[i]

		num = 1/lambda * (P_i %*% u_i)
		denom = 1 + 1/lambda * (u_i %*% P_i %*% u_i)
		g_i = num/denom[1]

		P_i = 1/lambda * (P_i - g_i %*% u_i %*% P_i)

# 		print(P_i)

		step = (d_i - u_i %*% w_i)

# 		print(w_i)
# 		print(g_i)
# 		print(step[1])
		w_i = w_i + g_i[,1] * step[1]
	}

	return(w_i)
}

lambda = 1.0
rho = 1.0
tolerance = -Inf
initGuess = array(0, n)

print(A)
print("====")
print(x)
print("====")
print(b)

sl0 = SolveBayesianART(system, m, initGuess, rho, lambda, tolerance)
error01 = sqrt(crossprod(sl0))
error02 = sqrt(crossprod(A %*% sl0 - b))
error0 = error01 + error02

print("0 +++++++++++++++++++++++++++")
print(sl0)
print(error0)
print(error01)
print(error02)

ws = array(0, n)
sl1 = RLS(A, b, m, n, 1, ws)
# print(sl1)
error11 = sqrt(crossprod(sl1))
error12 = sqrt(crossprod(A %*% sl1 - b))
error1 = error11 + error12

print("1 +++++++++++++++++++++++++++")
print(t(sl1))
print(error1)
print(error11)
print(error12)

sl2 = SolveQR(system, m, n)
error2 = sqrt(crossprod(A %*% sl2 - b))

print("2 +++++++++++++++++++++++++++")
# print(sl2)
print(error2)
#           [,1]
# [1,] 0.4816404
# [2,] 0.3504340
# [3,] 0.0936315
# [4,] 0.2639397
#           [,1]
# [1,] 0.3557968