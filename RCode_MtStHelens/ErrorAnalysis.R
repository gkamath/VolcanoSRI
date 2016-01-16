#Verification test of the following error analysis functions
#TestErrorAnalysis(c(1,2,3), c(4,2,7))

#X = c(1,2,3)
#Y = c(4,2,7)
#whatever = TestErrorAnalysis(X, Y)

TestErrorAnalysis <- function(X, Y)
{
	errorD = dError(X, Y)
	normError = errorNorm(X, Y)
	print(paste("d1: ", errorD$d1))
	print(paste("d2: ", errorD$d2))
	print(paste("d3: ", errorD$d3))
	print(paste("error norm: ", normError))
	return(1)
}

#### system product Ax
SystemAx <- function(eqnSys, rows, x)
{
	b = array(0, rows)
	for(i in 1:rows)
	{
		b[i] = eqnSys[[i]]$A %*% x[eqnSys[[i]]$idx]
	}
	return(b)
}

#### angle between two vectors
Angle <- function(v1, v2, len)
{
	PI = 3.1415926535897932384626433
	
}

#### Calculate the square error ||Ax-b||_2
#### input: x - slowness perturbation result
####        eqnSys - equation system
####                 eqnSys[[i]] is the i-th row in the system
####                 eqnSys[[i]]$A is a_i, eqnSys[[i]]$b is b_i

SquareError <- function(eqnSys, rows, x)
{
	lsValue = 0		
	#rows = length(eqnSys)
	
	for(i in 1:rows)
	{
		iAx = 0
		numValues = length(eqnSys[[i]]$A)
		for(j in 1:numValues)
		{
			iAx = iAx + eqnSys[[i]]$A[j]*x[eqnSys[[i]]$idx[j]]
		}

		lsValue = lsValue + (eqnSys[[i]]$b - iAx)*(eqnSys[[i]]$b - iAx)
	}

	return(lsValue)
}

#### Calculate the normal equation residual
#### input: x - slowness perturbation result
####        eqnSys - equation system
####                 eqnSys[[i]] is the i-th row in the system
####                 eqnSys[[i]]$A is a_i, eqnSys[[i]]$b is b_i
####
#### Compute t(A)b-t(A)As, the solution of t(A)As=t(A)b is the least square solution
NormalEqnResidual <- function(eqnSys, rows, x)
{
	cols = length(x)
	s = matrix(0, cols, 1)
	s[1:cols,1] = x[1:cols]

# 	rows = length(eqnSys)
	
	matrix = list()
	A = matrix(0, rows, cols)
	b = matrix(0, rows, 1)
	
	for(i in 1:rows)
	{
		numValues = length(eqnSys[[i]]$A)
		for(j in 1:numValues)
		{
			A[i, eqnSys[[i]]$idx[j]] = eqnSys[[i]]$A[j]
		}
		b[i, 1] = eqnSys[[i]]$b
	}

	At = t(A)
	
	residual = At %*% b - At %*% A %*% s
	squareResidual = crossprod(residual, residual)
	squareResidual = sqrt(squareResidual)
	return(squareResidual)
}

#### quality assessment computation
#### d1, d2, d3 refer to Dr. Lees' paper, http://www.unc.edu/~leesj/FETCH/GRAB/Papers/Baysian_ART_ams.pdf
#### input:
####     st is the slowness truth
####     se is the slowness estimation

dError <- function(st, se)
{
	tLen = length(st)
	eLen = length(se)
	if(tLen != eLen)
	{
		print("error estimation dimension not match!")
		stop()
	}
	
	#### d1
	sm = mean(se)
	d1 = sqrt(sum((st-se)^2)/sum((st-sm)^2))

	#### d2
	d2 = sum(abs(st-se))/sum(abs(se))

	#### d3
	d3 = max(abs(st-se))

	derror = list(d1 = d1, d2 = d2, d3 = d3)
	return(derror)
}

#### quality assessment computation
#### eNorm refers to the L2 norm of the error between st and se
#### input:
####     st is the slowness truth
####     se is the slowness estimation

errorNorm <- function(st, se)
{
	tLen = length(st)
	eLen = length(se)
	if(tLen != eLen)
	{
		print("error estimation dimension not match!")
		stop()
	}

	eNorm = sqrt(sum((st-se)^2))
	return(eNorm)
}
