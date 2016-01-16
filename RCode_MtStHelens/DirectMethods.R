#### Direct methods for solving least square problems

#### SolveQR function definition
#### solve Ax = b by QR decomposition
####
#### input:
####   eqnSys - an array of lists contains the information of equation system
####     eqnSys[[i]]$A - the non-zero values in row i of matrix A
####     eqnSys[[i]]$idx - the indexes of the corresponding non-zero values in row i of matrix A
####     eqnSys[[i]]$b - b[i] of column vector b
####   rows - rows of eqnSys
####   cols - dimension of x
#### output:
####   estimation of x
####
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

#### SolveSVD function definition
#### solve Ax = b by SVD decomposition
####
#### input:
####   eqnSys - an array of lists contains the information of equation system
####     eqnSys[[i]]$A - the non-zero values in row i of matrix A
####     eqnSys[[i]]$idx - the indexes of the corresponding non-zero values in row i of matrix A
####     eqnSys[[i]]$b - b[i] of column vector b
####   rows - rows of eqnSys
####   cols - dimension of x
#### output:
####   estimation of x
####
SolveSVD <- function(eqnSys, rows, cols)
{
	m = rows
	n = cols
	b = array(0, m)

# 	print("++++++++")
# 	print(m)
# 	print("--------")
# 	print(n)

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

	#### compute decomposition
	s = svd(A)
	
	#### compute x
	c = t(s$u) %*% b
	y = c/s$d
	x = s$v %*% y

	return(x)
}