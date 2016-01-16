#### LsqrSol solves Ax = b or min ||b - Ax||_2 if damp = 0,
#### or   min ||(b) - (  A   )x||   otherwise.
####          ||(0)   (damp*I) ||_2
#### A is an m by n matrix (ideally sparse)
LsqrSol <- function(A, b, damp=0, atol=1.0e-6, btol=1.0e-6, conlim=1.0e+8, debug=TRUE)
{
	#### Initialization
	
	itn = 0
	istop = 0
	ctol = 0
	
	if(conlim > 0)
	{
		ctol = 1/conlim
	}
	
	Anorm = 0
	Acond = 0
	dampsq = damp^2
	ddnorm = 0
	res2 = 0
	xnorm = 0
	xxnorm = 0
	z = 0
	cs2 = -1
	sn2 = 0
	
	dims = dim(A)
	m = dims[1]
	n = dims[2]

	itnlim=10*n
	
	u = b[1:m]
	x = matrix(0, n, 1)
	
	alfa = 0
	beta = sqrt(crossprod(u,u))
	
	if(beta > 0)
	{
		u = (1/beta)*u
		v = t(A) %*% u
		alfa = sqrt(crossprod(v,v))
	}
	if(alfa > 0)
	{
		v = (1/alfa)[1]*v
		w = v
	}
	
	Arnorm = alfa*beta
	if(Arnorm == 0)
	{
		return(0)
	}
	
	rhobar = alfa
	phibar = beta
	bnorm  = beta
	rnorm  = beta
	r1norm = rnorm
	r2norm = rnorm
	
	while(itn < itnlim)
	{
		itn = itn + 1
		print(paste("    LSQR ====", itn, "iterations"))
		
		u = A %*% v - alfa[1]*u
		
		beta = sqrt(crossprod(u,u))
		if(beta > 0)
		{
			u  = (1/beta)[1]*u
			Anorm = sqrt(crossprod(c(Anorm,alfa,beta,damp),c(Anorm,alfa,beta,damp)))
			v = t(A) %*% u - beta[1]*v
			alfa  = sqrt(crossprod(v,v))
			if(alfa > 0)
			{
				v = (1/alfa)[1]*v
			}
		}

		#### Use a plane rotation to eliminate the damping parameter.
		#### This alters the diagonal (rhobar) of the lower-bidiagonal matrix.

		rhobar1 = sqrt(crossprod(c(rhobar,damp),c(rhobar,damp)))
		cs1 = rhobar/rhobar1
		sn1 = damp/rhobar1
		psi = sn1*phibar
		phibar = cs1*phibar
		
		#### Use a plane rotation to eliminate the subdiagonal element (beta)
		#### of the lower-bidiagonal matrix, giving an upper-bidiagonal matrix.

		rho     =   sqrt(crossprod(c(rhobar1,beta),c(rhobar1,beta)))
		cs      =   rhobar1/rho
		sn      =   beta/rho
		theta   =   sn*alfa
		rhobar  = - cs*alfa
		phi     =   cs*phibar
		phibar  =   sn*phibar
		tau     =   sn*phi

		#### Update x and w.

		t1      =   phi/rho
		t2      = - theta/rho
		dk      =   (1/rho)[1]*w

		x       =   x + t1[1]*w
		w       =   v + t2[1]*w
		ddnorm  =   ddnorm + crossprod(dk,dk)

		#### Use a plane rotation on the right to eliminate the
		#### super-diagonal element (theta) of the upper-bidiagonal matrix.
		#### Then use the result to estimate  norm(x).

		delta   =   sn2*rho
		gambar  = - cs2*rho
		rhs     =   phi - delta*z
		zbar    =   rhs/gambar
		xnorm   =   sqrt(xxnorm + zbar^2)
		gamma   =   sqrt(crossprod(c(gambar,theta),c(gambar,theta)))
		cs2     =   gambar/gamma
		sn2     =   theta/gamma
		z       =   rhs/gamma
		xxnorm  =   xxnorm + z^2

		#### Test for convergence.
		#### First, estimate the condition of the matrix  Abar,
		#### and the norms of  rbar  and  Abar'rbar.

		Acond   =   Anorm*sqrt(ddnorm)
		res1    =   phibar^2
		res2    =   res2 + psi^2
		rnorm   =   sqrt(res1 + res2)
		Arnorm  =   alfa*abs(tau)

		#### Distinguish between
		####    r1norm = ||b - Ax|| and
		####    r2norm = rnorm in current code
		####           = sqrt(r1norm^2 + damp^2*||x||^2).
		####    Estimate r1norm from
		####    r1norm = sqrt(r2norm^2 - damp^2*||x||^2).
		#### Although there is cancellation, it might be accurate enough.

		r1sq    =   rnorm^2 - dampsq*xxnorm
		r1norm  =   sqrt(abs(r1sq))
		if(r1sq < 0)
		{
		      r1norm = - r1norm
		}
		r2norm  =   rnorm

		#### Now use these norms to estimate certain other quantities,
		#### some of which will be small near a solution.

		test1   =   rnorm/bnorm
		test2   =   Arnorm/(Anorm*rnorm)
		test3   =   1/Acond
		t1      =   test1/(1 + Anorm*xnorm/bnorm)
		rtol    =   btol + atol*Anorm*xnorm/bnorm

		#### The following tests guard against extremely small values of
		#### atol, btol  or  ctol.  (The user may have set any or all of
		#### the parameters  atol, btol, conlim  to 0.)
		#### The effect is equivalent to the normal tests using
		#### atol = eps,  btol = eps,  conlim = 1/eps.

		if(itn >= itnlim)
		{istop = 7}
		if(1 + test3 <= 1)
		{istop = 6}
		if(1 + test2 <= 1)
		{istop = 5}
		if(1 + t1 <= 1)
		{istop = 4}

		#### Allow for tolerances set by the user.

		if(test3 <= ctol)
		{istop = 3}
		if(test2 <= atol)
		{istop = 2}
		if(test1 <= rtol)
		{istop = 1}

		if(istop > 0)
		{
			break
		}
	}
	
	# x = c(x, istop)
	return(x)
}




#### SolveLsqr call LsqrSol to solve Ax = b
#### where Ai = eqnSys[[i]]$A and b[i] = eqnSys[[i]]$b
SolveLsqr <- function(eqnSys, slowness, rows)
{
# 	rows = length(eqnSys)
	cols = length(slowness)
	
	print("rows of the system: ")
	print(rows)
	
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

	print(">>>> start solving LSQR <<<<")
	x = LsqrSol(A, b)
	print("<<<< end solving LSQR <<<<")
	
	sptb = array(0, cols)
	sptb[1:cols] = x[1:cols, 1]
	slowness = slowness + sptb
	
	return(slowness)
}

#### SolveLsqr call LsqrSol to solve Ax = b
#### where Ai = eqnSys[[i]]$A and b[i] = eqnSys[[i]]$b
SolveLsqrReal <- function(eqnSys, cols, rows)
{
# 	rows = length(eqnSys)
# 	cols = length(slowness)
	
	print("rows of the system: ")
	print(rows)
	
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

	print(">>>> start solving LSQR <<<<")
	x = LsqrSol(A, b)
	print("<<<< end solving LSQR <<<<")
	
	sptb = array(0, cols)
	sptb[1:cols] = x[1:cols, 1]
# 	slowness = slowness + sptb
	
	return(sptb)
}