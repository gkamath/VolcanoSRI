#ifndef INTERV_H
#define INTERV_H

#include <stdio.h>
#include <vector>
using namespace std;

int findInterval(double *xt, int n, double x, bool rightmost_closed, bool all_inside, int ilo, int *mflag)
{
    int istep, middle, ihi;

#define left_boundary  { *mflag = -1;	return(all_inside ? 1 : 0); }

#define right_boundary { *mflag = +1;					\
	return((all_inside || (rightmost_closed && x == xt[n]))		\
		? (n - 1) : n); }

    //printf("&&&& %d - %lf", n, xt[n]);
    --xt;
    //--n;
    //printf("&&&& %d - %lf", n, xt[n]);

    if(ilo <= 0) {
	if (x < xt[1])			left_boundary;
	ilo = 1;
    }
    ihi = ilo + 1;
    if (ihi >= n) {
	if (x >= xt[n])			right_boundary;
	if (n <= 1)             	left_boundary;
	ilo = n - 1;
	ihi = n;
    }

    if (x < xt[ihi]) {
	if (x >= xt[ilo]) {
	    *mflag = 0;	   return ilo;
	}
	for(istep = 1; ; istep *= 2) {
	    ihi = ilo;
	    ilo = ihi - istep;
	    if (ilo <= 1)
		break;
	    if (x >= xt[ilo])		goto L55;
	}
	ilo = 1;
	if (x < xt[1])			left_boundary;
    }
    else {
	for(istep = 1; ; istep *= 2) {
	    ilo = ihi;
	    ihi = ilo + istep;
	    if (ihi >= n)
		break;
	    if (x < xt[ihi])		goto L55;
	}
	if (x >= xt[n])			right_boundary;
	ihi = n;
    }

  L55:
    for(;;) {
	middle = (ilo + ihi) / 2;
	if (middle == ilo) {
	    *mflag = 0;	   return ilo;
	}
	if (x >= xt[middle])
	    ilo = middle;
	else
	    ihi = middle;
    }
}

void find_interv_vec(double *xt, int n, double *x,  int nx,
		     bool rightmost_closed, bool all_inside, int *indx)
{
    int i, ii, mfl;
    ii = 1;
    for(i = 0; i < nx; i++) {
		mfl = all_inside;
		ii = findInterval(xt, n, x[i], rightmost_closed, all_inside, ii,  &mfl);
		indx[i] = ii;
    }
}

#endif