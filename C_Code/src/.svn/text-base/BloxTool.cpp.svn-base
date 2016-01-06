#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <vector>
#include <algorithm>
#include "Interv.h"

#define ERROR 0.00000000000001
#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))

using namespace std;

void order(double vec[], int idx[], int len) {
  int i, j, count;
  for(i = 0; i < len; i++) {
    count = 0;
    for(j = 0; j < len; j++) {
      if(vec[j] < vec[i]) {
	count++;
      }
    }
    idx[count] = i;
  }
}

bool identical(double A, double B, double Error) {
  if (fabs(A - B) < Error)
    return true;
  return false;
}

bool different(double A, double B, double Error) {
  if (fabs(A - B) < Error)
    return false;
  return true;
}

int getEXlen(double x_1, double x_2, double xo[], int xlen) {
  int i, ix1, ix2, EXlen;
  ix1 = -1;
  ix2 = -1;
  for(i = 0; i < xlen; i++) {
    if(ix1 < 0 && xo[i] >= min(x_1, x_2)) {
      ix1 = i;
    }
    if(ix2 < 0 && xo[i] > max(x_1, x_2)) {
      ix2 = i - 1;
      break;
    }
  }
  
  if(ix1 > ix2) {
    EXlen = 2;
  }
  else {
    EXlen = ix2 - ix1 + 3;
  }
  
  return EXlen;
}

int getWHYlen(double y_1, double y_2, double yo[], int ylen) {
  int i, iy1, iy2, WHYlen;
  iy1 = -1;
  iy2 = -1;
  for(i = 0; i < ylen; i++) {
    if(iy1 < 0 && yo[i] >= min(y_1, y_2)) {
      iy1 = i;
    }
    if(iy2 < 0 && yo[i] > max(y_1, y_2)) {
      iy2 = i - 1;
      break;
    }
  }
      
  if(iy1 > iy2) {
    WHYlen = 0;
  }
  else {
    WHYlen = iy2 - iy1 + 1;
  }
  
  return WHYlen;
}

void get2Drayblox(double x_1, double y_1, double x_2, double y_2, double xo[], double yo[], int xlen, int ylen, 
		  int EXlen, int WHYlen, int *indexx, int *indexy, double *lengs) {
      //declaration
      double dx, dy, slope, intercept;
      int i, j;
      int ix1, ix2;
      int iy1, iy2;
      int kx, ky;
      
      //dynamic mem allocation
      double *EX, *EXY, *WHY, *WHYX;
      double *xp, *yp, *nodesx, *nodesy, *midsx, *midsy;
      int *o;
//       int *indexx, *indexy;
      //nbrlist = (MEMTableEntry*)malloc((pni->clsize-1)*sizeof(MEMTableEntry));
      
      dx = xo[1] - xo[0];
      dy = yo[1] - yo[0];
      
      slope = (y_2-y_1)/(x_2-x_1);
      intercept = y_2 - slope*x_2;
      
      ix1 = -1;
      ix2 = -1;
      for(i = 0; i < xlen; i++) {
	if(ix1 < 0 && xo[i] >= min(x_1, x_2)) {
	  ix1 = i;
	}
	if(ix2 < 0 && xo[i] > max(x_1, x_2)) {
	  ix2 = i - 1;
	  break;
	}
      }
//       
//       if(ix1 > ix2) {
// 	EXlen = 2;
//       }
//       else {
// 	EXlen = ix2 - ix1 + 3;
//       }
      
      EX = (double*)malloc(EXlen*sizeof(double));   
      EX[0] = min(x_1, x_2);
      EX[EXlen-1] = max(x_1, x_2);
      for(i = 1, j = ix1; i < EXlen-1; i++, j++) {
	EX[i] = xo[j];
      }
      
      EXY = (double*)malloc(EXlen*sizeof(double));
      for(i = 0; i < EXlen; i++) {
	EXY[i] = intercept + EX[i]*slope;
      }
      
      iy1 = -1;
      iy2 = -1;
      for(i = 0; i < ylen; i++) {
	if(iy1 < 0 && yo[i] >= min(y_1, y_2)) {
	  iy1 = i;
	}
	if(iy2 < 0 && yo[i] > max(y_1, y_2)) {
	  iy2 = i - 1;
	  break;
	}
      }
//       
//       if(iy1 > iy2) {
// 	WHYlen = 0;
//       }
//       else {
// 	WHYlen = iy2 - iy1 + 1;
//       }
      
      WHY = (double*)malloc(WHYlen*sizeof(double));
      WHYX = (double*)malloc(WHYlen*sizeof(double));
      
      if(WHYlen != 0)
      {
	for(i = 0, j = iy1; i < WHYlen; i++, j++) {
	  WHY[i] = yo[j];
	  WHYX[i] = (WHY[i] - intercept)/slope;
	}
      }
      
      xp = (double*)malloc((EXlen+WHYlen)*sizeof(double));
      yp = (double*)malloc((EXlen+WHYlen)*sizeof(double));
      
      for(i = 0; i < EXlen; i++) {
	xp[i] = EX[i];
      }
      
      for(j = 0; j < WHYlen; j++) {
	xp[i+j] = WHYX[j];
      }
      
      for(i = 0; i < EXlen; i++) {
	yp[i] = EXY[i];
      }
      
      for(j = 0; j < WHYlen; j++) {
	yp[i+j] = WHY[j];
      }
      
      o = (int*)malloc((EXlen+WHYlen)*sizeof(int));
      order(yp, o, EXlen+WHYlen);
      
//       kx = ky = EXlen+WHYlen;
      
      nodesx = (double*)malloc((EXlen+WHYlen)*sizeof(double));
      nodesy = (double*)malloc((EXlen+WHYlen)*sizeof(double));
      
      for(i = 0; i < EXlen+WHYlen; i++) {
	nodesx[i] = xp[o[i]];
      }
      
      for(i = 0; i < EXlen+WHYlen; i++) {
	nodesy[i] = yp[o[i]];
      }
      
      if(identical(y_2, y_1, ERROR)) {
	order(xp, o, EXlen+WHYlen);
	for(i = 0; i < EXlen+WHYlen; i++) {
	  nodesx[i] = xp[o[i]];
	}
	
	for(i = 0; i < EXlen+WHYlen; i++) {
	  nodesy[i] = yp[o[i]];
	}
      }
      
      if(y_2 < y_1) {
	j = EXlen+WHYlen-1;
	for(i = 0; i < EXlen+WHYlen; i++) {
	  nodesx[i] = xp[o[j-i]];
	}
	
	for(i = 0; i < EXlen+WHYlen; i++) {
	  nodesy[i] = yp[o[j-i]];
	}
      }
      
      midsx = (double*)malloc((EXlen+WHYlen-1)*sizeof(double));
      midsy = (double*)malloc((EXlen+WHYlen-1)*sizeof(double));
//       lengs = (double*)malloc((EXlen+WHYlen-1)*sizeof(double));

      for(i = 0; i < EXlen+WHYlen-1; i++) {
	midsx[i] = (nodesx[i+1]+nodesx[i])/2;
	midsy[i] = (nodesy[i+1]+nodesy[i])/2;
      }
      
      for(i = 0; i < EXlen+WHYlen-1; i++) {
	lengs[i] = sqrt((nodesx[i+1]-nodesx[i])*(nodesx[i+1]-nodesx[i])+
	                (nodesy[i+1]-nodesy[i])*(nodesy[i+1]-nodesy[i]));
      }
      
//       indexx = (int*)malloc((EXlen+WHYlen-1)*sizeof(int));
//       indexy = (int*)malloc((EXlen+WHYlen-1)*sizeof(int));
      
      find_interv_vec(xo, xlen, midsx, EXlen+WHYlen-1, false, false, indexx);
      find_interv_vec(yo, ylen, midsy, EXlen+WHYlen-1, false, false, indexy);
      
//       free(indexy);
//       free(indexx);
      free(midsy);
      free(midsx);
      free(nodesy);
      free(nodesx);
      free(o);
      free(WHYX);
      free(WHY);
      free(EXY);
      free(EX);
}

int get3Drayblox(double XNOD[], double YNOD[], double ZNOD[], 
		  double xo[], double yo[], double zo[], 
		  int nnod, int xlen, int ylen, int zlen, 
		  int indexx[], int indexy[], int indexz[], double lengs[]) {
  int i, j, k;
  int EXlen, WHYlen;
  int *index_x, *index_y, *index_z;
  double *lengths;
  double *MIDZNOD, seglen;
  double cosalph;
  int *FZNOD;
  
  int blox = 0;
  
  MIDZNOD = (double*)malloc((nnod-1)*sizeof(double));
//   seglen = (double*)malloc((nnod-1)*sizeof(double));
  
  for(i = 0; i < nnod-1; i++) {
    MIDZNOD[i] = ZNOD[i] + (ZNOD[i+1] - ZNOD[i])/2;
  }
  
  FZNOD = (int*)malloc((nnod-1)*sizeof(int));
  find_interv_vec(zo, zlen, MIDZNOD, nnod-1, false, false, FZNOD);
  
  for(i = 0; i < nnod-1; i++) {
    seglen = sqrt((ZNOD[i+1]-ZNOD[i])*(ZNOD[i+1]-ZNOD[i]) +
                  (YNOD[i+1]-YNOD[i])*(YNOD[i+1]-YNOD[i]) +
                  (XNOD[i+1]-XNOD[i])*(XNOD[i+1]-XNOD[i]));
    
    EXlen = getEXlen(XNOD[i], XNOD[i+1], xo, xlen);
    WHYlen = getWHYlen(YNOD[i], YNOD[i+1], yo, ylen);
    
    lengths = (double*)malloc((EXlen+WHYlen-1)*sizeof(double));
    index_x = (int*)malloc((EXlen+WHYlen-1)*sizeof(int));
    index_y = (int*)malloc((EXlen+WHYlen-1)*sizeof(int));
    index_z = (int*)malloc((EXlen+WHYlen-1)*sizeof(int));
    
    get2Drayblox(XNOD[i], YNOD[i], XNOD[i+1], YNOD[i+1], xo, yo, 
		 xlen, ylen, EXlen, WHYlen, index_x, index_y, lengths);
    
    cosalph = sqrt((XNOD[i] - XNOD[i + 1])*(XNOD[i] - XNOD[i + 1]) + 
                   (YNOD[i] - YNOD[i + 1])*(YNOD[i] - YNOD[i + 1]))/seglen;
    
    k = blox;
    for(j = 0; j < EXlen+WHYlen-1; j++) {
      lengths[j] = lengths[j]/cosalph;
      index_z[j] = FZNOD[i];
      blox++;
    }
    
    for(j = 0; j < EXlen+WHYlen-1; j++) {
      indexx[j+k] = index_x[j];
      indexy[j+k] = index_y[j];
      indexz[j+k] = index_z[j];
      lengs[j+k] = lengths[j];
    }
    
    free(index_z);
    free(index_y);
    free(index_x);
    free(lengths);
  }
  
  return blox;
}

void swap(double coor[2]) {
  double temp;
  temp = coor[0];
  coor[0] = coor[1];
  coor[1] = temp;
}

void vecSubScalar(double a[], double b, double c[], int n) {
	int i;  
	for(i=0; i<n; i++) {
		c[i] = a[i]-b;
	}
}

void scalarSubVec(double a, double b[], double c[], int n) {
	int i;  
	for(i=0; i<n; i++) {
		c[i] = a-b[i];
	}
}

void scalarAddVec(double a, double b[], double c[], int n) {
	int i;  
	for(i=0; i<n; i++) {
		c[i] = a+b[i];
	}
}

void vecMultiScalar(double a[], double b, double c[], int n) {
   int i;
   for(i=0; i<n; i++) {
	   c[i] = a[i]*b;
   }
}

// void printVec(double a[], int n) {
// 	int i;
// 	for(i = 0; i < n; i++) {
// 		printf("%.8f ", a[i]);
// 	}
// 	printf("\n");
// }
// 
// void printVecInt(int a[], int n) {
// 	int i;
// 	for(i = 0; i < n; i++) {
// 		printf("%d ", a[i]);
// 	}
// 	printf("\n");
// }

/*
 * get ray blocks between points (x_coor[0], y_coor[0], z_coor[0]) and (x_coor[1], y_coor[1], z_coor[1])
 * 
 */

/*int get_ray_blox(vector<double> &x_coor, vector<double> &y_coor, vector<double> &z_coor, 
		const vector<double> &xo, const vector<double> &yo, const vector<double> &zo, 
		vector<int> &ix, vector<int> &iy, vector<int> &iz, vector<double> &lens) {
	
	//to-do: adding vector checking for valid input
	
	if(z_coor[0] < z_coor[1]) {
		swap(x_coor[0], x_coor[1]);
	    swap(y_coor[0], y_coor[1]);
	    swap(z_coor[0], z_coor[1]);
	}
	
	double dee = sqrt((x_coor[1]-x_coor[0])*(x_coor[1]-x_coor[0]) + 
			(y_coor[1]-y_coor[0])*(y_coor[1]-y_coor[0]) + 
			(z_coor[1]-z_coor[0])*(z_coor[1]-z_coor[0]));
	
	double deexy = sqrt((x_coor[1]-x_coor[0])*(x_coor[1]-x_coor[0]) + 
			(y_coor[1]-y_coor[0])*(y_coor[1]-y_coor[0]));
	
	double alpha = asin(deexy/dee);
	
	vector<int> fznod(2, 0);
	//find_interv_vec(zo, zo.size(), z_coor, 2, false, false, fznod);
	
	int segments;
	return segments;
}*/

int getRayblox(double xCoor[], double yCoor[], double zCoor[], 
	       double xo[], double yo[], double zo[], 
	       int numPt, int xLen, int yLen, int zLen, 
	       int ix[], int iy[], int iz[], double lens[]) {
  
  double dee;
  double deexy;
  double alpha;
  
  double *xNod;
  double *yNod;
  double *zNod;
  double *RN;
  double *RN2;
  
  int *FZNOD;  
  int numNode;
  int i, j;
  int segs;
  
  if(zCoor[0] < zCoor[1]) {
    swap(xCoor);
    swap(yCoor);
    swap(zCoor);
  }
  
  dee = sqrt((xCoor[1]-xCoor[0])*(xCoor[1]-xCoor[0]) +
	     (yCoor[1]-yCoor[0])*(yCoor[1]-yCoor[0]) +
	     (zCoor[1]-zCoor[0])*(zCoor[1]-zCoor[0]));
  
  deexy = sqrt((xCoor[1]-xCoor[0])*(xCoor[1]-xCoor[0]) +
	       (yCoor[1]-yCoor[0])*(yCoor[1]-yCoor[0]));
  
  alpha = asin(deexy/dee);

  FZNOD = (int*)malloc(numPt*sizeof(int));
  
  find_interv_vec(zo, zLen, zCoor, numPt, false, false, FZNOD);
  
  //printVecInt(FZNOD, 2);
  
  if(FZNOD[0] == FZNOD[1]) {
    numNode = 2;
    zNod = (double*)malloc(numNode*sizeof(double));
    zNod[0] = zCoor[0];
    zNod[1] = zCoor[1];
  }else {
    if(identical(zCoor[0], zo[FZNOD[0]-1], ERROR)) {
      numNode = FZNOD[0] - (FZNOD[1]+1) + 2;
      zNod = (double*)malloc(numNode*sizeof(double));
      for(j = 0, i = FZNOD[0]-1; j < numNode - 1; j++, i--) {
    	  zNod[j] = zo[i];
      }
      zNod[numNode-1] = zCoor[1];
    }else {
      numNode = FZNOD[0] - (FZNOD[1]+1) + 3;
      zNod = (double*)malloc(numNode*sizeof(double));
      for(j = 1, i = FZNOD[0]-1; j < numNode - 1; j++, i--) {
    	  zNod[j] = zo[i];
    	  //printf("    %d - %lf\n", i, zo[i]);
      }
      zNod[numNode-1] = zCoor[1];
      zNod[0] = zCoor[0];
    }
  }
  
  RN = (double*)malloc(numNode*sizeof(double));
  RN2 = (double*)malloc(numNode*sizeof(double));
  vecSubScalar(zNod, zCoor[1], RN, numNode);
  vecMultiScalar(RN, tan(alpha), RN, numNode);
  scalarSubVec(deexy, RN, RN, numNode);
  
  vecMultiScalar(RN, (xCoor[1]-xCoor[0])/deexy, RN2, numNode);
  xNod = (double*)malloc(numNode*sizeof(double));
  scalarAddVec(xCoor[0], RN2, xNod, numNode);
  
  vecMultiScalar(RN, (yCoor[1]-yCoor[0])/deexy, RN2, numNode);
  yNod = (double*)malloc(numNode*sizeof(double));
  scalarAddVec(yCoor[0], RN2, yNod, numNode);
  
// 	RN = deexy-(zNod-zCoor[2])*tan(alpha)
//
// 	#### Mapping the interval boundary along z axis to x and y axises
// 	xNod = xCoor[1]+RN*(xCoor[2]-xCoor[1])/deexy
// 	yNod = yCoor[1]+RN*(yCoor[2]-yCoor[1])/deexy
//
// 	#### Get 3D ray path blocks
// 	rayPath = get3Drayblox(xNod, yNod, zNod, xo, yo, zo)
  
//   printVec(xNod, numNode);
//   printVec(yNod, numNode);
//   printVec(zNod, numNode);
		  
  segs = get3Drayblox(xNod, yNod, zNod, xo, yo, zo, numNode, xLen, yLen, zLen, ix, iy, iz, lens);
  
  free(FZNOD);
  free(zNod);
  free(RN);
  free(RN2);
  free(xNod);
  free(yNod);
  return segs;
}

// void split_rays(int part, int resolution_dim, int ix[], 
// 		int iy[], int iz[], int len, int ib[]) {
// 	int base = resolution_dim/part;
// 	for(int k = 0; k < len; k++) {
// 		int x_idx = ix[k];
// 		int y_idx = iy[k];
// 					
// 		int sec_idx = 0;
// 		
// 		for(int a = 1; a <= part; a++) {
// 			for(int b = 1; b <= part; b++) {
// 				++sec_idx;
// 				int xscale_a = (a-1)*base + 1;
// 				int xscale_b = a*base;
// 				int yscale_a = (b-1)*base + 1;
// 				int yscale_b = b*base;
// 				if(x_idx >= xscale_a && x_idx <= xscale_b && y_idx >= yscale_a && y_idx <= yscale_b) {
// 					ib[k] = sec_idx;
// 				}
// 			}
// 		}
// 		//idx[k] = (ix[k]-1)*resolution_dim*resolution_dim + (iy[k]-1)*resolution_dim + iz[k]
// 	}
// }
