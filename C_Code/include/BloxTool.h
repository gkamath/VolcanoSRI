#ifndef BLOX_TOOL_H
#define BLOX_TOOL_H

void order(double vec[], int idx[], int len);
bool identical(double A, double B, double Error);
bool different(double A, double B, double Error);

int getEXlen(double x_1, double x_2, double xo[], int xlen);
int getWHYlen(double y_1, double y_2, double yo[], int ylen);
void get2Drayblox(double x_1, double y_1, double x_2, double y_2, double xo[], double yo[], int xlen, int ylen, 
		  int EXlen, int WHYlen, int *indexx, int *indexy, double *lengs);
int get3Drayblox(double XNOD[], double YNOD[], double ZNOD[], 
		 double xo[], double yo[], double zo[], 
		 int nnod, int xlen, int ylen, int zlen, 
		 int indexx[], int indexy[], int indexz[], double lengs[]);

void swap(double coor[2]);
void vecSubScalar(double a[], double b, double c[], int n);
void scalarSubVec(double a, double b[], double c[], int n);
void scalarAddVec(double a, double b[], double c[], int n);
void vecMultiScalar(double a[], double b, double c[], int n);

int getRayblox(double xCoor[], double yCoor[], double zCoor[], 
	       double xo[], double yo[], double zo[], 
	       int numPt, int xLen, int yLen, int zLen, 
	       int ix[], int iy[], int iz[], double lens[]);
    
#endif