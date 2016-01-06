#ifndef BAYESIANART_H
#define BAYESIANART_H
#include <vector>
using namespace std;

double dotp(const vector<double> &v1, const vector<double> &v2);
double fnorm(const vector<double> &v);
void vec_subtract(const vector<double> &v1, const vector<double> &v2, vector<double> &diff);
void bayesian_art(const vector< vector<double> > &A, const vector< vector<int> > &idx, const vector<double> &b, vector<double> &s, 
		 int est_dim, double rho, double lambda, double tolerance, int max_round);

#endif
