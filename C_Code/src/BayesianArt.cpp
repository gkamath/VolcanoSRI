/*
 * perform Bayesian ART method on As=b
 * 
 */

#include <iostream>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
//#include "Status.h"
#include "LogUtil.h"
#define INF 9999.9999
#define RP_DE_RATE 1.0
using namespace std;

/*
 * dot product of two vectors
 * 
 */
double dotp(const vector<double> &v1, const vector<double> &v2) {
	if(v1.size() != v2.size()) {
		error(LOG_APPL, "BAYESIAN ART ERROR Non-conformable Arguments!");
		exit(1);
	}
	
	double product = 0.0;   
	int dim = v1.size();
	for(int i = 0; i < dim; i++) {
		product += v1[i]*v2[i];
	}
	
	return product;
}

/*
 * vector norm
 * 
 */
double fnorm(const vector<double> &v) {
	double product = 0.0;
	product = sqrt(dotp(v,v));
	
	return product;
}

/*
 * diff between two vectors
 * 
 */
void vec_subtract(const vector<double> &v1, const vector<double> &v2, vector<double> &diff) {
	if(v1.size() != v2.size()) {
		error(LOG_APPL, "BAYESIAN ART ERROR Non-conformable Arguments!");
		exit(1);
	}
	int dim = v1.size();
	
	for(int i = 0; i < dim; i++) {
		diff[i] = v1[i] - v2[i];
	}
}

/*
 * routines of Bayesian ART method
 * solving As=b
 * 
 * input: vector A, idx, b
 * output: vector s
 * 
 */
void bayesian_art(const vector< vector<double> > &A, const vector< vector<int> > &idx, const vector<double> &b, 
		vector<double> &s, int est_dim, double rho, double lambda, double tolerance, int max_round) {
	
	if(A.size() != idx.size() || A.size() != b.size()) {
		error(LOG_APPL, "BAYESIAN ART ERROR blox size does not match");
		exit(1);
	}
  
	int rows = A.size();
	vector<double> s0(est_dim, 0.0);
	vector<double> r(rows, 0.0);
	double update = INF;
        int bart_rounds = 0;
	
	while(bart_rounds < max_round){ //&& update > tolerance) {
		bart_rounds++;
		//info(LOG_APPL, "Bayesian ART: %d of %d", bart_rounds,max_round);
				
		s0.assign(s.begin(), s.end());
		for(int i = 0; i < rows; i++) {
			int m = A[i].size(); 
			double as = 0.0;
			
			for(int k = 0; k < m; k++) {
				as = as + A[i][k] * s[idx[i][k]];
			}
			
			double aa = dotp(A[i], A[i]);
			double num = b[i] - lambda*r[i] - as;
			double denom = lambda*lambda + aa;
			//info(LOG_APPL, "denom %f", denom);
			double gamma = (rho*num) / denom;
      
			//update
			r[i] = r[i] + gamma*lambda;
			for(int k = 0; k < m; k ++) {
				s[idx[i][k]] = s[idx[i][k]] + gamma*A[i][k];
				
			}
		}
		rho = rho / RP_DE_RATE;
    
	    vector<double> delta_s(est_dim, 0.0);
	    vec_subtract(s, s0, delta_s);
	    double delta_s_norm = fnorm(delta_s);
	    double s0_norm = fnorm(s0);
	    update = delta_s_norm / s0_norm;
	    
	  //info(LOG_APPL, "relative update: %.16f", update);
    
	}
}
