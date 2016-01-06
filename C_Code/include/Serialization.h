#ifndef SERIALIZATION_H
#define SERIALIZATION_H

#include <vector>
using namespace std;

int pack(const vector<double> &A_v, 
	 const vector<int> &idx_v, 
	 const double &b_d, 
	 char *buffer);

int unpack(vector<double> &A_v, 
	    vector<int> &idx_v, 
	    double &b_d, 
	    char *buffer);

void unpack_signal(vector<double> &station_loc, 
		   vector<double> &event_loc, 
		   double &t_time, int coors_dim, 
		   char *buffer);

int packing(const vector<double> &slow_v,
		    char *buffer, int solicit);

int unpacking(vector<double> &slow_v,
		    char *buffer);

#endif
