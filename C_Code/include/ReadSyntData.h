#ifndef READ_SYNTDATA_H
#define READ_SYNTDATA_H

#include <vector>
#include <stdlib.h>
using namespace std;

void read_synt_data(char *ttfile, char *rayfile, 
		    vector< vector<double> > &A, 
		    vector< vector<int> > &idx, 
		    vector<double> &b, int dim);

int read_synt_data2(char *ttfile, char *rayfile, 
		    vector< vector<double> > &A, 
		    vector< vector<int> > &idx, 
		    vector<double> &b, int dim);

int read_synt_data_arrt(char *event_file, char *station_file, 
			char *tt_file, pair<int, int> event_scale, 
			pair<int, int> station_scale,
			vector< vector<double> > &event_location,
			vector< vector<double> > &station_location, 
			vector<double> &travel_times);

#endif