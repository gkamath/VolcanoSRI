/*
 * readdatafrommatlab.h
 *
 *  Created on: Jan 23, 2014
 *      Author: sensorweb
 */

#ifndef READDATAFROMMATLAB_H_
#define READDATAFROMMATLAB_H_

#include <vector>
#include <string>

using namespace std;
void read_A_idx(char *ttfile, char *rayfile,vector< vector<double> > &A,vector< vector<int> > &idx,vector<double> &b, int dim);
void read_top_config_gout(char *top_config_file, int &dim,double &rho,double &lambda,double &tolerance,int &rounds);
void read_slowness_model_gout(char *slowness_file, vector<double> &slowness);
void write_slowness_gout(string slowness_output_file, const vector<double> &slowness_model, int res_dim);
void read_gossip_config(char *top_config_file, double &waittime, int &max_sleep_time,int &max_rounds, int &specified_rounds);
void read_nodeid(char* filename,int &node_id);
//void stonetchar(string ip_string, char ip_add[]);
void write_slowness_final(string slowness_output_file, const vector< vector<double> > &slowness_model);
//string itoas(int i);
//void read_A_idx(char*, char*, std::vector<std::vector<double, std::allocator<double> >, std::allocator<std::vector<double, std::allocator<double> > > >&, std::vector<std::vector<int, std::allocator<int> >, std::allocator<std::vector<int, std::allocator<int> > > >&, std::vector<double, std::allocator<double> >&, int);
#endif /* READDATAFROMMATLAB_H_ */
