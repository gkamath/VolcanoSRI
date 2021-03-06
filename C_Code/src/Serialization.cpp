#include <vector>
#include <iostream>
#include <stdlib.h>
#include <string.h>
#include "LogUtil.h"
using namespace std;

/* 
 * ray data message serialization
 * | + + + + | + + .... | + + .... | + + + + |
 *   length    seg len    index      time res
 */

int packing(const vector<double> &slow_v, char *buffer, int solicit) {
  int add = 0;
  int blox = slow_v.size();
  double slow_element;

  memcpy(buffer+add, &blox, sizeof(blox));
  add  += sizeof(blox);
  memcpy(buffer+add, &solicit, sizeof(solicit));
  add += sizeof(solicit);

  for(int i = 0; i < blox; i++) {
	slow_element = slow_v[i];
    memcpy(buffer+add, &slow_element, sizeof(slow_element));
    add += sizeof(slow_element);
  }

  if(solicit == 1) {
    info(LOG_APPL, "Sending a solicit msg");
  } else {
    info(LOG_APPL, "Sending a unsolicit msg");
  }

  return add;
}

int unpacking(vector<double> &slow_v, char *buffer) {
  int add = 0;
  int blox, solicit;
  double slow_element;

  slow_v.clear();

  memcpy(&blox, buffer+add, sizeof(blox));
  add += sizeof(blox);
  memcpy(&solicit, buffer+add, sizeof(solicit));
  add += sizeof(solicit);

  for(int i = 0; i < blox; i++) {
    memcpy(&slow_element, buffer+add, sizeof(slow_element));
    slow_v.push_back(slow_element);
    add += sizeof(slow_element);
  }

  if(solicit == 1) {
    info(LOG_APPL, "Received a solicit msg");
  } else {
    info(LOG_APPL, "Received a unsolicit msg");
  }

  if (blox <= 0)
    return -1;
  return solicit;

// return blox;
}

int pack(const vector<double> &A_v, 
		   const vector<int> &idx_v, 
		   const double &b_d, 
		   char *buffer) {
  int add = 0;
  int blox = A_v.size();
  double A_element;
  int idx_element;
  double b_element;
  
  memcpy(buffer+add, &blox, sizeof(blox));
  add += sizeof(blox);
  
  for(int i = 0; i < blox; i++) {
    A_element = A_v[i];
    memcpy(buffer+add, &A_element, sizeof(A_element));
    add += sizeof(A_element);
  }
  
  for(int i = 0; i < blox; i++) {
    idx_element = idx_v[i];
    memcpy(buffer+add, &idx_element, sizeof(idx_element));
    add += sizeof(idx_element);
  }
  
  b_element = b_d;
  memcpy(buffer+add, &b_element, sizeof(b_element));
  add += sizeof(b_element);
  
  return add;
}

/* 
 * unpack ray data message
 */
int unpack(vector<double> &A_v, 
	    vector<int> &idx_v, 
	    double &b_d, 
	    char *buffer) {
  int add = 0;
  int blox;
  double A_element;
  int idx_element;
  double b_element;
  
  A_v.clear();
  idx_v.clear();
  b_d = 0.0;
  
  memcpy(&blox, buffer+add, sizeof(blox));
  add += sizeof(blox);
  
  for(int i = 0; i < blox; i++) {
    memcpy(&A_element, buffer+add, sizeof(A_element));
    A_v.push_back(A_element);
    add += sizeof(A_element);
  }
  
  for(int i = 0; i < blox; i++) {
    memcpy(&idx_element, buffer+add, sizeof(idx_element));
    idx_v.push_back(idx_element);
    add += sizeof(idx_element);
  }
  
  memcpy(&b_element, buffer+add, sizeof(b_element));
  b_d = b_element;

  return blox;
}

/*
 * indexed version of serialization with the ray index
 * for data verification of testing mesh network in core
 */

/* 
 * ray data message serialization
 * | + + + + | + + .... | + + .... | + + + + |
 *   length    seg len    index      time res
 */
int packi(const int &ray_idx, 
        const vector<double> &A_v, 
        const vector<int> &idx_v, 
        const double &b_d, 
        char *buffer) {
  
  int add = 0;
  double A_element;
  int idx_element;
  double b_element;
  
  int ray_index = ray_idx;
  memcpy(buffer+add, &ray_index, sizeof(ray_index));
  add += sizeof(ray_index);

  int blox = A_v.size();
  memcpy(buffer+add, &blox, sizeof(blox));
  add += sizeof(blox);
  
  for(int i = 0; i < blox; i++) {
    A_element = A_v[i];
    memcpy(buffer+add, &A_element, sizeof(A_element));
    add += sizeof(A_element);
  }
  
  for(int i = 0; i < blox; i++) {
    idx_element = idx_v[i];
    memcpy(buffer+add, &idx_element, sizeof(idx_element));
    add += sizeof(idx_element);
  }
  
  b_element = b_d;
  memcpy(buffer+add, &b_element, sizeof(b_element));
  add += sizeof(b_element);
  
  return add;
}

/* 
 * unpack ray data message
 */
void unpacki(int &ray_idx,
	    vector<double> &A_v, 
	    vector<int> &idx_v, 
	    double &b_d, 
	    char *buffer) {
  int add = 0;
  int blox;
  int ray_index = 0;
  double A_element;
  int idx_element;
  double b_element;
  
  A_v.clear();
  idx_v.clear();
  b_d = 0.0;
  
  memcpy(&ray_index, buffer+add, sizeof(ray_index));
  ray_idx = ray_index;
  add += sizeof(ray_index);
  
  memcpy(&blox, buffer+add, sizeof(blox));
  add += sizeof(blox);
  
  for(int i = 0; i < blox; i++) {
    memcpy(&A_element, buffer+add, sizeof(A_element));
    A_v.push_back(A_element);
    add += sizeof(A_element);
  }
  
  for(int i = 0; i < blox; i++) {
    memcpy(&idx_element, buffer+add, sizeof(idx_element));
    idx_v.push_back(idx_element);
    add += sizeof(idx_element);
  }
  
  memcpy(&b_element, buffer+add, sizeof(b_element));
  b_d = b_element;
}


/* 
 * unpack event data from sensing port
 */
void unpack_signal(vector<double> &station_loc, 
		   vector<double> &event_loc, 
		   double &t_time, int coors_dim, 
		   char *buffer) {
  int add = 0;
  double element;
  
  station_loc.clear();
  event_loc.clear();
  t_time = 0.0;
  
  for(int i = 0; i < coors_dim; i++) {
    memcpy(&element, buffer+add, sizeof(element));
    station_loc.push_back(element);
    add += sizeof(element);
  }
  
  for(int i = 0; i < coors_dim; i++) {
    memcpy(&element, buffer+add, sizeof(element));
    event_loc.push_back(element);
    add += sizeof(element);
  }
  
  memcpy(&element, buffer+add, sizeof(element));
  t_time = element;
}
