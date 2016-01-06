#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <utility>
#include <stdlib.h>
#include "LogUtil.h"

using namespace std;

void read_synt_data(char *ttfile, char *rayfile, 
		    vector< vector<double> > &A, 
		    vector< vector<int> > &idx, 
		    vector<double> &b, int dim)
{
  string s;
  int rays;
  
  ifstream ttin(ttfile);
  ttin >> s;
  rays = atoi(s.c_str());
  
  for(int i = 0; i < rays; i++) {
    ttin >> s;
    double d = atof(s.c_str());
    b.push_back(d);
  }
  ttin.close();
  
  ifstream rayin(rayfile);
  rayin >> s;
  rays = atoi(s.c_str());
  
  int blox;
  int index;
  vector<int> ix;
  vector<int> iy;
  vector<int> iz;
  vector<int> index_v;
  vector<double> A_v;
  
  for(int i = 0; i < rays; i++) {
    rayin >> s;
    blox = atoi(s.c_str());
    
    ix.clear();
    iy.clear();
    iz.clear();
    index_v.clear();
    A_v.clear();
    
    for(int i = 0; i < blox; i++) {
      rayin >> s;
      ix.push_back(atoi(s.c_str()));
    }
    
    for(int i = 0; i < blox; i++) {
      rayin >> s;
      iy.push_back(atoi(s.c_str()));
    }
    
    for(int i = 0; i < blox; i++) {
      rayin >> s;
      iz.push_back(atoi(s.c_str()));
    }
    
    for(int i = 0; i < blox; i++) {
      index = (ix[i]-1)*dim*dim + (iy[i]-1)*dim + iz[i] - 1;
      index_v.push_back(index);
    }
    idx.push_back(index_v);
    
    for(int i = 0; i < blox; i++) {
      rayin >> s;
      A_v.push_back(atof(s.c_str()));
    }
    A.push_back(A_v);
  }
  
  rayin.close();
  
}

int read_synt_data2(char *ttfile, char *rayfile, 
		    vector< vector<double> > &A, 
		    vector< vector<int> > &idx, 
		    vector<double> &b, int dim)
{
  string s;
  int rays = 0;
  
  ifstream ttin(ttfile);
  
  while(ttin >> s) {
    rays++;
    double d = atof(s.c_str());
    b.push_back(d);
  }
  ttin.close();
  
  cout << "rays: " << rays << endl;
  
  ifstream rayin(rayfile);
  
  int blox;
  int index;
  vector<int> ix;
  vector<int> iy;
  vector<int> iz;
  vector<int> index_v;
  vector<double> A_v;
  
  for(int i = 0; i < rays; i++) {
    rayin >> s;
    blox = atoi(s.c_str());
    
    ix.clear();
    iy.clear();
    iz.clear();
    index_v.clear();
    A_v.clear();
    
    for(int i = 0; i < blox; i++) {
      rayin >> s;
      ix.push_back(atoi(s.c_str()));
    }
    
    for(int i = 0; i < blox; i++) {
      rayin >> s;
      iy.push_back(atoi(s.c_str()));
    }
    
    for(int i = 0; i < blox; i++) {
      rayin >> s;
      iz.push_back(atoi(s.c_str()));
    }
    
    for(int i = 0; i < blox; i++) {
      index = (ix[i]-1)*dim*dim + (iy[i]-1)*dim + iz[i] - 1;
      index_v.push_back(index);
    }
    idx.push_back(index_v);
    
    for(int i = 0; i < blox; i++) {
      rayin >> s;
      A_v.push_back(atof(s.c_str()));
    }
    A.push_back(A_v);
  }
  
  rayin.close();
  
  return rays;
  
}

int read_synt_data_arrt(char *event_file, char *station_file, 
			char *tt_file, pair<int, int> event_scale, 
			pair<int, int> station_scale,
			vector< vector<double> > &event_location,
			vector< vector<double> > &station_location, 
			vector<double> &travel_times) {
  string s;
  int count = 0;
  vector<double> temp;
  vector<double> coor_xyz(3, 0.0);
  int station_num, event_num, begin_y, begin_z;
  
  ifstream event_in(event_file);
  while(event_in >> s) {
    count++;
    temp.push_back(atof(s.c_str()));
  }
  
  event_num = count/3;
  begin_y = event_num;
  begin_z = event_num*2;
  
  for(int i = event_scale.first-1; i < event_scale.second; i++) {
    coor_xyz[0] = temp[i];
    coor_xyz[1] = temp[i+begin_y];
    coor_xyz[2] = temp[i+begin_z];
    
    event_location.push_back(coor_xyz);
  }
  
//   printf("events: %d\n", count);
//   printf("last element: %lf\n", temp[count-1]);
  
  event_in.close();
  
  count = 0;
  temp.clear();
  ifstream station_in(station_file);
  while(station_in >> s) {
    count++;
    temp.push_back(atof(s.c_str()));
  }
  
  station_num = count/3;
  begin_y = station_num;
  begin_z = station_num*2;
  
  for(int i = station_scale.first-1; i < station_scale.second; i++) {
    coor_xyz[0] = temp[i];
    coor_xyz[1] = temp[i+begin_y];
    coor_xyz[2] = temp[i+begin_z];
    
//     printf("+++++++++++++++++++++++++++++++++++++ %d\n", i);
    
    station_location.push_back(coor_xyz);
  }
  
//   printf("stations: %d\n", count);
//   printf("last element: %lf\n", temp[count-1]);
  station_in.close();
  
  count = 0;
  temp.clear();
  ifstream tt_in(tt_file);
  tt_in >> s;
  int sub_event_num = (atoi(s.c_str())/station_num);
  
  if(sub_event_num != (event_scale.second-event_scale.first+1)) {
    error(LOG_APPL, "Data files error!\n");
    error(LOG_APPL, "sub_event_num: %d\n", sub_event_num);
    error(LOG_APPL, "station_num: %d\n", station_num);
    error(LOG_APPL, "total tt num: %d\n", atoi(s.c_str()));
  } else {
    info(LOG_APPL, "File load done!\n");
  }
  
  while(tt_in >> s) {
    count++;
    temp.push_back(atof(s.c_str()));
  }
  
  for(int i = 0; i < event_scale.second-event_scale.first+1; i++) {
    for(int j = i*station_num+station_scale.first-1; 
	j < i*station_num+station_scale.second; j++) {
      //printf("----------: %d, %lf\n", j, temp[j]);
      travel_times.push_back(temp[j]);
    }
  }
  
//   for(int i = event_scale.first-1; i < event_scale.second; i++) {
//     for(int j = i*station_num+station_scale.first-1; 
// 	j < i*station_num+station_scale.second; j++) {
//       //printf("----------: %d, %lf\n", j, temp[j]);
//       travel_times.push_back(temp[j]);
//     }
//   }
  
//   printf("tts: %d\n", count);
//   printf("last element: %lf\n", temp[99]);
  
  tt_in.close();
  
//   ifstream ttin(tt_file);
//   
//   while(ttin >> s) {
//     rays++;
//     double d = atof(s.c_str());
//     b.push_back(d);
//   }
//   ttin.close();
//   
//   cout << "rays: " << rays << endl;
//   
//   ifstream rayin(rayfile);
//   
//   int blox;
//   int index;
//   vector<int> ix;
//   vector<int> iy;
//   vector<int> iz;
//   vector<int> index_v;
//   vector<double> A_v;
//   
//   for(int i = 0; i < rays; i++) {
//     rayin >> s;
//     blox = atoi(s.c_str());
//     
//     ix.clear();
//     iy.clear();
//     iz.clear();
//     index_v.clear();
//     A_v.clear();
//     
//     for(int i = 0; i < blox; i++) {
//       rayin >> s;
//       ix.push_back(atoi(s.c_str()));
//     }
//     
//     for(int i = 0; i < blox; i++) {
//       rayin >> s;
//       iy.push_back(atoi(s.c_str()));
//     }
//     
//     for(int i = 0; i < blox; i++) {
//       rayin >> s;
//       iz.push_back(atoi(s.c_str()));
//     }
//     
//     for(int i = 0; i < blox; i++) {
//       index = (ix[i]-1)*dim*dim + (iy[i]-1)*dim + iz[i] - 1;
//       index_v.push_back(index);
//     }
//     idx.push_back(index_v);
//     
//     for(int i = 0; i < blox; i++) {
//       rayin >> s;
//       A_v.push_back(atof(s.c_str()));
//     }
//     A.push_back(A_v);
//   }
//   
//   rayin.close();
  
  return station_num;
  
}
