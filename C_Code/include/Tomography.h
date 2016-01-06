#ifndef TOMOPARTITION_H
#define TOMOPARTITION_H

#include <vector>
#include <map>
#include <string>
#include <iostream>
#include <fstream>
#include <pthread.h>
#include <ctime>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <ctime>
#include <sstream>
#include <vector>
#include <iostream>
#include <iterator>
#include <pthread.h>
#include <networkutils.h>
#include "SMPI.h"
#include "TomoParams.h"
#include "readdatafrommatlab.h"

#define SPACE_CHAR " "
#define STAR_CHAR "*"
#define UNDER_SCORE "_"
#define STATUS_FILE "status.ui"
#define STATUS_FILE_INI "status_init.ui"


using namespace std;

extern vector< vector<double> > A_l;
extern vector< vector<int> > idx_l;
extern vector<double> b_l;

extern double rho;
extern double lambda;
extern double tolerance;
extern int rounds;
extern int rays;
extern int vector_len;
extern int node_id;

extern vector< vector<double> > xFinal;


#endif