#ifndef GOSSIPSOCLICITSEND_H
#define GOSSIPSOCLICITSEND_H

#include "SMPI.h"

void *gossip_solicit_send(void *arg);
void handle_solicit_receive(params_t *gossip_params, char* neighbor_ip);
int get_random_neighbor(char* neighbor_ip);
int gossip_to_neighbor(std::vector<double> slow_self, char *destination_ip, int solicit);

#endif