#pragma once

#include <stdint.h>
#include <string>
#include <vector>

#define ATTR_ID_X 0
#define ATTR_ID_V 1
#define ATTR_ID_A 2
#define ATTR_ID_Q 3
#define ATTR_ID_W 4

namespace fishbowl_comm {

int connect();
void disconnect();
std::vector<uint32_t> get_all_entities();
int get_position(uint32_t entity, double &x, double &y, double &z);
int get_orientation(uint32_t entity, double &q0, double &q1, double &q2, double &q3);
int get_xattr(uint32_t entity, std::string key, std::string &data);

}
