#include <iostream>
#include <stdexcept>
#include <fstream>

#include <lib/json.h>

#include "map.hpp"
#include "parse.hpp"

namespace cuauv {
namespace conf {

namespace s = std;

s::vector<object> parse_objects(const Json::Value& arr) {
  if (!arr.isArray()) throw s::runtime_error("cuauv::conf::load_map: Expected array of objects.");
  
  s::vector<object> objs;
  for (unsigned int i = 0; i < arr.size(); i++) {
    Json::Value root = arr[i];
    if (!root.isObject()) throw s::runtime_error("cuauv::conf::load_map: Expected object.");
    PARSE_STRING(name);
    PARSE_VECTOR4(initial_position);
    PARSE_MATRIX4(initial_covariance);
    object o { name, initial_position, initial_covariance };
    objs.push_back(o);
  };

  return objs;
};

/*
sub parse_submarine(const Json::Value& arr) {
  Json::Value root = arr;
  PARSE_VEC6(initial_pose);
  PARSE_MATRIX6(control_covariance);
  sub s { initial_pose, control_covariance };
  return s;
};
*/

map load_map(void) {
  char* dir = getenv("CUAUV_SOFTWARE");
  if (dir == nullptr) throw s::runtime_error("cuauv::conf::load_map: CUAUV_SOFTWARE must be set to the root of the software repository with a trailing slash.");

  char* locale = getenv("CUAUV_LOCALE");
  if (locale == nullptr) throw s::runtime_error("cuauv::conf::load_map: CUAUV_LOCALE must be defined corresponding to a config file in the conf directory.");

  s::string filename = s::string(dir) + "conf/" + s::string(locale) + ".conf";
  s::ifstream ifs(filename);
  if (!ifs) throw s::runtime_error("cuauv::conf::load_map: Locale file does not exist or cannot be opened.");
  
  Json::Value root;
  Json::Reader reader;
  if (!reader.parse(ifs, root)) throw s::runtime_error("cuauv::conf::load_map: Locale file cannot be parsed.");

  PARSE_OBJECTS(objects);
  PARSE_INTEGER(num_particles);

  map m {
    objects,
    num_particles
  };
 
  return m;
 
};

}
}
