#include <iostream>
#include <map>
#include <memory>
#include <string>

#include "math.cpp"
#include "pmap.cpp"


#define GRANULARITY 200
#define XMIN 0
#define XMAX 40
#define YMIN 0
#define YMAX 40
#define ZMIN 0
#define ZMAX 10

/*
typedef std::map<std::string, ProbabilityMap<GRANULARITY>*> state;

state objects;

void register_object(std::string name) {
  auto xr = std::pair<double, double>(XMIN, XMAX);
  auto yr = std::pair<double, double>(YMIN, YMAX);
  auto zr = std::pair<double, double>(ZMIN, ZMAX);
  auto a = std::make_tuple(xr, yr, zr);
  objects[name] = new ProbabilityMap<GRANULARITY>(a);
}

int is_registered(std::string name) { return objects.count(name) == 0 ? 0 : 1; }

void cleanup() { 
  for (state::iterator it = objects.begin(); it != objects.end(); it++) {
    delete it->second;
  }
}
*/

int main() {
  auto x = new ProbabilityMap<500>(std::make_tuple(std::pair<double, double>(XMIN, XMAX), std::pair<double, double>(YMIN, YMAX), std::pair<double, double>(ZMIN, ZMAX)));
  x->update([](double x, double y, double z){ return x * y * z; });
}
