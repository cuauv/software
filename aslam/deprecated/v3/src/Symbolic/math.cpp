/* ASLAM C++ */

#include <vector>
#include <cmath>

static inline double gaussian (double mu, double sigma, double x) {
  return exp(-pow(x - mu, 2) / (2*pow(sigma, 2)));
}

class PMap {
  public:
    PMap(int size);
    void exponentiate(double p);
    void normalize();
    int argmax();
    
    double* data;
    int size;
};

PMap::PMap(int size) { 
  this->size = size; 
  this->data = new double[size]; 
  for (int i = 0; i < size; i++) this->data[i] = 1.0;
  this->normalize();
}

void PMap::exponentiate(double p) {
  for (int i = 0; i < size; i++)
    data[i] = pow(data[i], p);
}

void PMap::normalize() {
  double s = 0;
  for (int i = 0; i < size; i++) s += data[i];
  for (int i = 0; i < size; i++) data[i] /= s;
}

int PMap::argmax() {
  int ind = 0;
  double max = 0;
  for (int i = 0; i < size; i++) {
    if (data[i] > max) {
      max = data[i];
      ind = i;
    }
  }
  return ind;
}
