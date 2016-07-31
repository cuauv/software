/*
  Vector Operations
*/

#include <vector>
#include <cmath>

static inline double gaussian (double mu, double sigma, double x) {
  return exp(-pow(x - mu, 2) / (2*pow(sigma, 2)));
}

/*

template<typename T, unsigned int ND, unsigned int DS>
class ProbabilityMap { 
  public:
    ProbabilityMap(T** ranges);
    void update(T(*f)(T*));
    void exponentiate(T p);
    void normalize();
    T  read(int* l);
    T* centroid();
    T* error();

    T* data;
    T** ranges;
};

template<typename T, unsigned int ND, unsigned int DS>
ProbabilityMap<T, ND, DS>::ProbabilityMap(T** _ranges) {
  data = new T[static_cast<int>(pow(DS, ND))];
  for (int i = 0; i < pow(DS, ND); i++) data[i] = 1;
  ranges = _ranges;
}

template<typename T, unsigned int ND, unsigned int DS>
void ProbabilityMap<T, ND, DS>::update(T(*f)(T*)) {
  T* c = new T[ND];
  for (int i = 0; i < ND; i++) c[i] = 0;
  for (int i = 0; i < pow(DS, ND); i++) {
    data[i] *= f(c);
    for (int d = ND - 1; d >= 0; d--) {
      if (c[d] < DS - 1) { 
        c[d] += 1; 
        for (int f = d + 1; f < ND; f++) c[f] = 0;
        break; 
      }
    }
  }
}

template<typename T, unsigned int ND, unsigned int DS>
void ProbabilityMap<T, ND, DS>::exponentiate(T p) {
  for (int i = 0; i < pow(DS, ND); i++)
    data[i] = pow(data[i], p);
}

template<typename T, unsigned int ND, unsigned int DS>
void ProbabilityMap<T, ND, DS>::normalize() {
  T s = 0;
  for (int i = 0; i < pow(DS, ND); i++) s += data[i];
  for (int i = 0; i < pow(DS, ND); i++) data[i] /= s;
}

template<typename T, unsigned int ND, unsigned int DS>
T ProbabilityMap<T, ND, DS>::read(int* l) {
  int i = 0;
  for (int d = ND - 1; d >= 0; d--) {
    i += l[d] * pow(DS, ND - d - 1);
  }
  return data[i];
}

// Precondition: normalized.
template<typename T, unsigned int ND, unsigned int DS>
T* ProbabilityMap<T, ND, DS>::centroid() {
  T* c = new T[ND];
  for (int i = 0; i < ND; i++) c[i] = 0;
  T* x = new T[ND];
  for (int i = 0; i < ND; i++) x[i] = 0;
  for (int i = 0; i < pow(DS, ND); i++) {
    for (int j = 0; j < ND; j++) { x[j] += c[j] * data[i]; }
    for (int d = ND - 1; d >= 0; d--) {
      if (c[d] < DS - 1) {
        c[d] += 1;
        for (int f = d + 1; f < ND; f++) c[f] = 0;
        break;
      }
    }
  }
  return x;
}

*/
