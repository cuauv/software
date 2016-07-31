#include "math.h"

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
VarImpl<Type, scale, reduce, unit, zero>::VarImpl(vector<pair<Type, double>> initial) {
  data = initial;
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
void VarImpl<Type, scale, reduce, unit, zero>::predicate(bool(*f)(Type)) {
  for (int i = 0; i < this->data.length(); i++) data[i].second *= f(data[i].first) ? 1.0 : 0.0;
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
void VarImpl<Type, scale, reduce, unit, zero>::chain(double(*f)(Type)) {
  for (int i = 0; i < this->data.length(); i++) data[i].second *= f(data[i].first);
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
void VarImpl<Type, scale, reduce, unit, zero>::normalize() {
  double sum = 0.0;
  for (int i = 0; i < this->data.length(); i++) sum += data[i].second;
  for (int i = 0; i < this->data.length(); i++) data[i].second /= sum;
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
void VarImpl<Type, scale, reduce, unit, zero>::move(Type delta) {
  for (int i = 0; i < this->data.length(); i++) data[i].first = reduce(data[i].first, delta); 
};


template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
void VarImpl<Type, scale, reduce, unit, zero>::spread(double uncertainty) {
  // TODO
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
void VarImpl<Type, scale, reduce, unit, zero>::resample(ASLAM_RESAMPLE_TYPE type) {
  // TODO
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
pair<Type, double> VarImpl<Type, scale, reduce, unit, zero>::argmax() {
  pair<Type, double> argmax = make_pair(zero, 0.0);
  for (int i = 0; i < this->data.length(); i++)
    if (data[i].second > argmax.second)
      argmax = data[i];
  return argmax;
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
double VarImpl<Type, scale, reduce, unit, zero>::predicateConfidence(bool(*f)(Type)) {
  double sum = 0.0;
  for (int i = 0; i < this->data.length(); i++) if (f(data[i].first)) sum += data[i].second;
  return sum;
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
double VarImpl<Type, scale, reduce, unit, zero>::valueConfidence(Type) {
  // TODO
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
Type VarImpl<Type, scale, reduce, unit, zero>::weightedMean() {
  Type mean = zero;
  for (int i = 0; i < this->data.length(); i++)
    mean = reduce(mean, scale(this->data[i].first, this->data[i].second));
  return mean;
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
vector<pair<Type, double>> VarImpl<Type, scale, reduce, unit, zero>::sample(int n) {
  // TODO
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
void VarImpl<Type, scale, reduce, unit, zero>::lock() {
  lck.lock();
};

template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
void VarImpl<Type, scale, reduce, unit, zero>::unlock() {
  lck.unlock();
};

/* Instantiations for Common Types */

REAL r_scale(REAL& a, REAL& b) { return a * b; }
REAL r_reduce(REAL& a, REAL& b) { return a + b; };
REAL r_unit = 1.0;
REAL r_zero = 0.0;

typedef VarImpl<REAL, r_scale, r_reduce, r_unit, r_zero> VAR_REAL;

VEC2 v2_scale(VEC2& a, REAL& b) { return make_pair(a.first * b, a.second * b); }
VEC2 v2_reduce(VEC2& a, VEC2& b) { return make_pair(a.first + b.first, a.second + b.second); }
VEC2 v2_unit = make_pair(1.0, 1.0);
VEC2 v2_zero = make_pair(0.0, 0.0);

typedef VarImpl<VEC2, v2_scale, v2_reduce, v2_unit, v2_zero> VAR_VEC2;

VEC3 v3_scale(VEC3& a, REAL& b) { return make_tuple(get<0>(a) * b, get<1>(a) * b, get<2>(a) * b); }
VEC3 v3_reduce(VEC3& a, VEC3& b) { return make_tuple(get<0>(a) + get<0>(b), get<1>(a) + get<1>(b), get<2>(a) + get<2>(b)); }
VEC3 v3_unit = make_tuple(1.0, 1.0, 1.0);
VEC3 v3_zero = make_tuple(0.0, 0.0, 0.0);

typedef VarImpl<VEC3, v3_scale, v3_reduce, v3_unit, v3_zero> VAR_VEC3;

/* Aux */

static inline REAL builtin_gaussian(REAL mu, REAL sigma, REAL x) {
  return exp(-pow(x - mu, 2) / (2*pow(sigma, 2)));
};

static inline REAL time() {
  timeval tv; 
  gettimeofday(&tv, NULL);
  return tv.tv_sec + tv.tv_usec / 1000000.;
};

static inline REAL error_floor(REAL error, REAL floor) {
  return error < floor ? floor : error;
};
