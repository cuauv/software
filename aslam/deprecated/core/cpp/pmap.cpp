/*
  Probability Maps
*/

#include <vector>
#include <cmath>
#include <tuple>

template<unsigned int DS>
class ProbabilityMap {
  public:
    ProbabilityMap(std::tuple<std::pair<double, double>, std::pair<double, double>, std::pair<double, double>> ranges);
    void update(double(*f)(double, double, double));
    void exponentiate(double exp);
    void normalize();
    std::tuple<double, double, double> centroid();
    std::tuple<double, double, double> error();
    
    double data [static_cast<int>(pow(DS, 3))];
    std::tuple<double, double, double> scales;
    std::tuple<double, double, double> offsets;
};

template<unsigned int DS>
ProbabilityMap<DS>::ProbabilityMap(std::tuple<std::pair<double, double>, std::pair<double, double>, std::pair<double, double>> ranges) {
  for (int i = 0; i < static_cast<int>(pow(DS, 3)); i++) { data[i] = 1; }
  scales = std::make_tuple( 
    abs(std::get<0>(ranges).second - std::get<0>(ranges).first) / static_cast<double>(DS),
    abs(std::get<1>(ranges).second - std::get<1>(ranges).first) / static_cast<double>(DS),
    abs(std::get<2>(ranges).second - std::get<2>(ranges).first) / static_cast<double>(DS)
  );
  offsets = std::make_tuple(
    std::get<0>(ranges).first + 0.5 * std::get<0>(scales),
    std::get<1>(ranges).first + 0.5 * std::get<1>(scales),
    std::get<2>(ranges).first + 0.5 * std::get<2>(scales)
  );
}

template<unsigned int DS>
void ProbabilityMap<DS>::update(double(*f)(double, double, double)) {
  int i = 0;
  for (int _1 = 0; _1 < DS; _1++) {
    for (int _2 = 0; _2 < DS; _2++) {
      for (int _3 = 0; _3 < DS; _3++) { 
        data[i] *= f( 
          (_1 * std::get<0>(scales)) + std::get<0>(offsets),
          (_2 * std::get<1>(scales)) + std::get<1>(offsets),
          (_3 * std::get<2>(scales)) + std::get<2>(offsets)
        );
        i += 1;
      }
    }
  }
}

template<unsigned int DS>
void ProbabilityMap<DS>::exponentiate(double exp) {
  for (int i = 0; i < static_cast<int>(pow(DS, 3)); i++) data[i] = pow(data[i], exp);
}

template<unsigned int DS>
void ProbabilityMap<DS>::normalize() {
  double s = 0;
  for (int i = 0; i < static_cast<int>(pow(DS, 3)); i++) s += data[i];
  for (int i = 0; i < static_cast<int>(pow(DS, 3)); i++) data[i] /= s;
}

template<unsigned int DS>
std::tuple<double, double, double> ProbabilityMap<DS>::centroid() {
  double* c = new double[3];
  int i = 0;
  for (int _1 = 0; _1 < DS; _1++) {
    for (int _2 = 0; _2 < DS; _2++) {
      for (int _3 = 0; _3 < DS; _3++) { 
        c[0] += data[i] * _1;
        c[1] += data[i] * _2;
        c[2] += data[i] * _3;
        i += 1;
      }
    }
  }
  c[0] = (c[0] * std::get<0>(scales)) + std::get<0>(offsets);
  c[1] = (c[1] * std::get<1>(scales)) + std::get<1>(offsets);
  c[2] = (c[2] * std::get<2>(scales)) + std::get<2>(offsets);
  return std::make_tuple(c[0], c[1], c[2]);
}

template<unsigned int DS>
std::tuple<double, double, double> ProbabilityMap<DS>::error() {
  double* c = new double[3];
  int i = 0;
  for (int _1 = 0; _1 < DS; _1++) {
    for (int _2 = 0; _2 < DS; _2++) {
      for (int _3 = 0; _3 < DS; _3++) { 
        c[0] += data[i] * _1;
        c[1] += data[i] * _2;
        c[2] += data[i] * _3;
        i += 1;
      }
    }
  }
  double* e = new double[3];
  i = 0;
  for (int _1 = 0; _1 < DS; _1++) {
    for (int _2 = 0; _2 < DS; _2++) {
      for (int _3 = 0; _3 < DS; _3++) { 
        e[0] += data[i] * pow(_1 - c[0], 2);
        e[1] += data[i] * pow(_2 - c[1], 2);
        e[2] += data[i] * pow(_3 - c[2], 2);
        i += 1;
      }
    }
  }
  e[0] = pow(e[0], 0.5);
  e[1] = pow(e[1], 0.5);
  e[2] = pow(e[2], 0.5);
  e[0] = (e[0] * std::get<0>(scales));
  e[1] = (e[1] * std::get<1>(scales));
  e[2] = (e[2] * std::get<2>(scales));
  return std::make_tuple(e[0], e[1], e[2]);
}
