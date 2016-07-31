#include "model.h"

/* DECLARATIONS */

function<double(double)> plusTwo = [](double val) -> double { return (val); };
Var<double> x = Var<double>( {} );

/* EOF */

void test(double v) {
  apply(function<double(double)>([&](double val) { return builtin_gaussian(v, 0.1, val); }), x);
};

double est() {
  return calc(function<double(double)>([](double v){return v;}), ASLAM_CALC_REDUCE_ARGMAX, x);
};
