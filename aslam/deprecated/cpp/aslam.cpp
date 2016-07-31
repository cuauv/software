#include "aslam.h"

unordered_map<string, void*> env;

template<typename ret, typename arg>
void applyH(function<ret(arg)> func, Var<arg> var, arg val) {
  var.chain(func);
};

template<typename ret, typename arg, typename ... args>
void applyH(function<ret(arg, args...)> func, Var<arg> var, Var<args>... vars, arg val, args... vals) {
  var.chain([&](arg v){ return func(v, vals...); });
  applyH([&](args... nargs){ return func(val, nargs...); }, vars..., vals...);
};

template<typename ret, typename ... args>
void apply(function<ret(args...)> func, Var<args>... vars) {
  applyH(func, vars..., vars.argmax().first...);
};

template<typename ret, typename ... args>
ret calc(function<ret(args...)> func, ASLAM_CALC_REDUCE_TYPE reduceType, Var<args>... vars) {
  switch(reduceType) {
    case ASLAM_CALC_REDUCE_ARGMAX:
      return func(vars.argmax().first...);
    case ASLAM_CALC_REDUCE_WTMEAN:
      return func(vars.weightedMean()...);
  };
};

Res handle (Req request) {
  Res response;

  switch (request.type()) {
    case REQ_OBSERVE:
      if (request.func() == "heading") {
        // TODO
      } else if (request.func() == "distance") {
        // TODO
      };
      break;
    case REQ_MOVE:
      break;
    case REQ_EVAL:
      if (request.func() == "position") {
        // TODO
      };
  };

  return response;
};

/*

void applyIdentity(double x, Var<double> y) {
  function<double(double)> l = [&](double z) { return gaussian(z, 0.1, x); };
  apply(l, y);
};

double calcT(Var<double> y) {
  function<double(double)> l = [&](double z) { return z; };
  return calc(l, ASLAM_CALC_REDUCE_ARGMAX, y);
};

*/
