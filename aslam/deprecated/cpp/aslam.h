#pragma once

#include <unordered_map>
#include <string>
#include <memory>
#include <functional>

#include "math.h"
#include "proto/rpc.pb.h"

// #include "auvlog/client.h"

typedef int ASLAM_CALC_REDUCE_TYPE;

#define ASLAM_CALC_REDUCE_ARGMAX 0
#define ASLAM_CALC_REDUCE_WTMEAN 1

/* std::function is necessary for type erasure (and using e.g. capture lambdas); call overhead is supposedly pretty low. */

template<typename ret, typename ... args>
void apply(function<ret(args...)> func, Var<args>...);

template<typename ret, typename ... args>
ret calc(function<ret(args...)> func, ASLAM_CALC_REDUCE_TYPE reduceType, Var<args>...);

Res handle (Req request);
