#pragma once

#include <vector>
#include <tuple>
#include <cmath>
#include <mutex>
#include <functional>

#include <sys/time.h>

using namespace std;

/* Constants */

typedef int ASLAM_RESAMPLE_TYPE;

#define ASLAM_RESAMPLE_TRANSFORM 0

template<typename Type>
class Var {
  public:
    virtual void predicate(function<bool(Type)> func);
    virtual void chain(function<double(Type)> func);
    virtual void normalize();
    virtual void move(Type delta);
    virtual void spread(double uncertainty);
  
    virtual void resample(ASLAM_RESAMPLE_TYPE type);
    
    virtual pair<Type, double> argmax();
    virtual double predicateConfidence(function<bool(Type)> func);
    /* Should be polymorphic on double. */
    virtual double expected(function<double(Type)> func);
    virtual double correlation(function<double(Type)> func);
    virtual double valueConfidence(Type);
    virtual Type weightedMean();
    virtual vector<pair<Type, double>> sample(int n);

    virtual void lock();
    virtual void unlock();
};

/* Note use of strictly compile-time determinable template parameters to ensure inlining. */
template<typename Type, Type(*scale)(Type &, double &), Type(*reduce)(Type &, Type &), Type& unit, Type& zero>
class VarImpl : public Var<Type> {
  public:
    VarImpl(vector<pair<Type, double>> initial);

    void predicate(bool(*f)(Type));
    void chain(double(*f)(Type));
    void normalize();
    void move(Type delta);
    void spread(double uncertainty);
  
    void resample(ASLAM_RESAMPLE_TYPE type);
    
    pair<Type, double> argmax();
    double predicateConfidence(bool(*f)(Type));
    double expected(double(*f)(Type));
    double correlation(double(*f)(Type));
    double valueConfidence(Type);
    Type weightedMean();
    vector<pair<Type, double>> sample(int n);

    void lock();
    void unlock();

  private:
    vector<pair<Type, double>> data;
    pair<Type, double> cachedArgmax;
    int updateCount;
    mutex lck;
};

/* Common Types */

#define REAL double
#define VEC2 pair<double, double>
#define VEC3 tuple<double, double, double>

// Register constant constraints: buoy1.x + buoy2.x = 5.
// + Variable constraints: buoy1.x + buoy2.x = 5 +/- 2.

/* 

  About Time

  1. Repeated observations should not cause increased certainty.
  1.5 Somehow exponentiate by priors.
  2. P(A | B) = P(B | A) P(A) / P(B)
      => P(A | B) = posterior
      => P(A) = grid point
      => P(B | A) = update func

*/

/* Auxiliary */

static inline REAL time();
static inline REAL error_floor(REAL error, REAL floor);

/* Builtins */

template<typename T>
static inline T builtin_add (T x, T y);

template<typename T>
static inline T builtin_sub (T x, T y);

template<typename T>
static inline T builtin_div (T x, T y);

template<typename T>
static inline T builtin_mul (T x, T y);

template<typename T>
static inline T builtin_mod (T x, T y);

template<typename T>
static inline T builtin_pow (T x, T y);

template<typename T>
static inline T builtin_neg (T x);

template<typename T>
static inline T builtin_abs (T x, T y);

template<typename T>
static inline bool builtin_gt(T x, T y);

template<typename T>
static inline bool builtin_get(T x, T y);

template<typename T>
static inline bool builtin_eq(T x, T y);

template<typename T>
static inline bool builtin_ne(T x, T y);

template<typename T>
static inline bool builtin_le(T x, T y);

template<typename T>
static inline bool builtin_lt(T x, T y);

template<typename T>
static inline T builtin_sin(T x);

template<typename T>
static inline T builtin_cos(T x);

template<typename T>
static inline T builtin_tan(T x);

template<typename T>
static inline T builtin_asin(T x);

template<typename T>
static inline T builtin_acos(T x);

template<typename T>
static inline T builtin_atan(T x);

template<typename T>
static inline T builtin_atan2(T x, T y);

template<typename T>
static inline T builtin_gaussian(T x, T y, T z);
