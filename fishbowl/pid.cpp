#include <math.h>
#include <iostream>

#include "pid.hpp"

namespace cuauv {
namespace fishbowl {

pid::pid(double p, double i, double d, double r)
    : p(p)
    , i(i)
    , d(d)
    , r(r)
{
}

double pid::step(double dt, double desired, double actual)
{
    double out = 0;
    double e = desired - actual;

    if (fabs(e) > r) {
        ei = 0;
    } else {
        ei += e * dt;
    }

    out += p * e;
    out += i * ei;

    if (epb) {
        out += d * (e - ep) / dt;
    } else {
        epb = true;
    }

    ep = e;

    return out;
}

epid::epid(double p, double i, double d, double r)
    : p(p)
    , i(i)
    , d(d)
    , r(r)
{
}

double epid::step(double dt, double desired, double actual)
{
    double out = 0;
    double e = fmod(desired, 360) - fmod(actual, 360);
    // Think of two vectors -- a desired vector, and an actual vector, with
    // their angles from the origin being the angles here.
    // If the difference [desired - actual] is greater than 180 degrees, then
    // the smallest change in angle you could make to the actual vector to
    // achieve the desired vector is in fact to go in the opposite direction
    // of the difference.
    // For example, supposing + is counterclockwise,
    //     D
    //     |
    //     + ) this space on the right is [desired - actual]
    //    /
    //   A
    // For the other case, consider switching the two vectors.
    if (e > 180) {
        e -= 360;
    } else if (e < -180) {
        e += 360;
    }

    if (fabs(e) > r) {
        ei = 0;
    } else {
        ei += e * dt;
    }

    out += p * e;
    out += i * ei;

    if (epb) {
        out += d * (e - ep) / dt;
    } else {
        epb = true;
    }

    ep = e;

    return out;
}

} // namespace fishbowl
} // namespace cuauv
