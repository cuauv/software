#ifndef CUAUV_SIM_PID_H
#define CUAUV_SIM_PID_H

#include <Eigen/Geometry>

namespace cuauv {
namespace fishbowl {

class pid {
public:
    pid(double p, double i, double d, double r);

    /**
     * Steps the PID controller and returns a new output value.
     *
     * @param dt The time in seconds since this method was last called.
     * @param desired The desired value.
     * @param actual The actual value.
     */
    double step(double dt, double desired, double actual);

    double p;
    double i;
    double d;
    double r;

    double ei = 0; //!< Integrated error.
    double ep = 0; //!< Error in the previous timestep.
    bool epb = false; //!< Whether or not to use ep. Changed to true after first step.
};

/**
 * Euler angle PID controller.
 */
class epid {
public:
    epid(double p, double i, double d, double r);

    double step(double dt, double desired, double actual);

    double p;
    double i;
    double d;
    double r;

    double ei = 0; //!< Integrated error.
    double ep = 0; //!< Error in the previous timestep.
    bool epb = false;
};

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_PID_H
