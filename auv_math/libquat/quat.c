#include "quat.h"

#include <math.h>

double tmp[3];

void quat_quat_mult(quat a, quat b, quat ret) {
    ret[0] = a[0]*b[0] - a[1]*b[1] - a[2]*b[2] - a[3]*b[3];
    ret[1] = a[0]*b[1] + a[1]*b[0] + a[2]*b[3] - a[3]*b[2];
    ret[2] = a[0]*b[2] + a[2]*b[0] + a[3]*b[1] - a[1]*b[3];
    ret[3] = a[0]*b[3] + a[3]*b[0] + a[1]*b[2] - a[2]*b[1];
}

/* Stefan Reinalter, molecularmusings.wordpress */
void quat_vector_mult(quat a, vectord b, vectord ret) {
    tmp[0] = 2*(a[2]*b[2] - a[3]*b[1]);
    tmp[1] = 2*(a[3]*b[0] - a[1]*b[2]);
    tmp[2] = 2*(a[1]*b[1] - a[2]*b[0]);

    ret[0] = a[2]*tmp[2] - a[3]*tmp[1] + tmp[0]*a[0] + b[0];
    ret[1] = a[3]*tmp[0] - a[1]*tmp[2] + tmp[1]*a[0] + b[1];
    ret[2] = a[1]*tmp[1] - a[2]*tmp[0] + tmp[2]*a[0] + b[2];
}

#define radians(deg) ((M_PI * (deg)) / 180)

void hpr_to_quat(double heading, double pitch, double roll, quat ret) {
    heading = radians(heading) / 2.0;
    pitch = radians(pitch) / 2.0;
    roll = radians(roll) / 2.0;
    ret[0] = cos(roll)*cos(pitch)*cos(heading) +
             sin(roll)*sin(pitch)*sin(heading);
    ret[1] = sin(roll)*cos(pitch)*cos(heading) -
             cos(roll)*sin(pitch)*sin(heading);
    ret[2] = cos(roll)*sin(pitch)*cos(heading) +
             sin(roll)*cos(pitch)*sin(heading);
    ret[3] = cos(roll)*cos(pitch)*sin(heading) -
             sin(roll)*sin(pitch)*cos(heading);
}

void quat_to_matrix(quat q, matrixd ret) {
    ret[0] = 1 - 2*q[2]*q[2] - 2*q[3]*q[3];
    ret[1] = 2*q[1]*q[2] - 2*q[3]*q[0];
    ret[2] = 2*q[1]*q[3] + 2*q[2]*q[0];
    ret[3] = 2*q[1]*q[2] + 2*q[3]*q[0];
    ret[4] = 1 - 2*q[1]*q[1] - 2*q[3]*q[3];
    ret[5] = 2*q[2]*q[3] - 2*q[1]*q[0];
    ret[6] = 2*q[1]*q[3] - 2*q[2]*q[0];
    ret[7] = 2*q[2]*q[3] + 2*q[1]*q[0];
    ret[8] = 1 - 2*q[1]*q[1] - 2*q[2]*q[2];
}
