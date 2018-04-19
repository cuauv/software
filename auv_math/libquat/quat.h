#ifndef QUAT_H
#define QUAT_H

#ifdef __cplusplus
extern "C" {
#endif

typedef double * quat;
typedef double * vectord;
typedef double * matrixd;

void quat_quat_mult(quat a, quat b, quat ret);
void quat_vector_mult(quat a, vectord b, vectord ret);

// We use the Z Y X (heading then pitch then roll) intrinsic Euler angle order.
// Heading, pitch, and roll are in degrees.
void hpr_to_quat(double heading, double pitch, double roll, quat ret);

void quat_to_matrix(quat q, matrixd ret);

#ifdef __cplusplus
}
#endif

#endif
