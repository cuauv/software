#ifndef SLAM_EKF_H
#define SLAM_EKF_H

#include "util.h"
#include <string>

template<int n, int m, int b>
class EKF {
    public:
        Eigen::Matrix<float, n, 1> xhat_;
        Eigen::Matrix<float, n, n> covs_;
    protected:
        Eigen::Matrix<float, n, b> B_;
        Eigen::Matrix<float, m, m> R_;
        Eigen::Matrix<float, n, n> Q_;

    public:
        EKF(const Eigen::Matrix<float, n, b> &B, const Eigen::Matrix<float, m, m> &R, 
            const Eigen::Matrix<float, n, n> &Q, const Eigen::Matrix<float, n, 1> &x, 
            const Eigen::Matrix<float, n, n> &p);

        void Predict(const Eigen::Matrix<float, b, 1> &u,
                     const Eigen::Matrix<float, n, 1> &xhat,
                     const Eigen::Matrix<float, n, n> &F);
        void Update(const Eigen::Matrix<float, m, 1> &y,
                    const Eigen::Matrix<float, m, n> &H);
};

class SlamEKF: public EKF<3, 3, 3> {
    public:
        std::string id_;

        SlamEKF(const vec3 &x, const mat3 &p, std::string id);
        SlamEKF(const SlamEKF &orig);

        void Predict(const vec3 &u);
        void Update(const vec3 &z);
};

#endif
