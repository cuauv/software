#include "ekf.h"

template<int n, int m, int b> 
EKF<n,m,b>::EKF(const Eigen::Matrix<float, n, b> &B, const Eigen::Matrix<float, m, m> &R,
         const Eigen::Matrix<float, n, n> &Q, const Eigen::Matrix<float, n, 1> &x,
         const Eigen::Matrix<float, n, n> &p)
    : B_(B), R_(R), Q_(Q), xhat_(x), covs_(p) {} 

template<int n, int m, int b> 
void EKF<n,m,b>::Predict(const Eigen::Matrix<float, b, 1> &u,
                  const Eigen::Matrix<float, n, 1> &xhat,
                  const Eigen::Matrix<float, n, n> &F) {
    xhat_ = xhat + B_*u;
    covs_ = F*covs_*F.transpose() + Q_;
}

template<int n, int m, int b> 
void EKF<n,m,b>::Update(const Eigen::Matrix<float, m, 1> &y,
                 const Eigen::Matrix<float, m, n> &H) {
    Eigen::Matrix<float, m, m> S = H*covs_*H.transpose() + R_;
    Eigen::Matrix<float, n, m> K = covs_*H.transpose()*S.inverse();
    xhat_ = xhat_ + K*y;
    covs_ = (Eigen::Matrix<float, n, n>::Identity() - K*H)*covs_;
}

SlamEKF::SlamEKF(const vec3 &x, const mat3 &p, std::string id)
    : EKF<3,3,3>::EKF(mat3::Zero(), mat3::Zero(), mat3::Zero(), x, p), id_(id) {
        R_ = mat3::Identity() * .3;
        Q_ = mat3::Identity() * .2;
}

SlamEKF::SlamEKF(const SlamEKF &orig) 
    : SlamEKF(orig.xhat_, orig.covs_, orig.id_) {}

void SlamEKF::Predict(const vec3 &u) {
    EKF<3,3,3>::Predict(u, xhat_, mat3::Identity());
}

void SlamEKF::Update(const vec3 &z) {
    EKF<3,3,3>::Update(z - xhat_, mat3::Identity());
}
    
