#include <fstream>
#include <iostream>
#include <memory>

#include <Eigen/Core>
#include <Eigen/Geometry>
#include <Eigen/Dense>

int main() {
    double a11, a12, a13, a21, a22, a23, a31, a32, a33;
    Eigen::Matrix3d Im;

    std::cin >> a11 >> a12 >> a13
             >> a21 >> a22 >> a23
             >> a31 >> a32 >> a33;

    if (std::cin.fail()) {
        std::cerr << "Invalid input. Expected 3x3 matrix in the form a11 a12 a13 a21 ... a33, separated by whitespace." << std::endl;
        abort();
    }

    Im << a11, a12, a13,
          a21, a22, a23,
          a31, a32, a33;

    // The inertia tensor should be a real symmetric matrix; real symmetric
    // matrices are self-adjoint.
    // A real symmetric n-dimensional matrix always possesses n real
    // mutually orthogonal eigenvectors/eigenvalues.
    // http://farside.ph.utexas.edu/teaching/336k/Newtonhtml/node66.html
    Eigen::SelfAdjointEigenSolver<Eigen::Matrix3d> eigenSolver(Im);

    if (eigenSolver.info() != Eigen::Success) {
        std::cerr << "Failed to solve for principal axes of rotation. Invalid inertia tensor?" << std::endl;
        abort();
    }

    // http://farside.ph.utexas.edu/teaching/336k/Newtonhtml/node67.html
    Eigen::Vector3d eigenvalues(eigenSolver.eigenvalues());
    std::cout << eigenvalues[0] << " "
              << eigenvalues[1] << " "
              << eigenvalues[2] << std::endl;
    Eigen::Quaterniond q(eigenSolver.eigenvectors());
    q.normalize();
    std::cout << q.w() << " "
              << q.x() << " "
              << q.y() << " "
              << q.z() << " " << std::endl;
}
