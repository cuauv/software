#define CATCH_CONFIG_MAIN  // This tells Catch to provide a main() - only do this in one cpp file
#include "catch.hpp"

#include <math.h>

#include <Eigen/Core>
#include <Eigen/Geometry>

#include <simulator2/world.hpp>
#include <simulator2/vision.hpp>

#include <iostream>

namespace fishbowl = cuauv::fishbowl;

// body 3-2-1
Eigen::Quaterniond euler_to_quat(double h, double p, double r)
{
    return Eigen::Quaterniond(Eigen::AngleAxisd(h, Eigen::Vector3d::UnitZ())
                            * Eigen::AngleAxisd(p, Eigen::Vector3d::UnitY())
                            * Eigen::AngleAxisd(r, Eigen::Vector3d::UnitX()));
}

TEST_CASE("SAP Camera basic functionality", "[vision]") {
    sim::world w;
    Eigen::Quaterniond pq(1, 0, 0, 0);
    Eigen::Vector3d px(-5, -3, 0);
    double f = 1;

    sim::entity_id eid = w.add_entity(sim::entity(1, 1, sim::inertia_tensor(1, 1, 1), Eigen::Quaterniond(1, 0, 0, 0)));
    sim::entity& e = w.get_entity(eid);
    e.x = Eigen::Vector3d(-4, -3, 0);

    sim::camera cam(&w, &pq, &px, euler_to_quat(-M_PI/2, 0, 0), Eigen::Vector3d(1, 1, -1), f);
    Eigen::Vector2d x;
    double r, d;

    std::tie(x, r, d) = cam.query(eid);
    REQUIRE(x[0] == Approx(0));
    REQUIRE(x[1] == Approx(1));
    REQUIRE(r != 0);

    // reposition #1

    e.x = Eigen::Vector3d(-4, -1, 0);

    std::tie(x, r, d) = cam.query(eid);
    REQUIRE(x[0] == Approx(0));
    REQUIRE(x[1] == Approx(-1));
    REQUIRE(r == 0);

    // resposition #2

    e.x = Eigen::Vector3d(-3, -4, 0);

    std::tie(x, r, d) = cam.query(eid);
    REQUIRE(x[0] == Approx(0.5));
    REQUIRE(x[1] == Approx(0.5));
    REQUIRE(r != 0);

    // change focal length

    cam.f = 0.5;
    cam.step();

    std::tie(x, r, d) = cam.query(eid);
    REQUIRE(x[0] == Approx(0.25));
    REQUIRE(x[1] == Approx(0.25));
    REQUIRE(r != 0);
}
