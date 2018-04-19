#include <functional>
#include <utility>

#include "engine.hpp"
#include "force.hpp"
#include "physics.hpp"
#include "world.hpp"

namespace cuauv {
namespace fishbowl {

physics::physics(world& w)
    : w(w)
    , next_force_id(0)
    , next_engine_id(0)
{
    using namespace std::placeholders;
    w_obs_id = w.add_entity_observer(std::bind(&physics::on_entity_event, this, _1, _2));
}

physics::~physics()
{
    w.remove_entity_observer(w_obs_id);
}

force_id physics::add_force(force* f)
{
    assert(f != nullptr);
    forces[next_force_id] = std::unique_ptr<force>(f);
    return next_force_id++;
}

bool physics::has_force(force_id id)
{
    return forces.count(id) == 1;
}

void physics::remove_force(force_id id)
{
    if (!has_force(id)) {
        throw std::invalid_argument("No force with given ID.");
    }
    
    forces.erase(id);
}

engine_id physics::add_engine(entity_id id, engine* e)
{
    assert(e != nullptr);
    if (!w.has_entity(id)) {
        throw std::invalid_argument("No engine with given ID.");
    }

    engines[next_engine_id] = std::make_pair(id, std::unique_ptr<engine>(e));
    entity_engines[id].push_back(next_engine_id);
    return next_engine_id++;
}

bool physics::has_engine(engine_id id)
{
    return engines.count(id) == 1;
}

void physics::remove_engine(engine_id id)
{
    if (!has_engine(id)) {
        throw std::invalid_argument("No engine with given ID.");
    }

    entity_id ent_id = engines.at(id).first;
    engines.erase(id);

    assert(entity_engines.count(ent_id) == 1);
    std::vector<engine_id>& ent_engines = entity_engines.at(ent_id);
    auto it = std::find(ent_engines.begin(), ent_engines.end(), id);
    assert(it != ent_engines.end());
    ent_engines.erase(it);
}

// Multiplies two quaternions stored as vectors (w, x, y, z).
// Ideally we would use Eigen for this, but when we want to mix quaternion
// additions (not supported 'natively' by Eigen) with quaternion
// multiplications we end up with a lot of Quaternion constructions.
Eigen::Vector4d qvmul(const Eigen::Vector4d& a, const Eigen::Vector4d& b)
{
    return Eigen::Vector4d(
            a[0]*b[0] - a[1]*b[1] - a[2]*b[2] - a[3]*b[3],
            a[0]*b[1] + a[1]*b[0] + a[2]*b[3] - a[3]*b[2],
            a[0]*b[2] - a[1]*b[3] + a[2]*b[0] + a[3]*b[1],
            a[0]*b[3] + a[1]*b[2] - a[2]*b[1] + a[3]*b[0]);
}

// quaternion vector (w, x, y, z) to quaternion
Eigen::Quaterniond qvtoq(const Eigen::Vector4d& v)
{
    return Eigen::Quaterniond(v[0], v[1], v[2], v[3]);
}

// quaternion to quaternion vector (w, x, y, z)
Eigen::Vector4d qtoqv(const Eigen::Quaterniond& q)
{
    return Eigen::Vector4d(q.w(), q.x(), q.y(), q.z());
}

void physics::step(double delta)
{
    // wd = ∂w
    // Update the positions and orientations to the end of the timestep, and
    // the velocities and angular accelerations to the middle of the
    // timestep. Save x to xp (x in the previous timestep).
    w.for_each_entity([delta](int id, entity& e) {
        e.v = e.v + e.a * delta/2;
        e.xp = e.x;
        e.x = e.x + e.v * delta + e.a * delta*delta/2;

        const Eigen::Vector3d I = e.get_I().diagonal();
        const Eigen::Vector3d Ir = e.get_Ir().diagonal();

        const Eigen::Vector4d q = qtoqv(e.q);
        Eigen::Vector3d wd;
        wd[0] = Ir[0] * (e.t[0] + (I[1] - I[2]) * e.w[1] * e.w[2]);
        wd[1] = Ir[1] * (e.t[1] + (I[2] - I[0]) * e.w[2] * e.w[0]);
        wd[2] = Ir[2] * (e.t[2] + (I[0] - I[1]) * e.w[0] * e.w[1]);
        const Eigen::Vector4d wq(0, e.w[0], e.w[1], e.w[2]);
        const Eigen::Vector4d wdq(0, wd[0], wd[1], wd[2]);
        Eigen::Vector4d qd = 0.5 * qvmul(q, wq);
        Eigen::Vector4d qdd = 0.5 * (qvmul(qd, wq) + qvmul(q, wdq));

        e.w = e.w + wd * delta/2;

        e.q = qvtoq(q + (qd * delta) + (qdd * delta*delta/2));
        e.q.normalize();
    });

    // Calculate forces and torques.
    for (auto& force_kv : forces) {
        force_kv.second->step(delta);
    }
    for (auto& engine_kv : engines) {
        engine_kv.second.second->step(delta);
    }

    w.for_each_entity([&](int id, entity& e) {
        // f: forces on the entity in the world frame
        // t: torques on the entity in the body frame
        Eigen::Vector3d f(0, 0, 0);
        Eigen::Vector3d t(0, 0, 0);

        if (e.corporeal) {
            for (auto& force_kv : forces) {
                auto screw = force_kv.second->on(id);
                f += screw.first;
                t += e.q.conjugate() * screw.second;
            }
            if (entity_engines.count(id) == 1) {
                for (engine_id eid : entity_engines.at(id)) {
                    auto screw = engines.at(eid).second->on();
                    f += e.q * screw.first;
                    t += screw.second;
                }
            }
        }

        e.t = t;
        // Now e.a is a(t), an is a(t + d)
        // NB: it looks like they assume force depends only on position.
        // Our force (drag!) depends on velocity too.
        // Ideally we'd rederive.
        Eigen::Vector3d an = f / e.get_m();
        e.v = e.v + delta * (e.a + an) / 2;
        e.a = an;

        const Eigen::Vector3d I = e.get_I().diagonal();
        const Eigen::Vector3d Ir = e.get_Ir().diagonal();

        // Zeroth order approx.
        Eigen::Vector3d wd0 = e.get_Ir() * t;
        Eigen::Vector3d w0 = e.w + wd0 * delta/2;

        // First order approx.
        Eigen::Vector3d wd1;
        wd1[0] = Ir[0] * ((I[1] - I[2]) * w0[1] * w0[2]);
        wd1[1] = Ir[1] * ((I[2] - I[0]) * w0[2] * w0[0]);
        wd1[2] = Ir[2] * ((I[0] - I[1]) * w0[0] * w0[1]);
        Eigen::Vector3d w1 = w0 + wd1 * delta/2;

        // Second order approx.
        Eigen::Vector3d wd2;
        wd2[0] = Ir[0] * ((I[1] - I[2]) * w1[1] * w1[2]);
        wd2[1] = Ir[1] * ((I[2] - I[0]) * w1[2] * w1[0]);
        wd2[2] = Ir[2] * ((I[0] - I[1]) * w1[0] * w1[1]);

        e.w = w0 + wd2 * delta/2;
    });
}

void physics::on_entity_event(entity_event event, entity_id id)
{
    switch (event) {
    case entity_event::ADDED:
        break;
    case entity_event::REMOVED:
        if (entity_engines.count(id) == 1) {
            for (engine_id engine_id : entity_engines.at(id)) {
                engines.erase(engine_id);
            }
            entity_engines.erase(id);
        }
        break;
    }
}

} // namespace cuauv
} // namespace fishbowl
