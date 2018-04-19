#ifndef CUAUV_SIM_PROTOCOL__H
#define CUAUV_SIM_PROTOCOL__H

#include <stdint.h>
#include <unordered_map>
#include <iostream>
#include <iomanip>
#include <string>
#include <stdexcept>

#include <sys/socket.h>

#include <Eigen/Core>

#include "world.hpp"
#include "physics.hpp"
#include "protocol.hpp"
#include "server.hpp"
#include "util.hpp"
#include "fmts.hpp"
#include "bits.hpp"
#include "bitreader.hpp"
#include "bitwriter.hpp"
#include "entity.hpp"
#include "simulator.hpp"
#include "vision.hpp"
#include "depvector.hpp"
#include "geometry.hpp"
#include "objects.hpp"

#define LOG(expr) \
    do { \
        if (log.get() != nullptr) { \
            log->expr; \
        } \
    } while (0);

#define UINT8_OF_RESP(type) static_cast<uint8_t>(response_type::type)

namespace cuauv {
namespace fishbowl {

// Enum values are automatically set to the previous value plus one, but I want
// to make it clear what values correspond to request types.
enum class request_type {
    REMOVE = 0,
    ELIST = 1,
    EADD = 2,
    ESET = 3,
    EGET = 4,
    OCLASSES = 5,
    OLIST = 6,
    OADD = 7,
    OSET = 8,
    CLIST = 9,
    CADD = 10,
    CSET = 11,
    CVIEW = 12,
    TADD = 13,
    ESTREAM = 14,
    CSTREAM = 15,
    CBCAST = 16,
    CONFIG = 17,
    OGET = 18,
    EXSET = 19,
    EXGET = 20,
    EXSTREAM = 21,
    REASSURE = 33,
    RUN = 254,
    PAUSE = 255,
};

enum class response_type {
    OK = 0,
    ERROR = 1,
    ADDED = 2,
    ELISTED = 3,
    EGOT = 4,
    OLISTED = 5,
    OGOT = 6,
    CGOT = 7,
    CONFIGED = 8,
    EXGOT = 9,
    TRIGGERED = 128,
    EDATA = 129,
    CDATA = 130,
    UNTRIGGERED = 131,
    PSTART = 132,
    PFAIL = 133,
    PEND = 134,
    EXDATA = 135,
    SPIED = 136,
    COMPLETED = 255,
};

enum item_type {
    ITEM_ENTITY = 0,
    ITEM_OBJECT = 1,
    ITEM_CAMERA = 2,
    ITEM_TRIGGER = 3,
    ITEM_ESTREAM = 4,
    ITEM_CSTREAM = 5,
    ITEM_CBCAST = 6,
    ITEM_EXSTREAM = 7,
    ITEM_SPY = 8,
};

enum spied_event {
    SPIED_REMOVED = 0,
    SPIED_ADDED = 1,
};

enum cstream_dep {
    CSTREAM_DEP_CLIENT = 0,
    CSTREAM_DEP_CAMERA = 1,
    CSTREAM_DEP_TARGET = 2,
};

enum camera_dep {
    CAMERA_DEP_SOURCE = 0
};

enum estream_dep {
    ESTREAM_DEP_CLIENT = 0,
    ESTREAM_DEP_ENTITY = 1,
};

enum ctrigger_dep {
    CTRIGGER_DEP_CLIENT = 0,
    CTRIGGER_DEP_ENTITY = 1,
};

enum cstrigger_dep {
    CSTRIGGER_DEP_CLIENT = 0,
    CSTRIGGER_DEP_ENTITY = 1,
};

enum ptrigger_dep {
    PTRIGGER_DEP_CLIENT = 0,
    PTRIGGER_DEP_ENTITY = 1,
};

enum spy_dep {
    SPY_DEP_CLIENT = 0,
};

class malformed_exn : public std::exception
{
public:
    malformed_exn() {}
    virtual const char* what() const throw() override { return ""; }
};

template<typename R, typename W>
protocol<R, W>::protocol(simulator& s)
    : s(s)
    , w(s.get_world())
    , p(s.get_physics())
    , lk(s.get_lock())
{
    s_hook_id = s.add_hook(std::bind(&protocol<R, W>::on_step, this));

    using namespace std::placeholders;
    w_obs_id = w.add_entity_observer(std::bind(&protocol::on_entity_event, this, _1, _2));

    omanagers = init_object_manager_map(s);
}

template<typename R, typename W>
protocol<R, W>::~protocol()
{
    s.remove_hook(s_hook_id);
    w.remove_entity_observer(w_obs_id);
}


template<typename R, typename W>
void protocol<R, W>::add_user(client_id id)
{
    users[id] = {};
}

template<typename R, typename W>
void protocol<R, W>::remove_user(client_id id)
{
    users.erase(id);

    cstream_deps.removed(CSTREAM_DEP_CLIENT, id);
    estream_deps.removed(ESTREAM_DEP_CLIENT, id);
    ctrigger_deps.removed(CTRIGGER_DEP_CLIENT, id);
    cstrigger_deps.removed(CSTRIGGER_DEP_CLIENT, id);
    ptrigger_deps.removed(PTRIGGER_DEP_CLIENT, id);
    spy_deps.removed(SPY_DEP_CLIENT, id);
}

template<typename R, typename W>
void protocol<R, W>::complete_user(client_id id)
{
    using namespace std::placeholders;
    open(id, std::bind(&protocol<R, W>::send_COMPLETED, this, _1, _2));
}

//static void dump_hex(uint8_t* data, uint32_t len)
//{
//    std::cout << std::hex;
//    for (uint32_t i = 0; i < len; i++) {
//        std::cout << std::setfill('0') << std::setw(2) << static_cast<uint32_t>(data[i]) << " ";
//    }
//    std::cout << std::dec;
//}

//static void dump(client_id id, uint32_t len, uint8_t type, uint8_t* data)
//{
//    printf("client %d sent message of length %d, type %d: ", id, len, type);
//    dump_hex(data, len);
//    std::cout << std::endl;
//}

// NB: This is only supposed to be called as a hook by the simulator, while
// the simulator still holds the simulator mutex. That way we won't run into
// race conditions wrt sending data.
// Called after the step.
template<typename R, typename W>
void protocol<R, W>::on_step()
{
    using namespace std::placeholders;

    if (run_steps != 0) {
        if (run_steps == 1) {
            s.pause();
            for (auto& x : users) {
                client_id clid = x.first;
                open(clid, std::bind(&protocol<R, W>::send_COMPLETED, this, _1, _2));
            }
        } else {
            run_steps--;
        }
    }

    for (auto& x : cameras) {
        camera_box& camb = x.second;
        if (!camb.enabled) {
            continue;
        }
        camb.cam.step();
    }

    for (auto& x : cstreams) {
        cstream& st = x.second;
        if (!st.camb->enabled) {
            continue;
        }
        st.counter++;
        if (st.counter == st.period) {
            open(st.clid, std::bind(&protocol<R, W>::send_CDATA, this, st, _1, _2));
            st.counter = 0;
        }
    }

    for (auto& x : estreams) {
        estream& st = x.second;
        st.counter++;
        if (st.counter == st.period) {
            open(st.clid, std::bind(&protocol<R, W>::send_EDATA, this, st, _1, _2));
            st.counter = 0;
        }
    }

    for (auto& x : ctriggers) {
        ctrigger& tg = x.second;
        Eigen::Vector3d d = tg.ea->x - tg.eb->x;
        double r2 = d.dot(d);
        bool intersect = r2 <= tg.r2;
        if (tg.active) {
            if (!intersect) {
                send_simple(tg.clid, tg.id, UINT8_OF_RESP(UNTRIGGERED));
                tg.active = false;
            }
        } else {
            if (intersect) {
                send_simple(tg.clid, tg.id, UINT8_OF_RESP(TRIGGERED));
                tg.active = true;
            }
        }
    }

    for (auto& x : cstriggers) {
        cstrigger& tg = x.second;
        double t0, t1;
        bool intersect = sphere_sphere_sweep(tg.ea->get_r(), tg.ea->xp, tg.ea->x, tg.eb->get_r(), tg.eb->xp, tg.eb->x, t0, t1);

        if (tg.active) {
            if (!intersect) {
                send_simple(tg.clid, tg.id, UINT8_OF_RESP(UNTRIGGERED));
                tg.active = false;
            }
        } else {
            if (intersect) {
                send_simple(tg.clid, tg.id, UINT8_OF_RESP(TRIGGERED));
                tg.active = true;
            }
        }
    }

    for (auto& x : ptriggers) {
        ptrigger& tg = x.second;
        const Eigen::Vector3d r0 = tg.e->x - tg.x0;
        const Eigen::Vector3d r1 = tg.e->x - tg.x1;

        double r20 = r0.dot(r0);
        double r21 = r1.dot(r1);
        double r2 = line_distance(tg.x0, tg.x1, tg.e->x);

        if (tg.active) {
            if (r21 <= tg.r2) {
                send_simple(tg.clid, tg.id, UINT8_OF_RESP(PEND));
                tg.active = false;
            } else if (r2 > tg.r2) {
                send_simple(tg.clid, tg.id, UINT8_OF_RESP(PFAIL));
                tg.active = false;
            }
        } else {
            if (r20 <= tg.r2) {
                send_simple(tg.clid, tg.id, UINT8_OF_RESP(PSTART));
                tg.active = true;
            }
        }
    }
}

template<typename R, typename W>
void protocol<R, W>::send_EDATA(estream& st, W& wt, uint8_t& rtype)
{
    wt.write(st.id);
    wt.write(static_cast<uint8_t>(st.attrs.size()));
    for (uint8_t attr : st.attrs) {
        send_eattr(*(st.e), attr, wt, rtype);
    }
    rtype = UINT8_OF_RESP(EDATA);
}

template<typename R, typename W>
void protocol<R, W>::send_CDATA(cstream& st, W& wt, uint8_t& rtype)
{
    double x, y, r, d;
    std::tie(x, y, r, d) = st.camb->cam.query(st.tid);
    wt.write(st.id);
    wt.write(static_cast<float>(x));
    wt.write(static_cast<float>(y));
    wt.write(static_cast<float>(r));
    wt.write(static_cast<float>(d));
    rtype = UINT8_OF_RESP(CDATA);
}

template<typename R, typename W>
void protocol<R, W>::send_simple(client_id clid, uint32_t id, uint8_t type)
{
    using namespace std::placeholders;
    open(clid, std::bind(&protocol<R, W>::send_simple_, this, id, type, _1, _2));
}

template<typename R, typename W>
void protocol<R, W>::send_simple_(uint32_t id, uint8_t type, W& wt, uint8_t& rtype)
{
    wt.write(id);
    rtype = type;
}

template<typename R, typename W>
void protocol<R, W>::send_COMPLETED(W& wt, uint8_t& rtype)
{
    rtype = UINT8_OF_RESP(COMPLETED);
}

template<typename R, typename W>
camera_id protocol<R, W>::add_camera(entity_id id, camera c, bool enabled)
{
    camera_box b { c, id, enabled };
    cameras.push_back({ next_camera_id, b });
    camera_deps.depend(next_camera_id, CAMERA_DEP_SOURCE, id);

    return next_camera_id++;
}

template<typename R, typename W>
camera_box& protocol<R, W>::get_camera(camera_id id)
{
    for (auto it = cameras.begin(); it != cameras.end(); it++) {
        if ((*it).first == id) {
            return (*it).second;
        }
    }

    throw std::invalid_argument("No camera with ID [id].");
}

template<typename R, typename W>
void protocol<R, W>::remove_camera(camera_id id)
{
    cstream_deps.removed(CSTREAM_DEP_CAMERA, id);
    camera_deps.remove(id);
}

template<typename R, typename W>
estream_id protocol<R, W>::add_estream(client_id clid, entity_id eid, uint32_t period, std::vector<uint8_t> attrs)
{
    entity& e = w.get_entity(eid);
    estream stream {
        next_estream_id,
        clid,
        eid,
        &e,
        attrs,
        period,
        0
    };
    estreams.push_back({ next_estream_id, stream });
    estream_deps.depend(next_estream_id, ESTREAM_DEP_CLIENT, clid);
    estream_deps.depend(next_estream_id, ESTREAM_DEP_ENTITY, eid);

    return next_estream_id++;
}

template<typename R, typename W>
void protocol<R, W>::remove_estream(estream_id id)
{
    estream_deps.remove(id);
}

template<typename R, typename W>
trigger_id protocol<R, W>::add_ctrigger(client_id clid, entity_id a, entity_id b, double r)
{
    ctrigger trigger {
        next_trigger_id,
        clid,
        a,
        b,
        &w.get_entity(a),
        &w.get_entity(b),
        r * r,
        false,
    };
    ctriggers.push_back({ next_trigger_id, trigger });
    ctrigger_deps.depend(next_trigger_id, CTRIGGER_DEP_CLIENT, clid);
    ctrigger_deps.depend(next_trigger_id, CTRIGGER_DEP_ENTITY, a);
    ctrigger_deps.depend(next_trigger_id, CTRIGGER_DEP_ENTITY, b);

    return next_trigger_id++;
}

template<typename R, typename W>
void protocol<R, W>::remove_ctrigger(trigger_id id)
{
    ctrigger_deps.remove(id);
}

template<typename R, typename W>
trigger_id protocol<R, W>::add_cstrigger(client_id clid, entity_id a, entity_id b)
{
    cstrigger trigger {
        next_trigger_id,
        clid,
        a,
        b,
        &w.get_entity(a),
        &w.get_entity(b),
        false,
    };
    cstriggers.push_back({ next_trigger_id, trigger });
    cstrigger_deps.depend(next_trigger_id, CTRIGGER_DEP_CLIENT, clid);
    cstrigger_deps.depend(next_trigger_id, CTRIGGER_DEP_ENTITY, a);
    cstrigger_deps.depend(next_trigger_id, CTRIGGER_DEP_ENTITY, b);

    return next_trigger_id++;
}

template<typename R, typename W>
void protocol<R, W>::remove_cstrigger(trigger_id id)
{
    cstrigger_deps.remove(id);
}

template<typename R, typename W>
trigger_id protocol<R, W>::add_ptrigger(client_id clid, entity_id e, Eigen::Vector3d x0, Eigen::Vector3d x1, double r)
{
    ptrigger trigger {
        next_trigger_id,
        clid,
        e,
        &w.get_entity(e),
        x0,
        x1,
        r * r,
        false,
    };
    ptriggers.push_back({ next_trigger_id, trigger });
    ptrigger_deps.depend(next_trigger_id, PTRIGGER_DEP_CLIENT, clid);
    ptrigger_deps.depend(next_trigger_id, PTRIGGER_DEP_ENTITY, e);

    return next_trigger_id++;
}

template<typename R, typename W>
void protocol<R, W>::remove_ptrigger(trigger_id id)
{
    ptrigger_deps.remove(id);
}

template<typename R, typename W>
cstream_id protocol<R, W>::add_cstream(client_id clid, camera_id cmid, uint32_t period, entity_id tid)
{
    camera_box& camb = get_camera(cmid);
    cstreams.push_back({ next_cstream_id, { next_cstream_id, clid, cmid, tid, &camb, period, 0 } });
    cstream_deps.depend(next_cstream_id, CSTREAM_DEP_CLIENT, clid);
    cstream_deps.depend(next_cstream_id, CSTREAM_DEP_CAMERA, cmid);
    cstream_deps.depend(next_cstream_id, CSTREAM_DEP_TARGET, tid);

    return next_cstream_id++;
}

template<typename R, typename W>
void protocol<R, W>::remove_cstream(cstream_id id)
{
    cstream_deps.remove(id);
}

template<typename R, typename W>
void protocol<R, W>::on_entity_event(entity_event event, entity_id id)
{
    switch (event) {
    case entity_event::ADDED:
        send_SPIED(SPIED_ADDED, ITEM_ENTITY, id);
        break;
    case entity_event::REMOVED:
        send_SPIED(SPIED_REMOVED, ITEM_ENTITY, id);

        auto rs = camera_deps.removed(CAMERA_DEP_SOURCE, id);
        for (camera_id cmid : rs) {
            cstream_deps.removed(CSTREAM_DEP_CAMERA, cmid);
        }
        cstream_deps.removed(CSTREAM_DEP_TARGET, id);
        estream_deps.removed(ESTREAM_DEP_ENTITY, id);
        ctrigger_deps.removed(CTRIGGER_DEP_ENTITY, id);
        cstrigger_deps.removed(CSTRIGGER_DEP_ENTITY, id);
        ptrigger_deps.removed(PTRIGGER_DEP_ENTITY, id);
        entity_metas.erase(id);
    }
}


template<typename R>
static void assert_rd_ended(R& rd)
{
    rd.end();
    if (rd.failed()) {
        throw malformed_exn();
    }
}

template<typename W>
static void respond_ERROR(W& wt, uint8_t& rtype, uint32_t code, const std::string& message)
{
    wt.write(code);
    wt.write(message);

    rtype = UINT8_OF_RESP(ERROR);
}

template<typename W>
static void respond_ADDED(W& wt, uint8_t& rtype, uint8_t added_type, uint32_t added_id)
{
    wt.write(added_type);
    wt.write(added_id);

    rtype = UINT8_OF_RESP(ADDED);
}

template<typename R, typename W>
void protocol<R, W>::send_SPIED(uint8_t event, uint8_t type, uint32_t id)
{
    for (const auto& x : spies) {
        const spy& s = x.second;
        open(s.clid, [&](W& wt, uint8_t& rtype) {
            wt.write(event);
            wt.write(type);
            wt.write(id);
            rtype = UINT8_OF_RESP(SPIED);
        });
    }
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_REMOVE(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    rtype = UINT8_OF_RESP(OK);

    uint8_t type;
    uint32_t removed_id;

    type = rd.uint8();
    removed_id = rd.uint32();

    assert_rd_ended(rd);

    switch (type) {
    case ITEM_ENTITY: 
        if (!w.has_entity(removed_id)) {
            respond_ERROR(wt, rtype, 0, "No entity with given ID.");
        } else {
            w.remove_entity(removed_id);
        }
        break;
    case ITEM_OBJECT: 
        // We offset engine IDs by 1 << 31 (i.e. 2^31).
        if (removed_id >= (1U << 31)) {
            // Engine
            if (!p.has_engine(removed_id - (1U << 31))) {
                respond_ERROR(wt, rtype, 0, "No engine with given ID.");
            } else {
                p.remove_engine(removed_id - (1 << 31));
            }
        } else {
            // Force
            if (!p.has_force(removed_id)) {
                respond_ERROR(wt, rtype, 0, "No force with given ID.");
            } else {
                p.remove_force(removed_id);
            }
        }
        break;
    case ITEM_CAMERA: {
        try {
            remove_camera(removed_id);
        } catch (const std::invalid_argument& e) {
            respond_ERROR(wt, rtype, 0, "No camera with given ID.");
        }
        break;
    }
    case ITEM_TRIGGER: // Trigger
        // XXX implement
        break;
    case ITEM_ESTREAM: // Entity Stream
        // XXX implement
        break;
    // Camera Stream
    case ITEM_CSTREAM: {
        try {
            remove_cstream(removed_id);
        } catch (const std::invalid_argument& e) {
            respond_ERROR(wt, rtype, 0, "No camera stream with given ID.");
        }
        break;
    }
    case ITEM_CBCAST: // Camera Broadcast
        // XXX implement
        break;
    case ITEM_EXSTREAM:
        break;
    case ITEM_SPY:
        try {
            spy_deps.remove(removed_id);
        } catch (const std::invalid_argument& e) {
            respond_ERROR(wt, rtype, 0, "No spy with given ID.");
        }
        break;
    }

    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_ELIST(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    assert_rd_ended(rd);

    uint32_t n = 0;

    wt.write(n); // We will overwrite this later.

    w.for_each_entity([&n, &wt](entity_id eid, const entity& e) mutable {
        n++;
        wt.write(eid);
    });

    bytes_of_uint32(wt.data(), n); // Overwrite the placeholder n value.

    rtype = UINT8_OF_RESP(ELISTED);
    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_EADD(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    // Read

    double m, r;
    inertia_tensor I;
    Eigen::Quaterniond btom_rq;
    Eigen::Vector3d x;
    Eigen::Quaterniond q;
    bool corporeal;

    m = rd.float64();
    r = rd.float64();
    I.diagonal()[0] = rd.float64();
    I.diagonal()[1] = rd.float64();
    I.diagonal()[2] = rd.float64();
    btom_rq.coeffs()[3] = rd.float64();
    btom_rq.coeffs()[0] = rd.float64();
    btom_rq.coeffs()[1] = rd.float64();
    btom_rq.coeffs()[2] = rd.float64();
    x[0] = rd.float64();
    x[1] = rd.float64();
    x[2] = rd.float64();
    q.coeffs()[3] = rd.float64();
    q.coeffs()[0] = rd.float64();
    q.coeffs()[1] = rd.float64();
    q.coeffs()[2] = rd.float64();
    corporeal = rd.bool_();

    uint8_t xattrs_n = rd.uint8();
    if (rd.failed()) {
        throw malformed_exn();
    }
    std::vector<std::pair<std::string, std::string>> xattrs;
    for (int i = 0; i < xattrs_n; i++) {
        std::string key = rd.string(true); // u8len=true
        std::string data = rd.string();
        xattrs.push_back({key, data});
    }

    assert_rd_ended(rd);

    // Act

    uint32_t eid;
    try {
        entity e(m, r, I, btom_rq);
        e.x = x;
        e.q = q;
        e.corporeal = corporeal;

        eid = w.add_entity(e);
    } catch (std::invalid_argument& e) {
        respond_ERROR(wt, rtype, 0, e.what());
        return {};
    }

    for (auto x : xattrs) {
        entity_metas[eid].xattrs[x.first] = x.second;
    }

    // Respond

    respond_ADDED(wt, rtype, ITEM_ENTITY, eid);

    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_ESET(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    uint32_t eid = rd.uint32();
    uint8_t attribute = rd.uint8();

    if (rd.failed()) {
        throw malformed_exn();
    }

    if (!w.has_entity(eid)) {
        respond_ERROR(wt, rtype, 0, "No entity with given ID.");
        return {};
    }

    entity& e = w.get_entity(eid);

    switch (attribute) {
    // x
    case 0: {
        Eigen::Vector3d x;
        x[0] = rd.float64();
        x[1] = rd.float64();
        x[2] = rd.float64();
        assert_rd_ended(rd);
        e.x = x;
        break;
    }
    // v
    case 1: {
        Eigen::Vector3d v;
        v[0] = rd.float64();
        v[1] = rd.float64();
        v[2] = rd.float64();
        assert_rd_ended(rd);
        e.v = v;
        break;
    }
    // a
    case 2: {
        Eigen::Vector3d a;
        a[0] = rd.float64();
        a[1] = rd.float64();
        a[2] = rd.float64();
        assert_rd_ended(rd);
        e.a = a;
        break;
    }
    // q
    case 3: {
        Eigen::Quaterniond q;
        q.coeffs()[3] = rd.float64();
        q.coeffs()[0] = rd.float64();
        q.coeffs()[1] = rd.float64();
        q.coeffs()[2] = rd.float64();
        assert_rd_ended(rd);
        // Need to convert client input from the model frame to the body frame.
        e.q = q.normalized() * e.get_btom_rm();
        break;
    }
    // w
    case 4: {
        Eigen::Vector3d w;
        w[0] = rd.float64();
        w[1] = rd.float64();
        w[2] = rd.float64();
        assert_rd_ended(rd);
        // Need to convert client input from the model frame to the body frame.
        e.w = e.get_mtob_rm() * w;
        break;
    }
    // corporeal
    case 5: {
        bool corporeal = rd.bool_();
        assert_rd_ended(rd);
        e.corporeal = corporeal;
        break;
    }
    default:
        respond_ERROR(wt, rtype, 1, "Invalid entity attribute ID.");
        return {};
    }

    rtype = UINT8_OF_RESP(OK);
    return {};
}

#define is_eattr(attr) (attr <= 5)

template<typename R, typename W>
void protocol<R, W>::send_eattr(const entity& e, uint8_t attr, W& wt, uint8_t& rtype)
{
    wt.write(attr);

    switch (attr) {
    case 0: // x
        wt.write(e.x[0]);
        wt.write(e.x[1]);
        wt.write(e.x[2]);
        break;
    case 1: // v
        wt.write(e.v[0]);
        wt.write(e.v[1]);
        wt.write(e.v[2]);
        break;
    case 2: // a
        wt.write(e.a[0]);
        wt.write(e.a[1]);
        wt.write(e.a[2]);
        break;
    case 3: // q
        wt.write(e.q.coeffs()[3]);
        wt.write(e.q.coeffs()[0]);
        wt.write(e.q.coeffs()[1]);
        wt.write(e.q.coeffs()[2]);
        break;
    case 4: // w
    {
        // Need to convert from the body to the model frame.
        Eigen::Vector3d w_m = e.get_btom_rm() * e.w;
        wt.write(w_m[0]);
        wt.write(w_m[1]);
        wt.write(w_m[2]);
        break;
    }
    case 5: // corporeal
        wt.write(e.corporeal);
        break;
    default:
        throw std::invalid_argument("Invalid entity attribute ID.");
    }
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_EGET(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    uint32_t eid = rd.uint32();
    uint8_t attribute = rd.uint8();

    assert_rd_ended(rd);

    if (!is_eattr(attribute)) {
        respond_ERROR(wt, rtype, 1, "No attribute with given ID.");
        return {};
    }

    if (!w.has_entity(eid)) {
        respond_ERROR(wt, rtype, 0, "No entity with given ID.");
        return {};
    }

    const entity& e = w.get_entity(eid);

    send_eattr(e, attribute, wt, rtype);
    rtype = UINT8_OF_RESP(EGOT);

    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_CADD(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    uint32_t eid;
    Eigen::Quaterniond q;
    Eigen::Vector3d x;
    double f;
    bool enabled;

    eid = rd.uint32();
    q.coeffs()[3] = rd.float64();
    q.coeffs()[0] = rd.float64();
    q.coeffs()[1] = rd.float64();
    q.coeffs()[2] = rd.float64();
    x[0] = rd.float64();
    x[1] = rd.float64();
    x[2] = rd.float64();
    f = rd.float64();
    enabled = rd.bool_();

    assert_rd_ended(rd);

    if (!w.has_entity(eid)) {
        respond_ERROR(wt, rtype, 0, "No entity with given ID.");
        return {};
    }

    entity& e = w.get_entity(eid);
    camera cam(&w, &(e.q), &(e.x), Eigen::Quaterniond(e.get_mtob_rm() * q), e.get_mtob_rm() * x, f);

    camera_id cid = add_camera(eid, cam, enabled);
    respond_ADDED(wt, rtype, ITEM_CAMERA, cid);

    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_OADD(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    std::string oclass = rd.string();
    if (rd.failed()) {
        throw malformed_exn();
    }

    if (omanagers.count(oclass) == 0) {
        respond_ERROR(wt, rtype, 0, "No such object class.");
        return {};
    }

    object_manager* om = omanagers.at(oclass).get();
    bool ok;
    std::string msg;
    std::tie(ok, msg) = om->add(next_object_id, rd);

    if (!ok) {
        respond_ERROR(wt, rtype, 0, msg);
        return {};
    }

    oclasses.insert({next_object_id, oclass});
    respond_ADDED(wt, rtype, ITEM_OBJECT, next_object_id);
    next_object_id++;
    
    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_ESTREAM(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    uint32_t eid, period;
    uint8_t n;

    eid = rd.uint32();
    period = rd.uint32();
    n = rd.uint8();

    std::vector<uint8_t> attrs;
    for (uint8_t i = 0; i < n; i++) {
        attrs.push_back(rd.uint8());
    }

    assert_rd_ended(rd);

    for (uint8_t attr : attrs) {
        if (!is_eattr(attr)) {
            respond_ERROR(wt, rtype, 0, "No attribute with given ID.");
            return {};
        }
    }

    if (!w.has_entity(eid)) {
        respond_ERROR(wt, rtype, 1, "No entity with given ID.");
        return {};
    }

    cstream_id sid = add_estream(id, eid, period, attrs);
    respond_ADDED(wt, rtype, ITEM_ESTREAM, sid);
    
    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_TADD(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    uint8_t type;

    type = rd.uint8();

    if (rd.failed()) {
        throw malformed_exn();
    }

    switch (type) {
    // collision
    case 0: {
        uint32_t a, b;
        double r;

        a = rd.uint32();
        b = rd.uint32();
        r = rd.float64();

        assert_rd_ended(rd);

        if (a == b) {
            respond_ERROR(wt, rtype, 1, "Entity IDs must be different.");
            return {};
        }
        if (!w.has_entity(a)) {
            respond_ERROR(wt, rtype, 2, "No entity with given ID a.");
            return {};
        }
        if (!w.has_entity(b)) {
            respond_ERROR(wt, rtype, 3, "No entity with given ID b.");
            return {};
        }
        // if r < 0, set it to the sum of the radii of the two entities
        if (r < 0) {
            r = w.get_entity(a).get_r() + w.get_entity(b).get_r();
        }

        trigger_id tid = add_ctrigger(id, a, b, r);
        respond_ADDED(wt, rtype, ITEM_TRIGGER, tid);

        break;
    }
    // collision (sweep)
    case 1: {
        uint32_t a, b;

        a = rd.uint32();
        b = rd.uint32();

        assert_rd_ended(rd);

        if (a == b) {
            respond_ERROR(wt, rtype, 1, "Entity IDs must be different.");
            return {};
        }
        if (!w.has_entity(a)) {
            respond_ERROR(wt, rtype, 2, "No entity with given ID a.");
            return {};
        }
        if (!w.has_entity(b)) {
            respond_ERROR(wt, rtype, 3, "No entity with given ID b.");
            return {};
        }

        trigger_id tid = add_cstrigger(id, a, b);
        respond_ADDED(wt, rtype, ITEM_TRIGGER, tid);

        break;
    }
    // bytecode
    case 2:
        break;
    // path
    case 3: {
        uint32_t e;
        Eigen::Vector3d x0;
        Eigen::Vector3d x1;
        double r;

        e = rd.uint32();

        x0[0] = rd.float64();
        x0[1] = rd.float64();
        x0[2] = rd.float64();

        x1[0] = rd.float64();
        x1[1] = rd.float64();
        x1[2] = rd.float64();

        r = rd.float64();

        assert_rd_ended(rd);

        if (!w.has_entity(e)) {
            respond_ERROR(wt, rtype, 1, "No entity with given ID.");
            return {};
        }

        if (r <= 0) {
            respond_ERROR(wt, rtype, 2, "r must be positive.");
            return {};
        }

        trigger_id tid = add_ptrigger(id, e, x0, x1, r);
        respond_ADDED(wt, rtype, ITEM_TRIGGER, tid);

        break;
    }
    default: {
        respond_ERROR(wt, rtype, 0, "Unrecognized trigger type.");
        break;
    }
    }

    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_CSTREAM(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    uint32_t cid, period, tid;

    cid = rd.uint32();
    period = rd.uint32();
    tid = rd.uint32();

    assert_rd_ended(rd);

    camera_box* camb = nullptr;
    try {
        camb = &get_camera(cid);
    } catch (const std::invalid_argument& e) {
        respond_ERROR(wt, rtype, 0, "No camera with given ID.");
        return {};
    }

    if (!w.has_entity(tid)) {
        respond_ERROR(wt, rtype, 1, "No entity with given ID.");
        return {};
    }

    if (camb->sid == tid) {
        respond_ERROR(wt, rtype, 2, "Cannot target entity with camera on same entity.");
        return {};
    }

    cstream_id sid = add_cstream(id, cid, period, tid);
    respond_ADDED(wt, rtype, ITEM_CSTREAM, sid);

    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_CSET(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    uint32_t cid = rd.uint32();
    uint8_t attribute = rd.uint8();

    if (rd.failed()) {
        throw malformed_exn();
    }

    // has_camera and get_camera repeat work.
    camera_box* camb = nullptr;
    try {
        camb = &get_camera(cid);
    } catch (const std::invalid_argument& e) {
        respond_ERROR(wt, rtype, 0, "No camera with given ID.");
    }

    camera& c = camb->cam;

    switch (attribute) {
    // q
    case 0: {
        Eigen::Quaterniond q;
        q.coeffs()[3] = rd.float64();
        q.coeffs()[0] = rd.float64();
        q.coeffs()[1] = rd.float64();
        q.coeffs()[2] = rd.float64();
        assert_rd_ended(rd);
        c.q = q.normalized();
        break;
    }
    // x
    case 1: {
        Eigen::Vector3d x;
        x[0] = rd.float64();
        x[1] = rd.float64();
        x[2] = rd.float64();
        assert_rd_ended(rd);
        c.x = x;
        break;
    }
    // f
    case 2: {
        double f;
        f = rd.float64();
        assert_rd_ended(rd);
        c.f = f;
        break;
    }
    // enabled
    case 3: {
        bool enabled = rd.bool_();
        assert_rd_ended(rd);
        camb->enabled = enabled;
        break;
    }
    default:
        respond_ERROR(wt, rtype, 1, "Invalid camera attribute ID.");
        return {};
    }

    rtype = UINT8_OF_RESP(OK);
    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_CONFIG(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    wt.write(rc->frequency);
    wt.write(rc->speed);
    wt.write(rc->non_real_time);
    wt.write(rc->use_des_thrusters);
    rtype = UINT8_OF_RESP(CONFIGED);
    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_RUN(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    if (!s.paused()) {
        respond_ERROR(wt, rtype, 0, "Not paused.");
    } else {
        double steps;
        steps = rd.uint64();
        assert_rd_ended(rd);
        run_steps = steps;
        s.unpause();
        rtype = UINT8_OF_RESP(OK);
        LOG(info("Client {} unpaused the simulator.", id));
    }
    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_PAUSE(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    if (s.paused()) {
        respond_ERROR(wt, rtype, 0, "Not running.");
    } else {
        s.pause();
        rtype = UINT8_OF_RESP(OK);
        LOG(info("Client {} paused the simulator.", id));
    }
    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_EXSET(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    uint32_t eid = rd.uint32();
    std::string key = rd.string(true); // u8len=true
    std::string data = rd.string();

    if (rd.failed()) {
        throw malformed_exn();
    }
    assert_rd_ended(rd);

    entity_metas[eid].xattrs[key] = data;
    rtype = UINT8_OF_RESP(OK);
    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_EXGET(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    uint32_t eid = rd.uint32();
    std::string key = rd.string(true); // u8len=true

    if (rd.failed()) {
        throw malformed_exn();
    }
    assert_rd_ended(rd);

    if (entity_metas[eid].xattrs.count(key) == 0) {
        respond_ERROR(wt, rtype, 0, "No extended entity attribute with given key.");
    } else {
        wt.write(key, true); // u8len=true
        wt.write(entity_metas[eid].xattrs[key]);
        rtype = UINT8_OF_RESP(EXGOT);
    }

    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle_SPY(client_id id, R& rd, W& wt, uint8_t& rtype)
{
    assert_rd_ended(rd);

    spy s {
        next_spy_id,
        id,
    };
    spies.push_back({ next_spy_id, s });
    spy_deps.depend(next_spy_id, SPY_DEP_CLIENT, id);
    respond_ADDED(wt, rtype, ITEM_SPY, next_spy_id);
    next_spy_id++;

    return {};
}

template<typename R, typename W>
server_rp protocol<R, W>::handle(client_id id, uint8_t type, R& rd, W& wt, uint8_t& rtype)
{
    server_rp res;
    lk.lock();
    //dump(id, len, type, data);
    try {
        // XXX Undefined behavior if [type] is out of range of [request_type].
        // Should we care?
        switch (static_cast<request_type>(type)) {
        case request_type::REMOVE:
            res = handle_REMOVE(id, rd, wt, rtype);
            break;
        case request_type::ELIST:
            res = handle_ELIST(id, rd, wt, rtype);
            break;
        case request_type::EADD:
            res = handle_EADD(id, rd, wt, rtype);
            break;
        case request_type::ESET:
            res = handle_ESET(id, rd, wt, rtype);
            break;
        case request_type::EGET:
            res = handle_EGET(id, rd, wt, rtype);
            break;
        case request_type::OADD:
            res = handle_OADD(id, rd, wt, rtype);
            break;
        case request_type::CADD:
            res = handle_CADD(id, rd, wt, rtype);
            break;
        case request_type::CSET:
            res = handle_CSET(id, rd, wt, rtype);
            break;
        case request_type::TADD:
            res = handle_TADD(id, rd, wt, rtype);
            break;
        case request_type::ESTREAM:
            res = handle_ESTREAM(id, rd, wt, rtype);
            break;
        case request_type::CSTREAM:
            res = handle_CSTREAM(id, rd, wt, rtype);
            break;
        case request_type::CONFIG:
            res = handle_CONFIG(id, rd, wt, rtype);
            break;
        case request_type::REASSURE:
            rtype = UINT8_OF_RESP(OK);
            break;
        case request_type::RUN:
            res = handle_RUN(id, rd, wt, rtype);
            break;
        case request_type::PAUSE:
            res = handle_PAUSE(id, rd, wt, rtype);
            break;
        case request_type::EXSET:
            res = handle_EXSET(id, rd, wt, rtype);
            break;
        case request_type::EXGET:
            res = handle_EXGET(id, rd, wt, rtype);
            break;
        default:
            res = { server_code::ERR_CLIENT_MALFORMED, "Unrecognized request type." };
            break;
        }
    } catch (const malformed_exn& e) {
        res = { server_code::ERR_CLIENT_MALFORMED, "Client sent malformed message." };
    }
    lk.unlock();
    return res;
}

#undef LOG
#undef UINT8_OF_RESP

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_PROTOCOL__H
