#ifndef CUAUV_SIM_PROTOCOL_H
#define CUAUV_SIM_PROTOCOL_H

#include <stdint.h>
#include <mutex>
#include <unordered_map>

#include "spdlog/spdlog.h"

#include "world.hpp"
#include "util.hpp"
#include "physics.hpp"
#include "simulator.hpp"
#include "vision.hpp"
#include "bitwriter.hpp"
#include "depvector.hpp"
#include "objects.hpp"

namespace cuauv {
namespace fishbowl {

typedef uint32_t camera_id;
typedef uint32_t cstream_id;
typedef uint32_t estream_id;
typedef uint32_t trigger_id;
typedef uint32_t spy_id;

/**
 * A user object stores the application-level data associated with a particular
 * client.
 */
struct user {
};

struct camera_box {
    camera cam;
    // sid SHOULD NOT be modified. We can't mark it const because then we
    // couldn't put it in a vector.
    entity_id sid;
    bool enabled;
};

struct cstream {
    cstream_id id;
    client_id clid;

    camera_id cmid;
    entity_id tid;
    // NB: Use pointer instead of ref, ref not assignable, can't put in vector...
    camera_box* camb;
    uint32_t period;
    uint32_t counter;
};

struct estream {
    estream_id id;
    client_id clid;

    entity_id eid;
    entity* e;
    std::vector<uint8_t> attrs;
    uint32_t period;
    uint32_t counter;
};

struct ctrigger {
    trigger_id id;
    client_id clid;

    entity_id a;
    entity_id b;
    entity* ea;
    entity* eb;
    double r2;

    bool active;
};

struct cstrigger {
    trigger_id id;
    client_id clid;

    entity_id a;
    entity_id b;
    entity* ea;
    entity* eb;
    
    bool active;
};

struct ptrigger {
    trigger_id id;
    client_id clid;

    entity_id eid;
    entity* e;

    Eigen::Vector3d x0;
    Eigen::Vector3d x1;

    double r2;

    bool active;
};

struct entity_meta {
    entity_id id;
    std::unordered_map<std::string, std::string> xattrs;
};

struct spy {
    spy_id id;
    client_id clid;
};

template<typename R, typename W>
class protocol {
public:
    protocol(simulator& s);
    ~protocol();

    protocol(const protocol&) = delete;
    protocol& operator=(const protocol&) = delete;

    server_rp handle(client_id id, uint8_t type, R& reader, W& writer, uint8_t& rtype);

    void add_user(client_id id);
    void remove_user(client_id id);
    void complete_user(client_id id);

    void on_step();

    void on_entity_event(entity_event event, entity_id id);

    /**
     * Injected by the server for the protocol to send out-of-band data.
     * open(c : client_id, f : W& -> uint8_t& -> void) calls f on
     * bitwriter and uint8_t references. After f has returned, the data written
     * to the bitwriter and the value of the uint8_t are sent to the client
     * as a message body and response type, respectively.
     *
     * The reason this isn't direct is because originally I imagined the
     * protocol having no knowledge of the actual server.
     */
    std::function<void(client_id, std::function<void(W&, uint8_t&)>)> open;

    std::shared_ptr<spdlog::logger> log;

    const run_config* rc;

private:
    camera_id add_camera(entity_id id, camera c, bool enabled);
    camera_box& get_camera(camera_id id);
    void remove_camera(camera_id id);

    // The values given to the add_* methods MUST be validated beforehand.

    cstream_id add_cstream(client_id clid, camera_id cmid, uint32_t period, entity_id tid);
    void remove_cstream(cstream_id id);

    estream_id add_estream(client_id clid, entity_id id, uint32_t period, std::vector<uint8_t> attrs);
    void remove_estream(estream_id id);

    // collision trigger
    trigger_id add_ctrigger(client_id clid, entity_id a, entity_id b, double r);
    void remove_ctrigger(trigger_id id);

    // collision (sweep) trigger
    trigger_id add_cstrigger(client_id clid, entity_id a, entity_id b);
    void remove_cstrigger(trigger_id id);

    // path trigger
    trigger_id add_ptrigger(client_id clid, entity_id e, Eigen::Vector3d x0, Eigen::Vector3d x1, double r);
    void remove_ptrigger(trigger_id id);
    
    server_rp handle_REMOVE(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_ELIST(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_EADD(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_ESET(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_EGET(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_OADD(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_CADD(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_CSET(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_TADD(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_ESTREAM(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_CSTREAM(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_CONFIG(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_RUN(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_PAUSE(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_EXSET(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_EXGET(client_id id, R& rd, W& wt, uint8_t& rtype);
    server_rp handle_SPY(client_id id, R& rd, W& wt, uint8_t& rtype);

    void send_eattr(const entity& e, uint8_t attr, W& wt, uint8_t& rtype);
    void send_EDATA(estream& st, W& wt, uint8_t& rtype);
    void send_CDATA(cstream& st, W& wt, uint8_t& rtype);
    // only consist of ID
    void send_simple(client_id clid, uint32_t id, uint8_t type);
    void send_simple_(uint32_t id, uint8_t type, W& wt, uint8_t& rtype);
    void send_COMPLETED(W& wt, uint8_t& rtype);
    void send_SPIED(uint8_t event, uint8_t type, uint32_t id);

    simulator& s;
    world& w;
    physics& p;

    entity_observer_id w_obs_id;

    std::vector<std::pair<camera_id, camera_box>> cameras;
    camera_id next_camera_id = 0;

    depvector<camera_id, camera_box> camera_deps { cameras };

    std::vector<std::pair<cstream_id, cstream>> cstreams;
    cstream_id next_cstream_id = 0;
    depvector<cstream_id, cstream> cstream_deps { cstreams };

    std::vector<std::pair<estream_id, estream>> estreams;
    estream_id next_estream_id = 0;
    depvector<estream_id, estream> estream_deps { estreams };

    trigger_id next_trigger_id = 0;

    // collision triggers (referred to as ctriggers)
    std::vector<std::pair<trigger_id, ctrigger>> ctriggers;
    depvector<trigger_id, ctrigger> ctrigger_deps { ctriggers };

    // collision (sweep) triggers (referred to as cstriggers)
    std::vector<std::pair<trigger_id, cstrigger>> cstriggers;
    depvector<trigger_id, cstrigger> cstrigger_deps { cstriggers };

    std::vector<std::pair<trigger_id, ptrigger>> ptriggers;
    depvector<trigger_id, ptrigger> ptrigger_deps { ptriggers };

    std::vector<std::pair<spy_id, spy>> spies;
    spy_id next_spy_id = 0;
    depvector<spy_id, spy> spy_deps { spies };

    hook_id s_hook_id;

    std::unique_lock<std::mutex> lk;

    std::unordered_map<client_id, user> users;

    object_manager_map omanagers;
    std::unordered_map<object_id, std::string> oclasses;
    object_id next_object_id = 0;

    std::unordered_map<entity_id, entity_meta> entity_metas;

    // 0 means no limit
    uint64_t run_steps = 0;
};

} // namespace fishbowl
} // namespace cuauv

#include "protocol_.hpp"

#endif // CUAUV_SIM_PROTOCOL_H
