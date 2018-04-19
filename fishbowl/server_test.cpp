#include <iostream>
#include <thread>
#include <signal.h>

#include "spdlog/spdlog.h"

#include "server.hpp"
#include "world.hpp"
#include "physics.hpp"
#include "util.hpp"
#include "bitreader.hpp"
#include "bitwriter.hpp"
#include "protocol.hpp"
#include "simulator.hpp"

namespace fishbowl = cuauv::fishbowl;
namespace spd = spdlog;

sim::server *sv;

void on_signal(int type)
{
    std::cout << "received signal" << std::endl;
    sv->shutdown();
}

void run(int port)
{
    sim::server_rp res = sv->serve(8080);

    // do something with res
    std::cout << res.second << std::endl;
}

int main()
{
    auto log = spd::stdout_logger_mt("sim");
    log->set_level(spd::level::info);
    log->info("hi there!");

    sim::world w;
    sim::physics p(w);
    sim::simulator s(w, p);

    sim::protocol<sim::bitreader, sim::bitwriter> proto(s);
    sv = new sim::server(proto);

    proto.log = log;
    sv->log = log;

    signal(SIGINT, on_signal);

    std::thread st(run, 8080);

    st.join();

    delete sv;
}
