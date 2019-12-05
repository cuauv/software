#include <iostream>
#include <thread>
#include <signal.h>
#include <stdint.h>

#include <libshm/c/shm.h>
#include <conf/vehicle.hpp>

#include "lib/tclap/CmdLine.h"
#include "spdlog/spdlog.h"

#include "server.hpp"
#include "world.hpp"
#include "physics.hpp"
#include "util.hpp"
#include "bitreader.hpp"
#include "bitwriter.hpp"
#include "protocol.hpp"
#include "simulator.hpp"

namespace conf = cuauv::conf;
namespace fishbowl = cuauv::fishbowl;
namespace spd = spdlog;

fishbowl::simulator* s;
fishbowl::server* sv;

int main(int argc, char* argv[])
{
    auto log = spd::stdout_logger_mt("sim");
    log->set_level(spd::level::debug);
    log->info("Starting the CUAUV Simulator...");

    try {
        TCLAP::CmdLine cmd("CUAUV's AUV simulator.", ' ', "0.1");

        // ValueArg (const std::string &flag, const std::string &name, const std::string &desc, bool req, T value, const std::string &typeDesc, CmdLineInterface &parser, Visitor *v=NULL)
        // SwitchArg (const std::string &flag, const std::string &name, const std::string &desc, CmdLineInterface &parser, bool def=false, Visitor *v=NULL)

        TCLAP::ValueArg<std::string> simulator_conf_path("", "simulator-conf",
                "The path to the simulator.json file to use. Defaults to conf/simulator.json.",
                false, "conf/simulator.json", "string", cmd, nullptr);

        TCLAP::ValueArg<uint16_t> port("p", "port",
                "The port to run on. Defaults to 7772.",
                false, 7772, "[1-65535]", cmd, nullptr);

        // XXX Weird things happen with high frequencies -- once you pass a
        // point, you often get errors where the time elapsed is slightly
        // greater than the maximum timestep. The weird part is that these
        // elapsed times seem to relate to the maximum timestep -- increase
        // the frequency further past the point and you will get smaller
        // elapsed times, decrease the frequency but remain past the point and
        // you will get greater elapsed times. Not high priority because for
        // such high frequencies it makes more sense to run in non-real-time
        // mode.
        TCLAP::ValueArg<double> frequency("f", "frequency",
                "The simulation frequency. Defaults to 100 Hz.", 
                false, 100, "positive real", cmd, nullptr);
        TCLAP::ValueArg<double> speed("s", "speed",
                "The simulation speed. Defaults to 1x, i.e. real time.",
                false, 1, "positive real", cmd, nullptr);

        TCLAP::SwitchArg non_real_time("n", "non-real-time",
                "Run in non-real-time mode. Normally the simulator tries to run "
                "near real-time, or near some multiple of real-time. In "
                "non-real-time mode, the simulator runs as fast as possible. "
                "The simulated timestep length is fixed to [frequency / speed].",
                cmd, false, nullptr);

        TCLAP::SwitchArg use_des_thrusters("", "use-des-thrusters",
                "Use the internal desires-based approximating controller, instead of the control_internal_wrench -based real controller.",
                cmd, false, nullptr);

        TCLAP::SwitchArg verbose("v", "verbose",
                "Show verbose output.",
                cmd, false, nullptr);

        cmd.parse(argc, argv);

        fishbowl::run_config cfg {
            conf::load_vehicle(),
            frequency.getValue(),
            speed.getValue(),
            non_real_time.getValue(),
            use_des_thrusters.getValue(),
            verbose.getValue(),
        };
    
        shm_init();

        fishbowl::world w;
        fishbowl::physics p(w);
        s = new fishbowl::simulator(w, p);

        fishbowl::protocol<fishbowl::bitreader, fishbowl::bitwriter> proto(*s);
        sv = new fishbowl::server(proto);

        proto.log = log;
        sv->log = log;

        proto.rc = &cfg;

        signal(SIGINT, [](int signal) {
            sv->shutdown();
            s->stop();
            // Assume we are the only one modifying the simulator so we don't
            // need to hold the simulator lock.
            if (s->paused()) {
                s->unpause();
            }
        });

        s->pause();

        std::thread simt([&log, &cfg]() mutable {
            int res = s->run(cfg);
            log->info("Simulator exited ({}).", res);
            raise(SIGINT);
        });

        std::thread servert([&log, &port]() {
            fishbowl::server_rp res = sv->serve(port.getValue());
            log->info("Server exited: {} ({}).", res.second == "" ? "All's well!" : res.second, static_cast<uint32_t>(res.first));
            if (res.first != fishbowl::server_code::OK) {
                std::cerr << "auv-fishbowl: " << res.second << std::endl;
            }
            raise(SIGINT);
        });

        servert.join();
        simt.join();
    } catch (TCLAP::ArgException& e) {
        std::cerr << "An error occurred while parsing argument " << e.argId() << ": " << e.error() << std::endl;
        return 1;
    }
}
