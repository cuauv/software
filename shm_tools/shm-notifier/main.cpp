#include <string>
#include <iostream>
#include <fstream>
#include <mutex>
#include <unordered_map>
#include <stdlib.h>
#include <stdio.h>
#include <functional>
#include <thread>

#include <signal.h>

#include <libshm/c/shm.h>
#include <libshm/c/dshm.h>
#include <libshm/c/watcher.h>

std::mutex out_mutex;
std::unordered_map<int, watcher_t> watchers;

#define DSHM_TYPE_GETTER(type) dshm_get_##type

#define WATCH_BODY_START(type) \
    type ox, x; \
    if (!DSHM_TYPE_GETTER(type)(sid, &ox)) { \
        std::cerr << "-Fatal: Failed to get variable with ID " << sid << std::endl; \
        exit(1); \
    } \
    for (;;) { \
        if (wait_watcher(w, true)) { \
            lock.lock(); \
            std::cerr << "-Fatal: wait_watcher failed." << std::endl; \
            exit(1); \
        } \
        lock.lock(); \
        if (!DSHM_TYPE_GETTER(type)(sid, &x)) { \
            std::cerr << "-Fatal: Failed to get variable with ID " << sid << std::endl; \
            exit(1); \
        } \
        if (ox != x) {

#define WATCH_BODY_END \
            ox = x; \
        } \
        lock.unlock(); \
    } \

#define WATCH_BODY(type) \
    do { \
        WATCH_BODY_START(type) \
        std::cout << "!" << sid << "=" << x << std::endl; \
        WATCH_BODY_END \
    } while (0); 

int main(int argc, char** argv)
{
    char* dir = getenv("CUAUV_SOFTWARE");
    if (dir == nullptr) {
        throw std::runtime_error("cuauv::conf::load_vehicle: CUAUV_SOFTWARE must be set to the root of the software repository, with a trailing slash.");
    }

    shm_init();

    std::unordered_map<std::string, int> names;
    std::ifstream indexf(std::string(dir) + "libshm/index/dshm_index");
    std::string line;
    dshm_var_id id = 0;
    for (;;) {
        if (!std::getline(indexf, line)) break;
        names.insert({ line, id });
        id++;
    }
    
    signal(SIGINT, [](int signal) {
        std::unique_lock<std::mutex> lock(out_mutex);

        for (auto& x : watchers) {
            destroy_watcher(x.second);
        }

        exit(0);
    });

    std::unique_lock<std::mutex> lock(out_mutex, std::defer_lock);
    char cmd;
    std::string body;
    for (;;) {
        cmd = std::cin.get();
        std::getline(std::cin, body, '\n');
        if (std::cin.fail()) {
            exit(1);
        }
        lock.lock();
        switch (cmd) {
        case '?': {
            dshm_var_id sid = -1;
            bool nonint = false;
            try {
                sid = std::stoi(body);
            } catch (const std::invalid_argument& e) {
                nonint = true;
            } catch (const std::out_of_range& e) {
                nonint = true;
            }
            if (nonint) {
                if (names.count(body) == 1) {
                    sid = names.at(body);
                }
            } else {
                if (sid < 0 || sid >= id) {
                    sid = -1;
                }
            }

            if (sid == -1) {
                std::cout << "-Unrecognized variable." << std::endl;
                break;
            }
            if (dshm_is_string(sid)) {
                std::cout << "-String variable notifications not supported." << std::endl;
                break;
            }
            if (watchers.count(sid)) {
                std::cout << "-Already watching variable." << std::endl;
                break;
            }

            watcher_t w = create_watcher();
            if (w == -1) {
                std::cout << "-Out of watchers." << std::endl;
                break;
            }
            if (!dshm_watch(sid, w)) {
                std::cout << "-Failed to start watching." << std::endl;
                break;
            }

            std::thread wt([sid, w]() {
                std::unique_lock<std::mutex> lock(out_mutex, std::defer_lock);

                if (dshm_is_int(sid)) {
                    WATCH_BODY(int);
                } else if (dshm_is_bool(sid)) {
                    WATCH_BODY_START(bool)
                    std::cout << "!" << sid << "=" << (x ? "#t" : "#f") << std::endl;
                    WATCH_BODY_END
                } else if (dshm_is_float(sid)) {
                    WATCH_BODY(float);
                } else if (dshm_is_double(sid)) {
                    WATCH_BODY(double);
                } else {
                    std::cerr << "-Fatal: Unsupported variable type." << sid << std::endl;
                    exit(1);
                }
                fflush(stdout);
            });
            wt.detach();

            watchers.insert({ sid, w });
            std::cout << "+Added watcher." << std::endl;
            break;
        }
        default:
            std::cerr << "-Unrecognized command." << std::endl;
        }
        lock.unlock();
    }
}
