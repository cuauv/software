#pragma once

#include <atomic>
#include <mutex>
#include <thread>

class AsyncManager {
  public:
    AsyncManager();
    virtual ~AsyncManager();
    void start_update_thread();
    virtual void maybe_update() = 0;

  protected:
    void mark_new_data();
    virtual void update_thread() = 0;

    std::mutex mtx;
    std::atomic_bool exit;
    bool new_data;
    int ready_ind;

  private:
    std::thread thread;
    bool started;
};
