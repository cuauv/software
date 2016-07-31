#include "async_manager.h"

AsyncManager::AsyncManager() :
  exit(false), new_data(false), ready_ind(0), started(false) {}

AsyncManager::~AsyncManager() {
  if (started) {
    exit.store(true);
    thread.join();
  }
}

void AsyncManager::start_update_thread() {
  thread = std::thread(&AsyncManager::update_thread, this);
  started = true;
}

void AsyncManager::mark_new_data() {
  std::lock_guard<std::mutex> lck(mtx);
  new_data = true;
  ready_ind = 1 - ready_ind;
}
