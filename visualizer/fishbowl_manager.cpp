#include "fishbowl_manager.h"

#include <chrono>

#include "fishbowl_comm.h"
#include "graphics_engine/graphics_engine.h"

FishbowlManager::FishbowlManager(std::unordered_map<std::string, ModelSceneObject *> &render_tag_map_) :
  AsyncManager(), render_tag_map(render_tag_map_) {}

void FishbowlManager::maybe_update() {
  std::lock_guard<std::mutex> lck(mtx);
  if (new_data) {
    for (const auto &ready_data : fishbowl_data[ready_ind]) {
      const auto &obj_name = ready_data.first;
      const auto &obj_data = ready_data.second;
      auto iter = render_tag_map.find(obj_name);
      if (iter != render_tag_map.end()) {
        auto *scene_object = iter->second;
        if (obj_data.pos_valid) {
          glm::vec3 position = obj_data.position;
          scene_object->get_position = [position] {
            return position;
          };
        }

        if (obj_data.q_valid) {
          glm::fquat orientation = obj_data.orientation;
          scene_object->get_orientation = [orientation] {
            return orientation;
          };
        }
      }
    }
  }
}

void FishbowlManager::update_thread() {
  while (!exit.load()) {
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    if (!connected) {
      connected = !fishbowl_comm::connect();
      continue;
    }

    std::vector<uint32_t> entities = fishbowl_comm::get_all_entities();
    if (!entities.size()) {
      connected = false;
      continue;
    }

    for (auto &e_id : entities) {
      std::string obj_name;
      int res;
      if ((res = fishbowl_comm::get_xattr(e_id, "render", obj_name))) {
        // TODO Fragile. Use better way of passing error.
        if (res == -2) {
          connected = false;
        }
        continue;
      }

      struct object_data new_data;
      new_data.pos_valid = true;
      new_data.q_valid = true;

      double x, y, z;
      if ((res = fishbowl_comm::get_position(e_id, x, y, z))) {
        fprintf(stderr, "WARNING: Failed to get position from fishbowl "
                        "for render tag \"%s\"\n", obj_name.c_str());
        new_data.pos_valid = false;
        connected = false;
      }
      else {
        new_data.position = glm::vec3(x, y, z);
      }

      double q0, q1, q2, q3;
      if ((res = fishbowl_comm::get_orientation(e_id, q0, q1, q2, q3))) {
        fprintf(stderr, "WARNING: Failed to get orientation from fishbowl "
                        "for render tag \"%s\"\n", obj_name.c_str());
        new_data.q_valid = false;
        connected = false;
      }
      else {
        new_data.orientation = glm::fquat(q0, q1, q2, q3);
      }

      if (new_data.pos_valid || new_data.q_valid) {
        fishbowl_data[1 - ready_ind].emplace_back(obj_name, std::move(new_data));
      }
    }

    if (connected) {
      mark_new_data();
      fishbowl_data[1 - ready_ind].clear();
    }
  }

  fishbowl_comm::disconnect();
}
