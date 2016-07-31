#pragma once

#include <unordered_map>
#include <vector>

#include <glm/vec3.hpp>
#include <glm/gtc/quaternion.hpp>

#include "async_manager.h"

class ModelSceneObject;

class FishbowlManager : public AsyncManager {
  public:
    FishbowlManager(std::unordered_map<std::string, ModelSceneObject *> &render_tag_map);
    void maybe_update() override;

  protected:
    void update_thread() override;

  private:
    bool connected;

    struct object_data {
      glm::vec3 position;
      bool pos_valid;
      glm::fquat orientation;
      bool q_valid;
    };

    std::unordered_map<std::string, ModelSceneObject *> &render_tag_map;

    std::vector<std::pair<std::string, struct object_data>> fishbowl_data[2];
};
