#include "graphics_engine.h"
#include "renderer.h"

#include <glm/vec3.hpp>
#include <glm/gtc/quaternion.hpp>
#include <glm/gtx/transform.hpp>

ModelSceneObject::ModelSceneObject() {
  get_position = [] { return glm::vec3(0, 0, 0); };
  get_orientation = [] { return glm::fquat(1, 0, 0, 0); };
  get_scale = [] { return glm::vec3(1, 1, 1); };
  mesh_offset = glm::fquat(1, 0, 0, 0);

  // TODO Pass in renderer just like Material works! Should simplify things...
  uniform_requests.push_back([this] () {
    return make_uniform_request("model_to_world", transformation);
  });

}

void ModelSceneObject::update_transformation() {
  T = glm::translate(get_position());
  R = glm::mat4_cast(get_orientation() * mesh_offset);
  S = glm::scale(get_scale());
  transformation = T * R * S;
}

MeshObject::MeshObject() {
  uniform_requests.push_back([this] () {
    return make_uniform_request("tex", texture);
  });
  uniform_requests.push_back([this] () {
    return make_uniform_request<GLuint>("texture_enabled", texture_enabled);
  });
  uniform_requests.push_back([this] () {
    return make_uniform_request("alpha", alpha);
  });
}

void MeshObject::set_texture(GLuint texture) {
  this->texture = texture;
  texture_enabled = true;
  override_texture = true;
}

void MeshObject::set_alpha(float alpha) {
  this->alpha = alpha;
  override_alpha = true;
}
