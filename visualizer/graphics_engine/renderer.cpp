#include "renderer.h"

#include "gl_utils.h"
#include "graphics_engine.h"
#include "material.h"
#include "renderable.h"

#include <glm/mat4x4.hpp>
#include <glm/vec3.hpp>
#include <glm/gtc/type_ptr.hpp>

void update_uniform(GLint loc, GLuint value) {
  glUniform1i(loc, value);
}

void update_uniform(GLint loc, float value) {
  glUniform1f(loc, value);
}

void update_uniform(GLint loc, glm::vec3 vec) {
  glUniform3fv(loc, 1, glm::value_ptr(vec));
}

void update_uniform(GLint loc, glm::mat4 mat) {
  glUniformMatrix4fv(loc, 1, GL_FALSE, glm::value_ptr(mat));
}

template<typename T>
uniform_request make_uniform_request(const std::string &name, T v) {
  // For some reason GCC 5.1.0 cannot deduce the template argument for the
  // overloaded function "update_uniform" below.
  return std::move(std::make_pair(name, std::bind<void(GLint, T)>(update_uniform, std::placeholders::_1, v)));
}

// Need to instantiate these for "scene_object.cpp".
template uniform_request make_uniform_request(const std::string &name, GLuint value);
template uniform_request make_uniform_request(const std::string &name, float value);
template uniform_request make_uniform_request(const std::string &name, glm::vec3 value);
template uniform_request make_uniform_request(const std::string &name, glm::mat4 value);

int Renderer::init(const std::string &v_shader, const std::string &f_shader) {
  GLuint vert_shader = make_shader(GL_VERTEX_SHADER, v_shader);
  GLuint frag_shader = make_shader(GL_FRAGMENT_SHADER, f_shader);
  if (!vert_shader || !frag_shader) {
    return -1;
  }

  shader_prog = make_program({ vert_shader, frag_shader });
  if (!shader_prog) {
    return -1;
  }

  return 0;
}

GLint Renderer::get_uni_location(const std::string &name) {
  auto it = location_map.find(name);
  if (it == location_map.end()) {
    GLint loc = glGetUniformLocation(shader_prog, name.c_str());
    if (loc < 0) {
      fprintf(stderr, "WARNING: Could not get location of uniform \"%s\"\n",
              name.c_str());
    }

    location_map[name] = loc;
    return loc;
  }

  return it->second;
}

GLint Renderer::get_att_location(const char* name) const {
  return glGetAttribLocation(shader_prog, name);
}

template <typename T>
void Renderer::update_uniform(const std::string &name, T value) {
  to_update.push_back(make_uniform_request(name, value));
}

// Need to instantiate these for "graphics_engine.cpp".
template void Renderer::update_uniform(const std::string &name, GLuint value);
template void Renderer::update_uniform(const std::string &name, float value);
template void Renderer::update_uniform(const std::string &name, glm::vec3 value);
template void Renderer::update_uniform(const std::string &name, glm::mat4 value);

void Renderer::render_all(char render_type) {
  glUseProgram(shader_prog);

  auto handle_req = [this] (uniform_request pair) {
    pair.second(get_uni_location(pair.first));
  };

  // Update any uniforms set globally for the render pass.
  for (const auto &pair : to_update) {
    handle_req(pair);
  }
  to_update.clear();

  for (auto &pair : objects_to_render) {
    if (pair.first) {
      if (pair.first->exclude & render_type) {
        continue;
      }

      // Update any uniforms set by this particular object, e.g. texture.
      for (const auto &req_pair : pair.first->uniform_requests) {
        handle_req(req_pair());
      }
    }

    bool override_texture = pair.first && pair.first->override_texture;
    bool override_alpha = pair.first && pair.first->override_alpha;
    for (const auto &renderable : *pair.second) {
      renderable.material.update_renderer(this, override_texture, override_alpha);
      for (const auto &req_pair : to_update) {
        handle_req(req_pair);
      }
      to_update.clear();

      renderable.draw();
    }
  }

  glUseProgram(0);
}

void Renderer::add_object(SceneObject *object, std::vector<Renderable> *renderables) {
  objects_to_render.emplace_back(object, renderables);
}
