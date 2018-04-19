#include "graphics_engine.h"

#include <unordered_map>

#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/quaternion.hpp>
#include <glm/gtx/rotate_vector.hpp>

#include "misc/utils.h"

#include "gl_utils.h"
#include "mesh.h"
#include "render_buffer.h"
#include "renderable.h"
#include "renderer.h"
#include "shadow_map.h"
#include "skybox.h"

#define BIN2SHADERS "../shaders/" // Relative path from binary to shader sources
#define BIN2DATA "../models/"     // Relative path from binary to model of sub
#define BIN2TEXTURES "../textures/"
#define BIN2SKYBOX "../textures/skybox/"

bool skybox_enabled = true;
bool shadows_enabled = true;
bool offscreen_enabled = false;

Renderer phong;
Renderer color_direct;
Renderer points;
Renderer skybox_shader;
ModelSceneObject skybox_scene_object;

// There is only one skybox, so we can use the same active texture
// as a GL_TEXTURE_2D. (Skybox uses GL_TEXTURE_CUBE_MAP)
constexpr GLuint skybox_texture_index = 0;

std::vector<std::pair<Light *, ShadowMap>> lights;

struct RenderTarget {
  const Camera *cam;
  RenderBuffer render_buffer;
  timespec acq_time;
  std::function<void (unsigned char *data, timespec *acq_time)> image_callback;
};

std::vector<struct RenderTarget> render_targets;
// Here because we want memory to live. TODO fix.
std::vector<std::unique_ptr<std::vector<Renderable>>> renderable_clumps;

int init_engine(const std::string &skybox_name, bool shadows_on) {
  struct shader_config {
    Renderer *renderer;
    std::string v_shader;
    std::string f_shader;
  };

  std::vector<struct shader_config> configs =
    { { &phong, "phong", "phong" },
      { &color_direct, "phong", "color_direct" },
      { &points, "dummy", "color_direct" }
    };

  if (skybox_name.size()) {
    configs.push_back(
      { &skybox_shader, "skybox", "skybox" }
    );
  }

  std::string shaders_dir = getBinDir() + BIN2SHADERS;

  for (const auto &config : configs) {
    if (config.renderer->init(shaders_dir + config.v_shader + ".vert",
                              shaders_dir + config.f_shader + ".frag")) {
      fprintf(stderr, "Could not initialize shader (%s, %s).\n",
              config.v_shader.c_str(), config.f_shader.c_str());
      return -1;
    }
  }

  if (shadows_on) {
    shadows_enabled = true;
  }

  // Create skybox.
  if (skybox_name.size()) {
    skybox_enabled = true;
    auto skybox_mesh = OBJBuilder(getBinDir() + BIN2DATA + "skybox.obj").build();
    if (!skybox_mesh.size()) {
      fprintf(stderr, "WARNING: Could not build skybox.\n");
      return 0;
    }
    skybox_mesh[0]->material = nullptr;
    auto skybox = get_renderable(skybox_mesh[0].get(), &skybox_shader);
    auto skybox_dir = getBinDir() + BIN2SKYBOX + skybox_name + "/";
    create_cube_map(
        (skybox_dir + "negz.jpg").c_str(),
        (skybox_dir + "posz.jpg").c_str(),
        (skybox_dir + "posy.jpg").c_str(),
        (skybox_dir + "negy.jpg").c_str(),
        (skybox_dir + "negx.jpg").c_str(),
        (skybox_dir + "posx.jpg").c_str(),
        GL_TEXTURE0 + skybox_texture_index
    );
    renderable_clumps.push_back(std::make_unique<std::vector<Renderable>>());
    renderable_clumps[renderable_clumps.size() - 1]->push_back(std::move(skybox));
    skybox_shader.add_object(
      &skybox_scene_object,
      renderable_clumps[renderable_clumps.size() - 1].get()
    );

    // Skybox shouldn't cast shadows.
    skybox_scene_object.exclude = RENDER_SHADOW;
  }

  return 0;
}

template <class T>
std::unique_ptr<MeshBuilder> meta_make(const std::string& filename) {
  return std::make_unique<T>(filename);
};

std::unique_ptr<MeshBuilder> dispatch_builder(const std::string& filename) {
  std::unordered_map<std::string, std::function<std::unique_ptr<MeshBuilder>(const std::string &)>> dispatch_map = {
    { "obj", meta_make<OBJBuilder> },
    { "stl", meta_make<STLBuilder> }
  };

  std::string index("stl");
  if (filename.size() >= 3) {
    std::string suffix = filename.substr(filename.size() - 3);
    if (dispatch_map.find(suffix) == dispatch_map.end()) {
      fprintf(stderr, "WARNING: Could not deduce \"%s\" type, assuming STL.\n",
              filename.c_str());
    }
    else {
      index = suffix;
    }
  }

  return dispatch_map[index](filename);
}

Renderable *insert_one(SceneObject *scene_object, Renderable renderable, Renderer &renderer) {
  renderable_clumps.push_back(std::make_unique<std::vector<Renderable>>());
  auto &latest_clump = renderable_clumps[renderable_clumps.size() - 1];
  latest_clump->push_back(std::move(renderable));
  renderer.add_object(scene_object, latest_clump.get());
  // Return the address of the renderable's final resting place.
  return &(latest_clump->at(0));
}

Renderable *load_line(SceneObject *scene_object) {
  auto renderable = get_line_renderable(&points);
  return insert_one(scene_object, renderable, points);
}

Renderable *load_points(SceneObject *scene_object, int n_points) {
  auto renderable = get_points_renderable(&points, n_points);
  return insert_one(scene_object, renderable, points);
}

int load_axes(SceneObject *scene_object) {
  auto renderable = get_axes_renderable(&color_direct);
  insert_one(scene_object, renderable, color_direct);
  return 0;
}

std::unordered_map<std::string, std::vector<Renderable> *> obj_cache;
int load_model(const std::string &filename, SceneObject *scene_object) {
  if (obj_cache.find(filename) == obj_cache.end()) {
    auto builder = dispatch_builder(filename);
    auto meshes_found = builder->build();
    if (!meshes_found.size()) {
      fprintf(stderr, "ERROR: No meshes found in \"%s\".\n", filename.c_str());
      return -1;
    }

    for (const auto &mesh : meshes_found) {
      if (!mesh->vertices.size()) {
        fprintf(stderr, "ERROR: \"%s\" contains empty meshes.\n", filename.c_str());
        return -1;
      }
    }

    auto renderables = std::make_unique<std::vector<Renderable>>();
    for (auto &mesh : meshes_found) {
      if (!mesh->has_normal) {
        fprintf(stderr, "WARNING: \"%s\" has mesh without normals; color "
                        "will be incorrect.\n", filename.c_str());
      }
      renderables->emplace_back(std::move(get_renderable(mesh.get(), &phong)));
    }
    obj_cache[filename] = renderables.get();
    renderable_clumps.push_back(std::move(renderables));
  }

  phong.add_object(scene_object, obj_cache[filename]);
  return 0;
}

void add_light(Light *light) {
  ShadowMap shadow_map(GL_TEXTURE0 + get_next_texture());
  if (shadow_map.init()) {
    fprintf(stderr, "WARNING: Failed to initialize shadow map for light.\n");
  }

  lights.push_back(std::make_pair(light, shadow_map));
}

void add_render_target(const Camera *cam,
                     std::function<void (unsigned char *data, timespec *acq_time)> image_callback) {
  RenderTarget target;
  target.cam = cam;
  target.render_buffer.init(cam->width, cam->height);
  target.image_callback = image_callback;

  render_targets.push_back(std::move(target));
}

glm::mat4 compute_mvp_matrix(glm::vec3 pos, glm::vec3 direction, glm::vec3 up,
                             float aspect_ratio, float fovy) {
  return glm::infinitePerspective(fovy, aspect_ratio, 0.1f) *
         glm::lookAt(pos, pos + direction, up);
}

glm::mat4 compute_mvp_matrix(const Camera& cam) {
  return compute_mvp_matrix(cam.position, cam.direction, cam.up,
                            (float)cam.width / cam.height, cam.fovy);
}

void render_scene(const Camera& cam, bool skybox_enabled, char render_type) {
  glClear(GL_DEPTH_BUFFER_BIT);
  glViewport(0, 0, cam.width, cam.height);

  glm::mat4 view_proj_mat = compute_mvp_matrix(cam);

  // Draw the skybox first.
  skybox_shader.update_uniform("enabled", (GLuint)skybox_enabled);
  skybox_shader.update_uniform("world_to_cam", view_proj_mat);
  skybox_shader.update_uniform("cube_texture", skybox_texture_index);
  // Skybox should follow the camera.
  skybox_scene_object.transformation = glm::translate(cam.position);

  glDepthMask(GL_FALSE);
  skybox_shader.render_all(render_type);
  glDepthMask(GL_TRUE);

  phong.update_uniform("cam_pos", cam.position);
  phong.update_uniform("world_to_cam", view_proj_mat);
  color_direct.update_uniform("world_to_cam", view_proj_mat);
  points.update_uniform("world_to_cam", view_proj_mat);

  points.render_all(render_type);
  color_direct.render_all(render_type);
  phong.render_all(render_type);
}

void render_all(const Camera& main_cam) {
  phong.update_uniform("shadows_enabled", (GLuint)shadows_enabled);
    // Compute shadow maps by rendering to textures from light perspectives.
  for (unsigned int i = 0; i < lights.size(); i++) {
    std::string ind_s = "[" + std::to_string(i) + "]";
    phong.update_uniform("light_pos" + ind_s, lights[i].first->position);
    if (shadows_enabled) {
      lights[i].second.bind_for_writing();

      Camera light_cam;
      light_cam.position = lights[i].first->position;
      light_cam.direction = glm::vec3(0, 0, 1);
      light_cam.fovy = 0.35f;
      light_cam.width = lights[i].second.width;
      light_cam.height = lights[i].second.height;
      light_cam.up = glm::vec3(0, 1, 0);
      glm::mat4 light_mvp = compute_mvp_matrix(light_cam);

      // It's harmless to set these uniforms here.
      phong.update_uniform("world_to_light" + ind_s, light_mvp);
      phong.update_uniform("shadow_map" + ind_s, lights[i].second.texture_unit - GL_TEXTURE0);

      render_scene(light_cam, false, RENDER_SHADOW);
    }
  }

  // Render to the main screen.
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  if (shadows_enabled) {
    phong.update_uniform("light_count", (GLuint)lights.size());
    for (auto& pair : lights) {
      pair.second.bind_for_reading();
    }
  }

  render_scene(main_cam, skybox_enabled, RENDER_MAIN);

  // Render to any targets, e.g. vision.
  if (offscreen_enabled) {
    for (auto& target : render_targets) {
      void *data = target.render_buffer.get_data();
      target.image_callback((unsigned char *)data, &target.acq_time);
      target.render_buffer.data_cleanup();
    }

    for (auto& target : render_targets) {
      target.render_buffer.bind_for_writing();
      render_scene(*target.cam, skybox_enabled, RENDER_OFFSCREEN);

      // Store the time the image was acquired.
      clock_gettime(CLOCK_REALTIME, &target.acq_time);

      // Initiate GPU to CPU DMA transfer!
      target.render_buffer.bind_for_reading();
    }
  }
}

void toggle_skybox() {
  skybox_enabled = not skybox_enabled;
}

void toggle_shadows() {
  shadows_enabled = not shadows_enabled;
}

void toggle_offscreen_rendering() {
  offscreen_enabled = not offscreen_enabled;
}
