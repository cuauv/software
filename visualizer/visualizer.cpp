/*
    The CUAUV Visualizer uses OpenGL 3.x to render models in a 3D
    environment. The models are used to convey important information about
    the vehicle's state.

    TODO:
        show things other than orientation (e.g. velocity, desires, thrusters)
        more shaders
        normal mapping / bump mapping
        water
        fonts
        make a menu (press ESC)
        add sounds
        HUD
        etc.

      Technical:
        Shaders should be shared among renderers.
        Use Uniform Buffer Objects.
        Memory de allocation in the GPU.

    Resources:
        http://ogldev.atspace.co.uk/www/tutorial23/tutorial23.html
        http://fabiensanglard.net/shadowmapping/index.php
        http://en.wikibooks.org/wiki/OpenGL_Programming/Glescraft_4
        the cs 4620 course repo
        and many more...

    Authors:
      Alex Spitzer
      Daryl Sew
*/

#define VERSION "2.1"

#include <chrono>
#include <cmath>
#include <csignal>
#include <cstdio>
#include <dlfcn.h>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <GLFW/glfw3.h>
#if (GLFW_VERSION_MAJOR == 3 && GLFW_VERSION_MINOR < 2)
// Warning?
#else
#define ENABLE_FULLSCREEN_TOGGLE
#endif

#define GLM_FORCE_RADIANS
#include <glm/vec3.hpp>
#include <glm/gtc/quaternion.hpp>
#include <glm/gtx/rotate_vector.hpp>

#if (GLM_VERSION_MINOR == 9 && GLM_VERSION_PATCH <= 5)
#error "Visualizer requires GLM 0.9.6 or newer. https://github.com/g-truc/glm/releases"
#endif

#include <libconfig.h++>
#if (LIBCONFIGXX_VER_MAJOR == 1 && LIBCONFIGXX_VER_MINOR <= 4)
#error "Visualizer requires libconfig++ 1.5.0 or newer. https://github.com/hyperrealm/libconfig/releases"
#endif

#include <libshm/c/dynamic.h>
#include <libshm/c/shm.h>
#include <misc/utils.h>

#include "auv_math/camera.hpp"
#include "auv_math/libquat/quat.h"

#include "aslam_comm.h"
#include "fishbowl_manager.h"
#include "keyboard.h"
#include "point_manager.h"
#include "vision_link.h"

#include "graphics_engine/gl_utils.h"
#include "graphics_engine/graphics_engine.h"

#define BIN2DATA "../models/"     // Relative path from binary to model of sub
#define BIN2TEXTURES "../textures/"
#define BIN2CONFIG "../"
#define VISION_LINK_LIB "libvision_link.so"

#define DEFAULT_CONFIG "world.cfg"
#define DEFAULT_SKYBOX "city"
#define DEFAULT_WIDTH 512
#define DEFAULT_HEIGHT 512
#define DEFAULT_FOV 90

// Heading, pitch, and roll are in degrees.
auto quat_from_hpr(float heading, float pitch, float roll) {
  double q[4];
  hpr_to_quat(heading, pitch, roll, q);
  return glm::fquat(q[0], q[1], q[2], q[3]);
}

auto NORTH = glm::vec3(1, 0, 0);
auto UP = glm::vec3(0, 0, -1);

struct SubCamera {
  std::unique_ptr<Camera> cam;
  glm::vec3 offset_p;
  glm::fquat offset_q;
  VisionLink *vision_link;

  ModelSceneObject *parent;
};

std::vector<struct SubCamera> sub_cameras;
// Maps from camera names e.g. forward_left, forward_right, etc.
// to indices into sub_cameras.
std::unordered_map<std::string, unsigned int> camera_map;

std::unordered_map<std::string, std::unique_ptr<cuauv::dshm::Group>> shm_groups;
bool add_shm_group(const std::string &group_name,
                   std::vector<std::string> var_reqs={}) {
  if (shm_groups.find(group_name) == shm_groups.end()) {
    std::unique_ptr<cuauv::dshm::Group> shm_group_new;
    try {
      shm_group_new = cuauv::dshm::newGroup(group_name);
    }
    catch (std::invalid_argument e) {
      fprintf(stderr, "ERROR: SHM group \"%s\" doesn't exist.\n",
              group_name.c_str());
      return false;
    }

    shm_groups[group_name] = std::move(shm_group_new);
  }

  auto shm_group = shm_groups[group_name].get();
  for (const auto &var_str : var_reqs) {
    try {
      shm_group->var(var_str);
    }
    catch (std::invalid_argument e) {
      fprintf(stderr, "ERROR: SHM group \"%s\" doesn't have required "
                      "variable \"%s\".\n",
              group_name.c_str(), var_str.c_str());
      return false;
    }
  }

  return true;
}

auto get_shm_group_hpr(cuauv::dshm::Group *shm_group) {
  float heading = shm_group->var("heading")->shmDouble();
  float pitch = shm_group->var("pitch")->shmDouble();
  float roll = shm_group->var("roll")->shmDouble();
  return quat_from_hpr(heading, pitch, roll);
}

auto get_shm_group_quat(cuauv::dshm::Group *shm_group) {
  glm::fquat q;
  q.w = shm_group->var("q0")->shmDouble();
  q.x = shm_group->var("q1")->shmDouble();
  q.y = shm_group->var("q2")->shmDouble();
  q.z = shm_group->var("q3")->shmDouble();
  return q;
}

auto get_shm_group_position(cuauv::dshm::Group *shm_group) {
  return glm::vec3(shm_group->var("north")->shmDouble(),
                   shm_group->var("east")->shmDouble(),
                   shm_group->var("depth")->shmDouble());
}

auto get_shm_group_line_data(cuauv::dshm::Group *shm_group) {
  return std::make_pair(glm::vec3(shm_group->var("n0")->shmDouble(),
                                  shm_group->var("e0")->shmDouble(),
                                  shm_group->var("d0")->shmDouble()),
                        glm::vec3(shm_group->var("n1")->shmDouble(),
                                  shm_group->var("e1")->shmDouble(),
                                  shm_group->var("d1")->shmDouble()));
}

// 0 bool
// 1 int
// 2 double
std::unordered_map<cuauv::dshm::Var *, int> var_cache;
double try_int_or_double(cuauv::dshm::Var *var) {
  if (var_cache.find(var) == var_cache.end()) {
    try {
      var->shmBool();
      var_cache[var] = 0;
    }
    catch (std::exception &e) {
      try {
        var->shmInt();
        var_cache[var] = 1;
      }
      catch (std::exception &e) {
        var_cache[var] = 2;
      }
    }
  }

  if (var_cache[var] == 2) {
    return var->shmDouble();
  }
  else if (var_cache[var] == 1) {
    return var->shmInt();
  }

  return var->shmBool();
}

auto get_vision_group_line_data(cuauv::dshm::Group *shm_group,
                                std::string center_x,
                                std::string center_y,
                                std::string camera,
                                std::string probability) {
  int x = try_int_or_double(shm_group->var(center_x));
  int y = try_int_or_double(shm_group->var(center_y));
  std::string cam_name = shm_group->var(camera)->shmString();
  double prob = try_int_or_double(shm_group->var(probability));

  // TODO Use probability to change color of the line.
  if (prob <= 0 || camera_map.find(cam_name) == camera_map.end()) {
    return std::make_pair(glm::vec3(0, 0, 0), glm::vec3(0, 0, 0));
  }

  auto &cam = sub_cameras[camera_map[cam_name]];
  auto start = cam.cam->position;
  auto end = start;
  end += cam.cam->direction;

  // TODO The below should be in auv_math/camera.hpp.
  auto diff_x = x - cam.cam->width / 2;
  auto diff_y = y - cam.cam->height / 2;

  auto up_slope = std::tan(cam.cam->fovy / 2) / cam.cam->height;
  auto up_amt =   -2 * diff_y * up_slope;
  auto third_amt = 2 * diff_x * up_slope;

  end += up_amt * cam.cam->up;
  glm::vec3 third = glm::cross(cam.cam->direction, cam.cam->up);
  end += third_amt * third;

  auto direction = end - start;
  direction *= 20;
  end = start + direction;

  return std::make_pair(start, end);
}

/*glm::fquat get_force_quat() {
  glm::vec3 force = glm::vec3(shm_get_control_internal_wrench_f_y(),
                             -shm_get_control_internal_wrench_f_z(),
                              shm_get_control_internal_wrench_f_x());
  return glm::rotation(glm::normalize(force), glm::vec3(0, 0, 1));
}*/

/*glm::vec3 get_force_scale() {
  glm::vec3 force = glm::vec3(shm_get_control_internal_wrench_f_x(),
                             -shm_get_control_internal_wrench_f_z(),
                              shm_get_control_internal_wrench_f_y());
  return glm::vec3(glm::length(force), 1, 1);
}*/

std::vector<std::unique_ptr<ModelSceneObject>> scene_objects;
// Map from render tags to ModelSceneObject pointers.
std::unordered_map<std::string, ModelSceneObject *> render_tag_map;
ModelSceneObject *sub = nullptr;

// Only one SceneObject for all point and line objects.
SceneObject points_sc;
SceneObject lines_sc;
// Pointers because std::mutex is immovable...! TODO Fix.
std::vector<std::unique_ptr<PointManager>> point_managers;
aslam_comm::ASLAMComm aslam_comms;
FishbowlManager fishbowl_manager(render_tag_map);

// TODO Fuse together async_updates and point_managers.
// Should derive from the same subclass.
std::vector<std::function<void()>> async_updates;

enum State {
  MOUSE,
  FIXED,
  STATES
};

std::function<void (GLFWwindow *, double, double)> mouse_handlers[STATES];
State mouse_state;

bool first_person = false;
bool sub_follow = false;
// Used only during sub follow mode.
float cam_angle = 0.0;

bool heading_lock = false;

std::function<VisionLink *(void)> create_link;
std::function<void *(VisionLink *)> destroy_link;
std::function<std::vector<struct visualizer_camera> (void)> get_vehicle_cameras;

#define LOAD_VISION_LINK_SYMBOL(symbol, type) \
  symbol = reinterpret_cast<type>(dlsym(handle, #symbol)); \
  if (!symbol) { \
    fprintf(stderr, "ERROR: Could not load " #symbol " in vision link lib.\n");\
    return -1; \
  }

int load_vision_link_lib() {
  void *handle = dlopen((getBinDir() + VISION_LINK_LIB).c_str(), RTLD_LAZY);
  if (!handle) {
    fprintf(stderr, "ERROR: Could not load vision link lib: %s\n", dlerror());
    return -1;
  }

  LOAD_VISION_LINK_SYMBOL(create_link, VisionLink *(*)())
  LOAD_VISION_LINK_SYMBOL(destroy_link, void *(*)(VisionLink *))
  LOAD_VISION_LINK_SYMBOL(get_vehicle_cameras, std::vector<struct visualizer_camera> (*)())

  return 0;
}

void add_sub_camera(const std::string& cam_name,
                    unsigned int width, unsigned int height, float fovy,
                    glm::vec3 offset_p, glm::fquat offset_q,
                    ModelSceneObject *parent) {
  SubCamera target;
  target.cam = std::make_unique<Camera>();
  if (!target.cam) {
    fprintf(stderr, "ERROR: Could not allocate memory for Camera!\n");
    return;
  }

  target.cam->width = width;
  target.cam->height = height;
  target.cam->fovy = fovy;

  target.vision_link = create_link();
  if (!target.vision_link) {
    fprintf(stderr, "ERROR: Failed to create vision link\n");
    return;
  }

  if (target.vision_link->init(cam_name, width, height)) {
    fprintf(stderr, "WARNING: Failed to initialize vision link for camera \"%s"
                    "\"; vision output will fail.\n", cam_name.c_str());
    destroy_link(target.vision_link);
    return;
  }

  target.offset_p = offset_p;
  target.offset_q = offset_q;

  VisionLink *vlp = target.vision_link;
  add_render_target(target.cam.get(),
                    [vlp] (unsigned char *data, timespec *acq_time) {
                      vlp->post(data, acq_time);
  });

  target.parent = parent;

  camera_map[cam_name] = sub_cameras.size();
  sub_cameras.push_back(std::move(target));
}

double dt = 0;

Camera cam;
void handle_input(GLFWwindow *window) {
  static constexpr float meters_per_s = 3.0;
  float move_speed = meters_per_s * dt;

  bool shift = glfwGetKey(window, GLFW_KEY_LEFT_SHIFT) == GLFW_PRESS ||
               glfwGetKey(window, GLFW_KEY_RIGHT_SHIFT) == GLFW_PRESS;

  auto get_speed = [shift, window] (auto c) -> float {
    bool pressed = glfwGetKey(window, c);
    if (pressed && shift)
      return 0.14;
    if (pressed)
      return 1;
    return 0;
  };

  if (first_person) {
    static float off_x, off_y, off_z = 0.0;
    off_x += move_speed * get_speed(GLFW_KEY_W);
    off_x -= move_speed * get_speed(GLFW_KEY_S);
    off_y += move_speed * get_speed(GLFW_KEY_D);
    off_y -= move_speed * get_speed(GLFW_KEY_A);
    off_z += move_speed * get_speed(GLFW_KEY_E);
    off_z -= move_speed * get_speed(GLFW_KEY_Q);

    cam.direction = sub->get_orientation() * NORTH;
    cam.up = sub->get_orientation() * UP;
    auto third = glm::cross(cam.direction, cam.up);
    cam.position = sub->get_position() + cam.direction * off_x +
                   cam.up * off_z + third * off_y;
  }

  // Keep the sub centered in the view
  else if (sub_follow) {
    static float distance = -1.5;
    static float height = 0.0;

    if (distance < -0.5) {
      distance += move_speed * get_speed(GLFW_KEY_W);
    }
    distance -= move_speed * get_speed(GLFW_KEY_S);
    height += move_speed * get_speed(GLFW_KEY_Q);
    height -= move_speed * get_speed(GLFW_KEY_E);

    float starting_angle = 0.0;
    if (heading_lock) {
      // GLM Euler angles are pitch x, heading y, and roll z. Our "heading" is
      // rotation about the z axis, so we get the roll.
      // TODO This is susceptible to gimbal lock. Could use hysteresis to fix.
      starting_angle = -glm::roll(sub->get_orientation());
    }

    static constexpr float rads_per_s = 1.8;
    cam_angle -= rads_per_s * dt * get_speed(GLFW_KEY_A);
    cam_angle += rads_per_s * dt * get_speed(GLFW_KEY_D);

    cam.direction = glm::rotate(NORTH, starting_angle + cam_angle, cam.up);
    cam.position = sub->get_position() +
                   cam.direction * distance + cam.up * height;
  }

  // Free look mode
  else {
    glm::vec3 to_add = glm::vec3(0, 0, 0);
    to_add += cam.direction * get_speed(GLFW_KEY_W);
    to_add -= cam.direction * get_speed(GLFW_KEY_S);
    to_add += glm::cross(cam.up, cam.direction) * get_speed(GLFW_KEY_A);
    to_add -= glm::cross(cam.up, cam.direction) * get_speed(GLFW_KEY_D);
    to_add += cam.up * get_speed(GLFW_KEY_Q);
    to_add -= cam.up * get_speed(GLFW_KEY_E);

    cam.position += to_add * move_speed;
  }
}

double last_x;
double last_y;
void mouse_move_mode_on(GLFWwindow *window) {
  mouse_state = MOUSE;
  // Disabled mode automatically keeps the cursor centered; neat!
  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
  glfwGetCursorPos(window, &last_x, &last_y);
}

void mouse_move_mode_off(GLFWwindow *window) {
  mouse_state = FIXED;
  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL);
}

void handle_mouse_move(GLFWwindow *window, double x, double y) {
  static constexpr float move_speed = 0.003;

  float x_pix = last_x - x;
  float y_pix = last_y - y;
  last_x = x;
  last_y = y;

  float x_diff = x_pix * move_speed;
  float y_diff = y_pix * move_speed;

  if (sub_follow) {
    cam_angle -= x_diff;
  }

  // Free look mode
  else {
    glm::vec3 third = glm::cross(cam.direction, cam.up);

    // Rotate based on vertical mouse motion: pitch
    cam.direction = glm::rotate(cam.direction, y_diff, third);
    cam.up = glm::rotate(cam.up, y_diff, third);

    // Rotate based on horizontal mouse motion: heading
    cam.direction = glm::rotate(cam.direction, x_diff, UP);
    cam.up = glm::rotate(cam.up, x_diff, UP);
  }
}

Light light1;
Light light2;
void update_transformations() {
  for (const auto &object : scene_objects) {
    object->update_transformation();
  }

  // Update any cameras on any objects.
  for (auto &target : sub_cameras) {
    auto parent_q = target.parent->get_orientation();
    target.cam->position = target.parent->get_position() +
                           (parent_q * target.offset_p);

    auto cam_q = parent_q * target.offset_q;
    target.cam->direction = cam_q * NORTH;
    target.cam->up = cam_q * UP;
  }

  glm::vec3 sub_pos = glm::vec3(0, 0, 0);
  if (sub) {
    sub_pos = sub->get_position();
  }

  // The lights follow the submarine around.
  // TODO Make lights more configurable.
  sub_pos.z = 0;
  light1.position = glm::vec3(-1, 1, -11) + sub_pos;
  light2.position = glm::vec3(1, -1, -11) + sub_pos;
}

bool done = false;
void signal_handler(int param) {
  done = true;
}

std::chrono::steady_clock::time_point last_time;
void draw_loop(GLFWwindow *window) {
  while (!glfwWindowShouldClose(window) && !done) {
    auto now_time = std::chrono::steady_clock::now();
    dt = std::chrono::duration_cast<std::chrono::duration<double>>(now_time - last_time).count();
    last_time = now_time;

    for (const auto &pair : shm_groups) {
      pair.second->pull();
    }

    update_transformations();
    handle_input(window);

    for (const auto &async_update : async_updates) {
      async_update();
    }

    render_all(cam);

    glfwSwapBuffers(window);
    glfwPollEvents();
  }
}

void reshape_callback(GLFWwindow *window, int w, int h) {
  cam.width = w;
  cam.height = h;
}

void error_callback(int error, const char *description) {
  fprintf(stderr, "GLFW ERROR (%d): %s\n", error, description);
}

int main(int argc, char **argv) {
  // Read configuraiton file.
  std::string model_filename = getBinDir() + BIN2CONFIG + DEFAULT_CONFIG;
  if (argc >= 2 && std::string(argv[1])[0] != '-') {
    model_filename = argv[1];
  }

  for (int i = 0; i < argc; i++) {
    if (std::string(argv[i]) == "-v") {
      toggle_offscreen_rendering();
    }
  }

  libconfig::Config config;
  config.setOptions(libconfig::Setting::OptionAutoConvert);

  try {
    config.readFile(model_filename.c_str());
  }
  catch (libconfig::ParseException &pe) {
    fprintf(stderr, "ERROR: Failed to parse config file %s:%d %s\n",
            pe.getFile(), pe.getLine(), pe.getError());
    return -1;
  }
  catch (libconfig::FileIOException &fe) {
    fprintf(stderr, "ERROR: Failed to open config file %s\n",
            model_filename.c_str());
    return -1;
  }

  std::string skybox(DEFAULT_SKYBOX);
  config.lookupValue("skybox", skybox);

  int starting_width = DEFAULT_WIDTH;
  int starting_height = DEFAULT_HEIGHT;
  float starting_fov = DEFAULT_FOV;
  config.lookupValue("width", starting_width);
  config.lookupValue("height", starting_height);
  config.lookupValue("fov", starting_fov);

  if (starting_width < 1 || starting_width > 10000) {
    fprintf(stderr, "ERROR: Invalid screen width: %d. Must be in [1, 10000]\n",
            starting_width);
    return -1;
  }

  if (starting_height < 1 || starting_height > 10000) {
    fprintf(stderr, "ERROR: Invalid screen height: %d. Must be in [1, 10000]\n",
            starting_height);
    return -1;
  }

  if (starting_fov <= 0 || starting_fov >= 180) {
    fprintf(stderr, "ERROR: Invalid screen field of view: %f. "
                    "Must be inside (0, 180)\n", starting_fov);
    return -1;
  }

  // Create window.
  glfwSetErrorCallback(error_callback);
  if (!glfwInit()) {
    return -1;
  }

  glfwWindowHint(GLFW_SAMPLES, 4); // TODO how many samples?
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  GLFWwindow *window = glfwCreateWindow(starting_width, starting_height,
                                      "CUAUV Visualizer " VERSION, NULL, NULL);
  if (!window) {
    fprintf(stderr, "ERROR: Failed to create window.\n");
    return -1;
  }

  glfwMakeContextCurrent(window);

  cam.direction = NORTH;
  cam.up = UP;
  cam.width = starting_width;
  cam.height = starting_height;
  cam.fovy = starting_fov * M_PI / 180;

  if (init_engine(skybox, true)) {
    fprintf(stderr, "ERROR: Failed to initalize graphics engine.\n");
    return -1;
  }

  shm_init();

  // Parse configuration file.
  std::unordered_map<std::string, GLuint> texture_map;

  auto &objects = config.lookup("objects");
  for (const auto &object : objects) {
    std::string name("Poor nameless object");
    object.lookupValue("name", name);

    std::unique_ptr<ModelSceneObject> scene_object;

    bool is_sub = (object.exists("sub") && ((bool)object.lookup("sub")) == true)
                  || objects.getLength() == 1;
    if (!object.exists("model")) {
      if (object.exists("points")) {
        std::string addr("127.0.0.1");
        object.lookupValue("addr", addr);

        if (aslam_comms.connect(addr.c_str())) {
          fprintf(stderr, "WARNING: Failed to connect to ASLAM on addr %s; "
                  "no particles will be displayed.\n", addr.c_str());
          continue;
        }

        int num_points = aslam_comms.n_points;
        printf("ASLAM told us to wait for %d points!\n", num_points);
        if (num_points < 0 || num_points > 10000000) {
          fprintf(stderr, "ERROR: Invalid number of points: %d\n", num_points);
          continue;
        }
        else if (num_points > 2000000) {
          fprintf(stderr, "WARNING: ASLAM requested a very high number of "
                          "points (%d). Framerate may suffer!\n", num_points);
        }

        using namespace std::placeholders;
        fill_data_func fill_func = std::bind(&aslam_comm::ASLAMComm::fill_data,
                                             aslam_comms, _1, _2, _3);

        auto point_manager = std::make_unique<PointManager>(
          load_points(&points_sc, num_points), num_points, fill_func);
        async_updates.push_back(std::bind(&PointManager::maybe_update,
                                          point_manager.get()));
        point_managers.push_back(std::move(point_manager));
        continue;
      }

      else if (object.exists("line")) {
        if (!object.exists("group")) {
          fprintf(stderr, "ERROR: line object must specify SHM group to read "
                          "from.\n");
          continue;
        }

        std::string group_name;
        if (!object.lookupValue("group", group_name)) {
          fprintf(stderr, "ERROR: Invalid SHM group for line object on line "
                          "%d.\n", object.getSourceLine());
          continue;
        }

        bool shm_line = true;
        std::string type;
        bool proper_type = object.lookupValue("line", type);
        if (proper_type && type == "shm") {
          shm_line = true;
        }
        else if (proper_type && type == "vision") {
          shm_line = false;
        }
        else {
          fprintf(stderr, "ERROR: Invalid type for line (line %d). Options "
                          "are \"shm\" and \"vision\".\n",
                  object.getSourceLine());
          continue;
        }

        Renderable *line = nullptr;
        std::function<std::pair<glm::vec3, glm::vec3> (cuauv::dshm::Group *)> update_f;
        cuauv::dshm::Group *shm_group = nullptr;
        if (shm_line) {
          if (add_shm_group(group_name, {"n0", "e0", "d0", "n1", "e1", "d1"})) {
            shm_group = shm_groups[group_name].get();
            update_f = get_shm_group_line_data;
          }
        }

        else {
          std::string center_x = "center_x";
          std::string center_y = "center_y";
          std::string camera = "camera";
          std::string probability = "probability";
          for (auto *var : { &center_x, &center_y, &camera, &probability } ) {
            object.lookupValue(*var, *var); // This is cool!
          }

          if (add_shm_group(group_name,
                          {center_x, center_y, camera, probability})) {
            shm_group = shm_groups[group_name].get();
            using namespace std::placeholders;
            update_f = std::bind(get_vision_group_line_data, _1,
                                 center_x, center_y, camera, probability);
          }
        }

        if (shm_group) {
          line = load_line(&lines_sc);
          auto line_update_f = [line, shm_group, update_f] () {
            glm::vec3 start, end;
            std::tie(start, end) = update_f(shm_group);
            GLfloat update[6] = { start[0], start[1], start[2],
                                  end[0],   end[1],   end[2] };
            line->update_attribute("in_position", update);
          };

          async_updates.push_back(line_update_f);
        }

        if (line && object.exists("color")) {
          auto &color_att = object.lookup("color");
          if (color_att.getLength() == 3) {
            GLfloat update[6] = { color_att[0], color_att[1], color_att[2],
              color_att[0], color_att[1], color_att[2] };
            line->update_attribute("in_color", update);
          }
          else {
            fprintf(stderr, "WARNING: Color of line must be of length 3 "
                "(line %d)\n", color_att.getSourceLine());
          }
        }
        else if (!line) {
          fprintf(stderr, "ERROR: Could not add line.\n");
        }

        continue;
      }

      else {
        scene_object = std::make_unique<ModelSceneObject>();
        if (!scene_object) {
          fprintf(stderr, "ERROR: Could not allocate memory for SceneObject!\n");
          continue;
        }

        load_axes(scene_object.get());
      }
    }

    else {
      auto mesh_object = std::make_unique<MeshObject>();
      if (!mesh_object) {
        fprintf(stderr, "ERROR: Could not allocate memory for MeshObject!\n");
        continue;
      }
      std::string mesh_name = object.lookup("model");

      auto get_file_name = [] (const std::string &filename, const char *folder) {
        if (filename[0] == '/') {
          return filename;
        }
        return getBinDir() + folder + filename;
      };

      if (load_model(get_file_name(mesh_name, BIN2DATA), mesh_object.get())) {
        fprintf(stderr, "ERROR: failed to make model \"%s\".\n", mesh_name.c_str());
      }

      if (object.exists("texture")) {
        std::string texture = object.lookup("texture");
        if (texture_map.find(texture) == texture_map.end()) {
          GLuint texture_ind;
          if (load_texture(get_file_name(texture, BIN2TEXTURES), texture_ind)) {
            fprintf(stderr, "WARNING: texture \"%s\" failed to load.\n", texture.c_str());
          }

          texture_map[texture] = texture_ind;
        }

        mesh_object->set_texture(texture_map[texture]);
      }

      if (object.exists("alpha")) {
        mesh_object->set_alpha(object.lookup("alpha"));
      }

      scene_object = std::move(mesh_object);
    }

    auto grab_vec = [&object, &name] (const std::string &att_name, glm::vec3 id_v) -> std::function<glm::vec3()> {
      auto id = [id_v] { return id_v; };
      if (object.exists(att_name)) {
        auto &att = object.lookup(att_name);
        if (att.getLength() == 3) {
          glm::vec3 vec((float)att[0], (float)att[1], (float)att[2]);
          return [vec] { return vec; };
        }

        std::string att_s = att;
        if (add_shm_group(att_s, {"north", "east", "depth"})) {
          return std::bind(get_shm_group_position, shm_groups[att_s].get());
        }
        else {
          fprintf(stderr, "WARNING: Invalid attribute %s for object %s.\n",
                  att_name.c_str(), name.c_str());
        }
      }
      else {
        return id;
      }
      return id;
    };

    auto grab_orientation = [&object, &name] (const std::string &att_prefix) -> std::function<glm::fquat()> {
      auto id = [] { return quat_from_hpr(0, 0, 0); };
      if (object.exists(att_prefix + "_hpr")) {
        auto &att = object.lookup(att_prefix + "_hpr");
        if (att.getLength() == 3) {
          glm::fquat q = quat_from_hpr(att[0], att[1], att[2]);
          return [q] { return q; };
        }
        std::string att_s = att;
        if (add_shm_group(att_s, {"heading", "pitch", "roll"})) {
          return std::bind(get_shm_group_hpr, shm_groups[att_s].get());
        }
        else {
          fprintf(stderr, "ERROR: Invalid orientation_hpr for object \"%s\".\n", name.c_str());
        }
      }
      else if (object.exists(att_prefix + "_q")) {
        std::string att_s = object.lookup(att_prefix + "_q");
        if (add_shm_group(att_s, {"q0", "q1", "q2", "q3"})) {
          return std::bind(get_shm_group_quat, shm_groups[att_s].get());
        }
        else {
          fprintf(stderr, "ERROR: Invalid orientation_q for object \"%s\".\n", name.c_str());
        }
      }

      return id;
    };

    scene_object->get_position = grab_vec("position", glm::vec3(0, 0, 0));
    scene_object->get_orientation = grab_orientation("orientation");
    scene_object->mesh_offset = grab_orientation("mesh_offset")();
    scene_object->get_scale = grab_vec("scale", glm::vec3(1, 1, 1));

    if (object.exists("exclude_renders")) {
      auto &excludes = object.lookup("exclude_renders");
      for (const auto &render : excludes) {
        std::unordered_map<std::string, char> value_map = {
          {"main", RENDER_MAIN},
          {"offscreen", RENDER_OFFSCREEN},
          {"shadow", RENDER_SHADOW}
        };

        if (value_map.find(render) == value_map.end()) {
          std::string s;
          for (const auto &pair : value_map) {
            s += pair.first + " ";
          }

          fprintf(stderr, "WARNING: Invalid exclude_renders for object \"%s\". "
                          "Possible values are: %s\n", name.c_str(), s.c_str());
        }
        else {
          scene_object->exclude |= value_map[render];
        }
      }
    }

    bool vehicle_cams = false;
    if ((object.lookupValue("cameras_from_vehicle", vehicle_cams) && vehicle_cams) ||
        object.exists("camera_attachments")) {
      if (!load_vision_link_lib()) {
        if (vehicle_cams) {
          for (const auto &cam : get_vehicle_cameras()) {
            glm::fquat orientation = quat_from_hpr(cam.orientation_hpr.x,
                                                   cam.orientation_hpr.y,
                                                   cam.orientation_hpr.z);
            double fovy = cuauv::math::calc_field_of_view(cam.sensor_height,
                                                          cam.focal_length);
            add_sub_camera(cam.tag, cam.width, cam.height, fovy, cam.position,
                           orientation, scene_object.get());
          }
        }

        else if (object.exists("camera_attachments")) {
          auto &cams = object.lookup("camera_attachments");
          for (const auto &cam : cams) {
            auto &pos = cam.lookup("pos");
            auto &orient = cam.lookup("orientation");

            unsigned int width = DEFAULT_WIDTH;
            unsigned int height = DEFAULT_HEIGHT;
            float fovy = DEFAULT_FOV;
            cam.lookupValue("width", width);
            cam.lookupValue("height", height);
            cam.lookupValue("fovy", fovy);

            add_sub_camera(cam.lookup("name"), width, height, fovy * M_PI / 180,
                           glm::vec3((float)pos[0], (float)pos[1], (float)pos[2]),
                           quat_from_hpr(orient[0], orient[1], orient[2]),
                           scene_object.get());
          }
        }
      }
      else {
        fprintf(stderr, "WARNING: Loading vision link library failed; "
                        "vision output unavailable.\n");
      }
    }

    if (object.exists("render_tag")) {
      scene_object->render_tag = object.lookup("render_tag").c_str();
      render_tag_map[scene_object->render_tag] = scene_object.get();
    }

    if (is_sub) {
      sub = scene_object.get();
    }

    scene_objects.push_back(std::move(scene_object));

    if (check_for_gl_error()) {
      fprintf(stderr, "GL Error during \"%s\" object initialization.\n",
              name.c_str());
    }
  }

  if (!sub) {
    fprintf(stderr, "WARNING: no sub designated; sub follow mode will not work.\n");
  }

  bool enable_fishbowl;
  if (config.exists("fishbowl_data") &&
      config.lookupValue("fishbowl_data", enable_fishbowl) &&
      enable_fishbowl) {
    printf("Enabling asynchronous Fishbowl data transfer.\n");
    fishbowl_manager.start_update_thread();
    async_updates.push_back(std::bind(&FishbowlManager::maybe_update,
                                      &fishbowl_manager));
  }

  add_light(&light1);
  add_light(&light2);

  for (auto &point_manager : point_managers) {
    point_manager->start_update_thread();
  }

  // Points and lines don't cast shadows and don't appear in simulated vision.
  points_sc.exclude |= RENDER_SHADOW | RENDER_OFFSCREEN;
  lines_sc.exclude |= RENDER_SHADOW | RENDER_OFFSCREEN;

  glfwSetFramebufferSizeCallback(window, reshape_callback);
  glfwSetKeyCallback(window, key_callback);
  glfwSetCursorPosCallback(window, [] (GLFWwindow *w, double x, double y) {
    mouse_handlers[mouse_state](w, x, y);
  });

  glPointSize(2.0);

  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClearDepth(1.0);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  mouse_move_mode_off(window);

  mouse_handlers[MOUSE] = handle_mouse_move;
  mouse_handlers[FIXED] = [] (GLFWwindow *w, int x, int y) {};
  register_action(GLFW_KEY_M, [window] {
    if (mouse_state == MOUSE) {
      mouse_move_mode_off(window);
    }
    else {
      mouse_move_mode_on(window);
    }
  });

  register_action(GLFW_KEY_F, [] {
    if (sub) {
      sub_follow = !sub_follow;
      if (sub_follow) {
        cam.up = UP;
      }
    }
  });
  register_action(GLFW_KEY_H, [] {
    heading_lock = !heading_lock;
    if (heading_lock) {
      cam_angle = 0.0;
    }
  });

  register_action(GLFW_KEY_X, toggle_skybox);
  register_action(GLFW_KEY_Z, toggle_shadows);
  register_action(GLFW_KEY_V, toggle_offscreen_rendering);
  register_action(GLFW_KEY_ESCAPE, [window] () { mouse_move_mode_off(window); });

  register_action(GLFW_KEY_1, [] {
    sub_follow = false;
    cam.position = glm::vec3(0, 0, 0);
    cam.direction = NORTH;
    cam.up = UP;
  });

  register_action(GLFW_KEY_2, [] {
    sub_follow = false;
    cam.position = glm::vec3(0, 0, -25);
    cam.direction = glm::vec3(0, 0, 1);
    cam.up = glm::vec3(1, 0, 0);
  });

  register_action(GLFW_KEY_3, [] {
    sub_follow = false;
    cam.position = glm::vec3(0, 14, -9);
    cam.direction = glm::vec3(0, -0.707, 0.707);
    cam.up = glm::vec3(0, -0.707, -0.707);
  });

  register_action(GLFW_KEY_9, [] {
    first_person = !first_person;
    if (first_person) {
      sub_follow = false;
    }
  });

#ifdef ENABLE_FULLSCREEN_TOGGLE
  // 0 key toggles full screen mode.
  int old_x, old_y, old_w, old_h;
  register_action(GLFW_KEY_0, [window, &old_x, &old_y, &old_w, &old_h] {
    if (glfwGetWindowMonitor(window)) {
      glfwSetWindowMonitor(window, NULL, old_x, old_y, old_w, old_h, 0);
    }
    else {
      // Save the window's position and size.
      glfwGetWindowPos(window, &old_x, &old_y);
      glfwGetFramebufferSize(window, &old_w, &old_h);

      // Use the current monitor video mode.
      auto monitor = glfwGetPrimaryMonitor();
      auto vid_mode = glfwGetVideoMode(monitor);
      glfwSetWindowMonitor(window, monitor, 0, 0,
                           vid_mode->width, vid_mode->height, GLFW_DONT_CARE);
    }
  });
#endif

  int width, height;
  glfwGetFramebufferSize(window, &width, &height);
  reshape_callback(window, width, height);

  signal(SIGINT, signal_handler);
  signal(SIGTERM, signal_handler);
  signal(SIGHUP, signal_handler);

  draw_loop(window);

  // Clean up vision links.
  for (auto &target : sub_cameras) {
    destroy_link(target.vision_link);
  }

  glfwDestroyWindow(window);
  glfwTerminate();
  return 0;
}
