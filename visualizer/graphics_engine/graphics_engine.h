#ifndef _GRAPHICS_ENGINE_H
#define _GRAPHICS_ENGINE_H

#include <ctime>
#include <functional>
#include <string>
#include <memory>
#include <vector>

#include <GL/gl.h>

#include <glm/mat4x4.hpp>
#include <glm/vec3.hpp>
#include <glm/gtc/quaternion.hpp>

#include "renderable.h"
#include "renderer.h"

class Axes;
class Renderable;
class RenderableMesh;
class Renderer;

class Light {
  public:
    glm::vec3 position;
    glm::vec3 direction;
};

class Camera : public Light {
  public:
    glm::vec3 up;
    int width;
    int height;
    float fovy; // in radians
};

class SceneObject {
  public:
    virtual void update_transformation() {}

    std::vector<std::function<uniform_request ()>> uniform_requests;
    char exclude = 0;

    // TODO Figure out a better way to do overrides.
    bool override_texture = false;
    bool override_alpha = false;
};

class ModelSceneObject : public SceneObject {
  public:
    ModelSceneObject();

    glm::mat4 transformation;
    void update_transformation() override;
    std::function<glm::vec3()> get_position;
    std::function<glm::fquat()> get_orientation;
    std::function<glm::vec3()> get_scale;
    glm::mat4 T;
    glm::mat4 R;
    glm::mat4 S;

    glm::fquat mesh_offset;
    std::string render_tag;
};

class MeshObject : public ModelSceneObject {
  public:
    MeshObject();
    void set_texture(GLuint texture);
    void set_alpha(float alpha);

    GLuint texture;
    float alpha = 1.0;
    bool texture_enabled = false;
};

// Should all be powers of two.
#define RENDER_MAIN 1
#define RENDER_SHADOW 2
#define RENDER_OFFSCREEN 4

int init_engine(const std::string &skybox_name, bool shadows);
Renderable *load_points(SceneObject *scene_object, int n_points);
Renderable *load_line(SceneObject *scene_object);
int load_axes(SceneObject *scene_object);
int load_model(const std::string &filename, SceneObject *scene_object);

void add_light(Light *light);
void add_render_target(const Camera *cam, std::function<void (unsigned char *data, timespec *acq_time)>);
void render_all(const Camera& cam);

void toggle_skybox();
void toggle_shadows();
void toggle_offscreen_rendering();

#endif
