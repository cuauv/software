#ifndef _RENDERER_H_
#define _RENDERER_H_

#include <functional>
#include <memory>
#include <unordered_map>
#include <string>
#include <vector>

#include <GL/gl.h>

std::pair<std::string, std::function<void (GLint loc)>> typedef uniform_request;

template<typename T>
uniform_request make_uniform_request(const std::string &name, T v);

class Renderable;
class SceneObject;

class Renderer {
  public:
    int init(const std::string &v_shader, const std::string &f_shader);

    GLint get_att_location(const char *name) const;

    template<typename T>
    void update_uniform(const std::string &name, T value);

    void add_object(SceneObject *object, std::vector<Renderable> *renderables);
    void render_all(char render_type);

  private:
    GLint get_uni_location(const std::string &name);

    // Maps uniforms to their locations.
    std::unordered_map<std::string, GLint> location_map;
    // Any globally set uniforms that need to be updated before rendering.
    std::vector<uniform_request> to_update;
    // All of the objects registered to be rendered with this renderer.
    std::vector<std::pair<SceneObject *, std::vector<Renderable> *>> objects_to_render;

    GLuint shader_prog;
};

#endif
