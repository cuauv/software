#ifndef _RENDERABLE_H_
#define _RENDERABLE_H_

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <GL/gl.h>

class Material;
class Mesh;
class Renderer;

class RenderableMaterial {
  public:
    int init(const Material *material);
    void update_renderer(Renderer *renderer, bool override_texture, bool override_alpha) const;

  private:
    bool texture_enabled;
    GLuint texture_ind;
    const Material *material = nullptr;
};

struct AttributeMeta {
  GLuint buffer_ind;
  int n;
  int width;
};

class Renderable {
  public:
    void begin_init(Renderer *renderer_, int n_indices_, GLenum draw_type_);
    int add_attribute(const char *name, GLfloat *source, int n, int width,
                      GLenum usage=GL_STATIC_DRAW);
    int add_index_buffer(GLuint *index_buffer, int n);
    void draw() const;

    int update_attribute(std::string name, GLfloat *data);

    Renderer *renderer;
    RenderableMaterial material;

  protected:
    GLuint vao;

  private:
    GLenum draw_type;
    int n_indices;
    std::unordered_map<std::string, AttributeMeta> attribute_buffer_map;
};

Renderable get_points_renderable(Renderer *renderer, int n_points);
Renderable get_line_renderable(Renderer *renderer);
Renderable get_axes_renderable(Renderer *renderer);
Renderable get_renderable(Mesh *mesh, Renderer *renderer);
#endif
