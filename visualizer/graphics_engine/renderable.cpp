#include "renderable.h"

#include <glm/gtc/type_ptr.hpp>

#include "colors.h"
#include "gl_utils.h"
#include "graphics_engine.h"
#include "material.h"
#include "mesh.h"
#include "renderer.h"

int RenderableMaterial::init(const Material *material) {
  this->material = material;

  if (material) {
    texture_enabled = material->map_Kd.size();
    if (texture_enabled) {
      if (load_texture(material->map_Kd, texture_ind)) {
        fprintf(stderr, "ERROR: Texture \"%s\" could not be loaded, requested "
                        "by material \"%s\"\n",
                material->map_Kd.c_str(), material->name.c_str());
        return -1;
      }
    }
  }

  return 0;
}

void RenderableMaterial::update_renderer(Renderer *renderer, bool override_texture, bool override_alpha) const {
  if (material) {
    renderer->update_uniform("ambient_color", material->Ka);
    renderer->update_uniform("diffuse_color", material->Kd);
    renderer->update_uniform("specular_color", material->Ks);
    renderer->update_uniform("shininess", material->Ns);
    if (!override_alpha) {
      renderer->update_uniform("alpha", material->d);
    }

    if (!override_texture) {
      if (texture_enabled) {
        renderer->update_uniform("tex", texture_ind);
      }
    }
    renderer->update_uniform<GLuint>("texture_enabled", texture_enabled || override_texture);
  }
}

void Renderable::begin_init(Renderer *renderer_, int n_indices_, GLenum draw_type_) {
  renderer = renderer_;
  n_indices = n_indices_;
  draw_type = draw_type_;

  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);
}

int Renderable::add_attribute(const char *name, GLfloat *source, int n, int width,
                  GLenum usage) {
  GLuint buffer_ind;
  glGenBuffers(1, &buffer_ind);

  AttributeMeta meta { buffer_ind, n, width };
  attribute_buffer_map.emplace(name, meta);

  glBindBuffer(GL_ARRAY_BUFFER, buffer_ind);
  glBufferData(GL_ARRAY_BUFFER, n * width * sizeof(GLfloat), source, usage);

  GLint att_loc = renderer->get_att_location(name);
  if (att_loc < 0) {
    fprintf(stderr, "Could not get attribute %s in shader.\n", name);
    return -1;
  }

  glEnableVertexAttribArray(att_loc);
  glVertexAttribPointer(att_loc, width, GL_FLOAT, GL_FALSE, 0, 0);

  return 0;
}

int Renderable::add_index_buffer(GLuint *index_buffer, int n) {
  // Make the index buffer.
  GLuint i_buffer;
  glGenBuffers(1, &i_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, i_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, n * sizeof(GLuint), index_buffer,
               GL_STATIC_DRAW);

  glBindVertexArray(0);

  if (check_for_gl_error()) {
    fprintf(stderr, "ERROR: GL Error during Renderable initialization.\n");
    return -1;
  }

  return 0;
}

void Renderable::draw() const {
  glBindVertexArray(vao);
  glDrawElements(draw_type, n_indices, GL_UNSIGNED_INT, 0);
  glBindVertexArray(0);
}

int Renderable::update_attribute(std::string name, GLfloat *data) {
  auto attr = attribute_buffer_map.find(name);
  if (attr == attribute_buffer_map.end()) {
    fprintf(stderr, "ERROR: Attempted to update undefined attribute \"%s\" "
                    "of a Renderable.\n", name.c_str());
    return 1;
  }

  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, attr->second.buffer_ind);
  glBufferSubData(GL_ARRAY_BUFFER, 0,
                  attr->second.n * attr->second.width * sizeof(GLfloat), data);
  glBindVertexArray(0);

  return 0;
}

// Points renderable.
Renderable get_points_renderable(Renderer *renderer, int n_points) {
  // I don't mess with undefined behavior; initialize the arrays.
  std::vector<GLfloat> verts;
  std::vector<GLfloat> colors;
  std::vector<GLuint> inds;
  for (int i = 0; i < n_points; i++) {
    inds.push_back(i);
    verts.push_back(0); verts.push_back(0); verts.push_back(0);
    colors.push_back(1); colors.push_back(0); colors.push_back(0);
  }

  Renderable renderable;
  renderable.begin_init(renderer, n_points, GL_POINTS);
  renderable.add_attribute("in_position", verts.data(), n_points, 3, GL_DYNAMIC_DRAW);
  renderable.add_attribute("in_color", colors.data(), n_points, 3, GL_DYNAMIC_DRAW);
  renderable.add_index_buffer(inds.data(), n_points);
  return std::move(renderable);
}

Renderable get_line_renderable(Renderer *renderer) {
  GLfloat verts[2][3] = { { 0., 0., 0.}, { 0., 0., 0. } };
  GLfloat colors[6] = { WHITE, WHITE };
  GLuint inds[2] = { 0, 1 };

  Renderable renderable;
  renderable.begin_init(renderer, 2, GL_LINES);
  renderable.add_attribute("in_position", (GLfloat *)verts, 2, 3, GL_DYNAMIC_DRAW);
  renderable.add_attribute("in_color", colors, 2, 3);
  renderable.add_index_buffer(inds, 2);
  return std::move(renderable);
}

// Axis renderable.
Renderable get_axes_renderable(Renderer *renderer) {
  GLfloat verts[8][3] = { { 1., 0., 0. },{ 0., 1., 0. },{ 0., 0., 1.},
                          { -1., 0., 0.},{ 0., -1., 0.},{ 0., 0., -1.},
                          { 0., 0., 0. },{ 0., 0., 0. }};
  GLfloat colors[24] = { FADED_BLUE, FADED_BLUE, FADED_BLUE, FADED_RED, FADED_RED, FADED_RED, BLUE, RED };
  GLuint inds[12] = { 6, 0, 6, 1, 6, 2, 7, 3, 7, 4, 7, 5 };

  Renderable renderable;
  renderable.begin_init(renderer, 12, GL_LINES);
  renderable.add_attribute("in_position", (GLfloat *)verts, 8, 3);
  renderable.add_attribute("in_color", colors, 8, 3);
  renderable.add_index_buffer(inds, 12);
  return std::move(renderable);
}

// OBJ mesh renderable.
Renderable get_renderable(Mesh *mesh, Renderer *renderer) {
  auto verts = std::unique_ptr<GLfloat[]>(new GLfloat[mesh->vertices.size() * 3]);
  auto normals = std::unique_ptr<GLfloat[]>(new GLfloat[mesh->vertices.size()*3]);
  auto uvs = std::unique_ptr<GLfloat[]>(new GLfloat[mesh->vertices.size()*2]);
  auto *inds = mesh->indices.data();
  for (unsigned int i = 0; i < mesh->vertices.size(); i++) {
    for (int j = 0; j < 3; j++) {
      verts[i*3 + j] = glm::value_ptr(mesh->vertices[i].pos)[j];
      normals[i*3 + j] = glm::value_ptr(mesh->vertices[i].normal)[j];
      if (j < 2)
        uvs[i*2 + j] = glm::value_ptr(mesh->vertices[i].uv)[j];
    }
  }

  Renderable renderable;
  renderable.begin_init(renderer, mesh->indices.size(), GL_TRIANGLES);
  renderable.add_attribute("in_position", verts.get(), (int)mesh->vertices.size(), 3);
  if (mesh->has_normal) {
    renderable.add_attribute("in_normal", normals.get(), (int)mesh->vertices.size(), 3);
  }
  if (mesh->has_uv) {
    renderable.add_attribute("in_uv", uvs.get(), (int)mesh->vertices.size(), 2);
  }

  renderable.add_index_buffer(inds, mesh->indices.size());

  if (renderable.material.init(mesh->material)) {
    fprintf(stderr, "WARNING: Failed to initialize Renderable material.\n");
  }

  return std::move(renderable);
}
