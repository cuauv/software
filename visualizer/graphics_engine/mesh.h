#ifndef _MESH_H_
#define _MESH_H_

#include <memory>
#include <string>
#include <vector>

#include <GL/gl.h>
#include <glm/vec2.hpp>
#include <glm/vec3.hpp>

#include "stl_read.h"

typedef struct Vertex {
  glm::vec3 pos;
  glm::vec3 normal;
  glm::vec2 uv;
} Vertex;

class Material;

typedef struct Mesh {
  public:
    // The vertex buffer.
    std::vector<Vertex> vertices;
    // The index buffer.
    std::vector<GLuint> indices;

    bool has_normal;
    bool has_uv;

    const Material *material;
} Mesh;

class MeshBuilder {
  public:
    virtual std::vector<std::unique_ptr<Mesh>> build() = 0;
};

class STLBuilder : public MeshBuilder {
  public:
    explicit STLBuilder(const std::string &stl_filename);
    virtual std::vector<std::unique_ptr<Mesh>> build();

  private:
    std::string filename;
    STLFile stl_file;
};

class OBJBuilder : public MeshBuilder {
  public:
    explicit OBJBuilder(const std::string &obj_filename);
    virtual std::vector<std::unique_ptr<Mesh>> build();

  private:
    std::string filename;
};

#endif
