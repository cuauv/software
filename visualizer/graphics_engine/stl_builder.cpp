#include "mesh.h"

STLBuilder::STLBuilder(const std::string &stl_filename) : filename(stl_filename) {}

std::vector<std::unique_ptr<Mesh>> STLBuilder::build() {
  int stat = stl_file.open(filename);
  if (stat) {
    fprintf(stderr, "ERROR: Could not open file \"%s\"\n", filename.c_str());
    return std::vector<std::unique_ptr<Mesh>>();
  }

  auto mesh = std::make_unique<Mesh>();

  float points[12];
  while (stl_file.get_next_triangle(points)) {
    int k = 3; // Dimension of space.
    int n = 3; // Number of vertices.
    glm::vec3 verts[n];
    for (int i = 1; i < n + 1; i++) {
      verts[i - 1] = glm::vec3(points[k*i], points[k*i + 1], points[k*i + 2]);
    }

    for (const auto& v : verts) {
      Vertex vert;
      vert.pos = v;
      vert.normal = glm::vec3(0.0, 0.0, 0.0);
      vert.normal = glm::vec3(points[0], points[1], points[2]);
      mesh->vertices.push_back(std::move(vert));
      mesh->indices.push_back(mesh->vertices.size() - 1);
    }
  }

  mesh->has_normal = true;
  mesh->has_uv = false;

  std::vector<std::unique_ptr<Mesh>> meshes;
  meshes.emplace_back(std::move(mesh));
  return std::move(meshes);
}
