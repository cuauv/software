#include "mesh.h"

#include <algorithm>
#include <array>
#include <cstdio>
#include <fstream>
#include <sstream>
#include <unordered_map>

#include <glm/geometric.hpp>

#include "material.h"

OBJBuilder::OBJBuilder(const std::string &obj_filename) : filename(obj_filename) {}

struct ArrHasher {
  std::size_t operator()(const std::array<int, 3>& a) const {
    return a[0] + 19751 * a[1] + 99969 * a[2];
  }
};

std::vector<std::unique_ptr<Mesh>> OBJBuilder::build() {
  std::ifstream file;
  file.open(filename, std::ios::in);
  if (!file.is_open()) {
    fprintf(stderr, "ERROR: Could not open file \"%s\"\n", filename.c_str());
    return std::vector<std::unique_ptr<Mesh>>();
  }

  std::vector<glm::vec3> positions;
  std::vector<glm::vec3> normals;
  std::vector<glm::vec2> uvs;

  std::string line;
  std::string token;
  std::unordered_map<std::array<int, 3>, int, ArrHasher> vertex_map;

  MaterialLibrary mtllib;

  std::unordered_map<const Material *, std::unique_ptr<Mesh>> material_mesh_map;

  const Material *material_in_use = mtllib.get_default();
  auto first_mesh = std::make_unique<Mesh>();
  first_mesh->material = material_in_use;
  material_mesh_map[material_in_use] = std::move(first_mesh);

  unsigned int line_number = 0;
  unsigned int small_faces = 0;
  unsigned int small_faces_first_line;
  while (std::getline(file, line)) {
    std::istringstream iss(line);
    line_number++;

    auto put_vec = [] (std::istringstream& iss, std::vector<glm::vec3>& vecs) {
      glm::vec3 v;
      iss >> v.x >> v.y >> v.z;
      vecs.push_back(v);
    };

    iss >> token;
    if (token == "mtllib") {
      iss >> token;
      std::string mtl_filename;
      int end_ind = line.size();
      if (line[line.size() - 1] == '\r') {
        end_ind--;
      }

      int start_ind = line.find(token);
      token = line.substr(start_ind, end_ind - start_ind);
      std::string mtl_file = filename.substr(0, filename.find_last_of('/')+1) + token;
      if (mtllib.add_material_library(mtl_file)) {
        fprintf(stderr, "ERROR: failed to import material library \"%s\" from "
                        "OBJ file \"%s\" on line %u.\n",
                mtl_file.c_str(), filename.c_str(), line_number);
      }
    }
    else if (token == "usemtl") {
      iss >> token;
      material_in_use = mtllib.get_material(token);
      if (!material_in_use) {
        fprintf(stderr, "ERROR: OBJ file \"%s\" uses material \"%s\" that is "
                        "not found; line %u\n",
                filename.c_str(), token.c_str(), line_number);
      }

      if (material_mesh_map.find(material_in_use) == material_mesh_map.end()) {
        auto new_mesh = std::make_unique<Mesh>();
        new_mesh->material = material_in_use;
        material_mesh_map[material_in_use] = std::move(new_mesh);
      }
    }
    else if (token == "v") {
      put_vec(iss, positions);
    }
    else if (token == "vn") {
      put_vec(iss, normals);
    }
    else if (token == "vt") {
      glm::vec2 uv;
      iss >> uv.x >> uv.y;
      uvs.push_back(uv);
    }
    else if (token == "f") {
      std::vector<int> gl_indices;
      Mesh *mesh = material_mesh_map[material_in_use].get();
      while (iss >> token) {
        std::istringstream token_stream(token);
        std::string index_s;
        // Vertex, UV, Normal. -1 if not present.
        std::array<int, 3> indices = { -1, -1, -1 };
        for (int i = 0; i < 3; i++) {
          if (std::getline(token_stream, index_s, '/') && index_s.size()) {
            indices[i] = std::stoi(index_s);
          }
        }

        if (vertex_map.find(indices) == vertex_map.end()) {
          auto safe_set = [this, &indices, line_number]
                          (auto &pos, auto &vec, int index_ind) {
            int index = indices[index_ind];
            if (index > 0) {
              if (index - 1 < (int)vec.size()) {
                pos = vec[index - 1];
                return true;
              }
              else {
                fprintf(stderr, "WARNING: OBJ file \"%s\" references vertex at "
                                "index %d, which is not defined, line %u\n",
                                filename.c_str(), index, line_number);
              }
            }

            return false;
          };

          Vertex new_vert;
          safe_set(new_vert.pos, positions, 0);
          mesh->has_uv = safe_set(new_vert.uv, uvs, 1) ? true : mesh->has_uv;
          mesh->has_normal = safe_set(new_vert.normal, normals, 2) ? true : mesh->has_normal;
          mesh->vertices.push_back(new_vert);
          vertex_map.insert({ indices, mesh->vertices.size() - 1 });
        }

        gl_indices.push_back(vertex_map[indices]);
      }

      if (gl_indices.size() < 3) {
        if (!small_faces) {
          small_faces_first_line = line_number;
        }

        small_faces++;
        continue;
      }

      // Alert of concave faces.
      // TODO Implement ear clipping triangulation to support concave faces.
      glm::vec3 normal_dir(0, 0, 0);
      for (unsigned int i = 0; i < gl_indices.size(); i++) {
        auto vl = mesh->vertices[gl_indices[(i - 1) % gl_indices.size()]];
        auto vc = mesh->vertices[gl_indices[i]];
        auto vr = mesh->vertices[gl_indices[(i + 1) % gl_indices.size()]];
        auto normal_here = glm::cross(vl.pos - vc.pos, vr.pos - vc.pos);
        if (glm::dot(normal_here, normal_dir) < 0) {
          fprintf(stderr, "WARNING: OBJ \"%s\" has a concave face on line"
                          " %u; this is not supported. Please consider "
                          "triangulating faces during OBJ export.\n",
                  filename.c_str(), line_number);
          break;
        }
        normal_dir = normal_here;
      }

      // Pick one vertex and draw diagonals to triangulate.
      // We assume the face is convex.
      for (unsigned int i = 0; i < gl_indices.size() - 2; i++) {
        mesh->indices.push_back(gl_indices[0]);
        mesh->indices.push_back(gl_indices[i + 1]);
        mesh->indices.push_back(gl_indices[i + 2]);
      }
    }
  }

  material_library_save(std::move(mtllib));

  if (small_faces) {
    fprintf(stderr, "WARNING: OBJ \"%s\" has %u faces with less than 3 vertices"
                    " starting at line %u; this is not supported. If you are "
                    "using Blender, consider not including edges during the OBJ"
                    " export.\n",
            filename.c_str(), small_faces, small_faces_first_line);
  }

  std::vector<std::unique_ptr<Mesh>> meshes_found;
  for (auto &key_value : material_mesh_map) {
    if (key_value.second->vertices.size()) {
      meshes_found.push_back(std::move(key_value.second));
    }
  }

  // Sort in attempt to avoid alpha blending issues.
  // Opaque objects (alpha = 1) need to be drawn first so that
  // transparent objects blend correctly with everything behind them.
  std::sort(meshes_found.begin(), meshes_found.end(),
    [] (const auto &a, const auto &b) {
      return a->material->d > b->material->d;
    }
  );

  return std::move(meshes_found);
}
