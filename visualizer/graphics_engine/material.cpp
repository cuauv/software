#include "material.h"

#include <fstream>
#include <sstream>
#include <unordered_set>
#include <vector>

#include <glm/geometric.hpp>

Material::Material(const std::string &name) : name(name) {}

const Material *MaterialLibrary::get_default() {
  default_material = std::make_unique<Material>();
  return default_material.get();
}


int MaterialLibrary::add_material_library(const std::string &filename) {
  std::ifstream file;
  file.open(filename, std::ios::in);
  if (!file.is_open()) {
    fprintf(stderr, "ERROR: Could not open file \"%s\"\n", filename.c_str());
    return -1;
  }

  std::string line;
  Material *current_mtl = nullptr;
  unsigned int line_number = 0;
  while (std::getline(file, line)) {
    line_number++;
    if (!line.size() || line[0] == '#' || line[0] == '\r') {
      continue;
    }

    std::string word;
    std::istringstream iss(line);
    iss >> word;

    std::unordered_set<std::string> bump_map_set = {
      "bump", "map_Bump", "map_Disp", "B", "bump", "disp"
    };

    auto parse_vec = [&iss, &filename, line_number] (glm::vec3 &vec) {
      std::string word;
      iss >> vec.x;
      if (iss >> vec.y) {
        if (!(iss >> vec.z)) {
          fprintf(stderr, "ERROR: Bad vector value in \"%s\" line %u\n",
                  filename.c_str(), line_number);
        }
      }
      else {
        vec.y = vec.z = vec.x;
      }
    };

    if (word == "newmtl") {
      iss >> word;
      if (material_map.find(word) != material_map.end()) {
        fprintf(stderr, "WARNING: Material file \"%s\" contains duplicate "
                        "material \"%s\" on line %u\n",
                filename.c_str(), word.c_str(), line_number);
      }
      else {
        material_map.emplace(std::make_pair(word, std::make_unique<Material>(word)));
      }

      current_mtl = material_map[word].get();
    }
    else if (current_mtl == nullptr) {
      fprintf(stderr, "WARNING: Material file \"%s\" describes material before "
                      "naming it; line %u\n", filename.c_str(), line_number);
    }
    else if (word == "Ns" || word == "ns") {
      iss >> current_mtl->Ns;
    }
    else if (word == "Ka" || word == "ka") {
      parse_vec(current_mtl->Ka);
    }
    else if (word == "Kd" || word == "kd") {
      parse_vec(current_mtl->Kd);
    }
    else if (word == "Ks" || word == "ks") {
      parse_vec(current_mtl->Ks);
    }
    else if (word == "Ke" || word == "ke") {
      glm::vec3 emissive_color;
      parse_vec(emissive_color);
      if (glm::length(emissive_color) > 0) {
        fprintf(stderr, "WARNING: Unsupported emissive color (%f, %f, %f) in "
                        "material file \"%s\" line %u\n",
                emissive_color.x, emissive_color.y, emissive_color.z,
                filename.c_str(), line_number);
      }
    }
    else if (word == "d") {
      iss >> current_mtl->d;
    }
    else if (word == "Ni") {
      float Ni;
      iss >> Ni;
      if (Ni != 1.0) {
        fprintf(stderr, "WARNING: Unsupported optical density %f in material "
                        "file \"%s\" line %u\n",
                Ni, filename.c_str(), line_number);
      }
    }
    else if (word == "illum") {
      iss >> word;
      if (word != "2") {
        fprintf(stderr, "WARNING: Unsupported illum \"%s\" in material file "
                        "\"%s\" line %u\n",
                word.c_str(), filename.c_str(), line_number);
      }
    }
    else if (word == "map_Kd" || word == "map_kd") {
      // Handle CRLF line terminators and whitespace
      int end_ind = line.size();
      if (line[line.size() - 1] == '\r') {
        end_ind--;
      }

      std::string texture_file;
      iss >> texture_file;

      int start_ind = line.find(texture_file);
      texture_file = line.substr(start_ind, end_ind - start_ind);

      // Relative from the material file's location
      if (texture_file[0] != '/') {
        texture_file = filename.substr(0, filename.find_last_of('/')+1) + texture_file;
      }

      current_mtl->map_Kd = texture_file;
    }
    else if (bump_map_set.find(word) != bump_map_set.end()) {
      iss >> word;
      // TODO Figure out what a dot means in Blender exports.
      if (word != ".") {
        fprintf(stderr, "WARNING: Bump and displacement mapping is not supp "
                        "orted; requested by material file \"%s\" line %u\n",
                filename.c_str(), line_number);
      }
    }
    else if (word == "Tf" || word == "tf") {
      float Tf;
      iss >> Tf;
      if (Tf != 1.0) {
        fprintf(stderr, "WARNING: Unsupported transmission filter %f in "
                        "material file \"%s\" line %u\n",
                Tf, filename.c_str(), line_number);
      }
    }
    else {
      fprintf(stderr, "WARNING: Unsupported material property \"%s\" in "
                      "material file \"%s\" line %u\n",
              word.c_str(), filename.c_str(), line_number);
    }
  }

  return 0;
}

const Material *MaterialLibrary::get_material(const std::string &name) const {
  if (material_map.find(name) != material_map.end()) {
    return material_map.at(name).get();
  }
  return nullptr;
}

std::vector<MaterialLibrary> lib_dump;
void material_library_save(MaterialLibrary mtllib) {
  lib_dump.push_back(std::move(mtllib));
}
