#ifndef _MATERIAL_H
#define _MATERIAL_H

#include <memory>
#include <string>
#include <unordered_map>

#include <glm/vec3.hpp>

#include "renderer.h"

class Material {
  friend class MaterialLibrary;
  friend class RenderableMaterial;

  public:
    Material() {}
    explicit Material(const std::string &name);

    // Exposed for alpha sorting.
    float d = 1.0;   // Alpha value

  private:
    glm::vec3 Ka = glm::vec3(1, 1, 1);       // Ambient color
    glm::vec3 Kd = glm::vec3(1, 1, 1);       // Diffuse color
    glm::vec3 Ks = glm::vec3(0.4, 0.4, 0.4); // Specular color
    float Ns = 97.0; // Specular coefficient
    std::string map_Kd = "";

    std::string name;
};

class MaterialLibrary {
  public:
    const Material *get_default();
    int add_material_library(const std::string &filename);
    const Material *get_material(const std::string &name) const;

  private:
    std::unique_ptr<Material> default_material;
    std::unordered_map<std::string, std::unique_ptr<Material>> material_map;
};

void material_library_save(MaterialLibrary mtllib);

#endif
