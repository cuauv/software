#include "stl_read.h"

#include <string.h>

/*
 STL binary format

 80 byte header
 UINT32 - Number of triangles
 <foreach triangle>
 FLOAT32[3] - Normal
 FLOAT32[3] - Vertex 1
 FLOAT32[3] - Vertex 2
 FLOAT32[3] - Vertex 3
 UINT16 - Attribute byte count (???)
 <endfor>
*/

int STLFile::open(const std::string& filename) {
  file.open(filename, std::ios::in | std::ios::binary);
  if (!file.is_open()) {
    return -1;
  }

  char first_six[6] = {0};
  file.read(first_six, 5);
  ascii = !strcmp(first_six, "solid");

  if (!ascii) {
    file.seekg(80);
    file.read((char *) &N, 4);
  }
  else {
    // Eat the name.
    std::string line;
    std::getline(file, line);
    iss << file.rdbuf();
  }

  return 0;
}

bool STLFile::get_next_triangle_binary(float *out) {
  if (current == N) {
    return false;
  }

  for (int i = 0; i < 12; i++) {
    file.read((char *) &out[i], 4);
  }

  file.seekg(2, std::ios_base::cur);
  current++;
  return true;
}

bool STLFile::get_next_triangle_ascii(float *out) {
  std::string _;

  // Eat "facet"
  if (!(iss >> _)) {
    return false;
  }

  // Eat "normal"
  iss >> _;

  // Read triangle normal.
  for (int i = 0; i < 3; i++) {
    iss >> out[i];
  }

  // Eat "outer loop".
  iss >> _; iss >> _;

  // Read 3 vertices.
  for (int i = 0; i < 3; i++) {
    // Eat "vertex".
    iss >> _;
    for (int j = 0; j < 3; j++) {
      iss >> out[3*(i+1) + j];
    }
  }

  // Eat "endloop".
  iss >> _;
  // Eat "endfacet".
  iss >> _;

  return true;
}

bool STLFile::get_next_triangle(float *out) {
  if (ascii) {
    return get_next_triangle_ascii(out);
  }
  return get_next_triangle_binary(out);
}

void STLFile::close() {
  file.close();
}
