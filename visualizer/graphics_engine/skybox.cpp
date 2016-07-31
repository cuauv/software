#include "skybox.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

bool load_cube_map_side (GLenum side_target, const char* file_name) {
  int x, y, n;
  int force_channels = 4;
  unsigned char* image_data = stbi_load(
    file_name, &x, &y, &n, force_channels);
  if (!image_data) {
    fprintf (stderr, "ERROR: could not load \"%s\" as cube map side.\n",
             file_name);
    return false;
  }
  // non-power-of-2 dimensions check 
  if ((x & (x - 1)) != 0 || (y & (y - 1)) != 0) {
    fprintf (
      stderr, "WARNING: image %s is not power-of-2 dimensions\n", file_name
    );
  }
  
  // copy image data into 'target' side of cube map
  glTexImage2D(
    side_target,
    0,
    GL_RGBA,
    x,
    y,
    0,
    GL_RGBA,
    GL_UNSIGNED_BYTE,
    image_data
  );
  stbi_image_free(image_data);
  return true;
}

void create_cube_map (
  const char* top,
  const char* bottom,
  const char* right,
  const char* left,
  const char* back,
  const char* front,
  GLenum texture) {
  // generate a cube-map texture to hold all the sides
  glActiveTexture(texture);
  GLuint tex_cube;
  glGenTextures(1, &tex_cube);
  glBindTexture(GL_TEXTURE_CUBE_MAP, tex_cube);

  // load each image and copy into a side of the cube-map texture
  load_cube_map_side(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, left);
  load_cube_map_side(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, right);
  load_cube_map_side(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, top);
  load_cube_map_side(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, bottom);
  load_cube_map_side(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, back);
  load_cube_map_side(GL_TEXTURE_CUBE_MAP_POSITIVE_X, front);

  // format cube map texture
  glTexParameteri (GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri (GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri (GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
  glTexParameteri (GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri (GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
}
