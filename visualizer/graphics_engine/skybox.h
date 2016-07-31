#ifndef _SKYBOX_H_
#define _SKYBOX_H_

#include "gl_utils.h"

bool load_cube_map_side(GLenum side_target, const char* file_name);
void create_cube_map(
  const char* top,
  const char* bottom,
  const char* right,
  const char* left,
  const char* back,
  const char* front,
  GLenum texture);

#endif
