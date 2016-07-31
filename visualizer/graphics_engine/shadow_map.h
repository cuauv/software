#ifndef _SHADOW_MAP_H_
#define _SHADOW_MAP_H_

#include <GL/gl.h>

class ShadowMap {
  public:
    ShadowMap(GLenum texture_unit);
    int init();
    void bind_for_writing();
    void bind_for_reading();
    GLenum texture_unit;

    unsigned int width;
    unsigned int height;

  private:
    GLuint fbo;
    GLuint shadow_map;
};

#endif
