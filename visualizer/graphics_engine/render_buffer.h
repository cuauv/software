#ifndef _RENDER_BUFFER_H_
#define _RENDER_BUFFER_H_

#include <GL/gl.h>

class RenderBuffer {
  constexpr static unsigned int delay = 2;
  public:
    int init(unsigned int width, unsigned int height);
    void bind_for_writing();
    void bind_for_reading();

    void *get_data();
    // Must be called after client is done with data returned by get_data.
    void data_cleanup();

  private:
    int index = 0;
    unsigned int width;
    unsigned int height;

    GLuint fbo[delay];
    GLuint color_rbo[delay];
    GLuint depth_rbo[delay];
    GLuint pbo[delay];
};

#endif
