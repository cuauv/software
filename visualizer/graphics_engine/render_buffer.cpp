#include "render_buffer.h"

#include "gl_utils.h"

#include <cstdio>

int RenderBuffer::init(unsigned int width, unsigned int height) {
  this->width = width;
  this->height = height;

  for (unsigned int i = 0; i < delay; i++) {
    // Color render buffer
    glGenRenderbuffers(1, &color_rbo[i]);
    glBindRenderbuffer(GL_RENDERBUFFER, color_rbo[i]);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_RGBA8, width, height);

    // Depth render buffer
    glGenRenderbuffers(1, &depth_rbo[i]);
    glBindRenderbuffer(GL_RENDERBUFFER, depth_rbo[i]);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, width, height);

    // Attach the two to a framebuffer.
    glGenFramebuffers(1, &fbo[i]);
    glBindFramebuffer(GL_FRAMEBUFFER, fbo[i]);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, color_rbo[i]);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depth_rbo[i]);

    // Use a Pixel Buffer Object for asynchronous GPU to CPU data transfer.
    glGenBuffers(1, &pbo[i]);
    glBindBuffer(GL_PIXEL_PACK_BUFFER, pbo[i]);
    glBufferData(GL_PIXEL_PACK_BUFFER, width * height * 4, NULL, GL_DYNAMIC_READ);

    glReadBuffer(GL_COLOR_ATTACHMENT0);

    if (check_for_gl_error()) {
      fprintf(stderr, "ERROR: GL Error during RenderBuffer initialization.\n");
      return -1;
    }

    auto s = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (s != GL_FRAMEBUFFER_COMPLETE) {
      fprintf(stderr, "Error creating frame buffer for render target: %x\n", s);
      return -1;
    }
  }

  return 0;
}

void RenderBuffer::bind_for_writing() {
  glBindFramebuffer(GL_FRAMEBUFFER, fbo[index]);
}

void RenderBuffer::bind_for_reading() {
  glBindBuffer(GL_PIXEL_PACK_BUFFER, pbo[index]);
  // It is very important that the below format used is BGRA!
  // This is the GPU's internal format and without it the below operation
  // takes an order of magnitude longer (seems to block DMA for some reason...).
  glReadPixels(0, 0, width, height, GL_BGRA, GL_UNSIGNED_BYTE, 0);
}

void *RenderBuffer::get_data() {
  glBindBuffer(GL_PIXEL_PACK_BUFFER, pbo[(index - 1) % delay]);
  return glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
}

void RenderBuffer::data_cleanup() {
  glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
  index++;
  if (index == delay) {
    index = 0;
  }
}
