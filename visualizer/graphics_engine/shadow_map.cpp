#include "shadow_map.h"

#include "gl_utils.h"

#include <cstdio>

ShadowMap::ShadowMap(GLenum texture_unit_) : texture_unit(texture_unit_) {}

int ShadowMap::init() {
  width = 512;
  height = 512;

  glGenFramebuffers(1, &fbo);

  glGenTextures(1, &shadow_map);
  glActiveTexture(texture_unit);
  glBindTexture(GL_TEXTURE_2D, shadow_map);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, width, height, 0,
               GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE,
                  GL_COMPARE_REF_TO_TEXTURE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LESS);

  glBindFramebuffer(GL_FRAMEBUFFER, fbo);
  glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
                         GL_TEXTURE_2D, shadow_map, 0);

  glDrawBuffer(GL_NONE);
  glReadBuffer(GL_NONE);

  auto s = glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if (s != GL_FRAMEBUFFER_COMPLETE) {
    fprintf(stderr, "Error creating frame buffer for shadow map: %x\n", s);
    return -1;
  }

  if (check_for_gl_error()) {
    fprintf(stderr, "GL Error after shadow map initialization.\n");
  }

  return 0;
}

void ShadowMap::bind_for_writing() {
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fbo);
}

void ShadowMap::bind_for_reading() {
  glActiveTexture(texture_unit);
  glBindTexture(GL_TEXTURE_2D, shadow_map);
}
