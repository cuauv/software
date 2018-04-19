#ifndef _GL_UTILS_H_
#define _GL_UTILS_H_

#include <string>
#include <vector>

#include <GL/gl.h>

GLuint make_shader(GLenum shader_type, const std::string& filename);
GLuint make_program(const std::vector<GLuint>&shaders);
GLuint get_next_texture();
int load_texture(const std::string &filename, GLuint &texture_ind);

int check_for_gl_error();

#endif
