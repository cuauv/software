#include "gl_utils.h"

#include <algorithm>
#include <cstdio>
#include <fstream>
#include <functional>

#include "stb_image.h"

GLuint make_shader(GLenum shader_type, const std::string &filename) {
  // Read the shader source file.
  std::ifstream in(filename.c_str());
  if (in.fail()) {
    fprintf(stderr, "Failed to open shader source \"%s\"\n",
            filename.c_str());
  }

  std::string contents((std::istreambuf_iterator<char>(in)),
                        std::istreambuf_iterator<char>());

  GLuint shader = glCreateShader(shader_type);
  if (!shader) {
    fprintf(stderr, "Failed to create shader.\n");
    return shader;
  }

  // Compile the shader.
  const char *file_str = contents.c_str();
  glShaderSource(shader, 1, (const GLchar**)&file_str, 0);
  glCompileShader(shader);

  // Check compilation status.
  GLint stat;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &stat);
  if (stat != GL_TRUE) {
    fprintf(stderr, "Shader %s failed to compile:\n", filename.c_str());

    GLint info_log_len;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &info_log_len);

    GLchar info_buf[info_log_len];
    glGetShaderInfoLog(shader, info_log_len, 0, info_buf);

    fprintf(stderr, "\t%s\n", (char *)info_buf);
    return 0;
  }

  return shader;
}

GLuint make_program(const std::vector<GLuint>&shaders) {
  GLuint shader_prog = glCreateProgram();
  if (!shader_prog) {
    fprintf(stderr, "Error creating GL Program\n");
    return 0;
  }

  std::for_each(shaders.begin(), shaders.end(),
                std::bind(glAttachShader,
                          shader_prog,
                          std::placeholders::_1
                )
  );

  glLinkProgram(shader_prog);

  GLint stat;
  glGetProgramiv(shader_prog, GL_LINK_STATUS, &stat);
  if (stat != GL_TRUE) {
    fprintf(stderr, "Failed to link GL program:\n");

    GLint info_log_len;
    glGetProgramiv(shader_prog, GL_INFO_LOG_LENGTH, &info_log_len);

    GLchar info_buf[info_log_len];
    glGetProgramInfoLog(shader_prog, info_log_len, 0, info_buf);

    fprintf(stderr, "\t%s\n", (char *)info_buf);
    return 0;
  }

  if (check_for_gl_error()) {
    fprintf(stderr, "ERROR: GL Error during shader program creation.\n");
    return 0;
  }

  return shader_prog;
}

GLuint available_texture = 0;
GLuint get_next_texture() {
  return available_texture++;
}

int load_texture(const std::string &filename, GLuint &texture_ind) {
  int width, height, bpp;
  unsigned char* image_data = stbi_load(filename.c_str(),
                                        &width, &height, &bpp, 0);
  if (!image_data) {
    fprintf(stderr, "ERROR: Could not load texture \"%s\"\n",
            filename.c_str());
    return -1;
  }

  GLint internal_format;
  GLenum format;

  if (bpp == 3) {
    internal_format = GL_RGB;
    format = GL_RGB;
  }
  else if (bpp == 4) {
    internal_format = GL_RGBA;
    format = GL_RGBA;
  }
  else {
    fprintf(stderr, "ERROR: Texture \"%s\" has %d bytes per pixel; "
                    "only 3 and 4 supported\n", filename.c_str(), bpp);
    return -1;
  }

  GLuint texture_name;
  glGenTextures(1, &texture_name);

  texture_ind = get_next_texture();
  GLenum texture_unit = GL_TEXTURE0 + texture_ind;
  glActiveTexture(texture_unit);
  glBindTexture(GL_TEXTURE_2D, texture_name);
  glTexImage2D(GL_TEXTURE_2D, 0, internal_format, width, height, 0, format,
               GL_UNSIGNED_BYTE, image_data);
  glGenerateMipmap(GL_TEXTURE_2D);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
                  GL_LINEAR_MIPMAP_LINEAR);

  stbi_image_free(image_data);
  return 0;
}

int check_for_gl_error() {
  GLenum err;
  int error = 0;
  while ((err = glGetError())) {
    error = 1;
    fprintf(stderr, "Unexpected GL error! (0x%04X)\n", err);
  }

  return error;
}
