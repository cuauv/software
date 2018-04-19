#version 130

in vec3 texcoords;
uniform samplerCube cube_texture;
out vec4 color;

uniform int enabled;

mat3 to_gl = mat3(0.0, 0.0, 1.0,
                  1.0, 0.0, 0.0,
                  0.0, -1.0, 0.0);

void main() {
  if (enabled == 1) {
    color = texture(cube_texture, to_gl * texcoords);
  }
  else {
    color = vec4(0.0, 0.0, 0.0, 1.0);
  }
}
