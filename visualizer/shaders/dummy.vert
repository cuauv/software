#version 130

in vec3 in_position;
in vec3 in_color;
uniform mat4 world_to_cam;

out vec3 f_color;

void main() {
  f_color = in_color;
  gl_Position = world_to_cam * vec4(in_position, 1.0);
}
