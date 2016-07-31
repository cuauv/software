#version 130

in vec4 in_position;

uniform mat4 model_to_world;
uniform mat4 world_to_cam;
out vec3 texcoords;

void main() {
  texcoords = in_position.xyz;
  gl_Position = world_to_cam * model_to_world * in_position;
}
