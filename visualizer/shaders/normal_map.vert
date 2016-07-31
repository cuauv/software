#version 130

in vec3 in_position;
in vec3 in_normal;
in vec2 in_uv;
in vec3 in_color;

uniform mat4 model_to_world;
uniform mat4 world_to_cam;
uniform mat4 world_to_light;

out vec3 f_pos;
out vec3 f_normal;
out vec2 f_uv;
out vec3 f_color;
out vec4 f_light_pos;

void main() {
  vec4 world_pos = model_to_world * vec4(in_position, 1.0);
  f_pos = world_pos.xyz;
  f_normal = normalize((model_to_world * vec4(in_normal, 0.0)).xyz);
  f_uv = in_uv;
  f_color = in_color;

  f_light_pos = world_to_light * (model_to_world * vec4(in_position, 1.0));

  gl_Position = world_to_cam * world_pos;
}
