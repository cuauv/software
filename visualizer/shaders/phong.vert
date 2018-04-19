#version 130

in vec3 in_position;
in vec3 in_normal;
in vec2 in_uv;
in vec3 in_color;

uniform mat4 model_to_world;
uniform mat4 world_to_cam;
uniform mat4 world_to_light[16];

uniform int light_count;

out vec3 f_pos;
out vec3 f_normal;
out vec2 f_uv;
out vec3 f_color;
out vec4 f_light_pos[16];

void main() {
  vec4 world_pos = model_to_world * vec4(in_position, 1.0);
  f_pos = world_pos.xyz;
  f_normal = normalize((model_to_world * vec4(in_normal, 0.0)).xyz);
  f_uv = in_uv;
  f_color = in_color;

  for (int i = 0; i < light_count; i++) {
    f_light_pos[i] = world_to_light[i] * world_pos;
  }

  gl_Position = world_to_cam * world_pos;
}
