#version 130

in vec3 in_position;
in vec3 in_normal;
in vec3 in_color;

uniform mat4 model_to_world;
uniform mat4 world_to_cam;

uniform vec3 cam_pos;
uniform vec3 light_pos;

out vec3 f_color;

uniform float I_amb = 0.05;
uniform vec3 ambient_color = vec3(1.0, 1.0, 1.0);
uniform vec3 diffuse_color = vec3(0.4, 0.0, 0.6);
uniform vec3 specular_color = vec3(0.7, 0.5, 0.5);
uniform float shininess = 4.0;

void main() {
  vec4 world_pos = model_to_world * vec4(in_position, 1.0);
  //f_pos = world_pos;
  vec3 N = normalize((model_to_world * vec4(in_normal, 0.0)).xyz);

  vec3 L = normalize(light_pos - world_pos.xyz);
  float I_diff = max(dot(N, L), 0.0);

  vec3 V = normalize(cam_pos - world_pos.xyz);
  vec3 H = normalize(L + V);
  float I_spec = pow(max(dot(N, H), 0), shininess);

  float r = length(light_pos - world_pos.xyz);

  f_color = (diffuse_color * I_diff + specular_color * I_spec) / (r*r) +
            I_amb * ambient_color;

  gl_Position = world_to_cam * world_pos;
}
