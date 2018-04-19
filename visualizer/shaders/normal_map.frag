#version 130

precision highp float;

uniform vec3 cam_pos;
uniform vec3 light_pos;

uniform float light_intensity = 0.50;

uniform float I_amb = 0.08;
uniform vec3 ambient_color = vec3(1.0, 1.0, 1.0);
uniform vec3 diffuse_color = vec3(1.0, 1.0, 0.5);
uniform vec3 specular_color = vec3(1.0, 1.0, 0.5);
uniform float shininess = 4.0;

uniform float alpha;

uniform sampler2D tex;
uniform sampler2D normal_map;
uniform sampler2DShadow shadow_map;

in vec3 f_pos;
in vec3 f_normal;
in vec2 f_uv;
in vec3 f_color;

in vec4 f_light_pos;

out vec4 color;

void main() {
  vec3 N = normalize(f_normal);
  vec3 L = normalize(light_pos - f_pos);
  float I_diff = max(dot(N, L), 0.0);

  vec3 V = normalize(cam_pos - f_pos);
  vec3 H = normalize(L + V);
  float I_spec = pow(max(dot(N, H), 0.0), shininess);

  float r = distance(light_pos, f_pos);

  vec3 material_color = texture(normal_map, f_uv).rgb;
  vec3 light_color = diffuse_color * I_diff + specular_color * I_spec;


  vec3 light_pos_proj = vec3(f_light_pos.xyz) / f_light_pos.w;
  vec3 uv_shadow = 0.5 * vec3(light_pos_proj.xyz + vec3(1, 1, 1));
  uv_shadow.z += -0.0001;

  if (uv_shadow.x < 0.99 && uv_shadow.x > 0.01 &&
      uv_shadow.y < 0.99 && uv_shadow.y > 0.01) {
    float light_percent = texture(shadow_map, uv_shadow);
    I_spec *= light_percent;
    I_diff *= light_percent;
  }

  vec3 fin_color = light_intensity * light_color / (r*r) +
                   ambient_color * I_amb +
                 (light_intensity * (I_diff + I_spec) + I_amb) * material_color;

  color = vec4(fin_color, alpha);
}
