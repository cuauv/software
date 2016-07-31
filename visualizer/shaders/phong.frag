#version 130

uniform vec3 cam_pos;
uniform vec3 light_pos[16];

uniform float light_intensity = 30;
uniform int light_count;
uniform int shadows_enabled;

uniform vec3 diffuse_color = vec3(1.0, 1.0, 1.0);
uniform vec3 specular_color = vec3(0.4, 0.4, 0.4);
uniform int texture_enabled;

uniform float I_amb = 0.15;
uniform vec3 ambient_color = vec3(1.0, 1.0, 1.0);
uniform float shininess = 96.0;

uniform float alpha = 1.0;

uniform sampler2D tex;
uniform sampler2DShadow shadow_map[16];

in vec3 f_pos;
in vec3 f_normal;
in vec2 f_uv;
in vec3 f_color;

in vec4 f_light_pos[16];

out vec4 color;

void main() {
  vec3 N = normalize(f_normal);
  vec3 V = normalize(cam_pos - f_pos);

  vec3 material_color = ambient_color;
  vec3 diffuse_color_m = diffuse_color;
  if (texture_enabled == 1) {
    material_color = texture(tex, f_uv).rgb;
    diffuse_color_m = texture(tex, f_uv).rgb;
  }

  vec3 fin_color = material_color * I_amb;
  for (int i = 0; i < light_count; i++) {
    vec3 L = normalize(light_pos[i] - f_pos);
    float I_diff = max(dot(N, L), 0.0);
    vec3 H = normalize(L + V);
    float I_spec = pow(max(dot(N, H), 0.0), shininess);

    float r = distance(light_pos[i], f_pos);

    vec3 light_pos_proj = vec3(f_light_pos[i].xyz) / f_light_pos[i].w;
    vec3 uv_shadow = 0.5 * vec3(light_pos_proj.xyz + vec3(1, 1, 1));

    // Avoid shadow acne... YUCK!
    uv_shadow.z += -0.0001;

    if (uv_shadow.x < 0.99 && uv_shadow.x > 0.01 &&
        uv_shadow.y < 0.99 && uv_shadow.y > 0.01) {
      float light_percent;
      if (shadows_enabled == 1) {
        // TODO GLSL < 4.00 does not support iterating over sampler arrays
        // with dynamically constant expressions such as loop counters...
        if (i > 0)
          light_percent = texture(shadow_map[1], uv_shadow);
        else
          light_percent = texture(shadow_map[0], uv_shadow);
      }
      else {
        light_percent = 1.0;
      }

      I_spec *= light_percent;
      I_diff *= light_percent;
    }

    fin_color += (diffuse_color_m * I_diff + specular_color * I_spec) / (0.01 * r*r);
  }

  color = vec4(fin_color, alpha);
}
