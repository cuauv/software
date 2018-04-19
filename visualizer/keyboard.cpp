#include "keyboard.h"

bool pressed[GLFW_KEY_LAST + 1];
std::function<void ()> actions[GLFW_KEY_LAST + 1];

void key_callback(GLFWwindow *window, int key, int scancode, int action, int mods) {
  if (key < 0 || key > GLFW_KEY_LAST) {
    return;
  }

  if (action == GLFW_PRESS) {
    pressed[key] = true;
    if (actions[key]) {
      actions[key]();
    }
  }
  else if (action == GLFW_RELEASE) {
    pressed[key] = false;
  }
}

bool key_pressed(unsigned int key) {
  return pressed[key];
}

void register_action(unsigned int key, std::function<void ()> f) {
  actions[key] = f;
}
