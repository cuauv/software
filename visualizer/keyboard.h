#ifndef _KEYBOARD_H_
#define _KEYBOARD_H_

#include <functional>

#include <GLFW/glfw3.h>

void key_callback(GLFWwindow *window, int key, int scancode, int action, int mods);
bool key_pressed(unsigned int key);

// The provided function will be called every time the key is pressed.
// Overwrites any previous action bound to the key.
void register_action(unsigned int key, std::function<void (void)> f);

#endif
