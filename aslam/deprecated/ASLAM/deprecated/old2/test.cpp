#include <stdio.h>
#include <stdlib.h>

// #include <GL/glew.h>
#define GLFW_INCLUDE_GLU
#include <GLFW/glfw3.h>
// #include <glm/glm.hpp>

// using namespace glm;

int main() {
        if (!glfwInit()) {
                return -1;
        }
        //glfwWindowHint(GLFW_FSAA_SAMPLES, 4); // 4x AA
        //glfwWindowHint(GLFW_OPENGL_VERSION_MAJOR, 3);
        //glfwWindowHint(GLFW_OPENGL_VERSION_MINOR, 3);
        //glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

        GLFWWindow* window = glfwCreateWindow(1024, 768, "Testing...", NULL, NULL);
        if (window == NULL) {
                glfwTerminate();
                return -1;
        }
        glfwMakeContextCurrent(window);

        /*
        glewExperimental = true;
        if (glewInit() != GLEW_OK) {
                return -1;
        }*/
        
        glfwSetInputMode(window, GLFW_STICKY_KEYS, GL_TRUE);

        do {
                glfwSwapBuffers(window);
                glfwPollEvents();
        } while (glfwGetKey(window, GLFW_KEY_ESCAPE) != GLFW_PRESS && glfwWindowShouldClose(window) == 0);

}
