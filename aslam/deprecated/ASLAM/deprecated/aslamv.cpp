// ASLAM Visualizer
// Written by Christopher Goes (cwg46@cornell.edu)

// Last Update 5 May 2014

#include "aslam.h"

#include <iostream>
#include <cstring>
#include <time.h>

#include <GLFW/glfw3.h>
#include <GL/glc.h>

using std::cin;
using std::cout;
using std::endl;

#define SCALING_FACTOR 0.001
#define X_OFFSET DIMX/2
#define Y_OFFSET DIMY/2

static void log(string s) {
    time_t rtime;
    tm* tinfo;
    char b[100];

    time(&rtime);
    tinfo = gmtime(&rtime);
    strftime(b, 100, "[%Y.%m.%d %H:%M:%S UTC] ", tinfo);

    cout << b << s << endl;
}

static void plot(vector<vector<double>> v) {
    glBegin(GL_POINTS);
    for (int x = 0; x < DIMX; x++) {
        for (int y = 0; y < DIMY; y++) {
            glColor3f(v[x][y] / SCALING_FACTOR, 0., (SCALING_FACTOR / v[x][y]));
            glVertex3f((x - X_OFFSET) * SCALING_FACTOR, (y - Y_OFFSET) * SCALING_FACTOR, v[x][y] * 10);
        }
    }
    glEnd();
}

double scale = 1.;
double trans [3] = {0., 0., 0.};
double rotate [3] = {0., 0., 0.};

static void onkey(GLFWwindow* window, int key, int sc, int act, int mods) {
        if (key == GLFW_KEY_UP) trans[1] += 0.01;
        if (key == GLFW_KEY_DOWN) trans[1] -= 0.01;
        if (key == GLFW_KEY_RIGHT) trans[0] += 0.01;
        if (key == GLFW_KEY_LEFT) trans[0] -= 0.01;
        if (key == GLFW_KEY_W) scale *= 1.1;
        if (key == GLFW_KEY_S) scale *= 0.9;
}

static void onmouse(double dx, double dy) {
        rotate[1] += dx;
        rotate[0] += dy;
}

int main() {
    log("ASLAM visualizer launched");

    GLFWwindow* window;
    glfwInit();
    window = glfwCreateWindow(800, 600, "ASLAM Visualizer (ALPHA)", NULL, NULL);
    glfwMakeContextCurrent(window);
    glfwSetKeyCallback(window, onkey);
    glPointSize(2);

    int width, height;
    float ratio;
    double xpos, ypos;
    while (!glfwWindowShouldClose(window)) {
            glfwGetFramebufferSize(window, &width, &height);
            ratio = width / (float) height;
            glViewport(0, 0, width, height);
            glClear(GL_COLOR_BUFFER_BIT);
            glMatrixMode(GL_PROJECTION);
            glLoadIdentity();
            glOrtho(-ratio, ratio, -1.f, 1.f, 1.f, -1.f);
            glMatrixMode(GL_MODELVIEW);
            glLoadIdentity();
            glScalef(scale, scale, scale);
            glTranslatef(trans[0], trans[1], trans[2]);
            glRotatef(rotate[0], 1., 0., 0.);
            glRotatef(rotate[1], 0., 1., 0.);
            // Poll SHM
            // for object in objects:
            // plot object pmap vals
            // ex: plot(s->objects["pipe"]->pmap->vals);
            glfwGetCursorPos(window, &xpos, &ypos);
            onmouse(width / 2. - xpos, height / 2. - ypos);
            glfwSetCursorPos(window, width / 2., height / 2.);
            glfwSwapBuffers(window);
            glfwPollEvents();
    }
    log("ASLAM visualizer now terminating");
    glfwDestroyWindow(window);
    glfwTerminate();
    exit(EXIT_SUCCESS);
}
