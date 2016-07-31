#include <GLFW/glfw3.h>
#include <stdlib.h>
#include <stdio.h>
#include <GL/glc.h>

#include <iostream>
#include <vector>
#include <cmath>
#include <sys/time.h>

#define NDIM 3

using std::vector;
using std::cout;
using std::endl;
using std::copy;
using std::initializer_list;
using std::sqrt;
using std::to_string;

template<typename T>
struct Space {
        T bounds [NDIM][2]; // should be a pointer, maybe
};

struct CP {
        double color [3];
        double pos [NDIM];
};

/** Maps finite grid of points over input space with spacing given by delta to CP tuples on output space with color given by the input function. */
template<typename Func>
static vector<CP> genpts(Func f, Space<double> in, Space<double> out, double delta) {
        vector<CP> pts;
        
        // Tied to 3D. Generalize eventually (?)
        double cval [NDIM];
        double ratio [NDIM];
        for (int i = 0; i < NDIM; i++) { ratio[i] = (out.bounds[i][1] - out.bounds[i][0]) / (in.bounds[i][1] - in.bounds[i][0]); }
        cval[0] = in.bounds[0][0];
        while (cval[0] <= in.bounds[0][1]) {
                cval[1] = in.bounds[1][0];
                while (cval[1] <= in.bounds[1][1]) {
                        cval[2] = in.bounds[2][0];
                        while (cval[2] <= in.bounds[2][1]) {
                                CP p;
                                double* clr = f(cval);
                                for (int i = 0; i < NDIM; i++) { p.color[i] = clr[i]; } // copy doesn't work because C++
                                for (int i = 0; i < NDIM; i++) { p.pos[i] = out.bounds[i][0] + (cval[i] - in.bounds[i][0]) * ratio[i]; }
                                //cout << p.pos[0] << "," << p.pos[1] << "," << p.pos[2] << ":" << p.color[0] << "," << p.color[1] << "," << p.color[2] << endl;
                                pts.push_back(p);
                                cval[2] += delta;
                        }
                        cval[1] += delta;
                }
                cval[0] += delta;
        }

        return pts;
}

static double dist(double pos1 [3], double pos2 [3]) {
        double sum = 0.;
        for (int i = 0; i < 3; i++) {
                sum += (pos2[i] - pos1[i]) * (pos2[i] - pos1[i]);
        }
        return sqrt(sum);
}

struct f {
        double* operator()(double coords []) {
                double pt [3] = {5., 5., 5.};
                double clr [3] = {1. / dist(coords, pt), 0., dist(coords, pt)};
                return clr;
        }
};

static void plotpts(vector<CP> pts) {
        glBegin(GL_POINTS);
        for (CP p : pts) {
                glColor3f(p.color[0], p.color[1], p.color[2]);
                glVertex3f(p.pos[0], p.pos[1], p.pos[2]);
        }
        glEnd();
}

double scale = 1.;
double trans [3] = {0., 0., 0.};
double rotate [3] = {0., 0., 0.};

static void onkey(GLFWwindow* window, int key, int sc, int act, int mods) {
        if (key == GLFW_KEY_UP) trans[1] += 0.1;
        if (key == GLFW_KEY_DOWN) trans[1] -= 0.1;
        if (key == GLFW_KEY_RIGHT) trans[0] += 0.1;
        if (key == GLFW_KEY_LEFT) trans[0] -= 0.1;
        if (key == GLFW_KEY_W) scale *= 1.1;
        if (key == GLFW_KEY_S) scale *= 0.9;
}

static void onmouse(double dx, double dy) {
        rotate[1] += dx;
        rotate[0] += dy;
}

GLint ctx;

static void overlayinit() {
        ctx = glcGenContext();
        glcContext(ctx);
        auto fnt = glcGenFontID();
        glcNewFontFromFamily(fnt, "Arial");
        glcFont(fnt);
        glcRenderStyle(GLC_TRIANGLE);
}

static void overlay() {
        timeval tv;
        gettimeofday(&tv, NULL);
        time_t ct = time(0);
        tm* gmt = gmtime(&ct);
        double scale = 0.03;
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        glOrtho(-1., 1., -1., 1., -1., 1.);
        glScalef(scale, scale, scale);
        glTranslatef(-0.75 / scale, 0.85 / scale, 0.); // 3/4, I think
        glColor3f(0.9f, 0.9f, 0.9f);
        glcRenderString("CUAUV ASLAM ALPHA: REALTIME BAYESIAN MAP RENDERING");
        glTranslatef(-0.95 / scale, -1.75 / scale, 0.);
        glcRenderString(asctime(gmt));
        //glcRenderString((to_string(tv.tv_usec)).c_str());
        glEnd();
        glScalef(1 / scale, 1 / scale, 1 / scale); // this is so hacky
        glEnd();
}

int main(void) {
        Space<double> s1;
        double s1b [NDIM][2] = { {0., 10.}, {0., 10.}, {0., 10.} };
        copy(s1b, s1b + NDIM, s1.bounds);
        Space<double> s2;
        double s2b [NDIM][2] = { {-1., 1.}, {-1., 1.}, {-1., 1.} };
        copy(s2b, s2b + NDIM, s2.bounds);
        f fxn; // can't even make it static
        vector<CP> points = genpts(fxn, s1, s2, 0.2);

        GLFWwindow* window;
        glfwInit();
        window = glfwCreateWindow(640, 480, "Testing...", NULL, NULL);
        glfwMakeContextCurrent(window);
        glfwSetKeyCallback(window, onkey);
        glPointSize(2);

        overlayinit(); overlay();
        while (!glfwWindowShouldClose(window)) {
                int width, height;
                glfwGetFramebufferSize(window, &width, &height);
                float ratio = width / (float) height;
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
                plotpts(points);
                overlay();
                //glScale(0.01f, 0.01f, 1);
                double xpos, ypos;
                glfwGetCursorPos(window, &xpos, &ypos);
                onmouse(width / 2. - xpos, height / 2. - ypos);
                glfwSetCursorPos(window, width / 2., height / 2.);
                glfwSwapBuffers(window);
                glfwPollEvents();
        }
        glfwDestroyWindow(window);
        glfwTerminate();
        exit(EXIT_SUCCESS);
}
