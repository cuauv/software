#!/usr/bin/env python2

import pyglet
from pyglet.gl import *

window = pyglet.window.Window()
glEnable(GL_POINT_SMOOTH)
glEnable(GL_MULTISAMPLE)
glPointSize(5)

@window.event
def on_draw():
    glClear(GL_COLOR_BUFFER_BIT)
    glLoadIdentity()
    glBegin(GL_POINTS)
    glVertex3f(0,0,0)
    glVertex3f(window.width,0,0)
    glVertex3f(window.width,window.height,0)
    pyglet.graphics.draw(1, pyglet.gl.GL_POINTS, ('v3f', (1., 1., 1.)))
    glEnd()

pyglet.app.run()
