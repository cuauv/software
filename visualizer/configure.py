#!/usr/bin/python

from build import ninja_common

build = ninja_common.Build("visualizer")

# Only build if all dependencies are present.
# TODO Create a better means of dependency checking.
import os
GL = os.path.isfile("/usr/include/GL/gl.h")
GLFW = os.path.isfile("/usr/include/GLFW/glfw3.h") or \
       os.path.isfile("/usr/local/include/GLFW/glfw3.h")
GLM = os.path.isdir("/usr/include/glm") or \
      os.path.isdir("/usr/local/include/glm")
CONF = os.path.isfile("/usr/include/libconfig.h++") or \
       os.path.isfile("/usr/local/include/libconfig.h++")

if GL and GLFW and GLM and CONF:
  sources = ['gl_utils.cpp',
             'graphics_engine.cpp',
             'material.cpp',
             'obj_builder.cpp',
             'render_buffer.cpp',
             'renderable.cpp',
             'renderer.cpp',
             'scene_object.cpp',
             'shadow_map.cpp',
             'skybox.cpp',
             'stl_builder.cpp',
             'stl_read.cpp']

  build.build_shared(
    'vis', [os.path.join('graphics_engine', source) for source in sources],
    pkg_confs=['gl'], cflags=['-DGL_GLEXT_PROTOTYPES']
  )

  # We compile this separately and link at RUNTIME to avoid
  # requiring OpenCV and Eigen for visualizer use.
  build.build_shared('vision_link', ['vision_link.cpp'],
    auv_deps=['auv-camera-message-framework', 'conf'], pkg_confs=['opencv', 'eigen3'],
    # XXX -Wno-deprecated-declarations is temporary, to deal with the following Eigen defect:
    # XXX The same is applied in .syntastic_cpp_config and in conf/configure.py.
    # XXX Remove as soon as the defect is resolved!
    # XXX http://eigen.tuxfamily.org/bz/show_bug.cgi?id=872
    cflags=["-Wno-deprecated-declarations"]
  )

  # TODO we should not be compiling units like below.
  build.build_shared('fishbowl_comm', ['fishbowl_comm.cpp', '../fishbowl/bits.cpp'],
                     auv_deps=['utils'])

  build.build_shared('aslam_comm', ['aslam_comm.cpp'], auv_deps=['utils'])

  build.build_cmd('auv-visualizer',
                  ['visualizer.cpp', 'keyboard.cpp', 'point_manager.cpp',
                   'fishbowl_manager.cpp', 'async_manager.cpp'],
                  auv_deps=['shm', 'utils', 'vis',
                            'fishbowl_comm', 'math', 'quat', 'aslam_comm'],
                  pkg_confs=['gl', 'libconfig++', 'glfw3'], lflags=['-ldl'])

else:
  print("Could not build the visualizer because you are missing:")
  s = ""
  if not GL:
      s += "\tOpenGL headers\n"
  if not GLFW:
      s += "\tGLFW (apt-get libglfw3-dev)\n"
  if not GLM:
      s += "\tGL Mathematics (apt-get libglm-dev)\n"
  if not CONF:
      s += "\tlibconfig++ (apt-get libconfig++-dev)\n"

  print(s)
