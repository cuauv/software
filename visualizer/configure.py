#!/usr/bin/env python3

from build import ninja_common

build = ninja_common.Build("visualizer")

# Only build if all dependencies are present.
# TODO Create a better means of dependency checking.
import os

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
pkg_confs=['gl'], cflags=['-DGL_GLEXT_PROTOTYPES', '-Wno-misleading-indentation']
)

# We compile this separately and link at RUNTIME to avoid
# requiring OpenCV and Eigen for visualizer use.
build.build_shared('vision_link', ['vision_link.cpp'],
auv_deps=['auv-camera-message-framework', 'conf'], pkg_confs=['opencv', 'eigen3'],
cflags=[]
)

# TODO we should not be compiling units like below.
build.build_shared('fishbowl_comm', ['fishbowl_comm.cpp', '../fishbowl/bits.cpp'],
                    auv_deps=['utils'])

build.build_shared('aslam_comm', ['aslam_comm.cpp'], auv_deps=['utils'])

build.build_cmd('auv-visualizer-nodisplay',
                ['visualizer.cpp', 'keyboard.cpp', 'point_manager.cpp',
                'fishbowl_manager.cpp', 'async_manager.cpp'],
                auv_deps=['shm', 'utils', 'vis',
                        'fishbowl_comm', 'math', 'quat', 'aslam_comm'],
                pkg_confs=['gl', 'libconfig++', 'glfw3'], lflags=['-ldl'])

build.install('auv-visualizer', f='visualizer/visualizer.sh')
