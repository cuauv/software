#!/usr/bin/env python3

from build import ninja_common
import os

build = ninja_common.Build('aslam/doc')

build.generate(out_files=['aslam.pdf'], script='aslam/doc/pandoc.sh',
               in_files=['aslam.pdc'], requires=['pandoc'])
