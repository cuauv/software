#!/usr/bin/env bash

if [[ "$(uname -m)" != "x86_64" ]]; then
	echo "Cannot install Caffe on $(uname -m)"
	exit 0
fi

# based on https://github.com/BVLC/caffe/wiki/Ubuntu-16.04-or-15.10-Installation-Guide

apt-get install -y unzip

CAFFE_VERSION="rc5"
BUILD_DIR="/opt/auv"
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

curl -L "https://github.com/BVLC/caffe/archive/${CAFFE_VERSION}.zip" -o caffe.zip

unzip caffe.zip
rm caffe.zip

cd "caffe-${CAFFE_VERSION}"

cp Makefile.config.example Makefile.config

patch Makefile.config <<'EOF'
8c8
< # CPU_ONLY := 1
---
> CPU_ONLY := 1
11c11
< # USE_OPENCV := 0
---
> USE_OPENCV := 0
21c21
< # OPENCV_VERSION := 3
---
> OPENCV_VERSION := 3
50c50
< BLAS := atlas
---
> BLAS := open
78,80c78,80
< # PYTHON_LIBRARIES := boost_python3 python3.5m
< # PYTHON_INCLUDE := /usr/include/python3.5m \
< #                 /usr/lib/python3.5/dist-packages/numpy/core/include
---
> PYTHON_LIBRARIES := boost_python-py35 python3.5m
> PYTHON_INCLUDE := /usr/include/python3.5m \
>                 /usr/lib/python3.8/dist-packages/numpy/core/include
91c91
< # WITH_PYTHON_LAYER := 1
---
> WITH_PYTHON_LAYER := 1
94,95c94,95
< INCLUDE_DIRS := $(PYTHON_INCLUDE) /usr/local/include
< LIBRARY_DIRS := $(PYTHON_LIB) /usr/local/lib /usr/lib
---
> INCLUDE_DIRS := $(PYTHON_INCLUDE) /usr/local/include /usr/include/hdf5/serial
> LIBRARY_DIRS := $(PYTHON_LIB) /usr/local/lib /usr/lib /usr/lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu/hdf5/serial
EOF

patch Makefile <<'EOF'
415c415
< NVCCFLAGS += -ccbin=$(CXX) -Xcompiler -fPIC $(COMMON_FLAGS)
---
> NVCCFLAGS += -D_FORCE_INLINES -ccbin=$(CXX) -Xcompiler -fPIC $(COMMON_FLAGS)
EOF

patch CMakeLists.txt <<'EOF'
65a66
> set(${CMAKE_CXX_FLAGS} "-D_FORCE_INLINES ${CMAKE_CXX_FLAGS}")
EOF

find . -type f -exec sed -i -e 's^"hdf5.h"^"hdf5/serial/hdf5.h"^g' -e 's^"hdf5_hl.h"^"hdf5/serial/hdf5_hl.h"^g' '{}' \;

pushd python
patch requirements.txt <<'EOF'
3d2
< scipy>=0.13.2
EOF

for req in $(cat requirements.txt); do pip3 install --upgrade $req; done
popd

# caffe requires dateutil < 2, but dateutil < 2 isn't compatiable with python3
# this fixes it, albeit hackily...
pip3 install --upgrade python-dateutil

make all -j $(($(nproc) + 1))
make pycaffe -j $(($(nproc) + 1))
make distribute -j $(($(nproc) + 1))

echo "export PYTHONPATH=\"$(pwd)/python:\${PYTHONPATH}\"" >> /opt/auv/.zshrc_system
