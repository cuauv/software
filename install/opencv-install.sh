packages=(
    cmake
    ffmpeg
    libavcodec-dev
    libavformat-dev
    libavresample-dev
    libavutil-dev
    libgflags-dev
    libgoogle-glog-dev
    libgphoto2-dev
    libgtk2.0-dev
    libhdf5-serial-dev
    libjpeg-dev
    liblapacke-dev
    libleveldb-dev
    liblmdb-dev
    libopenblas-dev
    libpng-dev
    libprotobuf-dev
    libsnappy-dev
    libswscale-dev
    pkg-config
    protobuf-compiler
)

apt-get install -y ${packages[@]}
apt-get install -y --no-install-recommends libboost-all-dev


mkdir -p /build_tmp_opencv
pushd /build_tmp_opencv

OPENCV_VERSION="3.4.1"

curl -L https://github.com/opencv/opencv/archive/$OPENCV_VERSION.tar.gz -o opencv.tar.gz
curl -L https://github.com/opencv/opencv_contrib/archive/$OPENCV_VERSION.tar.gz -o opencv_contrib.tar.gz
tar -xf opencv.tar.gz
tar -xf opencv_contrib.tar.gz

cd opencv-$OPENCV_VERSION
mkdir -p build
cd build

CMAKE_FLAGS=()
# link modules
CMAKE_FLAGS+=(-DOPENCV_EXTRA_MODULES_PATH=../../opencv_contrib-${OPENCV_VERSION}/modules)

# don't build opencv dnn, since importing it conflicts with importing caffe
CMAKE_FLAGS+=(-DBUILD_opencv_dnn_modern=OFF)

# Python!
CMAKE_FLAGS+=(-DBUILD_opencv_python2=ON -DBUILD_opencv_python3=ON)

# build perf flags
CMAKE_FLAGS+=(-DBUILD_EXAMPLES=OFF -DBUILD_opencv_apps=OFF -DBUILD_DOCS=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF)

if [[ "$(uname -m)" == "aarch64" ]]; then
    # CUDA
    CMAKE_FLAGS+=(-DWITH_CUDA=ON -DCUDA_TOOLKIT_ROOT_DIR=/usr/local/cuda-9.0)
fi

# Ensure FFMPEG
CMAKE_FLAGS+=(-DWITH_FFMPEG=ON)

# build with ninja
CMAKE_FLAGS+=(-GNinja)

cmake ${CMAKE_FLAGS[@]} ..
ninja
ninja install

rm -rf /build_tmp_opencv
