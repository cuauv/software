packages=(
    cmake
    libavcodec-dev
    libavformat-dev
    libavresample-dev
    libavutil-dev
    libgflags-dev
    libgoogle-glog-dev
    libgphoto2-dev
    # libgtk2.0-dev # depends on python2
    libgtk-3-dev
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

packages_no_recommends=(
    ffmpeg
    # libboost-all-dev # also depends on python2
)

apt-get install -y ${packages[@]}
apt-get install -y --no-install-recommends ${packages_no_recommends[@]}

mkdir -p /build_tmp_opencv
pushd /build_tmp_opencv

OPENCV_VERSION="4.1.1"

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
CMAKE_FLAGS+=(-DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=ON)

# build perf flags
CMAKE_FLAGS+=(-DBUILD_EXAMPLES=OFF -DBUILD_opencv_apps=OFF -DBUILD_DOCS=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF)

if [[ "$(uname -m)" == "aarch64" ]]; then
    # CUDA
    # Pascal is the version we need for the Jetson TX2
    CMAKE_FLAGS+=(-DWITH_CUDA=ON -DCUDA_TOOLKIT_ROOT_DIR=/usr/local/cuda-10.0 -DCUDA_GENERATION=Pascal)
fi

# Ensure FFMPEG
CMAKE_FLAGS+=(-DWITH_FFMPEG=ON)

# Enable non-free modules
CMAKE_FLAGS+=(-DOPENCV_ENABLE_NONFREE=ON)

# build with ninja
CMAKE_FLAGS+=(-GNinja)

# generate pkg-config
CMAKE_FLAGS+=(-DOPENCV_GENERATE_PKGCONFIG=ON)

cmake ${CMAKE_FLAGS[@]} ..
ninja
ninja install

rm -rf /build_tmp_opencv
