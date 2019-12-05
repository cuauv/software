# Dependencies that are large and don't change often.
#
# Changing these likely entails a long build, as opposed to changing something
# later in the build process.

# Debian dependencies solely for other foundational dependencies
apt-get install -y apt-utils

packages=(
    autoconf
    build-essential
    cmake
    curl
    # cython
    gcc
    git
    ninja-build
    # python
    # python-matplotlib
    # python-pip
    # python3
    # python3-dev
    # python3-numpy
    # python3-pip
    # python3-scipy
    screen
    sudo
    unzip
    wireshark
    wget
    xorg
    x11-apps
)

for f in "${packages[@]}";  do
    echo "========= Installing $f"
    apt-get install -y "$f"
done

# Upgrade pip
#pip2 install --upgrade pip
#pip3 install --upgrade pip

# Allow wireshark to be run by non-root users
echo "wireshark-common wireshark-common/install-setuid boolean true" | sudo debconf-set-selections
dpkg-reconfigure wireshark-common

# libliquid dsp for hydromathd
LIQUID_VERSION="1.3.1"
wget "https://github.com/jgaeddert/liquid-dsp/archive/v${LIQUID_VERSION}.zip"
unzip "v${LIQUID_VERSION}.zip"
pushd "liquid-dsp-${LIQUID_VERSION}"
./bootstrap.sh
./configure
make -j "$(nproc)"
make install
rm -r "/v${LIQUID_VERSION}.zip"
