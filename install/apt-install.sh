#!/usr/bin/env bash

packages=(
    # general
    autossh
    bc
    dialog
    gksu
    gnuplot
    htop
    iotop
    iputils-ping
    iputils-tracepath
    ipython
    ipython3
    jq
    libboost-python-dev
    libeigen3-dev
    libgtest-dev
    libncurses-dev
    libpopt-dev
    libpython-dev
    nano
    neovim
    nload
    screen
    shellcheck
    silversearcher-ag
    stow
    tmux
    usbutils
    wget
    vlc

    # serial
    libgtest-dev
    libgtkmm-3.0-dev
    libprotoc-dev
    protobuf-compiler

    # vision
    libavcodec-dev
    libavformat-dev
    libdc1394-22-dev
    libswscale-dev
    python3-yaml
    python3-gi-cairo

    # trogdor
    expect-dev
    python-dbus

    # visualizer
    libconfig++
    libglfw3-dev
    libglm-dev

    # fishbowl
    libeigen3-dev

    # auvlog
    libnanomsg-dev
    python-redis
    redis-server

    # syscheck
    sysstat

    # other
    python-redis # (to run the logging server)
    python-wxgtk3.0 # (for auv-shm-editor)
    python3-paramiko # (for uptime)
    python3-tabulate

    # aslam
    liblzma-dev

    #slam
    libzmq-dev
    python3-zmq
)

apt-get install -y software-properties-common # For add-apt-repository
apt-get install -y libdbus-1-dev
add-apt-repository ppa:neovim-ppa/stable
apt-get clean -y
apt-get update -o Acquire::CompressionTypes::Order::=gz -y

apt-get install -y "${packages[@]}" ||
    apt-get install -y "${packages[@]}" ||
    apt-get install -y "${packages[@]}"

# Add Neovim alternatives (https://github.com/neovim/neovim/wiki/Installing-Neovim)
update-alternatives --install /usr/bin/vi vi /usr/bin/nvim 60
update-alternatives --config vi
update-alternatives --install /usr/bin/vim vim /usr/bin/nvim 60
update-alternatives --config vim
update-alternatives --install /usr/bin/editor editor /usr/bin/nvim 60
update-alternatives --config editor


# ***************** gtest *******************
cd /usr/src/gtest
cmake CMakeLists.txt
make

# copy or symlink libgtest.a and libgtest_main.a to your /usr/lib folder
cp *.a /usr/lib
