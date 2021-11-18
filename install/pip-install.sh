#!/usr/bin/env bash
set -xeuo pipefail

apt-get install -y libffi-dev openssl libssl-dev libgirepository1.0-dev

if [[ "$(uname -m)" = "aarch64" ]]; then
    export LD_LIBRARY_PATH=/usr/lib/aarch64-linux-gnu/tegra/
    #pip2 install cupy
    pip3 install cupy
fi

packages=(
    pyserial
    watchdog

    #cython
    flask
    gevent
    matplotlib
    paramiko
    pyyaml
    redis
    requests
    six
    tabulate
    termcolor
    tornado

    eventlet
    posix_ipc

    pygobject

    # pyparsing>=3 conflicts with packaging for some reason
    "pyparsing<3"

    nanomsg

    #numpy
    #scipy

    protobuf

    fire

    tomlkit
)

# packages2=(
#     posix_ipc
#     # pygame
#     cryptography
# )

packages3=(
    flask
    flask-socketio
    posix_ipc

    pylint
    rope # Refactoring

    pgi
    pycairo

    mypy
)

# it would be better to not have this
FLAGS="--ignore-installed"

#pip2 install $FLAGS "${packages[@]}" "${packages2[@]}"
pip3 install $FLAGS "${packages[@]}" "${packages3[@]}"
