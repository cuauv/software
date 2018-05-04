#!/bin/bash
SCRIPT_NAME=$(basename "$0")

CUAUV_DOCKER_TMP_FILE="/tmp/cuauv-docker.config"

read -r -d '' HELP << EOM
 SYNOPSIS
    ${SCRIPT_NAME} command

 DESCRIPTION
    A CUAUV in house script to mangae the docker container.
    For any concerns message Tennyson or Zander.

 COMMANDS
    build                         Builds the docker container and tags it as
                                  lezed1/cuauv

    run                           Runs the docker container tagged as
                                  lezed1/cuauv, starts it's SSH server, and runs
                                  bash in that container in the forground. When
                                  bash exits the container will be deactivated

    vehicle VEHICLE_NAME          Similar to "run", but sets up extra
                                  environmental information for running directly
                                  on a vehicle

    ssh                           SSH into a container. This requires the ip
                                  of the container (which is printed out on the
                                  first line when the container first runs). See
                                  examples for how to provide the ip address. If
                                  no ip address is provided the script will
                                  prompt for it.

    help                          this information screen

 EXAMPLES
    ${SCRIPT_NAME} build
    ${SCRIPT_NAME} run
    ${SCRIPT_NAME} ssh
    ${SCRIPT_NAME} ssh 127.17.0.2
    ${SCRIPT_NAME} help
EOM

scriptHelp() {
    echo "$HELP"
}

dockerBuild() {
    docker build . -t lezed1/cuauv
}

promptToBuild() {
    # check with the user if they want to build the docker container now
     echo -n "Do you wish to build docker now? [Yn]"
     read -r yn
     case $yn in
         [Yy]* ) dockerBuild;;
         ""    ) dockerBuild;;
         [Nn]* ) echo "skipping" ;;
         *     ) echo "assuming no, skipping";;
     esac
 }

dockerRun() {
    if [ "$(uname)" == "Darwin" ]; then
        dockerMacRun
        return
    fi

    CUAUV_DIR=$(dirname "$(realpath "$0")")

    docker run \
        -it \
        -e 'DISPLAY=${DISPLAY}' \
        -v "$CUAUV_DIR:/home/software/cuauv/software" \
        -v "/tmp/.X11-unix:/tmp/.X11-unix" \
        -v /usr/share/icons:/usr/share/icons:ro \
        --device "/dev/dri:/dev/dri" \
        --ipc=host \
        lezed1/cuauv \
        /bin/bash -c "echo '==================' && hostname -i  && echo '==================' && sudo /sbin/my_init" \
    | tee $CUAUV_DOCKER_TMP_FILE
    rm -f $CUAUV_DOCKER_TMP_FILE
}

dockerMacRun() {
    CUAUV_DIR=$(dirname "$(realpath "$0")")

    docker run \
        -it \
        -e 'DISPLAY=${DISPLAY}' \
        -v "$CUAUV_DIR:/home/software/cuauv/software" \
        -v "/tmp/.X11-unix:/tmp/.X11-unix" \
        -p 2222:22 \
        -p 5000:5000 \
        -p 8080:8080 \
        --ipc=host \
        lezed1/cuauv \
        /bin/bash -c "echo '==================' && hostname -i  && echo '==================' && sudo /sbin/my_init" \
    | tee $CUAUV_DOCKER_TMP_FILE
    rm -f $CUAUV_DOCKER_TMP_FILE
}

dockerVehicle() {
    CUAUV_DIR=$(dirname "$(realpath "$0")")

    docker run \
           -it \
           -e "CUAUV_LOCALE=teagle" \
           -e "CUAUV_VEHCILE=${2}" \
           -e "CUAUV_CONTEXT=vehicle" \
           -v "$CUAUV_DIR:/home/software/cuauv/software" \
           -v /dev:/dev \
           -p 22:22 \
           -p 5000:5000 \
           -p 8080:8080 \
           --privileged \
           --ipc=host \
           asb322/cuauv-jetson \
           /bin/bash -c "echo '==================' && hostname -i  && echo '==================' && sudo /sbin/my_init" \
        | tee $CUAUV_DOCKER_TMP_FILE
    rm -f $CUAUV_DOCKER_TMP_FILE
}

dockerSsh() {
    if [ "$(uname)" == "Darwin" ]; then
        dockerMacSsh
        return
    fi

    IP=$(head -2 $CUAUV_DOCKER_TMP_FILE | tail -1)
    if [ ! -z "$IP" ]; then
        echo "Using IP address of most recently started container: ${IP}"
    fi
    while [ -z "$IP" ]; do
        echo    "What is the IP address of the container"
        echo -n "(first line the container prints out when run): "
        read -r IP
    done
    ssh -X -A software@"$IP" -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no
}

dockerMacSsh() {
    ssh -X -A software@localhost -p 2222 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no
}

case ${1} in
    build  ) dockerBuild;;
    run    ) dockerRun;;
    vehicle) dockerVehicle;;
    ssh    ) dockerSsh "${2}";;
    *      ) scriptHelp;;
esac
