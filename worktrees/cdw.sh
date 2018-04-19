#!/usr/bin/env bash
set -euo pipefail

export BRANCH=${1:-master}
echo Entering workspace "'$BRANCH'"

mkdir -p ~/cuauv/worktrees/
pushd ~/cuauv/worktrees/

if [ ! -d "software.git" ]; then
    git clone --bare ssh://git@bitbucket.cuauv.org:7999/sof/software.git software.git
fi

if [ ! -d "$BRANCH" ]; then
    pushd software.git
    git fetch origin "$BRANCH:$BRANCH" || (read -p "Create branch $BRANCH? [yN] "; [[ $REPLY =~ ^[Yy]$ ]] && git branch "$BRANCH" master)
    git worktree add "../$BRANCH" "$BRANCH"
    popd
    pushd "$BRANCH"
    sed -i.bak 's/.*software.git/..\/software.git/' .git
    rm .git.bak
    popd
fi

pushd "$BRANCH"

(docker-compose pull; docker-compose up -d)|| (export BRANCH=master && docker-compose pull; docker-compose up -d)

CONTAINER_IP=$(docker inspect "cuauv-${BRANCH:-master}"  | grep IPAddress | cut -d '"' -f 4)
sleep 1
TERM=xterm ssh auv-docker -o "Hostname=$CONTAINER_IP" -p 22
