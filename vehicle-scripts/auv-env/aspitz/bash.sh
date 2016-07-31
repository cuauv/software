#!/bin/sh
export MYVIMRC="$(dirname $0)/.vimrc"
export VIMINIT="source $MYVIMRC"
export GIT_AUTHOR_NAME="Alex Spitzer"
export GIT_AUTHOR_EMAIL="aes368@cornell.edu"
export GIT_COMMITTER_NAME="Alex Spitzer"
export GIT_COMMITTER_EMAIL="aes368@cornell.edu"
export EDITOR=vim
alias cs="cd $CUAUV_SOFTWARE"
alias bat="auv-shm-cli -w merge_status"
alias mr="auv-mr"
