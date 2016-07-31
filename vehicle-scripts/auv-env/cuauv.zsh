# Set envirnormental variables
export CUAUV_SOFTWARE='/home/software/cuauv/software/'
export PYTHONPATH="$PYTHONPATH:$CUAUV_SOFTWARE"
export CSC_OPTIONS="-L${CUAUV_SOFTWARE}link-stage -C -I${CUAUV_SOFTWARE} -I${CUAUV_SOFTWARE} -Wl,-rpath,${CUAUV_SOFTWARE}link-stage"
export CHICKEN_REPOSITORY=${CUAUV_SOFTWARE}link-stage/chicken
export GOPATH="${CUAUV_SOFTWARE}gocode"
export PATH="$PATH:${CUAUV_SOFTWARE}link-stage:${CUAUV_SOFTWARE}gocode/bin"

# Build the software repo
alias build="ninja -C "$CUAUV_SOFTWARE" -k 1000"
alias av="auv-visiond"

# Ensure anv-env-set is sourced
alias auv-env-set="source auv-env-set"

# Useful short aliases
alias t="trogdor"
alias c="auv-control-helm"
alias s="auv-shm-editor"
alias auv-mr="auv-mission-runner"
alias auv-pt="auv-pooltest"
alias aslam="auv-aslam-cli"
alias shm="auv-shm-cli"

# Disable shared history
setopt no_share_history

# Check pooltest status

RED="\033[1;31m"
BLUE="\033[0;34m"
YELLOW="\033[0;33m"
GREEN="\033[1;32m"
ENDCOLOR="\033[0m"
LINK="/var/log/auv/current"
if [[ -h "$LINK" ]]; then
  CURRENT=`readlink "$LINK"` 
  NAME=`basename "$CURRENT"`
  if ! [[ "$NAME" = "none" ]]; then
    START=`jq < "$LINK"/metaStart.json .startTime`
    DATE=`date -d@"$START"`
    echo -e "$GREEN""Pooltest currently running!""$ENDCOLOR" '\c'
    echo -e "Name:" '\c'
    echo -e "$BLUE""$NAME""$ENDCOLOR" '\c'
    echo -e "Start Time:" '\c'
    echo -e "$RED""$DATE""$ENDCOLOR"
  else
    echo -e "$YELLOW""No pooltest currently running!""$ENDCOLOR"
  fi
else
  echo -e "$YELLOW""No pooltest currently running!""$ENDCOLOR"
fi


auv-env-set
