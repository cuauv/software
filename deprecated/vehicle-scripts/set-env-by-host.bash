log() {
    echo "[CUAUV] [$1] $2"
}

if [ -z "$SSH_TTY" ]; then
    # Not running via SSH.
    # If we echo from this when connecting via SFTP, we get 'message too long' errors...
    return
fi

#LAST_HOST=$(last -a -d | grep "logged in" | head -n 1 | awk '{print $10}')
#LAST_HOST=$(who -u | grep " $(echo $$) " | awk '{ print substr($7, 2, length($7)-2) }')

# /dev/pts/3 -> 3
#NTTY=$(tty | awk 'BEGIN { FS = "/" } ; { print $4 }')
#LAST_HOST=$(who -u | grep "pts/$NTTY" | awk '{ print substr($7, 2, length($7)-2) }')
LAST_HOST=$(who -um | awk '{ print substr($7, 2, length($7)-2) }')
export SSHER=$LAST_HOST

log info "Loading environment for $LAST_HOST..."

# Users put their desired environment files in ~/envs/$LAST_HOST/ .

export MYENV=~/envs/$LAST_HOST

if [ ! -d "$MYENV" ]; then
    log warn "Environment directory for $LAST_HOST does not exist."
    return
fi

# Reconfigures VIM to look for a vimrc at $MYENV/vimrc.vim .
# Sources $MYENV/env .

# https://stackoverflow.com/a/26024869


if [ ! -f "$MYENV/vimrc.vim" ]; then
    log warn "vimrc.vim for $LAST_HOST does not exist."
else
    export MYVIMRC="$MYENV/vimrc.vim"
    export VIMINIT="source $MYVIMRC"
fi

if [ ! -f "$MYENV/env" ]; then
    log warn "env for $LAST_HOST does not exist."
else
    . $MYENV/env
fi
