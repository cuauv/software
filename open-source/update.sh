#!/usr/bin/env bash

# Usage: ./update.sh [dest_dir]

# dest_dir is a directory in which to create the open-source repo.
# Please don't do this inside the main repo, do it somewhere else.

# By default, assumes that this script is located in the master branch
# of the repo. If invoking from somewhere else, specify the path to
# the root of the repo as the second argument.

# Note: the excluded files still appear in the filesystem, but they
# are not tracked by the open-source repo.

cd "$1"

set -euo pipefail
LOC="$(dirname $0)"
F_EXCLUSIONS="$LOC/exclusions"
readarray -t EXCLUSIONS < "$F_EXCLUSIONS"
# this script lives in path/to/repo/open-source/
REPO_DIR="${2:-$(realpath $LOC/..)}"
REMOTE_URL="git@github.com:cuauv/software.git"

if [ $(git -C "$REPO_DIR" rev-parse --abbrev-ref HEAD) != "master" ]; then
    echo "Repo should probably be on master branch!" >&2
    echo -n "Continue (y/n)? " >&2
    read v
    [ "x$v" != "xy" ] && echo "Exiting." && exit 1
fi

if [ ! -d .git ]; then
    git init
    echo >> .git/info/exclude
    cat "$LOC/exclusions" >> .git/info/exclude
    # don't track the PREV_COMMIT file
    printf "\nPREV_COMMIT\n" >> .git/info/exclude

    git remote add origin "$REMOTE_URL"
fi

if [ -f PREV_COMMIT ]; then
    PREV_COMMIT="$(cat PREV_COMMIT)"
else
    PREV_COMMIT="4b825dc642cb6eb9a060e54bf8d69288fbee4904" # "first commit"
fi

FIRST_COMMIT="218b789f9528b941f9ab093996a99b9a5e505b91"

GIT_AUTHOR_NAME="CUAUV"
GIT_AUTHOR_EMAIL="leader@cuauv.org"
GIT_COMMITTER_NAME="CUAUV"
GIT_COMMITTER_EMAIL="leader@cuauv.org"
export GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL GIT_COMMITTER_NAME GIT_COMMITTER_EMAIL

prev_progress_len=0

git -C "$REPO_DIR" log $PREV_COMMIT..HEAD --format="%H %at %ai %ct %ci %s" | tac | (
nomatch=()
while read hash aunix_timestamp adate atime atimezone cunix_timestamp cdate ctime ctimezone message; do
    if case $message in "Merge pull request #"*) MSG="$message"; true;; *) [ $hash == $FIRST_COMMIT ] && MSG="First open-source commit";; esac; then
        printf "\rWriting commit \"$MSG\""
        printf ' %0.s' $(seq 1 $(($prev_progress_len-${#MSG})))
        prev_progress_len=${#MSG}

        git -C "$REPO_DIR" diff --binary --full-index "$PREV_COMMIT" "$hash" | git apply --whitespace=nowarn > /dev/null
        # sometimes we fail, probably because we're running git too fast?
        # anyway, retrying seems to fix it most of the time
        for i in {1..5}; do
            # add like this to ignore files correctly
            git add . \
                && break || sleep 1
        done

        for ((i=0;i<${#EXCLUSIONS[@]};i++)); do
            l="${EXCLUSIONS[$i]}"
            case l in
                "#"*) continue;;
                *)
                    ll=(`echo "$REPO_DIR/$l"`) 
                    [ ${#ll[@]} == 0 ] || [ ${#ll[@]} == 1 -a ! -e "${ll[0]}" ] && nomatch[$i]="$hash"
                ;;
            esac
        done

        echo -n "$hash" > PREV_COMMIT

        # same as above
        for i in {1..5}; do
            GIT_AUTHOR_DATE="$aunix_timestamp $atimezone" GIT_COMMIT_DATE="$cunix_timestamp $ctimezone" git commit -m "$MSG" > /dev/null \
                && break || sleep 1
        done

        PREV_COMMIT="$hash"
    fi
done
printf '\nFinished\n'
if [ -n ${nomatch+x} ] && [ ${#nomatch[@]} -gt 0 ]; then
    ml=0
    for ((i=0;i<${#EXCLUSIONS[@]};i++)); do
        nm="${EXCLUSIONS[$i]}"
        if [ $i -lt ${#nomatch[@]} ] && \
            [ -n ${nomatch[$i]+x} ] && [ ${#nm} -gt $ml ]; then
            ml=${#nm}
        fi
    done
    printf "%${ml}s  Last didn't match\n" "Exclusion"
    for ((i=0;i<${#EXCLUSIONS[@]};i++)); do
        if [ -z "${EXCLUSIONS[$i]}" ] || [ -z ${nomatch[$i]+x} ]; then continue; fi
        printf "%${ml}s  " "${EXCLUSIONS[$i]}"
        h=
        if [ "x$PREV_COMMIT" == "x${nomatch[$i]}" ]; then h=" (last)"; fi
        git --no-pager -C "$REPO_DIR" log --format="%h (%s)$h" -n1 ${nomatch[$i]}
    done
fi
)
