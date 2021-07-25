
BRANCH="$(git symbolic-ref HEAD | cut -f 3 -d /)"
if [ "$BRANCH" = "master" ]; then
    git fetch origin master
    git checkout --force -B master origin/master
    git reset --hard
    git clean -fdx
else
    echo "Error: auv-hard-reset can only be used on the master branch."
fi
