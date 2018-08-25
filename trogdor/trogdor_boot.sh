#!/usr/bin/env zsh

if [[ "$(uname -m)" != "aarch64" ]]; then
    exit 0
fi

sleep 5


tmux new -d -s trogdor_services
tmux send-keys -t trogdor_services.0 "trogdor start" ENTER


# Don't start the mission automatically (for now) uncomment for competition
screen -d -m auv-mission-runner

# tmux new -d -s trogdor_mission
# tmux send-keys -t trogdor_mission.0 "sleep 30; auv-mission-runner" ENTER

while :; do
    sleep 60
done
