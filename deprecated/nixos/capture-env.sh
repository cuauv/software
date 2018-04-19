#!/usr/bin/env bash

envScript="${CUAUV_SOFTWARE}"nixos/binaries/env.sh

mkdir -p "$(dirname "$envScript")"

env | grep -Ev '^(HOME|SSH_|DISPLAY|XDG_|REMOTE_|GIT_|NIX_PATH|PWD|TMPDIR)' |\
while read -r line; do
	if [[ -n "$line" ]]; then
		v="$(echo "$line" | cut -d= -f1)"
		val="$(echo "$line" | cut -d= -f2-)"
		echo "export $v='$val'"
	fi
done > "$envScript"
