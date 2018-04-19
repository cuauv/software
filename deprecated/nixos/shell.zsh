# Colors
red=$'\e[0;31m'
grn=$'\e[0;32m'
yel=$'\e[0;33m'
blu=$'\e[0;34m'
#mag=$'\e[1;35m'
#cyn=$'\e[1;36m'
endColor=$'\e[0m'

# Colorful prompt functions

echoPrompt() {
	echo "$1►  $endColor$2"
}

echoInfo() {
	echoPrompt $grn "$@"
}

echoWarn() {
	echoPrompt $yel "$@"
}

echoError() {
	echoPrompt $red "$@"
}

envScript="${CUAUV_SOFTWARE}nixos/binaries/env.sh"

switch() {
	if sudo nixos-rebuild switch; then
		echoInfo "Realized NixOS global configuration"
	else
		echoError "Error realizing NixOS global configuration"
	fi

	# Note: we could capture less environment vars if we could run a pure nix-shell,
	# but we can't because then we can't access sudo

	captureScript="${CUAUV_SOFTWARE}nixos/capture-env.sh"

	# Give the sub nix-shell a separate temporary dir,
	# or else it conflicts with the current one
	subTmp=/tmp/sub-nix-shell
	mkdir -p "$subTmp"

	# Clear env like nix-shell --pure (but we can still grab sudo)
	if sudo su -l software -c "TMPDIR=$subTmp nix-shell ${CUAUV_SOFTWARE}nixos/configs/shell.nix \
		--indirect --add-root ~/.gcroots/dep \
		--show-trace \
		--run \"export CUAUV_SOFTWARE='${CUAUV_SOFTWARE}'; $captureScript\""; then

		source "$envScript"
		echoInfo "Realized NixOS shell configuration"
	else
		echoError "Error realizing NixOS shell configuration"
	fi

	rm -rf "$subTmp"
}

[[ -f "$envScript" ]] && source "$envScript" || switch

[[ -n "$INSTALLING_NIXOS" ]] && exit 0

# Check for new version of install script
installSha256="$(git -C "$CUAUV_SOFTWARE" cat-file master:nixos/install.sh -p \
	| sha256sum "${CUAUV_SOFTWARE}nixos/install.sh" \
	| cut -d' ' -f1)"
installVersionPath="/home/software/.install-version"
if [[ -f "$installVersionPath" ]]; then
	if [[ "$(cat $installVersionPath)" != "$installSha256" ]]; then
		echoWarn "A new version of the software stack install script is available on master."
		echoWarn "Rerun it outside the container (or here if on a sub), and"
		echoWarn "don't overwrite the software stack installation."
		echoWarn "If on a container, reboot it."
		echo
	fi
else
	echo -n "$installSha256" > "$installVersionPath"
fi

# Setup git ssh keys (stolen from install script)
if ! [[ -f "$HOME/.ssh/id_rsa" ]]; then
	echoInfo "Generating SSH keys"
	echo -n "Bitbucket username (netid minus @cornell.edu): "
	read -r username
	echo -n "Bitbucket password: "
	read -rs password
	echo
	email="$username@cornell.edu"

	rm -rf .ssh
	ssh-keygen -C "$email" -N "" -f "$HOME/.ssh/id_rsa"

	echoInfo "Posting key to Bitbucket"
	json="{\"text\": \"$(cat "$HOME"/.ssh/id_rsa.pub)\"}"
	curl -s -X POST -H "Accept: application/json" -H "Content-Type: application/json" -d "$json" -u "$username":"$password" https://bitbucket.cuauv.org/rest/ssh/1.0/keys
	echo
fi

# Haskell vars
if [ -e ~/.nix-profile/bin/ghc ]; then
	export NIX_GHC="$HOME/.nix-profile/bin/ghc"
	export NIX_GHCPKG="$HOME/.nix-profile/bin/ghc-pkg"
	export NIX_GHC_DOCDIR="$HOME/.nix-profile/share/doc/ghc/html"
	export NIX_GHC_LIBDIR="$HOME/.nix-profile/lib/ghc-$($NIX_GHC --numeric-version)"
fi

# Hack for cave, explanation?
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$GSETTINGS_SCHEMAS_PATH"

# Set up home directory
setopt no_share_history
defaultDir="${CUAUV_SOFTWARE}vehicle-scripts/auv-env/home/default"
if [[ -z "$REMOTE_USER" ]] || [[ -z "$REMOTE_NETID" ]]; then
	echoWarn "Hi! You need to configure your public SSH key."
	echoWarn "See NixOS Software Stack Usage on the wiki."
	echo
	export HOME="$defaultDir"

else
	homeDir="$(dirname $defaultDir)/$REMOTE_NETID"
	if ! [[ -d "$homeDir" ]]; then
		echoWarn "Hi $REMOTE_USER ($REMOTE_NETID)! You're using the default config."
		echoWarn "To remove this message, you can create your own config in:"
		echoWarn "$homeDir"
		echo
		export HOME="$defaultDir"

	else
		export HOME="$homeDir"
		setopt share_history
		# Use default .zshrc if not specified in home dir
		[[ -f "$HOME"/.zshrc ]] || export ZDOTDIR="$defaultDir"
	fi
fi

# Set up .Xauthority for X forwarding or native X rendering
xauthPath="/home/software/.Xauthority"
[[ -f "$xauthPath" ]] && cp "$xauthPath" ~/

# Git identity
export GIT_AUTHOR_NAME="$REMOTE_USER"
export GIT_AUTHOR_EMAIL="$REMOTE_NETID@cornell.edu"
export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME"
export GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"

# Useful aliases
alias build="ninja -C "$CUAUV_SOFTWARE" -k 1000" # Build the software repo
alias cs="cd $CUAUV_SOFTWARE"
alias t="trogdor"
alias c="auv-control-helm"
alias s="auv-shm-editor"
alias auv-mr="auv-mission-runner"
alias auv-pt="auv-pooltest"
alias aslam="auv-aslam-cli"
alias shm="auv-shm-cli"

# Check pooltest status
if [[ "$CUAUV_CONTEXT" = "vehicle" ]]; then
	LINK="$CUAUV_LOG/current"
	if [[ -h "$LINK" ]]; then
		CURRENT="$(readlink "$LINK")"
		NAME="$(basename "$CURRENT")"
		if ! [[ "$NAME" = "none" ]]; then
			START="$(jq < "$LINK"/metaStart.json .startTime)"
			DATE="$(date -d@"$START")"
			echoInfo "Pooltest currently running!" '\c'
			echoInfo "Name:" "$blu""$NAME""$endColor"
			echoInfo "Start Time:" "$red""$DATE""$endColor"
		else
			echoWarn "No pooltest currently running!"
		fi
	else
		echoWarn "No pooltest currently running!"
	fi
	echo
fi
