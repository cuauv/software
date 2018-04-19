#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# This script bootstraps a NixOS environment around the CUAUV software stack.
# Run it outside NixOS to generate a development container, or run it on a sub
# inside a minimal NixOS installation to turn it into the CUAUV software NixOS
# configuration
# 
# Usage: install.sh [vehicle]
# vehicle defaults to "container", which refers to container.nix.
# Use a sub name like "thor" when installing on a sub.
# 
# See also: https://github.com/NixOS/nixpkgs/issues/9735

# Config
nixosImage="https://d3g5gsiof5omrk.cloudfront.net/nixos/17.03/nixos-17.03.914.a00c4aea5f/nixos-17.03.914.a00c4aea5f-x86_64-linux.ova"
nixosImageSha256="5c58a24ea55e2e6a0673dcfc11970cb39e5dd3c11dda27dbeb20a7f42548b2bd"
softwareRepo="ssh://git@bitbucket.cuauv.org:7999/sof/software.git"

containerName=cuauv
cacheDir="${USER_HOME:-}/.cache/cuauv/nixos"
containerBootTime=10
# End config

CUAUV_VEHICLE="${1:-container}"
containerDir="/var/lib/lxc/$containerName"

# Ubuntu/Debian may have executables in here we need
export PATH="$PATH:/sbin"

# http://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
scriptPath="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/$(basename "$0")"

dependencies="\
sudo
curl
tar
VBoxManage
losetup
rsync
dnsmasq
lxc-create
du
systemctl
xauth
sha256sum"

lxcConfig="
# Template used to create this container: /usr/share/lxc/templates/lxc-debian
# Parameters passed to the template:
# For additional config options, please look at lxc.container.conf(5)
lxc.network.type = veth
lxc.network.flags = up
lxc.network.link = lxcbr0
lxc.network.ipv4 = 10.0.3.141/24
# Common configuration
lxc.include = /usr/share/lxc/config/debian.common.conf
lxc.aa_allow_incomplete = 1
# Container specific configuration
lxc.arch = amd64
lxc.rootfs = /var/lib/lxc/cuauv/rootfs
lxc.utsname = auvbox
lxc.mount = /var/lib/lxc/cuauv/fstab
lxc.init_cmd = /nix/var/nix/profiles/system/init
lxc.mount.entry = /dev/dri dev/dri none bind,optional,create=dir
lxc.mount.entry = /dev/snd dev/snd none bind,optional,create=dir
lxc.mount.entry = /tmp/.X11-unix tmp/.X11-unix none bind,optional,create=dir
lxc.mount.entry = /dev/video0 dev/video0 none bind,optional,create=file
lxc.mount.entry = /dev/input dev/input none bind,optional,create=dir
lxc.cgroup.devices.allow = \" c 226:* rwm\"
"

lxcFstab="tmpfs run tmpfs rw,nodev,size=255648k,mode=755 0 0"

# Colors
red=$'\e[1;31m'
#grn=$'\e[1;32m'
yel=$'\e[1;33m'
blu=$'\e[1;34m'
#mag=$'\e[1;35m'
#cyn=$'\e[1;36m'
endColor=$'\e[0m'

echoPrompt() {
	echo "$1►  $endColor$2"
}

echoInfo() {
	echoPrompt $blu "$@"
}

echoWarn() {
	echoPrompt $yel "$@"
}

echoError() {
	echoPrompt $red "$@"
}

copyProgress() {
	rsync -a "$@" --info=progress2
}

yesOrNo() {
	while true; do
		read -rp "$* [y/n]: " yn
		case "$yn" in
			[Yy]*) return 0;;
			[Nn]*) return 1;;
		esac
	done
}

bootstrapConfig() {
	cd /root

	if ! [[ -d "/home/software/cuauv/software" ]] \
		|| yesOrNo "Overwrite /home/software/cuauv/software ?"; then

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

		echoInfo "Cloning CUAUV software repo"
		rm -rf software
		nix-shell -p git --run \
			"GIT_SSH_COMMAND=\"ssh -o StrictHostKeyChecking=no\"\
			git clone \"$softwareRepo\""

		echoInfo "Building global CUAUV NixOS config"
		rm -r /etc/nixos || :
		cp -r software/nixos/configs /etc/nixos
		ln -s /etc/nixos/"$CUAUV_VEHICLE".nix /etc/nixos/configuration.nix
		nixos-generate-config
		nixos-rebuild switch --upgrade || :

		echoInfo "Setting up software user"
		rm -rf /home/software/cuauv
		mkdir -p /home/software/cuauv
		mv software /home/software/cuauv/software

		rm -rf /home/software/.ssh
		mv .ssh /home/software/

		chown -R software:users /home/software

		# Use NixOS config in repo
		rm /etc/nixos/configuration.nix || :
		rm -r /etc/nixos || :
		ln -s /home/software/cuauv/software/nixos/configs /etc/nixos
		ln -s /etc/nixos/"$CUAUV_VEHICLE".nix /etc/nixos/configuration.nix
		nixos-generate-config
	fi

	echoInfo "Initializing nix-shell environment"
	touch /home/software/.zshrc # Keep parent shells quiet
	sha256sum /home/software/cuauv/software/nixos/install.sh | cut -d' ' -f1 > /home/software/.install-version
	chown -R software:users /home/software/
	su -l software -c "export INSTALLING_NIXOS=1; \$SHELL"

	echoInfo "Optimizing NixOS"
	nix-collect-garbage -d

	echoInfo "Cleaning container cache"
	rm -f .bashrc

	echoInfo "Exiting container"
	exit
}

createContainer() {
	echoInfo "Setting up install cache"
	mkdir -p "$cacheDir"
	cd "$cacheDir"

	echoInfo "Downloading and extracting NixOS VirtualBox appliance"
	if [[ ! -f "nixos.ova" ]]; then
		curl -L "$nixosImage" -o nixos.ova
	fi

	echoInfo "Checking NixOS VirtualBox appliance integrity"
	if ! sha256sum nixos.ova | grep "$nixosImageSha256" > /dev/null; then
		echoWarn "Checksum mismatch, redownloading image"
		rm -rf ./*
		createContainer
		exit
	fi

	echoInfo "Extracting image from NixOS VirtualBox appliance"
	ls ./*.vmdk > /dev/null 2>&1 || tar -xvf nixos.ova
	[[ ! -f "nixos.img" ]] && VBoxManage clonehd ./*.vmdk nixos.img --format RAW

	echoInfo "Mounting image"
	loopDevice=$(losetup -f --show -P nixos.img)
	mount "${loopDevice}p1" /mnt

	echoInfo "Creating container"
	lxc-create --name "$containerName" --template=none
	copyProgress /mnt/* "$containerDir/rootfs/"

	echoInfo "Unmounting image"
	umount /mnt 2> /dev/null
	losetup -d "${loopDevice}" 2> /dev/null

	if [[ -d "$cacheDir" ]]; then
		cacheSize=$(du -h "$cacheDir" | cut -d$'\t' -f1)
		yesOrNo "Delete $cacheSize temporary installation cache ($cacheDir)?" && rm -rf "$cacheDir"
	fi
}

start() {
	echoInfo "Welcome to the CUAUV software stack installation script!"

	echoInfo "Checking dependencies"
	foundMissingDep=false
	while read -r line; do
		if ! command -v "$line" >/dev/null 2>&1; then
			foundMissingDep=true
			echoError "Missing executable: $line"
		fi
	done <<< "$dependencies"
	if [[ "$foundMissingDep" == true ]]; then
		echoError "Exiting due to missing dependencies"
		exit 1
	fi
	badLxcVersion=$(($(lxc-create --version | cut -d. -f1) < 2))
	if [[ $badLxcVersion -eq 1 ]]; then
		echoError "lxc version >= v2.* required"
		exit 1
	fi

	lxc-ls | grep -Eq "^$containerName" >/dev/null || createContainer

	echoInfo "Configuring LXC"
	echo "$lxcConfig" > "$containerDir/config"
	echo "$lxcFstab" > "$containerDir/fstab"
	sed -i 's/USE_LXC_BRIDGE="false"/USE_LXC_BRIDGE="true"/' /etc/default/lxc
	systemctl enable lxc-net
	systemctl start lxc-net

	echoInfo "Copying this script into container"
	cp "$scriptPath" "$containerDir/rootfs/root/.bashrc"

	echoInfo "Booting container"
	if ! lxc-info -n cuauv | grep -E "^State:" | grep RUNNING > /dev/null; then
		systemctl start lxc@"$containerName"
		echoInfo "Waiting $containerBootTime to boot"
		sleep $containerBootTime
	fi

	echoInfo "Entering container"
	HOME="/root" lxc-attach -n "$containerName"

	echoInfo "Installation finished, happy hacking!"
}

# Bootstrap the config if we're inside NixOS
os="$(grep -E '^ID=' /etc/os-release | cut -d= -f2)"
if [[ $(id -u) -ne 0 ]]; then
	sudo USER_HOME="$HOME" "$scriptPath" "$@"
elif [[ "$os" = "nixos" ]]; then
	bootstrapConfig
else
	start
fi
