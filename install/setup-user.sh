#!/usr/bin/env bash
set -xeuo pipefail

useradd --create-home --shell /bin/bash --groups sudo,wireshark,dialout software
echo "software:software" | chpasswd

echo "%sudo   ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers
