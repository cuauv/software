#!/usr/bin/env bash
set -xeuo pipefail

useradd --create-home --shell /bin/bash --groups sudo,wireshark,dialout,video software
echo "software:software" | chpasswd

mv /dependencies/ssh /home/software/.ssh
chown -R software:software /home/software/.ssh
chmod 700 /home/software/.ssh
find /home/software/.ssh -type f -exec chmod 600 {} \;
chmod 644 /home/software/.ssh/id_rsa.pub

echo "%sudo   ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers

mkdir -p /var/log/auv/current
chown -R software:software /var/log/auv/current
