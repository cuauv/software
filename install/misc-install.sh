# **************** zsh ****************

apt-get -y install zsh

# The stacks should be mounted at /home/software/cuauv/software
rm /home/software/.bashrc

sudo -u software cat > /home/software/.zshrc <<'EOF'
. /home/software/.zshrc_user
. /opt/auv/.zshrc_system
EOF

sudo -u software ln -s /home/software/cuauv/software/install/zshrc /home/software/.zshrc_user

ZSH=/home/software/.oh-my-zsh
sudo -u software git clone --depth=1 https://github.com/robbyrussell/oh-my-zsh.git "${ZSH}"
# cp $ZSH/templates/zshrc.zsh-template /home/software/.zshrc
chsh -s $(grep /zsh$ /etc/shells | tail -1) software

# **************** ssh ****************

apt-get -y install openssh-server sshfs

echo "PermitEmptyPasswords yes" | tee -a /etc/ssh/sshd_config
echo "PasswordAuthentication yes" | tee -a /etc/ssh/sshd_config
echo "X11UseLocalhost no" | tee -a /etc/ssh/sshd_config
echo "UsePAM yes" | tee -a /etc/ssh/sshd_config

echo "auth sufficient pam_permit.so" > /etc/pam.d/sshd

sudo -u software mkdir -p /home/software/.ssh

sudo -u software cat > /home/software/.ssh/config <<'EOF'
Host loglan
  Hostname cuauv.org
  Port 2222
  User software
  ForwardX11 yes
  ForwardX11Timeout 20d
  ForwardX11Trusted yes
EOF

# **************** sloth ****************

mkdir -p /build_tmp_sloth

cd /build_tmp_sloth
git clone https://github.com/alexrenda/sloth.git sloth

cd sloth

python3 setup.py install

rm -rf /build_tmp_sloth

# **************** sloth ****************

mkdir -p /tmp/ueye
pushd /tmp/ueye

apt-get -y install libqtgui4

if [[ "$(uname -m)" == "x86_64" ]]; then
    wget https://cuauv.org/nix-res-private/uEye-Linux-4.90.06-64.tgz
    tar -xvf uEye-Linux-4.90.06-64.tgz
    ./ueyesdk-setup-4.90.06-eth-amd64.gz.run
fi


mkdir /var/log/auv && chown software /var/log/auv & chgrp software /var/log/auv

# **************** ueye ****************

mkdir -p /usr/local/share/ueye/ueyeethd/

sudo bash -c "cat > /usr/local/share/ueye/ueyeethd/ueyeethd.conf << 'EOF'
;Ni1
[Parameters]
 Interfaces = camc

[camc]
 Port_Base = 50000
EOF"
