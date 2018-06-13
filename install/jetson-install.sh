if [[ "$(uname -m)" != "aarch64" ]]; then
	echo "Skipping Jetson install because arch is $(uname -m)"
	exit 0
fi


# IDS uEye driver

pushd /tmp
wget https://cuauv.org/nix-res-private/uEyeSDK-4.90.00-ARM_LINUX_IDS_AARCH64_GNU.tgz
pushd /
tar -xvf /tmp/uEyeSDK-4.90.00-ARM_LINUX_IDS_AARCH64_GNU.tgz


# Inspired by https://github.com/charlielito/jetsontx2-opencv-tf/blob/master/Dockerfile

# COPY qemu-aarch64-static /usr/bin/

# These links come from the repository.json file downloaded by JetPack
NV_DL_PREFIX="http://developer.download.nvidia.com/devzone/devcenter/mobile/jetpack_l4t/3.2GA/m892ki/JetPackL4T_32_b196/"
NV_DEB_FILENAMES=(
    # "alias": "com.nvidia.l4t.driver4os_64_tx2", "version": "28.2"
    "Tegra186_Linux_R28.2.0_aarch64.tbz2"

    # "alias": "com.nvidia.cuda.l4t_64_tx2", "version": "9.0"
    "cuda-repo-l4t-9-0-local_9.0.252-1_arm64.deb"

    # "alias": "com.nvidia.middleware.cudnn_64_tx2", "version": "7.0"
    "libcudnn7_7.0.5.13-1+cuda9.0_arm64.deb"
    "libcudnn7-dev_7.0.5.13-1+cuda9.0_arm64.deb"
    "libcudnn7-doc_7.0.5.13-1+cuda9.0_arm64.deb"
)

packages=(
    bzip2
    curl
    wget
    unp
    sudo
)

apt-get install -y "${packages[@]}"

pushd /tmp

for NV_DEB_FILENAME in "${NV_DEB_FILENAMES[@]}"; do
    wget "${NV_DL_PREFIX}${NV_DEB_FILENAME}"
done

wget -qO - http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1404/x86_64/7fa2af80.pub | sudo apt-key add -

# drivers first

for NV_DEB_FILENAME in "${NV_DEB_FILENAMES[@]}"; do
    if [[ $NV_DEB_FILENAME == *.tbz2 ]]; then
        tar --use-compress-prog=bzip2 -xvf "./${NV_DEB_FILENAME}"
        ./Linux_for_Tegra/apply_binaries.sh -r /
        rm -rf ./LINUX_for_Tegra
    elif [[ $NV_DEB_FILENAME == *.deb ]]; then
        dpkg -i "./${NV_DEB_FILENAME}"
    else
        echo "Unhandled file type of ${NV_DEB_FILENAME}"
        exit 1
    fi
done

apt-get update -o Acquire::CompressionTypes::Order::=gz -y
apt-get install -y --allow-unauthenticated cuda-toolkit-9.0
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH:-}:/usr/lib/aarch64-linux-gnu/tegra
ln -s /usr/lib/aarch64-linux-gnu/libcuda.so /usr/lib/aarch64-linux-gnu/libcuda.so.1
ln -s /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so.28.2.0 /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so.1
ln -s /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so.28.2.0 /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so
