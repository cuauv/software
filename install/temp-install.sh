#!/usr/bin/env bash

if [[ "$(uname -m)" != "aarch64" ]]; then
    exit 0
fi


rm /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so
rm /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so.1
ln -s /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so.28.2.1 /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so
ln -s /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so.28.2.1 /usr/lib/aarch64-linux-gnu/tegra/libnvidia-ptxjitcompiler.so.1
