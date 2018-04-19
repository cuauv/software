#!/bin/zsh

source /home/software/cuauv/software/vehicle-scripts/auv-env/cuauv.zsh

vehicle_file=/home/software/.oh-my-zsh/custom/vehicle.zsh
[[ -f $vehicle_file ]] && source $vehicle_file

$@
