#!/bin/bash
if [ $CUAUV_VEHICLE_TYPE = "mainsub" ]; then
    FORE=$(python3 -c "print(0.6*$1)")
    DOWN=$(python3 -c "print(2.0*$1)")

    sed -i "s/^Exposure=[.0-9]*/Exposure=$FORE/" ~/cuauv/software/vision/c/configs/ueye_forward_castor.ini
    sed -i "s/^Exposure=[.0-9]*/Exposure=$DOWN/" ~/cuauv/software/vision/c/configs/ueye_downward_castor.ini
else
    FORE=$(python3 -c "print(0.7*$1)")
    DOWN=$(python3 -c "print(50.0*$1)")

    sed -i "s/^Exposure=[.0-9]*/Exposure=$FORE/" ~/cuauv/software/vision/c/configs/ueye_forward_mini.ini
    sed -i "s/^Exposure=[.0-9]*/Exposure=$DOWN/" ~/cuauv/software/vision/c/configs/ueye_downward_mini.ini
fi
