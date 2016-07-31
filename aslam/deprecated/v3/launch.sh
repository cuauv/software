#!/bin/sh

docker run -d -p 5555:5555 -v ~/trunk:/mnt aslam /bin/bash -c "cd /mnt/aslam/v3/src && ./main"""""
