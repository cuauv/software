#!/bin/bash
echo "Connecting to local client (based on SSH client):"
echo $SSH_CLIENT
auv-deadman ${SSH_CLIENT%% *} 3 &
