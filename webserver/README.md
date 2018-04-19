AUV-webserver
=============

Setup
-----

1. Install nodejs, npm: `sudo apt-get install nodejs npm`
2. Symlink nodejs to node (only necessary for Debian-based distros because there is already a node package): `sudo ln -s "$(which nodejs)" /usr/bin/node`
3. `npm install` inside the `auv-webserver` directory
4. `./node_modules/.bin/webpack --config webpack.config.js`
