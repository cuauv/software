# CUAUV Software

This is the repo used by the software team of Cornell University Autonomous
Vehicle (CUAUV). Our website is http://cuauv.org/.

## Installing and Running

### Docker (environment and dependencies)

This software stack can be run via docker. The necessary dependencies are:

1. docker (install with apt-get - see [this tutorial](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-16-04])
2. docker-compose (install with `sudo pip3`)

To build the docker container run this command in the top level of this repo:

`docker build . -t cuauv`

You may be able to speed up first build times by using cache layers from our CI
build server. To do this, you will have to first have Zander give your
hub.docker.com username access to the private image, pull the image and tell
Docker to use the pulled image as a cache source when building.

`docker pull lezed1/cuauv && docker build . -t cuauv --cache-from lezed1/cuauv`

Run the command below to then run that docker container using docker-compose in
the top level of this repo. This will mount the repo into the docker container
(so changes to files in the repo inside of the container and outside will be
immediately reflected in the other environment) and attach a zsh shell from the
container.

`docker-compose up`

### Software Stack (configuring and running)

The built docker container will not have compiled AUV software in it. To build
the software stack first configure it by running:

`cs && ./configure.py`

Then run ninja to build the stack by running `build` from any directory (`build`
is aliased to a shell command which will run ninja with the appropriate
settings).

### Docker troubleshooting

#### Cannot connect to docker daemon?

This is because you can only connect to Docker with `sudo` privileges. To get around this, you can add yourself to the `docker` group with the instructions [here](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-16-04#step-2-—-executing-the-docker-command-without-sudo-(optional))
