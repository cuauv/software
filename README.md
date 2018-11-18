
## CUAUV Software Stack

This is the repo used by the software team of Cornell University
Autonomous Underwater Vehicle (CUAUV). Our website is
[cuauv.org](http://cuauv.org/).

This is the entire software stack. The stack is now Dockerized, so it
is fairly easy to get everything up and running on your Linux box.

To prepare your system:
 1. Install Docker. (e.g. `sudo apt install docker` or
    `sudo pacman -S docker`))
 2. [Configure Docker](https://docs.docker.com/install/linux/linux-postinstall/)
    with a user group and enable run on start with systemctl.
 3. Set up an [SSH key](https://confluence.atlassian.com/bitbucketserver059/creating-ssh-keys-949254829.html#CreatingSSHkeys-CreatinganSSHkeyonLinux&macOS).
 4. Install two pip packages that are required for our Docker script:
    `sudo pip3 install docker clize`.

Set up Docker Hub:
 1. Create a [Docker Hub account](https://hub.docker.com/). This is
    where we store our Docker images for the time being.
 2. Ask Will to add your account to the CUAUV repository on Docker
    Hub. Without this, you won't be able to pull the image.
 3. Note: These steps are only required for team members because
    our build server makes images significantly faster than your
    computer can. If you don't have access to Docker Hub, you can
    build the image locally with the standard Docker build.

To get started with this repo:
 1. Clone this repo (any temporary place will do).
 2. Run `docker/auv-docker.py init`. This will set up your machine
    with worktrees and move the stack to `~/cuauv/`.
 3. Run `~/cuauv/workspaces/repo/docker/auv-docker.py cdw`. This will
    pull the latest docker image (might take a while to download, be
    patient), start a container, and ssh you into that container.
 4. In the container, run `./configure.py` and then `build`.

Now you're all set.

Some tips:
 - Make an alias for `auv-docker.py`. It will save you a lot of time.
 - Run `auv-docker.py --help` to see all available commands.
 - Run `auv-docker.py cdw [branch]` to start a container with a
   particular branch. The default is master. Our Docker script uses
   git worktrees to keep multiple branches in parallel. You can also
   run multiple containers at the same time.
 - Containers run on 172.17.0.2, 172.17.0.3, etc. You can ssh into
   them directly.
 - `cdw` and `start` start a container, but ending the ssh session
   does not stop them. Use `auv-docker.py stop [branch]` to kill the
   container.
 - The folder `~/cuauv/workspaces/repo` is the master branch of the
   repository. Each branch is stored separately in
   `~/cuauv/workspaces/worktrees/[branch]/`.
