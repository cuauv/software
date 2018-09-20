from pathlib import Path, PurePosixPath

defaults = {
    "WORKSPACE_DIRECTORY": Path("~/cuauv/workspaces").expanduser(),
    "CONTAINER_WORKSPACE_DIRECTORY": PurePosixPath("/home/software/cuauv/workspaces"),

    "DOCKER_REPO": "wds68/cuauv",

    "GIT_REPO_URL": "ssh://git@bitbucket.cuauv.org:7999/sof/subcode.git",
    "BRANCH": "master",
}


def get_config(key):
   return defaults[key]
