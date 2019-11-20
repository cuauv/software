from pathlib import Path, PurePosixPath

"""
You can override these defaults: create a new file called `user.py` (in this
folder, `docker`) with a dict called `config`. Any keys defined in this dict
will override the default ones below. An example user.py:

```
from pathlib import Path, PurePosixPath

config = {
    "WORKSPACE_DIRECTORY": Path("~/path/to/my/repo/").expanduser(),
}
```

We use a separate file so that we don't track user-specific changes.
"""

defaults = {
    "WORKSPACE_DIRECTORY": Path("~/cuauv/workspaces").expanduser(),
    "CONTAINER_WORKSPACE_DIRECTORY": PurePosixPath("/home/software/cuauv/workspaces"),

    "DOCKER_REPO": "wds68/cuauv",
    "DOCKER_REPO_JETSON": "asb322/cuauv-jetson",

    "GIT_REPO_URL": "ssh://git@bitbucket.cuauv.org:7999/sof/subcode.git",
    "BRANCH": "master",

    "GROUP_ID": 9999,
}

try:
   import user
   if hasattr(user, 'config'):
      conf = dict(defaults, **user.config)
   else:
      conf = defaults
except ImportError:
   conf = defaults

def get_config(key):
   return conf[key]
