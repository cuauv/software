#!/usr/bin/env python3
import datetime
import json
import os
from pathlib import Path
import subprocess

import fire

import pooltest.slack as slack


# Basic config/setup
log_dir = Path(os.environ["CUAUV_LOG"])
current_log_dir_alias = log_dir / "current"
no_pooltest_log_dir = log_dir / "no-pooltest"
meta_json_path = current_log_dir_alias / "meta.json"


no_pooltest_log_dir.mkdir(exist_ok=True)
try:
    current_log_dir_alias.symlink_to(no_pooltest_log_dir)
except FileExistsError:
    pass


# Helper functions
def follow_symlink(path):
    return Path(path).resolve()


def assert_valid_pooltest_name(name):
    if name in ["current", "no-pooltest"]:
        raise RuntimeError("'{}' is reserved and cannot be a pooltest name.".format(name))

    for char in name:
        if char not in "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-":
            raise RuntimeError("'{}' cannot be in the pooltest name.".format(char))

    return True


def get_command_output(command):
    sp = subprocess.run(command, stdout=subprocess.PIPE)
    stdout = sp.stdout.decode()
    return stdout.rstrip('\n')


def pooltest_is_active():
    current_log_dir = follow_symlink(current_log_dir_alias)
    return not current_log_dir.samefile(no_pooltest_log_dir)


def get_current_pooltest_name():
    assert pooltest_is_active(), "Cannot get pooltest name if pooltest is not active"

    current_log_dir = follow_symlink(current_log_dir_alias)
    relpath = current_log_dir.relative_to(log_dir)
    assert len(relpath.parents) == 0 or str(relpath.parents[0]) == '.', \
        "'{}' doesn't point to a sibling".format(current_log_dir_alias)
    return relpath.name


def setup_log_dir(name):
    assert_valid_pooltest_name(name)
    assert not pooltest_is_active(), "Cannot set up log dir if pooltest already running"

    current_log_dir = log_dir / name

    if current_log_dir_alias.exists():
        # Check if symlink to not lose data
        if not current_log_dir_alias.is_symlink():
            raise RuntimeError("'{}' is not a symlink. Cannot remove it without losing data.".format(current_log_dir_alias))
        current_log_dir_alias.unlink()

    current_log_dir.mkdir()
    current_log_dir_alias.symlink_to(current_log_dir)

    return current_log_dir


def unlink_log_dir():
    assert pooltest_is_active, "Cannot unlink log dir if pooltest is not active"
    current_log_dir_alias.unlink()
    current_log_dir_alias.symlink_to(no_pooltest_log_dir)


# Actual CLI exposed functions
class Pooltest:
    def start(self, name):
        assert_valid_pooltest_name(name)
        print("Starting {}".format(name))
        print('Run `trogdor restart` to restart daemons with logging')

        meta_start_info = {
            "pooltest_name": name,
            "start_time": str(datetime.datetime.now()),
            "start_commit": get_command_output(["git", "rev-parse", "HEAD"]),
            "vehicle": os.environ["CUAUV_VEHICLE"],
        }

        current_log_dir = setup_log_dir(name)

        with meta_json_path.open("w") as meta_json_file:
            json.dump(meta_start_info, meta_json_file, indent=2)
        slack.send('Pooltest has been started. Local logs will be available on the submarine at `{}`'.format(meta_json_path))

    def end(self):
        assert pooltest_is_active()

        current_log_dir = follow_symlink(current_log_dir_alias)
        pooltest_name = get_current_pooltest_name()
        print("Ending {}".format(pooltest_name))

        meta_end_info = {
            "end_time": str(datetime.datetime.now()),
            "end_commit": get_command_output(["git", "rev-parse", "HEAD"]),
        }

        with meta_json_path.open("r") as meta_json_file:
            meta_start_info = json.load(meta_json_file)

        # This can't be [dict(**meta_start_info, **meta_end_info)] as Debian Jessie (oldstable) is on Python 3.4
        meta_info = { **meta_start_info, **meta_end_info }

        with meta_json_path.open("w") as meta_json_file:
            json.dump(meta_info, meta_json_file, indent=2)

        remote = "software@cuauv.org:/srv/logs/current/pooltest"
        command = ["rsync", "-avzH", "-e", "ssh -p 2222", "--human-readable", "--progress",
                   str(current_log_dir), remote]
        print(command)
        subprocess.run(command)

        unlink_log_dir()
        slack.send('Pooltest has been completed. Logs can be accessed at https://cuauv.org/log/{}'.format(pooltest_name))

    def assert_shm(self):
        raise NotImplemented

    def cleanup(self):
        raise NotImplemented


if __name__ == '__main__':
    fire.Fire(Pooltest)

