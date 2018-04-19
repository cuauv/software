#!/usr/bin/env python2
from systemd.manager import Manager
import argparse
import traceback
from subprocess import call
from termcolor import colored

auv_services = {
    "start": [
        "camerad",
        "controld3",
        "deadman",
        "dvld",
        "3dmgx4d",
        "kalmand",
        "navigated",
        "seriald",
        "seriald-new",
        "sonard",
        "seriald",
        "shmserver",
        "ueyed",
        "uptimed",
    ],
    "status": [
        "log",
        "led",
        "shmserver",
    ],
}

auv_services["all"] = auv_services["start"] + auv_services["status"]

manager = Manager()


def start(service):
    # manager.start_unit(service, "replace")
    call(["sudo", "systemctl", "start", service])

def stop(service):
    # manager.stop_unit(service, "replace")
    call(["sudo", "systemctl", "stop", service])

def status(service):
    properties = manager.load_unit(service).properties
    if str(properties.LoadState) == "not-found":
        print("Unit {} not found.".format(service))
        return

    def get_colored(state):
        color_map = { "failed"   : "red",
                      "disabled" : "red",
                      "enabled"  : "green",
                      "active"   : "green",
                      "inactive" : "yellow",
                      "killed"   : "yellow"
        }

        if state not in color_map:
            return state

        return colored(state, color_map[state])

    # TODO Use tabulate.
    name_str = "\t\t" if len(str(properties.Names[0])) < 18 else "\t"

    print("Name: {}{}Enabled: {}\tActive: {}".format(colored(str(properties.Names[0]), "yellow"),
                                                     name_str,
                                                     get_colored(properties.UnitFileState),
                                                     get_colored(properties.ActiveState)))

def restart(service):
    # manager.restart(service, "replace")
    call(["sudo", "systemctl", "restart", service])

def list_services(service):
    print("\n".join(auv_services["all"]))
    exit()

def list_defaults(service):
    print("\n".join(auv_services["start"]))
    exit()



actions = {
    "start": start,
    "stop": stop,
    "status": status,
    "restart": restart,
    "list-services": list_services,
    "list-defaults": list_defaults,
}

parser = argparse.ArgumentParser(description="Systemd, but beter??",
                                 epilog="Availible services: " + "\n".join(auv_services["all"]))

parser.add_argument("action", choices=actions.keys(),
                    default="status", nargs="?", help="The action to take")
parser.add_argument("services", nargs="*", default=auv_services["start"],
                    help="The services on which to take action.")

args = parser.parse_args()

for service in args.services:
    try:
        actions[args.action]("auv-{}.service".format(service))
    except:
        print("Exception!")
        traceback.print_exc()
