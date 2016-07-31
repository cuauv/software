import random
import re
from time import sleep

from TwitterAPI.TwitterAPI import TwitterAPI

from colors import colors_by_name
from shm import led

__author__ = 'zander'

api = TwitterAPI("3izkk25JAjqyetdxz7UzwN9tr",
                 "W01dNPAmv2FIUmvsqPTkSjIg5364dxT4cbvdH8SltNbFUXzifn",
                 "61233167-E5VTAoVXV02RwbeMrpRdlJhZs6jLGS32JYHxOs97t",
                 "cjdI8OvMHVpTXzFAnmEw1TpwC2i1x2nEdZdh5znqfDyc5")

r = api.request("user")


def set_led_color(red, green, blue):
    led.port_red.set(red)
    led.port_green.set(green)
    led.port_blue.set(blue)

    led.starboard_red.set(red)
    led.starboard_green.set(green)
    led.starboard_blue.set(blue)


set_led_color(0, 100, 255)

api = TwitterAPI("3izkk25JAjqyetdxz7UzwN9tr",
                 "W01dNPAmv2FIUmvsqPTkSjIg5364dxT4cbvdH8SltNbFUXzifn",
                 "61233167-E5VTAoVXV02RwbeMrpRdlJhZs6jLGS32JYHxOs97t",
                 "cjdI8OvMHVpTXzFAnmEw1TpwC2i1x2nEdZdh5znqfDyc5")

r = api.request("user")

for item in r:
    print("Got message")
    if "friends" in item:
        print("Socket opened!")
        continue

    if "event" in item:
        print("Event of type {} received, ignoring!".format(item["event"]))
        continue

    if "entities" not in item:
        print("No entities in tweet object")
        continue

    if "leds" in (hashtag["text"].lower() for hashtag in item["entities"]["hashtags"]) and \
                    "cuauv" in (user_mention["screen_name"].lower() for user_mention in
                                item["entities"]["user_mentions"]):
        print("#LEDS tweet found!")

        colors = []

        colors_hex = (hashtag["text"].lower() for hashtag in item["entities"]["hashtags"] if
                      re.search(r"^[0-9a-fA-F]{6}$", hashtag["text"]))
        colors.extend(
            (int(color_hex[0:2], 16), int(color_hex[2:4], 16), int(color_hex[4:6], 16)) for color_hex in colors_hex)

        colors_name = (colors_by_name[hashtag["text"].lower()] for hashtag in item["entities"]["hashtags"] if
                       hashtag["text"].lower() in colors_by_name)
        colors.extend((int(color_name[1:3], 16), int(color_name[3:5], 16), int(color_name[5:7], 16)) for color_name in
                      colors_name)
        print("Found colors: {}".format(colors))

        if len(colors) > 1:
            print("Multiple colors found")
            api.request("statuses/update", {"in_reply_to_status_id": item["id_str"],
                                            "status": "@{}: It appears you specified multiple colors, try only one next time!"
                        .format(item["user"]["screen_name"])})
            continue
        elif len(colors) < 1:
            print("No colors found")
            api.request("statuses/update", {"in_reply_to_status_id": item["id_str"],
                                            "status": "@{}: It appears you specified did not specify a color. Try adding #{} next time!"
                        .format(item["user"]["screen_name"], random.choice(list(colors_by_name.keys())))})
            continue

        color = colors[0]

        set_led_color(*color)

        api.request("statuses/update", {"in_reply_to_status_id": item["id_str"],
                                        "status": "@{}: Argo's #LEDS are now set to RGB({}, {}, {}) just for you!"
                    .format(item["user"]["screen_name"], *color)})
