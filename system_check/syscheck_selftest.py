import os

from test import vehicle, level, Test, ARTEMIS, APOLLO, WARN, ERR

# Syscheck self-tests, so meta
@vehicle(APOLLO)
class IsApollo(Test):
    def is_apollo():
        return "apollo" == os.getenv("CUAUV_VEHICLE")

    class IsApollo(Test):
        def is_apollo():
            return "apollo" == os.getenv("CUAUV_VEHICLE")

    @vehicle(ARTEMIS)
    class IsArtemis(Test):
        def is_artemis():
            return "artemis" == os.getenv("CUAUV_VEHICLE")

@vehicle(ARTEMIS)
class IsArtemis(Test):
    def is_artemis():
        return "artemis" == os.getenv("CUAUV_VEHICLE")

    class IsArtemis(Test):
        def is_artemis():
            return "artemis" == os.getenv("CUAUV_VEHICLE")

    @vehicle(APOLLO)
    class IsApollo(Test):
        def is_apollo():
            return "apollo" == os.getenv("CUAUV_VEHICLE")
