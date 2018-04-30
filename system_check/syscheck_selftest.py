import os

from test import vehicle, level, Test, CASTOR, POLLUX, WARN, ERR

# Syscheck self-tests, so meta
@vehicle(POLLUX)
class IsApollo(Test):
    def is_pollux():
        return "pollux" == os.getenv("CUAUV_VEHICLE")

    class IsApollo(Test):
        def is_pollux():
            return "pollux" == os.getenv("CUAUV_VEHICLE")

    @vehicle(CASTOR)
    class IsArtemis(Test):
        def is_castor():
            return "castor" == os.getenv("CUAUV_VEHICLE")

@vehicle(CASTOR)
class IsArtemis(Test):
    def is_castor():
        return "castor" == os.getenv("CUAUV_VEHICLE")

    class IsArtemis(Test):
        def is_castor():
            return "castor" == os.getenv("CUAUV_VEHICLE")

    @vehicle(POLLUX)
    class IsApollo(Test):
        def is_pollux():
            return "pollux" == os.getenv("CUAUV_VEHICLE")
