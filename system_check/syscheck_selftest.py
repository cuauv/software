import os

from test import vehicle, level, Test, MAINSUB, MINISUB, WARN, ERR
from conf.vehicle import VEHICLE_TYPE

# Syscheck self-tests, so meta
@vehicle(MINISUB)
class IsMinisub(Test):
    def is_minisub():
        return "minisub" == VEHICLE_TYPE

    class IsMinisub(Test):
        def is_minisub():
            return "minisub" == VEHICLE_TYPE

    @vehicle(MAINSUB)
    class IsMainsub(Test):
        def is_mainsub():
            return "mainsub" == VEHICLE_TYPE

@vehicle(MAINSUB)
class IsMainsub(Test):
    def is_mainsub():
        return "mainsub" == VEHICLE_TYPE

    class IsMainsub(Test):
        def is_mainsub():
            return "mainsub" == VEHICLE_TYPE

    @vehicle(MINISUB)
    class IsMinisub(Test):
        def is_minisub():
            return "minisub" == VEHICLE_TYPE
