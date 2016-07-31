import shm

LEDS = { "green" : shm.led.port4,
         "blue"  : shm.led.port3, # red underglow
         "white" : shm.led.port6,
         "red"   : shm.led.port2
       }
LED_ORDER = [LEDS["white"], LEDS["green"], LEDS["red"], LEDS["blue"]]
