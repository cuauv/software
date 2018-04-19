"""
Logging Example!
"""

from auvlog.client import Logger, log

# A happy message to all!
log('Today is a good day!', copy_to_stdout = True)

# A secret message!
log.mymodule.secret({'answer': 43.12, 'havingFun': True}, copy_to_stdout = True)

'''

# Another!
log.fun.secret('The question, however...')

# Yet another!
log.really.fun.secret('... is unknown.')

# A complicated message!
log.mission.abstractcombinator.vectoredio.convolution(
    'Critical failure: code 92.')


# A realistic message!
def myfunc():
    log.vision.main.default('Vision system initialized.')

myfunc()

log = log.mission.main

log.default('xxx')
log.error('yyy')


# A structured message!
def funfunc():
    log.funfunc({'speed': 0.2, 'is_ramming': False})

funfunc()

# A Logger can be passed in prefixes easily
log = Logger(["super", "secret", "message"])
log("This is super secret!")
'''

import time; time.sleep(1)
