#!/usr/bin/env python2

import sys

from selftestengine import SelfTestEngine

'''
Program designed to carry out sensor checks and catch any failure
states automatically.

** To add new variables to be checked, or add new checking functions,
edit tests.cfg and tests.py respectively.

Jeff Heidel 2011.
'''

LINE_LENGTH = 40 #max length of lines, try increasing if weird things happen

#color definitions
head_redBG = '\033[1;41m'
head_greenBG = '\033[1;42m'
head_yellowBG = '\033[1;43m'
head_bold = '\033[1;49m'
tail = '\033[1;m'

good = 0
bad = 0

try:

    print "Starting all sensor tests..."
    print "-"*(LINE_LENGTH+8)

    for result in SelfTestEngine().run_tests():

        ret = "Test " + result['key'] + ":"
        ret += ' '*(LINE_LENGTH - len(ret)) #Alignment spaces
        if result["pass"]:
            if result["warn"]:
                ret += head_yellowBG + "  warn  " + tail
            else:
                ret += head_greenBG + "  pass  " + tail
            good += 1
        else:
            ret += head_redBG + "  fail  " + tail
            bad += 1

        print ret

        for f in result["messages"]:
            print " - " + f #Print any specific error messages

    print "-"*(LINE_LENGTH + 8)
    print head_bold + str(good) + tail + " tests passed.  " + head_bold + str(bad) + tail + " tests failed."

    if bad>0:    
        print "Failures detected in some tests."
        sys.exit(1)
    else:
        print "All tests completed successfully."
        sys.exit(0)
except KeyboardInterrupt:
    print "\nAborting..."
    sys.exit(0)

  
