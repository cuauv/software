import os

import shm

from configobj import ConfigObj
from tests import *
from threading import Thread, Condition, Lock

class SelfTestThread(Thread):
    """
    Thread to run a single test
    When done testing, this thread puts results in
    the results_list and notifies c
    """
    def __init__(self, key, config, results_list, c):
        Thread.__init__(self)
        self.key = key
        self.config = config
        self.c = c
        self.results_list = results_list
        self.start()

    def run(self):
        configvar = self.config[self.key]
        shared_var = shm._eval(configvar['var'])
        
        messages = []
        
        success = True 
        warning = False
        for test in configvar.sections:
            testparams = configvar[test]
            testparams['var'] = shared_var
            rval = eval(test)(**testparams)       
            if type(rval) == type(" "): #A returned string indicates failure (or warning)
                if "warn:" in rval.lower():
                    warning = True
                else:
                    success = False
                messages.append(rval)
            else: #If a boolean was returned, meaning is obvious
                success &= rval

        #create results dictionary
        results = {"key" : self.key,
                   "pass" : success,
                   "warn" : warning,
                   "messages" : messages}

        with self.c:
            self.results_list.append(results)
            self.c.notify()


class SelfTestEngine:
    
    def __init__(self):
        filename = os.path.join(os.environ['CUAUV_SOFTWARE'], "self_test", "tests.cfg") #config file for the tests
        self.config = ConfigObj(filename)
        if not self.config.sections:
          print "WARNING: tests configuration file at %s has no tests!" % filename

    def run_tests(self):
        """
        Iterator; spawns off all tests and yields results as they come in
        """

        c = Condition()
        results = []
        tests_remaining = len(self.config.sections)

        #run these tests, each in a seperate thread
        for test_key in self.config.sections:
            SelfTestThread(test_key, self.config, results, c)

        while tests_remaining:
            with c:
                while len(results) == 0:
                    c.wait()
                
                #handle result
                yield results.pop()
                tests_remaining -= 1
