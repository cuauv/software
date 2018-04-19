'''
Used for parsing meta data
'''

from misc.log import with_logging

@with_logging
class MetaParser(object):
    def __init__(self, s):
        self.raw = s

    def parse(self):
        self.log.debug("Parsing meta: %s" % self.raw)
        return self.raw # TODO: actually parse if deemed necessary


