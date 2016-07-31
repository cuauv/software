from abc import ABCMeta, abstractmethod

""" Abstract class to represent the features that you are learning on """
class Features(object):
    _metaclass_ = ABCMeta

    """ Initializes a new set of features """
    def __init__(self, labels, data):
        Yfuncs = self.Yfuncs(labels)
        self.Xlist = []
        self.Ylist = {f : [] for f in Yfuncs}
        for d in data:
            self.Xlist += self.Xfeatures(d)
            for f in Yfuncs:
                self.Ylist[f] += Yfuncs[f](d)

    """ Calculates the X feature vector """
    @abstractmethod
    def Xfeatures(self, data):
        pass

    """ Returns functions that calculate each Y vector based on input """
    """ Override using @staticmethod decorator """
    @abstractmethod
    def Yfuncs(self, labels):
        pass
