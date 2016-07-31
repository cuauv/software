from abc import ABCMeta, abstractmethod

""" Abstract class to represent a model """
class Model(object):
    __metaclass__ = ABCMeta

    """ Initializes a new model """
    """ Hacky way to overload initialization """
    """ Passing it a single string will load a filepath """
    """ Otherwise, it will learn a new model based on data """
    def __init__(self, Xlist, Ylist = None):
        if Ylist is None and type(Xlist) == str:
            self.load(Xlist)
        else:
            self.learn(Xlist, Ylist)

    """ Saves model to a file """
    @abstractmethod
    def save(self, filename):
        pass

    """ Loads model from a file """
    @abstractmethod
    def load(self, filename):
        pass

    """ Learns a new model from data """
    @abstractmethod
    def learn(self, Xlist, Ylist):
        pass

    """ Predicts output given values """
    @abstractmethod
    def predict(self, curvars):
        pass

