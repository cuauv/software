import os
from learn import importfile, loadfile
import shm

TIMESTAMP = 0.005
PATH = os.path.dirname(os.path.realpath(__file__))

class Predict(object):
    def __init__(self, path):
        path = os.path.join(PATH,path)
        print path

        FEATURES = importfile(os.path.join(path, 'features'))

        self.var = loadfile(os.path.join(path, 'models', 'labels.conf'))
        self.modelnames = loadfile(os.path.join(path, 'conf', 'models.conf'))

        labels = {self.var[i] : i for i in range(len(self.var))}

        self.features = [f for f in FEATURES.Features.Yfuncs(labels)]
        self.models = {}
        for feature in self.features:
            m = {}
            for model in self.modelnames:
                MODEL = importfile(os.path.join(path, 'pymodel', model))
                m[model] = MODEL.Model(os.path.join(path, 'models', feature, model, 'model'))
            self.models[feature] = m

    """ Gets one variable from shm """
    def getVar(self, var):
        var = var.split('.')
        return getattr(getattr(shm, var[0]), var[1]).get()

    """ Gets all of the current variables """
    def getVars(self):
        v = [0] * len(self.var)
        for i in range(len(self.var)):
            if self.var[i] == 'timestamp':
                v[i] = TIMESTAMP
            else:
                v[i] = self.getVar(self.var[i])
        return v

    """ Returns the prediction of a certain feature given a certain model """
    def predictOne(self, feature, model):
        curvars = self.getVars()
        p = self.models[feature][model].predict(curvars)
        try:
            return p[0]
        except:
            return p

    """ Returns all predictions for a certain feature """
    def predictFeature(self, feature):
        p = []
        for model in self.modelnames:
            p.append(self.predictOne(feature, model))
        return p

    """ Returns all predictions for every feature using a specific model """
    def predictModel(self, model):
        p = []
        for feature in self.features:
            p.append(self.predictOne(feature, model))
        return p

    """ Returns a dictionary containing all predictions in the module """
    def predictAll(self):
        p = {}
        for feature in self.features:
            p[feature] = self.predictFeature(feature)
        return p

