from learning import basemodel

class Model(basemodel.Model):
    def getModel(self):
        return basemodel.MODELS['Ridge']
