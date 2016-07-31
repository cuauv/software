from learning import features

class Features(features.Features):
    def Xfeatures(self, data):
        return data[:-1]

    @staticmethod
    def Yfuncs(labels):
        def getHeading(data):
            return [x[labels['kalman.heading']] for x in data[1:]]
        return {'heading' : getHeading}
