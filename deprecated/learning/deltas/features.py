from learning import features

class Features(features.Features):
    def Xfeatures(self, data):
        return data[:-1]

    @staticmethod
    def Yfuncs(labels):
        def getFeature(data, f):
            d = [x[labels[f]] for x in data]
            prev = None
            out = []
            for x in d:
                if prev is not None:
                    out.append(x - prev)
                prev = x
            return out
        return {'delta_x' : (lambda x : getFeature(x, 'kalman.velx')),
                'delta_y' : (lambda x : getFeature(x, 'kalman.vely'))}
