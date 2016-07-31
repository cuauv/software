from abc import ABCMeta, abstractmethod
from learning import model
from sklearn.externals import joblib
import sklearn.ensemble as ske
import sklearn.svm as svm
import sklearn.linear_model as skl

""" Some simple default models to work with """
MODELS = { 'GradientBoost' : ske.GradientBoostingRegressor(),
           'SVR' : svm.SVR(kernel='linear'),
           'rbf' : svm.SVR(kernel='rbf'),
           'Ridge' : skl.Ridge(),
           'RidgeHalf' : skl.Ridge(alpha=0.5),
           'RidgeZero' : skl.Ridge(alpha=0.0),
           'Lasso' : skl.Lasso(alpha=0.1),
           'ElasticNet' : skl.ElasticNet(alpha=0.1) }

""" Base model used for sklearn models """
class Model(model.Model):
    __metaclass__ = ABCMeta

    """ Saves via joblib """
    def save(self, filename):
        joblib.dump(self.model, filename)

    """ Loads via joblib """
    def load(self, filename):
        self.model = joblib.load(filename)

    """ Learns by fitting with the model """
    def learn(self, Xlist, Ylist):
        self.model = self.getModel().fit(Xlist, Ylist)

    """ Predicts based on curvars """
    def predict(self, curvars):
        return self.model.predict(curvars)

    """ Returns an instance of a sklearn model object """
    @abstractmethod
    def getModel(self):
        pass
