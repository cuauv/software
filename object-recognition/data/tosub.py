import pickle
from sklearn import svm
import sys
import os

"""
Hacky script is needed because of version differences between sklearn on the sub and
other computers. This script retrains the SVC and saves it to a new pickle file
"""

(clf, (X, y)) = pickle.load(open(sys.argv[1], 'r'))
clf2 = svm.SVC(probability=True)
clf2.fit(X, y)
o = (clf2, (X, y))
pickle.dump(o, open(sys.argv[1] + ".sub", 'wb'))

