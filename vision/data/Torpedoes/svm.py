#!/usr/bin/env python2.7

import cv2, sys, os, os.path, numpy, time, random, imp, threading
from sklearn import svm
from sklearn import neighbors
import collections

try:
  import cPickle as pickle
except ImportError:
  import pickle

if __name__ == '__main__':
  if len(sys.argv) < 2:
    years = ['1885', '1955', '1985', '2015']
  else:
    years = [year for year in set(sys.argv[1:]) if year in ['1885', '1955', '1985', '2015']]
    
  for year in years:
    numbers = [year[:2], year[2:]]
    dirname = os.path.realpath(os.path.dirname(sys.argv[0]))
    imp.load_module('vision_common', *imp.find_module('vision_common', ['{}/../../'.format(dirname)]))
    vision_common = sys.modules['vision_common']

    length = len([f for d in numbers for f in os.listdir(d) if os.path.isfile(d + '/' + f)])
    X, Y = numpy.array([], dtype=float), numpy.array([], dtype=int)

    dim = 40
    size = dim * dim
    #size = 22

    i = 0
    for d in numbers:
      for f in [f for f in os.listdir(d) if os.path.isfile(d + '/' + f)]:

        mat = cv2.imread(d + '/' + f)
        mat, _, _ = cv2.split(cv2.cvtColor(mat, cv2.COLOR_RGB2LUV))
        mat = cv2.adaptiveThreshold(mat, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, 15, 16)

        mat = cv2.resize(mat, (dim, dim))

        X = numpy.append(X, mat.flatten())

        # features = vision_common.extract_features(mat)
        # if len(features) != 22:
        #   continue
        # X = numpy.append(X, features)
        Y = numpy.append(Y, int(d))

    X = X.reshape((Y.shape[0], size))

    indices = list(range(Y.shape[0]))
    random.shuffle(indices)
    X = numpy.array([X[i] for i in indices])
    Y = numpy.array([Y[i] for i in indices])

    ind = int(Y.shape[0] * 0.7)

    X_tr, X_te = X[:ind], X[ind:]
    Y_tr, Y_te = Y[:ind], Y[ind:]

    def classifier():
      return svm.LinearSVC(C=0.8)
    
    def train():
      c = classifier()
      c.fit(X, Y)

      dirname = os.path.dirname(sys.argv[0])

      filename = '{}/svm_{}_{}.pkl'.format(dirname, year, time.time())
      with open(filename, 'w') as f:
        f.write(pickle.dumps(c))
      try:
        os.remove('{}/svm_latest_{}.pkl'.format(dirname, year))
      except OSError:
        pass
      os.symlink(filename, '{}/svm_latest_{}.pkl'.format(dirname, year))
      print('finished writing and symlinking {}'.format(year))

    if len(sys.argv) > 1 and 'train' in [x.lower() for x in sys.argv]:
      threading.Thread(target=train).start()
      
    c = classifier()
    c.fit(X_tr, Y_tr)
    print('{} training score: {}'.format(year, c.score(X_te, Y_te)))
