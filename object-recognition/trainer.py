#!/usr/bin/env python2

from sklearn import svm
import pickle
import shm
import sys
from cave_iterator import CaveIterator
from common import bin_options

if __name__ == '__main__':

    print "== Bins Machine Learning Training Utility =="

    if len(sys.argv) != 2:
        print "Usage: ./trainer.py [data output file]"
        sys.exit(1)
    FILENAME = sys.argv[1]
    
    # TODO: Load existing data from file (append to SVM)

    X = [] # Feature vector
    y = [] # Labels for the feature vectors

    for k,v in bin_options.items(): 
        if not "y" in raw_input("Do you wish to train for \"%s\"? [y/N] " % k):
            continue

        print "Prepare CAVE for training on \"%s\"." % k
        raw_input("Press [ENTER] when ready to start training...")

        print "Starting CAVE training on \"%s\"" % k

        label = v[0] #SVC label
        
        for vect in CaveIterator():
            if vect is not None:
                X.append(vect)
                y.append(label)

    print "All CAVE collection complete."

    print "Now training SVM..."

    clf = svm.SVC(probability=True)
    clf.fit(X, y)
    output = open(FILENAME, 'wb')

    output_obj = (clf, (X,y))

    pickle.dump(output_obj, output)

    print "SVM data written to \"%s\"" % FILENAME

    print "Self data fit score: %f" % clf.score(X, y)

