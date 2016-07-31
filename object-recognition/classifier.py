#!/usr/bin/env python2

import pickle
import shm
import signal
import sys
import os
from common import *
from itertools import permutations
from misc.utils import memoize, watch_thread_wrapper
from time import time

__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0]))) 

@memoize
def get_perms(shape_count, bin_count):
    perms = permutations(range(shape_count), bin_count)
    return [[[1 if t == i else 0 for i in range(shape_count)] for t in q] for q in perms] 

class Classifier:
    def __init__(self):
        data_file = open(os.path.join(__location__, SVC_FILENAME), 'r')
        self.clf = pickle.load(data_file)[0]


    def get_proba_match_error(self, l1, l2):
        return sum([sum([abs(e1 - e2) for e1, e2 in zip(v1, v2)]) for v1, v2 in zip(l1,l2)])

    def classify(self):

        # Get all data from shm at once
        #TODO: address thread safety concerns w/ shm; group access?
        visible_groups = [g for g in vision_groups if g.probability.get() > 0]
        positions = [(g.x.get(), g.y.get()) for g in visible_groups]
        vectors = [get_feature_vector_from_group(g) for g in visible_groups]

        print "Time is %6f" % time()
        print "DEBUG: Vectors: %s" % vectors

        try:
            probas = [self.clf.predict_proba(v)[0] for v in vectors]
        except Exception as e:
            print "CLASSIFY EXCEPTION: %s" % e
            return []

        def normalize(l):
            s = sum(l)
            return l if s == 0 else [x / s for x in l]
        probas_normalized = map(normalize, probas)
        probas_rounded = [[round(d,2) for d in l] for l in probas_normalized]

        print "-------------------"
        print "DEBUG: Probas: \n\t%s" % ("\n\t".join(["%d: %s" % (i+1,x) for i,x in enumerate(probas_rounded)]))

        shape_count = len(probas[0]) if len(probas) > 0 else 0
        bin_count = len(probas)
        perms = get_perms(shape_count, bin_count)
        errors = [(self.get_proba_match_error(probas_normalized, l),  i) for i,l in enumerate(perms)]
        min_err, min_perm_index = min(errors)
        best_perm = perms[min_perm_index]

        print "DEBUG: Best Fit: \n\t%s" % ("\n\t".join(["%d: %s" % (i+1,x) for i,x in enumerate(best_perm)]))
        svc_indicies = [sum([i if v == 1 else 0 for i,v in enumerate(l)]) for l in best_perm]

        shape_out_probs = [l[i] for i,l in zip(svc_indicies, probas_normalized)]
        shapes = [svc_index_to_name[i] for i in svc_indicies]

        print "DEBUG: Shapes Matched: \n\t%s" % ("\n\t".join(["%d: %s (%.3f)" % (i+1,x,p) for i,(x,p) in enumerate(zip(shapes,shape_out_probs))]))

        return [(name, (x, y, prob)) for name, (x,y), prob in zip(shapes, positions, shape_out_probs)]

if __name__ == "__main__":
    def daemon(w, quit_event):
        print "== Bins Machine Learning Classifier Daemon =="

        print "Loading SVM data..."
        c = Classifier()
        print "SVM data loaded!"

        w.watch(shm.shape_results_4)
        w.watch(shm.shape_settings)

        while 1:
            w.wait(True)
            if quit_event.is_set():
                break

            bins_set = []
            if shm.shape_settings.enabled:
                for (name, (x, y, prob)) in c.classify():
                    bins_set.append(name)
                    set_shm_for_key(name, x, y, prob)
            for name in set(bin_options.keys()) - set(bins_set):
                set_shm_for_key(name, 0, 0, 0.0)

    watch_thread_wrapper(daemon)
