import hashlib
import os
import sys
from collections import defaultdict
from functools import reduce

from scipy.spatial.distance import cdist
import numpy as np
import cv2
import sklearn
import sklearn.cluster
import pickle

from vision.framework.bcf.evolution import evolution
from vision.framework.bcf.shape_context import shape_context
from vision.framework.bcf.llc import llc_coding_approx


class BCF:
    def __init__(self, data_dir='data/cuauv/', codebook_file='codebook.data', classifier_file='classifier'):
        self.DATA_DIR = data_dir
        self.CODEBOOK_FILE = codebook_file
        self.CLASSIFIER_FILE = classifier_file
        self.classes = defaultdict(list)
        self.data = defaultdict(dict)
        self.counter = defaultdict(int)
        self.label_to_cls = {}
        self._load_labels()

    def _load_files(self, data_dir):
        for dir_name, subdir_list, file_list in os.walk(data_dir):
            if subdir_list:
                continue
            for f in sorted(file_list, key=hash):
                self.classes[dir_name.split('/')[-1]].append(os.path.join(dir_name, f))

    def _load_labels(self):
        for dir_name, subdir_list, file_list in os.walk(self.DATA_DIR):
            if subdir_list:
                continue
            cls = dir_name.split('/')[-1]
            self.label_to_cls[self.hash_cls(cls)] = cls

    def _read_images(self):
        for cls in self.classes:
            images = self.classes[cls]
            for image in images:
                image_id = self.get_image_identifier(cls)
                self.data[image_id]['image'] = cv2.imread(image, cv2.IMREAD_GRAYSCALE)
                if self.data[image_id]['image'] is None:
                    print("Failed to load " + image)

    def load_training_set(self):
        self._load_files(self.DATA_DIR)
        self._read_images()

    def load_testing_set(self, testing_data):
        self._load_files(testing_data)
        self._read_images()

    def normalize_shapes(self):
        for (cls, idx) in self.data.keys():
            image = self.data[(cls, idx)]['image']
            # Remove void space
            y, x = np.where(image > 50)
            max_y = y.max()
            min_y = y.min()
            max_x = x.max()
            min_x = x.min()
            trimmed = image[min_y:max_y, min_x:max_x] > 50
            trimmed = trimmed.astype('uint8')
            trimmed[trimmed > 0] = 255
            self.data[(cls, idx)]['normalized_image'] = trimmed

    def extract_cf(self):
        for (cls, idx) in self.data.keys():
            image = self.data[(cls, idx)]['normalized_image']
            _, contours, _ = cv2.findContours(image, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            contour = sorted(contours, key=len)[-1]
            mat = np.zeros(image.shape, np.int8)
            cv2.drawContours(mat, [contour], -1, (255, 255, 255))
            #self.show(mat)
            MAX_CURVATURE = 1.5
            N_CONTSAMP = 50
            N_PNTSAMP = 10
            C = None
            for pnt in contour:
                if C is None:
                    C = np.array([[pnt[0][0], pnt[0][1]]])
                else:
                    C = np.append(C, [[pnt[0][0], pnt[0][1]]], axis=0)
            cfs = self.extr_raw_points(C, MAX_CURVATURE, N_CONTSAMP, N_PNTSAMP)
            tmp = mat.copy()
            for cf in cfs:
                for pnt in cf:
                    cv2.circle(tmp, (pnt[0], pnt[1]), 2, (255, 0, 0))
                #self.show(tmp)
            num_cfs = len(cfs)
            print("Extracted %s points" % (num_cfs))
            feat_sc = np.zeros((300, num_cfs))
            xy = np.zeros((num_cfs, 2))

            for i in range(num_cfs):
                cf = cfs[i]
                sc, _, _, _ = shape_context(cf)
                # shape context is 60x5 (60 bins at 5 reference points)
                sc = sc.flatten(order='F')
                sc /= np.sum(sc) # normalize
                feat_sc[:, i] = sc
                # shape context descriptor sc for each cf is 300x1
                # save a point at the midpoint of the contour fragment
                xy[i, 0:2] = cf[np.round(len(cf) / 2. - 1).astype('int32'), :]
            sz = image.shape
            self.data[(cls, idx)]['cfs'] = (cfs, feat_sc, xy, sz)

    def learn_codebook(self):
        MAX_CFS = 800 # max number of contour fragments per image; if above, sample randomly
        CLUSTERING_CENTERS = 1500
        feats_sc = []
        for image in self.data.values():
            feats = image['cfs']
            feat_sc = feats[1]
            if feat_sc.shape[1] > MAX_CFS:
                # Sample MAX_CFS from contour fragments
                rand_indices = np.random.permutation(feat_sc.shape[1])
                feat_sc = feat_sc[:, rand_indices[:MAX_CFS]]
            feats_sc.append(feat_sc)
        feats_sc = np.concatenate(feats_sc, axis=1).transpose()
        print("Running KMeans...")
        kmeans = sklearn.cluster.KMeans(min(CLUSTERING_CENTERS, feats_sc.shape[0]), n_jobs=-1, algorithm='elkan').fit(feats_sc)
        print("Saving codebook...")
        with open(self.CODEBOOK_FILE, 'wb') as out_file:
            pickle.dump(kmeans, out_file, -1)
        return kmeans

    def encode_cf(self):
        K_NN = 5
        # Represent each contour fragment shape descriptor as a combination of K_NN of the
        # clustering centers
        with open(self.CODEBOOK_FILE, 'rb') as in_file:
            kmeans = pickle.load(in_file)
        for image in self.data.values():
            feat_sc = image['cfs'][1]
            image['encoded_shape_descriptors'] = llc_coding_approx(kmeans.cluster_centers_, feat_sc.transpose(), K_NN)

    def spp(self):
        PYRAMID = np.array([1, 2, 4])
        for image in self.data.values():
            feat = image['cfs']
            feas = self.pyramid_pooling(PYRAMID, feat[3], feat[2], image['encoded_shape_descriptors'])
            fea = feas.flatten()
            fea /= np.sqrt(np.sum(fea**2))
            image['spp_descriptor'] = fea

    def pyramid_pooling(self, pyramid, sz, xy, encoded_shape_descriptors):
        feas = np.zeros((encoded_shape_descriptors.shape[1], np.sum(pyramid**2)))
        counter = 0
        height = sz[0]
        width = sz[1]
        x = xy[:, 0] # midpoint for each contour fragment
        y = xy[:, 1]
        for p in range(len(pyramid)):
            for i in range(pyramid[p]):
                for j in range(pyramid[p]):
                    yrang = height * np.array([float(i), float(i+1)]) / pyramid[p]
                    xrang = width * np.array([float(j), float(j+1)]) / pyramid[p]
                    c = encoded_shape_descriptors[reduce(np.logical_and, [x >= xrang[0], x < xrang[1], y >= yrang[0], y < yrang[1]])] # get submatrix
                    if c.shape[0] == 0:
                        f = np.zeros(encoded_shape_descriptors.shape[1])
                    else:
                        f = np.amax(c, axis=0) # max vals in submatrix
                    feas[:len(f), counter] = f
                    counter += 1
        return feas

    def svm_train(self):
        clf = sklearn.svm.LinearSVC(multi_class='crammer_singer')
        training_data = []
        labels = []
        for (cls, idx) in self.data.keys():
            training_data.append(self.data[(cls, idx)]['spp_descriptor'])
            labels.append(self.hash_cls(cls))
        print("Training SVM...")
        clf = clf.fit(training_data, labels)
        print("Saving classifier...")
        with open(self.CLASSIFIER_FILE, 'wb') as out_file:
            pickle.dump(clf, out_file, -1)
        return clf

    def svm_classify(self):
        with open(self.CLASSIFIER_FILE, 'rb') as in_file:
            clf = pickle.load(in_file)
        testing_data = []
        for (cls, idx) in self.data.keys():
            testing_data.append(self.data[(cls, idx)]['spp_descriptor'])
        predictions = clf.predict(testing_data)
        return [self.label_to_cls[pred] for pred in predictions]

    def svm_classify_test(self):
        predictions = self.svm_classify()
        labels = []
        for (cls, idx) in self.data.keys():
            labels.append(cls)
        correct = 0
        for i in range(len(labels)):
            if predictions[i] == labels[i]:
                correct += 1
            else:
                print("Mistook %s for %s" % (labels[i], predictions[i]))
        print("Correct: %s out of %s (Accuracy: %.2f%%)" % (correct, len(predictions), 100. * correct / len(predictions)))

    def show(self, image):
        cv2.imshow('image', image)
        _ = cv2.waitKey()

    def extr_raw_points(self, c, max_value, N, nn):
        # -------------------------------------------------------
        # [SegmentX, SegmentY,NO]=GenSegmentsNew(a,b,maxvalue,nn)
        # This function is used to generate all the segments
        # vectors of the input contour
        # a and b are the input contour sequence
        #  maxvalue is the stop condition of DCE, usually 1~1.5
        #  nn is the sample points' number on each segment, in super's method,n=25
        # SegmentX,SegmentY denotes all the coordinates of all the segments of input contour
        # NO denotes the number of segments of input contour
        # -------------------------------------------------------
        kp, _, _ = evolution(c, N, max_value, 0, 0, 0) # critical points
        n2 = cdist(kp, c)

        i_kp = np.argmin(n2.transpose(), axis=0) # column-wise min
        n_kp = len(i_kp)
        n_cf = (n_kp - 1) * n_kp + 1
        pnts = [None] * n_cf

        s = 0
        for i in range(n_kp):
            for j in range(n_kp):
                if i == j:
                    continue
                if i < j:
                    cf = c[i_kp[i]:i_kp[j]+1, :]
                if i > j:
                    cf = np.append(c[i_kp[i]:, :], c[:i_kp[j]+1, :], axis=0)
                pnts[s] = self.sample_contour(cf, nn)
                s += 1
        pnts[s] = self.sample_contour(c, nn)
        return pnts

    def sample_contour(self, cf, nn):
        # Sample points from contour fragment
        _len = cf.shape[0]
        ii = np.round(np.arange(0, _len - 0.9999, float(_len - 1) / (nn - 1))).astype('int32')
        cf = cf[ii, :]
        return cf

    def next_count(self, cls):
        self.counter[cls] += 1
        return self.counter[cls]

    def get_image_identifier(self, cls):
        return (cls, self.next_count(cls))

    def train(self):
        self.load_training_set()
        self.normalize_shapes()
        self.extract_cf()
        self.learn_codebook()
        self.encode_cf()
        self.spp()
        self.svm_train()

    def test(self):
        self.load_testing_set('data/cuauv/')
        self.normalize_shapes()
        self.extract_cf()
        self.encode_cf()
        self.spp()
        self.svm_classify_test()

    def classify_once(self, img):
        self.data.clear()
        image_id = self.get_image_identifier('unknown')
        self.data[image_id]['image'] = img
        self.normalize_shapes()
        self.extract_cf()
        self.encode_cf()
        self.spp()
        return self.svm_classify()[0]

    def hash_cls(self, cls):
        return sum([ord(c) for c in cls])


if __name__ == "__main__":
    bcf = BCF()
    train = False
    if len(sys.argv) > 1:
        train = sys.argv[1] == "train"
    if train:
        print("Training mode")
        bcf.train()
    else:
        print("Testing mode")
        bcf.test()

