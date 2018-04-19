import os
import sys
from collections import defaultdict
from functools import reduce

from scipy.spatial.distance import cdist
import numpy as np
import cv2
import sklearn
import sklearn.cluster
import cPickle as pickle

from evolution import evolution
from shape_context import shape_context
from llc import llc_coding_approx

class BCF():
    def __init__(self):
        self.DATA_DIR = "data/cuauv"
        self.PERC_TRAINING_PER_CLASS = 0.5
        self.CODEBOOK_FILE = "codebook.data"
        self.CLASSIFIER_FILE = "classifier"
        self.classes = defaultdict(list)
        self.data = defaultdict(dict)
        self.counter = defaultdict(int)

    def load_classes(self):
        for dir_name, subdir_list, file_list in os.walk(self.DATA_DIR):
            if subdir_list:
                continue
            for f in sorted(file_list, key=hash):
                self.classes[dir_name.split('/')[-1]].append(os.path.join(dir_name, f))

    def load_training(self):
        for cls in self.classes:
            images = self.classes[cls]
            for image in images[:int(len(images) * self.PERC_TRAINING_PER_CLASS)]:
                image_id = self.get_image_identifier(cls)
                self.data[image_id]['image'] = cv2.imread(image, cv2.IMREAD_GRAYSCALE)
                if self.data[image_id]['image'] is None:
                    print("Failed to load " + image)

    def load_testing(self):
        for cls in self.classes:
            images = self.classes[cls]
            for image in images[int(len(images) * self.PERC_TRAINING_PER_CLASS):]:
                image_id = self.get_image_identifier(cls)
                self.data[image_id]['image'] = cv2.imread(image, cv2.IMREAD_GRAYSCALE)
                if self.data[image_id]['image'] is None:
                    print("Failed to load " + image)

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
            contours, _ = cv2.findContours(image, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
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
            labels.append(hash(cls))
        print("Training SVM...")
        clf = clf.fit(training_data, labels)
        print("Saving classifier...")
        with open(self.CLASSIFIER_FILE, 'wb') as out_file:
            pickle.dump(clf, out_file, -1)
        return clf

    def svm_classify_test(self):
        with open(self.CLASSIFIER_FILE, 'rb') as in_file:
            clf = pickle.load(in_file)
        testing_data = []
        labels = []
        label_to_cls = {}
        for (cls, idx) in self.data.keys():
            testing_data.append(self.data[(cls, idx)]['spp_descriptor'])
            labels.append(hash(cls))
            label_to_cls[hash(cls)] = cls
        predictions = clf.predict(testing_data)
        correct = 0
        for i in range(len(labels)):
            if predictions[i] == labels[i]:
                correct += 1
            else:
                print("Mistook %s for %s" % (label_to_cls[labels[i]], label_to_cls[predictions[i]]))
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

if __name__ == "__main__":
    bcf = BCF()
    bcf.load_classes()
    train = False
    if len(sys.argv) > 1:
        train = sys.argv[1] == "train"
    if train:
        print("Training mode")
        bcf.load_training()
        bcf.normalize_shapes()
        bcf.extract_cf()
        bcf.learn_codebook()
        bcf.encode_cf()
        bcf.spp()
        #for (cls, idx) in bcf.data.keys():
        #    image = bcf.data[(cls, idx)]
        #    print (cls, idx)
        #    print (np.sum(image['spp_descriptor']))
        bcf.svm_train()
    else:
        print("Testing mode")
        bcf.load_testing()
        bcf.normalize_shapes()
        bcf.extract_cf()
        bcf.encode_cf()
        bcf.spp()
        bcf.svm_classify_test()

   # C = np.array([
   # [6.0000,    5.8000],
   # [8.4189,    5.8000],
   #[10.8378,    5.8000],
   #[12.8425,    6.8000],
   #[15.2614,    6.8000],
   #[17.6803,    6.8000],
   #[19.7773,    7.5773],
   #[22.1039,    7.8000],
   #[24.1086,    6.8000],
   #[26.5275,    6.8000],
   #[28.9464,    6.8000],
   #[30.9511,    7.8000],
   #[33.2616,    8.0616],
   #[35.3747,    8.8000],
   #[37.3794,    9.8000],
   #[39.7983,    9.8000],
   #[42.1536,    9.6464],
   #[43.8640,    7.9360],
   #[46.1602,    7.9602],
   #[48.2313,    8.8000],
   #[50.4597,    9.2597],
   #[52.6548,    9.8000],
   #[54.6595,   10.8000],
   #[57.0555,   10.8555],
   #[59.0831,   11.8000],
   #[61.0878,   12.8000],
   #[63.3583,   13.1583],
   #[65.3616,   13.4384],
   #[67.1019,   11.8000],
   #[69.5208,   11.8000],
   #[71.9397,   11.8000],
   #[73.9444,   12.8000],
   #[75.9640,   13.7640],
   #[78.3680,   13.8000],
   #[80.3727,   14.8000],
   #[82.5597,   15.3597],
   #[84.7963,   15.8000],
   #[86.8593,   16.6593],
   #[89.1555,   16.9555],
   #[91.1588,   17.9588],
   #[92.2000,   19.9464],
   #[93.2000,   21.9511],
   #[93.2000,   24.3700],
   #[94.2000,   26.3747],
   #[94.7611,   28.5611],
   #[95.2000,   30.7983],
   #[94.0657,   32.2000],
   #[91.6468,   32.2000],
   #[89.2279,   32.2000],
   #[86.8090,   32.2000],
   #[84.8616,   31.0616],
   #[82.7996,   30.2000],
   #[80.5621,   29.7621],
   #[78.5587,   28.7587],
   #[76.3713,   28.2000],
   #[74.3666,   27.2000],
   #[73.2000,   28.9209],
   #[70.9430,   29.2000],
   #[68.6635,   28.8635],
   #[66.6602,   27.8602],
   #[64.5147,   27.2000],
   #[62.5100,   26.2000],
   #[60.5053,   25.2000],
   #[58.3540,   24.5540],
   #[56.4960,   23.2000],
   #[54.3474,   22.5474],
   #[52.0724,   22.2000],
   #[50.0479,   21.2479],
   #[48.0445,   20.2445],
   #[46.2000,   21.2245],
   #[45.6394,   23.2000],
   #[43.2205,   23.2000],
   #[41.2158,   22.2000],
   #[39.2111,   21.2000],
   #[37.1460,   20.3460],
   #[35.1426,   19.3426],
   #[33.1393,   18.3393],
   #[30.8431,   18.0431],
   #[28.7734,   17.2000],
   #[26.7687,   18.2000],
   #[24.5403,   17.7403],
   #[22.5370,   16.7370],
   #[20.7547,   15.2000],
   #[18.7500,   14.2000],
   #[16.7453,   13.2000],
   #[14.7406,   12.2000],
   #[12.7359,   11.2000],
   #[10.8099,   10.0099],
   # [8.7265,    9.2000],
   # [6.8033,    8.0033]
   # ])
   # max_curvature = 1.5
   # n_contsamp = 50
   # n_pntsamp = 10
   # print bcf.extr_raw_points(C, max_curvature, n_contsamp, n_pntsamp)

