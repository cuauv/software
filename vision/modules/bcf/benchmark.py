import cv2
import numpy as np
import cv2
import sklearn
import sklearn.cluster
import cPickle as pickle

from bcf import *

HU_CODEBOOK = 'hu.data'

def calc_hu_moments(data):
    hu_moments = {}
    for (cls, idx) in data.keys():
        image = data[(cls, idx)]['normalized_image']
        hu_moments[(cls, idx)] = cv2.HuMoments(cv2.moments(image))
    hu_moments_concat = []
    labels_concat = []
    for (cls, idx) in hu_moments.keys():
        labels_concat.append(cls)
        hu_moments_concat.append(hu_moments[(cls, idx)])
    hu_moments_concat = np.concatenate(hu_moments_concat, axis=1).transpose()
    return hu_moments_concat, labels_concat

def train_kmeans(hu_moments_concat, labels_concat):
    print("Running KMeans...")
    kmeans = sklearn.cluster.KMeans(min(len(labels_concat), len(bcf.classes.keys()) * 3), n_jobs=-1, algorithm='elkan').fit(hu_moments_concat)
    classification = {}
    for i in range(len(kmeans.labels_)):
        idx_label = kmeans.labels_[i]
        classification[idx_label] = labels_concat[i]
    print("Saving codebook...")
    with open(HU_CODEBOOK, 'wb') as out_file:
        pickle.dump({'classification': classification, 'kmeans': kmeans}, out_file, -1)

def test_kmeans(hu_moments_concat, labels_concat):
    with open(HU_CODEBOOK, 'rb') as in_file:
        codebook = pickle.load(in_file)
        classification = codebook['classification']
        kmeans = codebook['kmeans']
    print("Running KMeans...")
    predictions = [classification[idx] for idx in kmeans.predict(hu_moments_concat)]
    correct = 0
    for i in range(len(labels_concat)):
        if labels_concat[i] == predictions[i]:
            correct += 1
        else:
            print("Mistook %s for %s" % (labels_concat[i], predictions[i]))
    print("Correct: %s out of %s (Accuracy: %.2f%%)" % (correct, len(predictions), 100. * correct / len(predictions)))

# Compare with Hu moments + K-means
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
        hu_moments_concat, labels_concat = calc_hu_moments(bcf.data)
        train_kmeans(hu_moments_concat, labels_concat)
    else:
        print("Testing mode")
        bcf.load_testing()
        bcf.normalize_shapes()
        hu_moments_concat, labels_concat = calc_hu_moments(bcf.data)
        test_kmeans(hu_moments_concat, labels_concat)

