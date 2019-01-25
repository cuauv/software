import cv2
from vision.framework.bcf.classify import BCF


def init_shape_recognition_classifier(data_dir, codebook_file, classifier_file):
    """
    Initializes an instance of a shape recognition using the Bag of Contour Fragments method.
    The BCF algorithm works by extracting contour fragments from a shape, calculating a shape
    descriptor for each contour fragment, encoding the descriptor into a shape code, pooling the
    shape codes at various image resolutions to create a compact feature vector to represent the
    original shape, and then classifying the feature vector into one of multiple classes using a
    multi-class linear SVM. For more information, see Chesley Tan's Fall 2017 documentation.
    :param data_dir: directory containing training data, with examples of each class under a
        subdirectory named after the class.
    :param codebook_file: name of the file in which to save the learned codebook
    :param classifier_file: name of the file in which to save the learned classifier
    :return: a BCF classifier
    """
    return BCF(data_dir=data_dir, codebook_file=codebook_file, classifier_file=classifier_file)


def shape_recognition_train(classifier):
    """
    Train a Bag of Contour Fragments shape classifier using data from the provided data directory at
    initialization time.
    :param classifier: the BCF classifier
    :return: None
    """
    classifier.train()


def shape_recognition_classify(classifier, mat):
    """
    Classify an image using a pre-trained Bag of Contour Fragments model
    :param classifier: the BCF classifier
    :param mat: input image
    :return: the classification for the image
    """
    return classifier.classify_once(cv2.cvtColor(mat, cv2.COLOR_BGR2GRAY))

