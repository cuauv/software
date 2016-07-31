import os
import argparse
import pickle
import shutil

import cv2
from sklearn import svm

from caffew import caffe_classifier

parser = argparse.ArgumentParser(description="Helpers to train the Bins vision module")
subparsers = parser.add_subparsers()

train_parser = subparsers.add_parser("train")
train_parser.set_defaults(subparser="train")

test_parser = subparsers.add_parser("test")
test_parser.set_defaults(subparser="test")
test_parser.add_argument("image", type=str, help="Image to test")

args = parser.parse_args()

if not hasattr(args, "subparser"):
    parser.print_help()
    exit()

data_dirname = os.path.dirname(os.path.realpath(__file__))
classifier_bson_filename = data_dirname + "/bins_caffe_classifier.bson"
svm_filename = data_dirname + "/bins_svm_classifier.pkl"


def train():
    print("Begin training!")
    untrained_dirname = data_dirname + "/untrained"

    for filename in (os.path.join(untrained_dirname, filename) for filename in os.listdir(untrained_dirname)):
        print("\nTraining Key:\n"
              "  1 or b for banana\n"
              "  2 or f for bijection\n"
              "  3 or l for lightning\n"
              "  4 or s for soda\n"
              "  0 or m to move to bad image directory\n"
              "  ` to train with the current data"
              )

        cv2.imshow("Untrained Image", cv2.imread(filename))

        char = None
        while char not in ("1", "2", "3", "4", "b", "f", "l", "s", "0", "m", "`"):
            key = cv2.waitKey()
            while key not in range(256):
                key = cv2.waitKey()
            char = chr(key)

        if char in ("0", "m"):
            shutil.move(filename, data_dirname + "/rejected")
            continue

        if char == "`":
            break

        shape = {
            "1": "banana",
            "b": "banana",
            "2": "bijection",
            "f": "bijection",
            "3": "lightning",
            "l": "lightning",
            "4": "soda",
            "s": "soda"
        }[char]

        new_filname = data_dirname + "/{}/{}".format(shape, os.path.basename(filename))
        print(filename, new_filname)
        os.rename(filename, new_filname)

        print("Recieved character {}.".format(char))
        print("Image is of shape {}.".format(shape))
        print("Moved file {} to {}".format(filename, new_filname))

    cv2.destroyAllWindows()
    print("No more images to manually classify.")
    train_caffe()

def train_caffe():
    print("Proceding to caffe training!")
    training_data = []
    for shape_name in ("banana", "bijection", "lightning", "soda"):
        shape_dirname = data_dirname + "/{}/".format(shape_name)
        for filename in (os.path.join(shape_dirname, filename) for filename in os.listdir(shape_dirname)):
            training_data.append([filename, shape_name])

    classifier = caffe_classifier.CaffeTrainer()
    classifier.train(training_data, 500, classifier_bson_filename)

def train_svm():
    print("Training SVM")
    image_vectors = []
    image_classification = []
    for shape_number, shape_name in enumerate(("banana", "bijection", "lightning", "soda")):
        shape_dirname = data_dirname + "/{}/".format(shape_name)
        for filename in (os.path.join(shape_dirname, filename) for filename in os.listdir(shape_dirname)):
            image = cv2.imread(filename)
            moments = []
            for channel in cv2.split(image):
                moments.extend(cv2.HuMoments(cv2.moments(channel)).reshape(7))
            image_vectors.append(moments)
            image_classification.append(shape_number)

    pickle.dump(svm.SVC(probability=True).fit(image_vectors, image_classification),
                open(svm_filename, "w"))


def test():
    classifier = caffe_classifier.CaffeClassifier(classifier_bson_filename)
    print(args.image)
    print(classifier.classify(args.image))


{"train": train,
 "test": test}[args.subparser]()
