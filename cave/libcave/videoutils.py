import hashlib
import cv2

def verify_video(filename):
    cap = cv2.VideoCapture(filename)
    cap.set(cv2.CAP_PROP_POS_FRAMES, 0)

    #TODO: Verify accuracy of this
    length = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))

    width = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    height = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))

    success, test_frame = cap.read()
    # assert(success)
    height = len(test_frame)
    # assert(height == len(test_frame))
    width = len(test_frame[0])
    # assert(width == len(test_frame[0]))

    nchannels = len(test_frame[0][0]) #should be 3 elements per frame

    return (cap, width, height, length, nchannels)

def hash_video(filename):
    cap = cv2.VideoCapture(filename)
    length = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))

    if length < 10:
        print("Unable to hash video; bad length!")
        return ""

    hasher = hashlib.md5()
    for i in range(0, length, length / 10):
        cap.set(cv2.CAP_PROP_POS_FRAMES, i)
        success, frame = cap.read()
        if success:
            hasher.update(frame)
        else:
            print "Unable to hash full video, video incomplete?"
            break

    return hasher.hexdigest()
