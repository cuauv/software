import cv2
import numpy as np

class HomographyResult:
    def __init__(self, homography, template_image, template_kp, image_kp, matches, inlier_matches):
        self.homography = homography
        self.template_image = template_image
        self.template_kp = template_kp
        self.image_kp = image_kp
        self.matches = matches
        self.inlier_matches = inlier_matches

    def score(self):
        # a better way to do this would be to use a legitmate classifier based on the data that we have
        return len(self.inlier_matches) / len(self.matches)


class FeatureFinder:
    def __init__(self, detector, *images):
        # type: (Union[cv2.xfeatures2d_SIFT, cv2.xfeatures2d_SURF, cv2.xfeatures2d_DAISY, cv2.xfeatures2d_FREAK, cv2.xfeatures2d_LATCH, cv2.xfeatures2d_LUCID, cv2.AKAZE, cv2.BRISK, cv2.KAZE, cv2.MSER, cv2.ORB, cv2.SimpleBlobDetector, cv2.StereoBM, cv2.StereoSGBM, cv2.Tracker], *Union[Tuple[str, str], Tuple[str, str, str]]) -> None
        self.set_detector(detector)

        self._features = {}
        self._matcher = cv2.BFMatcher()

        for image_tuple in images:
            image_tuple_it = iter(image_tuple)
            try:
                name = next(image_tuple_it)
                image_file_name = next(image_tuple_it)
            except StopIteration:
                raise ValueError('Each image parameter to {classname} must have at least two elements'.format(classname=type(self).__name__))
            try:
                mask_file_name = next(image_tuple_it)
            except StopIteration:
                mask_file_name = None
            image = cv2.imread(image_file_name)
            if image is None:
                raise ValueError('Could not read image ({}, {})'.format(name, image_name))
            mask = None
            if mask_file_name is not None:
                mask = np.fromfile('mask_file_name', dtype=np.uint8)
                mask.reshape(image.shape[:2])

            kp, des = detector.detectAndCompute(image, mask)
            if des is None:
                raise ValueError('No keypoints found for ({}, {}{})'.format(name, image_file_name, '' if mask is None else ', {}'.format(mask_file_name)))

            self._features[name] = kp, des, image

    def set_detector(self, detector):
        if not hasattr(detector, 'detectAndCompute'):
            raise ValueError('Invalid feature detector! No \'detectAndCompute\' method found')
        self._detector = detector


    def match_against(self, image, ratio_thresh=0.7, ransac_thresh=5.0):
        img_kp, img_des = self._detector.detectAndCompute(image, None)
        if img_des is None:
            # if there are no features detected in the source image
            return {}

        res = {}
        for name in self._features:
            ref_kp, ref_des, ref_image = self._features[name]
            matches = self._matcher.knnMatch(img_des, ref_des, k=2)
            if len(matches) == 0 or len(matches[0]) == 1:
                # if we have no matches, or there is only one point to match against
                continue

            good = []
            for (m, n) in matches:
                m1, m2 = sorted((m, n), key=lambda match:match.distance)
                ratio_test_value = m1.distance / m2.distance
                if ratio_test_value > ratio_thresh:
                    good.append(m1)

            # consider increasing this to decrease noise
            if len(good) < 10:
                # if we have fewer than 10 good matches (unreliable - need at least 8 for homography, and there's some chance we have noise)
                continue

            img_pts = np.float32([img_kp[m.queryIdx].pt for m in good]).reshape(-1,1,2)
            ref_pts = np.float32([ref_kp[m.trainIdx].pt for m in good]).reshape(-1,1,2)

            homography, inlier_idxs = cv2.findHomography(ref_pts, img_pts, cv2.RANSAC, ransac_thresh)

            if inlier_idxs is None:
                # can be None if no homography was found
                continue

            inlier_matches = {good[i] for (i, v) in enumerate(inlier_idxs) if v}
            img_inliers = np.float32([img_kp[m.queryIdx].pt for m in inlier_matches]).reshape(-1,1,2)
            ref_inliers = np.float32([ref_kp[m.trainIdx].pt for m in inlier_matches]).reshape(-1,1,2)

            trans_ref_inliers = cv2.perspectiveTransform(ref_inliers, homography)
            img_inliers = img_inliers.reshape(-1, 2)
            ref_inliers = ref_inliers.reshape(-1, 2)
            trans_ref_inliers = trans_ref_inliers.reshape(-1, 2)

            dists = np.linalg.norm(img_inliers - trans_ref_inliers, axis=-1)

            res[name] = HomographyResult(homography, ref_image, ref_kp, img_kp, good, inlier_matches)
            '''
            print('homography:\n{}'.format(homography))
            print('number of all matches: {}'.format(len(matches)))
            print('number of good matches: {}'.format(len(good)))
            print('Number of inliers: {}'.format(len(dists)))
            print('mean inlier dist: {}'.format(sum(dists) / len(dists)))
            # print('m: {}'.format(m))
            '''
        return res



