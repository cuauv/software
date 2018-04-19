import math
import cv2
from collections import namedtuple
from functools import reduce

CONTOUR_CIRCULARITY_HEURISTIC_LIMIT = 10
CONTOUR_SCALED_HEURISTIC_LIMIT = 2
ContourAreaData = namedtuple('ContourAreaData', ['contour', 'area'])
ContourScoreData = namedtuple('ContourScoreData', ['contour', 'area', 'circularity', 'score', 'center', 'radius'])

def process(self, mat, results):
    results_g = results.get()
    self.post('orig', mat)
    lab_image = cv2.cvtColor(mat, cv2.COLOR_RGB2LAB)
    lab_split = cv2.split(lab_image)
    lab_athreshed = cv2.inRange(lab_split[1], self.options['lab_a_min'],
                                              self.options['lab_a_max'])
    if self.options['verbose']:
        self.post('lab a threshed', lab_athreshed)
    lab_bthreshed = cv2.inRange(lab_split[2], self.options['lab_b_min'],
                                              self.options['lab_b_max'])
    if self.options['verbose']:
        self.post('lab b threshed', lab_bthreshed)

    hls_image = cv2.cvtColor(mat, cv2.COLOR_RGB2HLS)
    hls_split = cv2.split(hls_image)
    hls_hthreshed = cv2.inRange(hls_split[0], self.options['hls_h_min'],
                                              self.options['hls_h_max'])
    if self.options['verbose']:
        self.post('hls h Threshed', hls_hthreshed)

    finalThreshed = hls_hthreshed & lab_athreshed & lab_bthreshed
    if self.options['verbose']:
        self.post('finalThreshed', finalThreshed)

    blurred = cv2.medianBlur(finalThreshed, self.options['blur_size'] * 2 - 1)
    self.post('blurred', blurred)

    _, contours, hierarchy = cv2.findContours(blurred.copy(), cv2.RETR_EXTERNAL,
                                                cv2.CHAIN_APPROX_SIMPLE)
    contourAreas = []
    for contour in contours:
        contourArea = cv2.contourArea(contour)
        if contourArea >= self.options['min_area']:
            contourAreas.append(ContourAreaData(contour, contourArea))
    contourAreas = sorted(contourAreas, key=lambda x: -x.area)[:CONTOUR_CIRCULARITY_HEURISTIC_LIMIT]

    contourScores = []
    for contourArea in contourAreas:
        center, radius = cv2.minEnclosingCircle(contourArea.contour)
        circularity = contourArea.area / (math.pi * radius ** 2)
        heuristic_score = circularity * contourArea.area
        if circularity >= self.options['min_circularity'] and\
           heuristic_score >= self.options['min_heuristic_score']:
            contourScores.append(ContourScoreData(contourArea.contour,
                contourArea.area, circularity, heuristic_score, center, radius))
    contourScores = sorted(contourScores, key=lambda x: -x.score)[:CONTOUR_SCALED_HEURISTIC_LIMIT]

    if contourScores:
        topContour = min(contourScores, key=lambda x: x.center[1]) # Zero is top-left of image
        _topContour = topContour._replace(score=topContour.score / 2) # Reduce score of top contour
        try:
            contourScores = [x if x != topContour else _topContour for x in contourScores]
        except:
            print(topContour)

        buoyContour = max(contourScores, key=lambda x: x.score)
        contoursMat = mat.copy()
        cv2.drawContours(contoursMat, [buoyContour.contour], -1, (255, 255, 0), 2)
        self.post("All contours", contoursMat)

        results_g.center_x = int(buoyContour.center[0])
        results_g.center_y = int(buoyContour.center[1])
        results_g.top_x = int(buoyContour.center[0])
        results_g.top_y = int(buoyContour.center[1] - buoyContour.radius / 2)
        results_g.area = buoyContour.area
        results_g.heuristic_score = buoyContour.score
        results_g.percent_frame = 100 * buoyContour.area / (mat.shape[0]*mat.shape[1])
        results_g.probability = buoyContour.score / reduce(lambda acc, x: acc + x.score, contourScores, 0)
    else:
        results_g.heuristic_score = 0
        results_g.probability = 0
        results_g.area = 0
        results_g.percent_frame = 0

    self.fill_single_camera_direction(results_g)
    results.set(results_g)
