from collections import namedtuple
from itertools import permutations
import math

from mission.framework.consistency import ConsistencyCheck

Match = namedtuple('Match', ['id', 'obs'])

class Matcher:
    MAX_OBJECTS = 6 # Combinatorial explosion with many objects

    class Tracker:
        def __init__(self, cons_obj):
            self.id = None
            self.cons_obj = cons_obj
            self.latest_obs = None

    def __init__(self, pattern, num_trackers=None, max_flow_distance=0.25):
        if num_trackers is None:
            num_trackers = len(pattern)
        if max(len(pattern), num_trackers) > self.MAX_OBJECTS:
            raise ValueError('Matching greater than {} objects too slow'.format(
                self.MAX_OBJECTS))

        self._pattern = pattern
        self._trackers = [self.Tracker(ConsistentObject()) for t in range(num_trackers)]
        self._max_flow_distance = max_flow_distance

    def match(self, observations, force_match=False):
        return self._match(observations, force_match)

    def _match(self, observations, force_match, pattern_changed=False):
        prev_cons_objs = [t.cons_obj.last_obj for t in self._trackers]

        tracker_matched = self._match_perfect(
            observations,
            prev_cons_objs,
            True,
            max_distance=self._max_flow_distance,
        )

        prev_tracked_count = sum(t.latest_obs is not None for t in self._trackers)

        # Update trackers with new observations
        tracker_obs = [self._trackers[i].cons_obj.map(observations[j])
                            for i, j in enumerate(tracker_matched)]
        for i, obs in enumerate(tracker_obs):
            tracker = self._trackers[i]
            tracker.latest_obs = obs
            if obs is None:
                # Object was lost! Remove our classification of it
                tracker.id = None

        tracked_count = sum(o is not None for o in tracker_obs)

        if ((tracked_count != prev_tracked_count or pattern_changed) and \
                tracked_count >= len(self._pattern)) or force_match:
            # Match tracked objects with pattern
            pattern_matches = self._match_perfect(tracker_obs, [m.obs for m in self._pattern], False)
            for i, j in enumerate(pattern_matches):
                self._trackers[j].id = self._pattern[i].id

        return [Match(t.id, t.latest_obs) for t in self._trackers]

    def update_pattern(self, pattern):
        self._pattern = pattern
        # Re-match the current observations
        return self._match([t.latest_obs for t in self._trackers], False, pattern_changed=True)

    def pattern(self):
        return self._pattern

    def _match_perfect(self, incoming, current, flow_dist, max_distance=None):
        """
        Optimally assign the n incoming observations to the current
        observation matches. Observations may be None. The number of incoming and
        current observations may differ.

        Returns a permutation of observations that should be paired as closely to old
        results as possible.
        """
        Perm = namedtuple('Perm', ['obs_indices', 'finite_distance'])

        # Extend incoming with None to make len(incoming) >= len(current)
        if len(incoming) < len(current):
            incoming += [None for i in range(len(current) - len(incoming))]

        # distances[incoming index][current index] is distance between the two

        distances = []
        for obs in incoming:
            obs_distances = []
            for current_obs in current:
                if obs is None or current_obs is None:
                    obs_distances.append(float('inf'))
                    continue

                distance = None
                if flow_dist:
                    distance = current_obs.flow_distance_sq(obs)
                else:
                    distance = current_obs.match_distance_sq(obs)
                if max_distance is None or distance <= max_distance:
                    obs_distances.append(distance)
                else:
                    obs_distances.append(float('inf'))

            distances.append(obs_distances)

        min_infinities = len(current)
        valid_perms = [] # Permutations with minimal infinite distances

        for perm in permutations(range(len(incoming)), len(current)):
            perm_distances = (distances[incoming_i][current_i] for current_i, incoming_i in enumerate(perm))

            finite_distance = 0
            infinities = 0
            for d in perm_distances:
                if d == float('inf'):
                    infinities += 1
                else:
                    finite_distance += d

            if infinities < min_infinities:
                # Discovered a new permutation with less infinities than before
                min_infinities = infinities
                valid_perms = []
            if infinities > min_infinities:
                # Other found permutation with less infinities would always be
                # chosen
                continue

            valid_perms.append(Perm(perm, finite_distance))

        # Find minimum sum of finite distance of permutations with the least
        # infinities
        min_perm = min(valid_perms, key=lambda x: x.finite_distance)
        return min_perm.obs_indices

    # def _match_greedy(self, objs_in):
        # """
        # Optimally assign the n objects to the current object mappings. Objects
        # may be None. The number of objects and the number of mappings may
        # differ.

        # Returns a permutation of objects that should be paired as closely to old
        # results as possible.
        # """
        # Mapping = namedtuple('Mapping', ['obj_i', 'distance'])

        # # Extend objs with None to make len(objs) >= len(self.mappings)
        # objs = objs_in.copy()
        # if len(objs) < len(self.mappings):
            # objs += [None for i in range(len(self.mappings) - len(objs))]

        # # distances[objs index][mapping index] is distance between the two
        # distances = [[self.distance_sq(mapping, obj) for mapping in self.mappings] for obj in objs]

        # final_mappings = [None for i in range(len(self.mappings))]

        # while sum(m is None for m in final_mappings) > 0 and sum(o is not None for o in objs) > 0:
            # possible_mappings = {i: [] for i in range(len(self.mappings)) if final_mappings[i] is None}
            # for obj_i, obj in enumerate(objs):
                # if obj is None:
                    # continue

                # closest_mapping_i = min(possible_mappings.items(), lambda x: 1 if len(x[1]) > 0 else 0)
                # min_distance = float('inf')

                # for mapping_i, mapped_objs in possible_mappings.items():
                    # distance = distances[obj_i][mapping_i]
                    # if distance < min_distance:
                        # closest_mapping_i = mapping_i
                        # min_distance = distance

                # possible_mappings[closest_mapping_i].append(Mapping(obj_i, min_distance))

            # for mapping_i, choices in possible_mappings.items():
                # if len(choices) == 0:
                    # continue
                # best_mapping = min(choices, lambda x: x.distance)
                # final_mappings[mapping_i] = objs[best_mapping.obj_i]
                # del possible_mappings[mapping_i]
                # objs[best_mapping.obj_i] = None

        # return [self.mappings[i].map(obj) for i, obj in enumerate(final_mappings)]

class ConsistentObject:
    """
    Consistency-check an object's existence
    """
    def __init__(self, seen_cons_check=(2, 3), unseen_cons_check=(5, 6)):
        self.last_obj = None
        self.tracking = False
        self.seen_cons_check = ConsistencyCheck(*seen_cons_check)
        self.unseen_cons_check = ConsistencyCheck(*unseen_cons_check)

    def map(self, obj):
        """
        Call with a valid object to count as "seeing the object", or with None
        to count as "not seeing the object"
        """
        if obj is not None:
            self.last_obj = obj
            if self.tracking:
                self.unseen_cons_check.add(False)
            else:
                self.seen_cons_check.add(True)
                if self.seen_cons_check.check():
                    self.tracking = True
                    self.seen_cons_check.clear()
                    self.unseen_cons_check.clear()

        else:
            if self.tracking:
                self.unseen_cons_check.add(True)
                if self.unseen_cons_check.check():
                    self.tracking = False
                    self.seen_cons_check.clear()
                    self.unseen_cons_check.clear()
                    self.last_obj = None

            else:
                self.seen_cons_check.add(False)
                self.last_obj = None

        if self.tracking:
            return self.last_obj
        else:
            return None

class Observation:
    def adopt_attrs(self, attrs):
        self._extra_attrs = attrs

    def __getattr__(self, attr):
        return getattr(self._extra_attrs, attr)

    """ Override these
    Attempt to normalize all components to within [-1, 1]
    """

    def flow_distance_sq(self, obs):
        pass

    def match_distance_sq(self, obs):
        pass

    """ Utilities for Observations """

    def linear_distance_sq(self, v1, v2, min, max):
        half_range = (max - min) / 2
        def scale(v): return ((v - min) / half_range) - 1
        return (scale(v2) - scale(v1)) ** 2

    def rotate_point(self, pt, theta):
        """
        Rotate a point in a coordinate system with +x right and +y down
        counterclockwise by theta.
        """
        cos, sin = math.cos(math.radians(theta)), -math.sin(math.radians(theta))
        return (pt[0] * cos - pt[1] * sin, -(pt[0] * sin + pt[1] * cos))

    def mod_distance_sq(self, v1, v2, mod=2*math.pi):
        def normalize(v): return (v % mod) / (mod / 2) - 1
        abs_distance = abs(normalize(v1) - normalize(v2))
        if abs_distance > 1:
            abs_distance = 2 - abs_distance
        return abs_distance ** 2

class ComplexColor(Observation):
    def __init__(self, lab_a, lab_b, ycrcb_cr, ycrcb_cb):
        self.lab_a, self.lab_b = lab_a, lab_b
        self.ycrcb_cr, self.ycrcb_cb = ycrcb_cr, ycrcb_cb

    def flow_distance_sq(self, obs):
        return 0

    def match_distance_sq(self, obs):
        def color_dist(a, b): return self.linear_distance_sq(a, b, 0, 255)
        return (
            color_dist(self.lab_a, obs.lab_a) +
            color_dist(self.lab_b, obs.lab_b) +
            color_dist(self.ycrcb_cr, obs.ycrcb_cr) +
            color_dist(self.ycrcb_cb, obs.ycrcb_cb)
        )

class CameraCoord(Observation):
    def __init__(self, x, y):
        self.x, self.y = x, y

    def flow_distance_sq(self, obs):
        return (self.x - obs.x) ** 2 + (self.y - obs.y) ** 2

    def match_distance_sq(self, obs):
        return self.flow_distance_sq(obs)

class HeadingInvCameraCoord(CameraCoord):
    def __init__(self, x, y, sub_heading):
        super().__init__(x, y)
        self.sub_heading = sub_heading

    def match_distance_sq(self, obs):
        rot_x1, rot_y1 = self.rotate_point(
            (self.x, self.y), -self.sub_heading)
        rot_x2, rot_y2 = self.rotate_point(
            (obs.x, obs.y), -obs.sub_heading)

        return (rot_x2 - rot_x1) ** 2 + (rot_y2 - rot_y1) ** 2

    def flow_distance_sq(self, obs):
        return self.match_distance_sq(obs)

class HeadingInvAngle(Observation):
    """
    Args:
        angle: Heading of object in degrees
        sub_heading: Heading of sub in degrees
        mod: Modulo of angle (Can be < 360 with symmetry)
    """

    def __init__(self, angle, sub_heading, mod=360):
        self.angle = angle
        self.sub_heading = sub_heading
        self.mod = mod

    def flow_distance_sq(self, obs):
        return 0

    def match_distance_sq(self, obs):
        return self.mod_distance_sq(
            self.angle + self.sub_heading,
            obs.angle + obs.sub_heading,
            self.mod, # Assume they both have same mod
        )
