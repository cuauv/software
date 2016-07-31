from mission.framework.helpers import ConsistencyCheck

class Tracker:
    """
    Input vision coordinates for two objects, get objects back that are as
    consistent with previously seen results as possible.

    All objects inputted must have an x and y field.
    """
    def __init__(self, reject_distance):
        self.reject_distance = reject_distance
        self.mappings = [ConsistentObject() for i in range(2)]

    def track(self, obj1, obj2):
        """
        Optimally assign the two objects to the given object mappings. Objects may be None.

        Returns a list of two objects that should be paired as closely to old results as possible.
        """
        # Represent not having a object as having a None object
        all_objects = [obj1, obj2]
        combinations = [
            [(all_objects[i], self.mappings[i]) for i in range(2)],
            [(all_objects[1-i], self.mappings[i]) for i in range(2)],
        ]
        distances = [[self.distance_sq(m[0], m[1]) for m in c] for c in combinations]

        sums = [sum(c) for c in distances]
        final_mapping = None

        if all(s < float('inf') for s in sums):
            if sums[0] < sums[1]:
                final_mapping = combinations[0]
            else:
                final_mapping = combinations[1]

        elif sums[0] < float('inf'):
            final_mapping = combinations[0]
        elif sums[1] < float('inf'):
            final_mapping = combinations[1]

        else:
            min_dists = [min(dists) for dists in distances]
            if min_dists[0] < min_dists[1]:
                final_mapping = combinations[0]
            else:
                final_mapping = combinations[1]

        return [m[1].map(m[0]) for m in final_mapping]

    def distance_sq(self, obj, mapping):
        """
        Returns the distance (squared) from the given object to the last object
        fed into the mapping
        """
        if obj is None or mapping.last_obj is None:
            return float('inf')

        distance_sq = (mapping.last_obj.x - obj.x) ** 2 + (mapping.last_obj.y - obj.y) ** 2
        if distance_sq >= self.reject_distance ** 2:
            return float('inf')

        return distance_sq

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
