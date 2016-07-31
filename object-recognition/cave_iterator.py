import shm
from math import sqrt
from common import get_feature_vector_from_group, vision_groups

"""
Iterator that works through shared memory

Eventually, we would like to pass in some object name, and use that object's name to determine which vision outputs to read. This serves to collect the proper training data. The name would be inputted into initializer, and perhaps we would build a command line interface in order to easily train.

Current approach:
    We collect the x and y tag coordinates as well as all of the x y coordinates from each shape_result in vision. We then determine which vision shape_result corresponds to the tag. The one that corresponds to the tag gets its hu moments returned as the result. These are then trained on by the trainer.

"""
class CaveIterator():
    def __init__(self):
        self.w = shm.watchers.watcher()
        self.w.watch(shm.cave_settings)

    def __iter__(self):
        return self

    def next(self):
        if not shm.cave_results.in_train_mode.get():
            raise StopIteration
        shm.cave_settings.trigger.set(True)
        self.w.wait(True)
        if not shm.cave_results.in_train_mode.get():
            raise StopIteration

        tag_x = shm.cave_results.x.get()
        tag_y = shm.cave_results.y.get()
        tag_rad = shm.cave_results.rad.get()
        tag_vis = shm.cave_results.visible.get()

        if not tag_vis:
            print "WARN: Tag not visible for frame. Skipping."
            return None

        vision_group_results = [(g.x.get(), g.y.get(), g.probability.get()) for g in vision_groups]

        vision_group_valid = [(r[2] and dist(r[0], r[1], tag_x, tag_y) <= tag_rad) for r in vision_group_results]

        if vision_group_valid.count(True) > 1:
            print "WARN: Vision returns multiple results within tag bounds. Skipping."
            return None

        if vision_group_valid.count(True) == 0:
            print "WARN: no valid vision results for frame"
            return None # Vision did not produce a valid result for this frame

        for valid, g in zip(vision_group_valid, vision_groups):
            if valid:
                return get_feature_vector_from_group(g)


def dist(x1, y1, x2, y2):
    diff_x = x2 - x1
    diff_y = y2 - y1
    return sqrt(diff_x ** 2 + diff_y ** 2)
