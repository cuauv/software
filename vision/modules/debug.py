#!/usr/bin/env python3

import cv2
import shm
from vision.modules.base import ModuleBase

class Debug(ModuleBase):
    def process(self, *mats):
        scale = shm.vision_debug.scale.get()
        thickness = shm.vision_debug.thickness.get()
        color_r = shm.vision_debug.color_r.get()
        color_g = shm.vision_debug.color_g.get()
        color_b = shm.vision_debug.color_b.get()
        color = (color_b, color_g, color_r)

        for i in range(10):
            x = getattr(shm.vision_debug, 'x{}'.format(i)).get()
            y = getattr(shm.vision_debug, 'y{}'.format(i)).get()
            text = getattr(shm.vision_debug, 'text{}'.format(i)).get()
            for mat in mats:
                cv2.putText(mat, text, (x, y), cv2.FONT_HERSHEY_PLAIN, scale, color, thickness=thickness)

        for i, mat in enumerate(mats):
            self.post(i, mat)

if __name__ == '__main__':
    Debug()()
