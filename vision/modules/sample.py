#!/usr/bin/env python3
from vision.modules.base import ModuleBase
from vision import options

import cv2

module_options = [
    options.DoubleOption('text_size', 1.0, 0, 1, 20.0),
    options.IntOption('text_thickness', 1, 1, 10)
]

class Hello(ModuleBase):
    def process(self, img):
        hello_img = cv2.putText(
            img, "Hello!", (100, 100), cv2.FONT_HERSHEY_SIMPLEX,
            self.options["text_size"], (0,0,0),
            thickness=self.options["text_thickness"])
        self.post("hello", hello_img)

if __name__ == '__main__':
    Hello("forward", module_options)()
