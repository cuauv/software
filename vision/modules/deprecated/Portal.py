import cv2
import numpy as np

from vision.modules import ModuleBase
import gui_options

import shm

capture_source = 'forward'

# fll - frame L* lower, flu - frame L* upper
# pbl - pike L* lower
vision_options = [gui_options.IntOption("fhl", 57, 0, 255), gui_options.IntOption("fhu", 100, 0, 255),
                  gui_options.IntOption("fsl", 0, 0, 255), gui_options.IntOption("fsu", 255, 0, 255),
                  gui_options.IntOption("fvl", 118, 0, 255), gui_options.IntOption("fvu", 225, 0, 255),
                  gui_options.IntOption("phl", 0, 0, 255), gui_options.IntOption("phu", 255, 0, 255),
                  gui_options.IntOption("psl", 20, 0, 255), gui_options.IntOption("psu", 80, 0, 255),
                  gui_options.IntOption("pvl", 0, 0, 255), gui_options.IntOption("pvu", 75, 0, 255),
                  gui_options.IntOption("Sfs", 16), gui_options.IntOption("Sps", 4), gui_options.IntOption("yl", 200),
                  gui_options.IntOption("al", 3000), gui_options.IntOption("pm", 3), gui_options.IntOption("pl", 10),
                  gui_options.FloatOption("plf", 0.35), gui_options.IntOption("pmdx", 10),
                  gui_options.FloatOption("pmdf", 0.25), gui_options.IntOption("bxl", 10),
                  gui_options.IntOption("bxu", 1010), gui_options.IntOption("pal", 1000)]

CAMERA_WIDTH = 1020


class Portal(ModuleBase.ModuleBase):
    def __init__(self):
        super(Portal, self).__init__(True)

    def process(self, Mp):
        self.post("orig", Mp)
        Mp2 = np.copy(Mp)
        # Apparently, some of these functions modify their arguments without documenting it well...

        self.kf = cv2.getStructuringElement(cv2.MORPH_RECT, (self.options["Sfs"], self.options["Sfs"]))
        self.kp = cv2.getStructuringElement(cv2.MORPH_RECT, (self.options["Sps"], self.options["Sps"]))

        M = cv2.cvtColor(Mp, cv2.COLOR_BGR2HSV)
        Ms = cv2.split(M)

        Mfh = cv2.inRange(Ms[0], self.options["fhl"], self.options["fhu"])
        Mfs = cv2.inRange(Ms[1], self.options["fsl"], self.options["fsu"])
        Mfv = cv2.inRange(Ms[2], self.options["fvl"], self.options["fvu"])

        Mph = cv2.inRange(Ms[0], self.options["phl"], self.options["phu"])
        Mps = cv2.inRange(Ms[1], self.options["psl"], self.options["psu"])
        Mpv = cv2.inRange(Ms[2], self.options["pvl"], self.options["pvu"])

        # Mfhh = cv2.equalizeHist(Mfh)

        Mf = Mfh & Mfv
        Mp = Mps

        #Mfh = cv2.equalizeHist(Mf)
        #Mph = cv2.equalizeHist(Mp)

        Mff = cv2.dilate(Mf, self.kf)
        Mpf = cv2.dilate(cv2.erode(Mp, self.kp), self.kp)

        self.post("Mff", Mff)
        self.post("Mpf", Mpf)

        # findContours modifies 
        Mff2 = np.copy(Mff)
        _, cs, _ = cv2.findContours(Mff2, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

        # largest contour with y greater than yl
        cm = None
        cma = -1
        for c in cs:
            ca = cv2.contourArea(c)
            if ca > cma:
                ylok = True
                for p in c:
                    if p[0][1] < self.options["yl"]:
                        ylok = False
                        break

                if not ylok:
                    continue
                cm = c
                cma = ca

        if cma < self.options["al"] or cm is None:
            print("Portal: failed to identify any contours.")
            shm.wire_results.area.set(-1)
            return

        Mc = np.copy(Mp2)
        cv2.drawContours(Mc, cs, -1, (255, 255, 0), thickness=2)
        self.post("all contours", Mc)

        Mc2 = np.copy(Mp2)
        cv2.drawContours(Mc2, [cm], -1, (255, 255, 0), thickness=2)
        self.post("max contour", Mc2)

        # get the longest side of the rectangle (should be the bottom, except for extreme angles!)
        #osx = rm[0][0]
        #osy = rm[0][1]
        #for i in range(1, )

        # identify left and right posts

        shm.wire_results.area.set(cma)

        cmr = []
        M = self.options["pm"]
        i = 0
        for p in cm:
            if i % M == 0:
                cmr.append(p)
            i += 1

        MDX = self.options["pmdx"]
        MDF = self.options["pmdf"]

        posts = []
        postxs = []
        postys = []
        post = [cmr[0]]
        ox = cmr[0][0]
        for p in cmr:
            dx = abs(p[0] - ox)

            if dx[0] > MDX:
                posts.append(post)
                postxs.append(ox[0])

                my = 10000
                for q in post:
                    if q[0][1] < my:
                        my = q[0][1]
                postys.append(my)

                post = [p]
                ox = p[0]
            else:
                post.append(p)
                ox += (p[0] - ox) * MDF

        posts = sorted(posts, key=lambda x: -len(x))

        pa = None
        pb = None

        Mc3 = np.copy(Mp2)

        if len(posts) > 0 and len(posts[0]) >= self.options["pl"]:
            pa = posts[0]
            cv2.drawContours(Mc3, [np.array(pa)], -1, (255, 0, 0), thickness=8)

            shm.wire_results.ap.set(1)
            shm.wire_results.ay.set(postys[0])
            shm.wire_results.ax.set(postxs[0])

            if len(posts) > 1 and len(posts[1]) >= self.options["pl"] and len(posts[1]) >= self.options["plf"] * len(
                    posts[0]):
                pb = posts[1]
                cv2.drawContours(Mc3, [np.array(pb)], -1, (0, 255, 0), thickness=8)

                shm.wire_results.bp.set(1)
                shm.wire_results.by.set(postys[1])
                shm.wire_results.bx.set(postxs[1])
            else:
                shm.wire_results.bp.set(0)

        else:
            shm.wire_results.ap.set(0)

        self.post("post", Mc3)

        # check if bounding box runs into border of screen
        # if it runs into the left side, we're probably on the RHS, if it runs into the right, probably on the LHS
        # thanks to Alex R. for the idea!b
        bl = False
        br = False

        for p in cm:
            if p[0][0] <= self.options["bxl"]:
                bl = True
            elif p[0][0] >= self.options["bxu"]:
                br = True

        #print(sorted([p[0][0] for p in cm])[0])

        print(Mpf.shape)
        Mpcb = np.zeros([Mpf.shape[0], Mpf.shape[1]], dtype=np.uint8)
        cv2.drawContours(Mpcb, [cv2.convexHull(cm)], -1, 255, thickness=cv2.FILLED)
        self.post("pikeb", Mpcb)
        Mpc = Mpcb & Mpf

        xs = 0
        ys = 0
        n = 0
        for x in range(0, Mpcb.shape[0], 5):
            for y in range(0, Mpcb.shape[1], 5):
                if Mpc[x][y] == 255:
                    xs += x
                    ys += y
                    n += 1
        if n > 0:
            shm.wire_results.y.set(xs / n)
            shm.wire_results.x.set(ys / n)
        else:
            shm.wire_results.y.set(-1)
            shm.wire_results.x.set(-1)
        
        self.post("pike", Mpc)

        xs = 0
        ys = 0
        n = 0
        for x in range(0, Mpc.shape[0], 5):
            for y in range(0, Mpc.shape[1], 5):
                if Mpc[x][y] == 255:
                    xs += x
                    ys += y
                    n += 1
        if n > 0:
            shm.wire_results.pp.set(1)
            shm.wire_results.px.set(ys / n)
            shm.wire_results.py.set(xs / n)
        else:
            shm.wire_results.pp.set(0)

        breach = False
        
        if bl:
            breach = True
            shm.wire_results.breach_left.set(True)
        else:
            shm.wire_results.breach_left.set(False)

        if br:
            breach = True
            shm.wire_results.breach_right.set(True)
        else:
            shm.wire_results.breach_right.set(False)

        shm.wire_results.breach.set(breach)

        angle = None

        if bl and not br:
            shm.wire_results.angle.set(-50)

        if br and not bl:
            shm.wire_results.angle.set(50)


        if not breach:
            rm = cv2.minAreaRect(cm)
            #print("ANGLE (- is CCW)")
            #print(rm[2])
            if rm[1][1] > rm[1][0]:
                #print("ANGLE (adjusted)")
                # if L > W, subtract from -90
                #print(-90 - rm[2])
                
                angle = 90 + rm[2]
            else:
                angle = rm[2]

            print((rm[0:2], angle))
            
            shm.wire_results.angle.set(angle)
