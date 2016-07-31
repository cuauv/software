'''Utility functions for turning screen-space coordinates into angles, etc.'''
from math import radians, degrees, atan, tan, sqrt, pi, asin, sin

FORWARD_LENS_PARAMS = (125., 109.) # Theia SY125M
DOWNWARD_LENS_PARAMS = (66.9, 52.7) # Kowa LM3NCM

# Converts lens params (in degrees, from datasheet)
# to actual field of view (in radians)
# uses Snell's Law
def compute_fov_from_lens_params(H, V):
    def snell(X):
        X = radians(X)
        RI_GLASS = 1.53 # borosilicate
        RI_WATER = 1.33
        phi = asin(1./RI_GLASS * sin(X/2))
        return 2. * asin(RI_GLASS / RI_WATER * sin(phi))
    return (snell(H), snell(V))

H_FOV, V_FOV = compute_fov_from_lens_params(*FORWARD_LENS_PARAMS)

H_PIXELS = 1024
V_PIXELS = 768

H_HALF_LENGTH = tan( H_FOV / 2)
V_HALF_LENGTH = tan( V_FOV / 2)

'''
ASCII ART TIME!
Top-down view
          H_HALF_LENGTH
               ____

            \           /
             \ _X______/ <----- camera's 'screen'
        |     \   |   /   
  length|      \  |  /
    1   |       \ | /
        |        \|/ <--- angle is H_FOV

The 'X' marks the x_pos of the point on the screen that we want the angle to.
We define the distance to the 'screen' to be 1 arbitray unit.
Then, by knowing the FOV, we calculate the width of half the screen in 'unit'.
For each point, we can then calculate what percent of this length out it is,
so we can then complete the right triangle as follows:
    side A: x_off (the horizontal side in the diagram)
    angle C: 90 degrees
    side B: 1 (the vertical side in the diagram)
The arctangent of x_off then gives us angle A, which is what we want.
Similar remarks hold for y_pos in the vertical plane.
'''

def screen_to_angle(x_pos, y_pos):
    '''Given x_pos and y_pos in screen coordinates (ie. pixels)
    determines the angles from vertical and horizontal to those objects.
    Undistortion is needed to ensure that this mapping is correct '''
    x_off = 2*(float(x_pos) - H_PIXELS/2)/H_PIXELS*H_HALF_LENGTH
    y_off = 2*(float(y_pos) - V_PIXELS/2)/V_PIXELS*V_HALF_LENGTH

    x_angle = atan( x_off )
    y_angle = atan( y_off )

    #Note: we reverse y angle since screen coordinates put +y going down
    return degrees(x_angle), -degrees(y_angle)

def area_to_distance(area, radius_in_meters):
    '''Given an on-screen area (in pixels) determine the distance
    that a sphere of size radius_in_meters would be.'''

    radius_in_pixels = sqrt(area / pi)

    #this is the size if the projected plane were 1 unit away
    normalized_diameter = 2*radius_in_pixels/(H_PIXELS/2.)*H_HALF_LENGTH

    return radius_in_meters/normalized_diameter
