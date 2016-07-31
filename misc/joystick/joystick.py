import pygame
from pygame.color import THECOLORS as color
from auval.shmem import SharedVar
from auval.heading import Heading
from threading import Thread
from time import sleep, time

axis_mapping = {0: 'sway', 1: 'surge', 2: 'yaw', 3: 'heave'}
rev_axis_mapping = {}
for x in axis_mapping:
    rev_axis_mapping[axis_mapping[x]] = x
button_mapping = {0: 'trigger'}
rev_button_mapping = {}
for x in button_mapping:
    rev_button_mapping[button_mapping[x]] = x

soft_kill = SharedVar('/settings/switches/soft_kill')
var_mapping = {'sway': SharedVar('/desires/sway_speed'), 'surge': SharedVar('/desires/speed'),
               'pitch': SharedVar('/desires/pitch'), 'heave': SharedVar('/desires/depth'),
               'yaw': SharedVar('/desires/heading')}
var_limits = {'sway': (-0.5, 0.5), 'surge': (-0.5, 0.6), 'pitch': (-30, 30), 'heave': (5, 0), 'yaw': (0, 360)}
is_additive = {'sway': False, 'surge': False, 'pitch': True, 'heave': False, 'yaw': True}
additive_change_limit = {'pitch': (-3, 3), 'yaw': (-3, 3)}
#axis_calibration = {'sway':[0, 0], 'surge':[0, 0], 'yaw':[0, 0], 'heave': [0, 0], 'pitch': [-1, 1]}
axis_calibration = {'sway': [-0.70983001190221873, 0.29410687582018497], 'yaw': [-0.73335978270821256, 0.39606921597949157], 
                    'pitch': [-1, 1], 'heave': [-0.87453840754417556, 0.37253944517349774], 
                    'surge': [-0.78041932432020023, 0.26273384807885986]}
center_calibration = {'sway': -0.21570482497634816, 'yaw': -0.18433179723502305, 
                      'heave': -0.82747886593218789, 'surge': -0.23923459578234199, 'pitch': 0}
last_update_time = {'yaw': 0, 'pitch': 0}
dead_zones = {'sway': 0.7, 'surge': 0.5, 'yaw': 0.55, 'heave': 0.1} # 0.x -> within x% of the calibrated center
dead_state = {}
axis_rects = {}

WINDOW_DIM = 320, 240
BAR_HEIGHT = 14

REPEAT_RATE = 10 # post additive events 10 times a second
DEAD_ZONE = 0.35 # dead zone is within 35% of the center value

def is_in_dead_zone(axis, value):
    if axis == 'pitch':
        return False
        
    x = abs(value-center_calibration[axis])/abs(center_calibration[axis]) <= dead_zones[axis]
    # if axis == 'sway':
    #     print axis, ':',x, ':', abs(value-center_calibration[axis])/abs(center_calibration[axis]), ' (',value, ', ', center_calibration[axis], ')'
    # if axis not in dead_state or dead_state[axis] != x:
    #     print axis, 'changed to ', str(x)
    #     dead_state[axis] = x
    return x
    
class PostAxisEvents(Thread):
    def __init__(self, joy):
        Thread.__init__(self)
        self.joy = joy
        self.keep_going = True
        
    def run(self):
        while self.keep_going:
            for x in axis_mapping:
                if is_additive[axis_mapping[x]]:
                    pygame.event.post(pygame.event.Event(pygame.JOYAXISMOTION, {'joy': 0, 'axis': x, 'value': self.joy.get_axis(x)}))
            
            sleep(1.0/REPEAT_RATE)

def handle_axis(axis, value, screen):
    if value <= center_calibration[axis]:
        calib_val = axis_calibration[axis][0]
    else:
        calib_val = axis_calibration[axis][1]

    v_scaled = (center_calibration[axis]-value)/abs(center_calibration[axis]-calib_val)
    
    # r = pygame.Rect(0,0, abs((270/2)*v_scaled),BAR_HEIGHT-3)
    #     
    #     if v_scaled >= 0:
    #         r.center = (40+(270/2)+abs((270/2)*v_scaled)/2, rev_axis_mapping[axis]*BAR_HEIGHT+3*(x+1))
    #     else:
    #         r.center = (40+(270/2)-abs((270/2)*v_scaled)/2, rev_axis_mapping[axis]*BAR_HEIGHT+3*(x+1))
    #     
    #     pygame.draw.rect(screen, color['blue'], r, 0)
    #     print "drawing thingy at: ", r
    
    if is_in_dead_zone(axis, value):
        if not is_additive[axis]:
            var_mapping[axis].set(0)
        return
    
    if is_additive[axis]:
        if v_scaled > 0:
            lim = additive_change_limit[axis][1]
        else:
            lim = additive_change_limit[axis][0]
            
        new_val = var_mapping[axis].get() + -1*v_scaled*abs(lim)
        
        if axis == 'yaw':
            new_val = Heading(new_val).degrees
    else:
        if v_scaled > 0:
            lim = var_limits[axis][1]
        else:
            lim = var_limits[axis][0]
            
        new_val = v_scaled*abs(lim)
        
        if axis == 'heave':
            new_val = abs(new_val)
        if axis == 'sway':
            new_val = -1*new_val
    
    if not is_additive[axis] or (time() - last_update_time[axis]) > 1.0/REPEAT_RATE:
        var_mapping[axis].set(new_val)
        
        if is_additive[axis]:
            last_update_time[axis] = time()

def handle_button(button):
    if button == 'trigger':
        print "PEW PEW PEW!!"
    
    soft_kill.set(1)

def handle_event(e, screen):
    if e.type == pygame.JOYAXISMOTION:
        axis = axis_mapping[e.dict['axis']]
        handle_axis(axis, e.dict['value'], screen)
    elif e.type == pygame.JOYBUTTONDOWN:
        if e.dict['button'] in button_mapping:
            handle_button(button_mapping[e.dict['button']])
    elif e.type == pygame.JOYHATMOTION:
        if e.dict['value'][1] != 0:
            handle_axis('pitch', e.dict['value'][1], screen)

def calibrate_axis(axis):
    while True:
        e = pygame.event.wait()
        if handle_calibration_event(axis, e):
            return
            
def handle_calibration_event(axis, e):
    if e.type == pygame.JOYAXISMOTION and axis == 'center':
        center_calibration[axis_mapping[e.dict['axis']]] = e.dict['value']
    elif e.type == pygame.JOYAXISMOTION and e.dict['axis'] == rev_axis_mapping[axis]:
        if e.dict['value'] < axis_calibration[axis][0]:
            axis_calibration[axis][0] = e.dict['value']
        elif e.dict['value'] > axis_calibration[axis][1]:
            axis_calibration[axis][1] = e.dict['value']
    elif e.type == pygame.JOYBUTTONDOWN:
        return True            
    
    return False
            
def calibrate_joystick():
    print "**Calibrating sway (x-axis)**"
    print "Move stick all the way to the left, then the right; push a button when finished"
    calibrate_axis('sway')
    print "**Calibrating surge (y-axis)**"
    print "Move stick all the way forward, then backwards; push a button when finished"
    calibrate_axis('surge')
    print "**Calibrating yaw (rotation)**"
    print "Rotate stick all the way to the left, then to the right; push a button when finished"
    calibrate_axis('yaw')
    print "**Calibrating heave (throttle)**"
    print "Move throttle all the way up, then all the way down; push a button when finished"
    calibrate_axis('heave')
    print "**Calibrating joystick center**"
    print "Move joystick along both axis for a short while, then leave it in rest position. \
           Push any button (except for the trigger) when finished."
    calibrate_axis('center')

def main():
    # initialize pygame
    #pygame.font.init()
    pygame.joystick.init()
    c = pygame.joystick.get_count()
    
    if c == 0:
        print "Please connect a joystick and run again."
        return
    elif c > 1:
        print "Multiple joysticks connected, using first joystick."
        
    joy = pygame.joystick.Joystick(0)
    joy.init()
    
    screen = pygame.display.set_mode((320, 240), 0, 8)
    #    font = pygame.font.Font(None, 18)
    #    pygame.display.set_caption('CUAUV joystick control')
    #    screen.fill(color['white'])
    
    # for x in axis_mapping:
    #         axis_rects[axis_mapping[x]] = pygame.draw.rect(screen, color['black'], pygame.Rect(40, x*BAR_HEIGHT+3*(x+1), 270, BAR_HEIGHT), 1)
    #         pygame.draw.rect(screen, color['red'], pygame.Rect(int(40+(270./2)-(270*DEAD_ZONE)/2), x*BAR_HEIGHT+3*(x+1), int(270*DEAD_ZONE), BAR_HEIGHT), 2)
    #         text = font.render(axis_mapping[x], False, color['black'])
    #         textpos = text.get_rect(left=0, top=x*BAR_HEIGHT+3*(x+1))
    #         screen.blit(text, textpos)
    
    # calibrate_joystick()
    #     print axis_calibration
    #     print center_calibration
    
    pae = None
    
    # main loop
    while True:
        try:
            e = pygame.event.wait()
        except KeyboardInterrupt:
            pae.keep_going = False
            return
        if not pae:
            pae = PostAxisEvents(joy)
            pae.start()
        if e.type == pygame.QUIT:
            pae.keep_going = False
            return
        handle_event(e, screen)
        
        #pygame.display.update()

if __name__ == '__main__':
    main()