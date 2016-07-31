#!/usr/bin/env python2
#encoding: utf-8
#
# JB Rajsky  21 October, 2008
#
# A very simple mapping utility based on DVL data

#TODO: Add text and make the bin width dynamically adaptive so the sub doesn't leave the screen

import pygame, os, sys, getopt, time
from pygame.locals import *
from auval.shmem import SharedVar
from auval.remote_shmem import RemoteSharedVar
from auval.watcher import VarWatcher

if not pygame.mixer: "WARNING: Sound Disabled"

global screen, background, vehicle

def main():
    
    global screen, background, vehicle
    
    pyro = False
    width = 2.0 #width of a box is 2.0 meters
    block_width = 47
    
    #Let's see if we're using Pyro
    try:
        opts, args = getopt.getopt(sys.argv[1:], "p", ["pyro"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-p", "--pyro"):
            pyro = True
        else:
            raise RuntimeError("Unrecognized option: "+opt)
            
    if pyro:
        print "Pyro enabled"
        dmg_x = RemoteSharedVar('/sensors/dvl/dmg/x')
        dmg_y = RemoteSharedVar('/sensors/dvl/dmg/y')
    else:
        print "Pyro disabled"
        dmg_x = SharedVar('/sensors/dvl/dmg/x')
        dmg_y = SharedVar('/sensors/dvl/dmg/y')
    
    #Other setup
    x_offset = dmg_x.get()  #Original x location is not zero due to Peter's transformation
    y_offset = dmg_y.get()  #ditto fo y
    pygame.init()
    pygame.mouse.set_cursor((8, 8), (4, 4), (255, 195, 165, 153, 153, 165, 195, 255), (0, 60, 90, 102, 102, 90, 60, 0))  #Don't ask
    if pygame.mixer:
        pygame.mixer.pre_init(16000, -16, 1) #Hehehe...
        time.sleep(1.0)
        ping = pygame.mixer.Sound(os.path.join(os.path.split(sys.argv[0])[0], "graphics/ping.wav"))
        
    #Draw the screen/backround/etc.
    icon = pygame.image.load(os.path.join(os.path.split(sys.argv[0])[0],'graphics/icon.png'))
    #icon.convert()
    pygame.display.set_icon(icon)  #Custom icon!
    screen = pygame.display.set_mode((800, 600))
    pygame.display.set_caption('Triton Underwater Navigational Instantaneous Positioning System')
    
    background = pygame.image.load(os.path.join(os.path.split(sys.argv[0])[0],'graphics/sonar_board2.png'))
    background.convert()
    background_rect = background.get_rect()
    
    x_center = background_rect[2]/2-8#Dumb offset
    y_center = background_rect[3]/2-8#Dumb offset number 2
    
    update_display(x_center, y_center)
    #Get ready to watch some variables
    if not pyro:
        watcher = VarWatcher()
        watcher.watch_var(dmg_x)
        watcher.watch_var(dmg_y)
    
    #Let the fun begin
    t1 = time.time()
    t2 = time.time()
    while True:
        if not pyro:
            #Wait for position to change
            watcher.checkpoint()
            watcher.wait_for_change()
        else:
            time.sleep(0.1)
        #Displacement from initial position in vehicle coordinates
        real_disp_x = (int)((dmg_x.get() - x_offset)/100.0)
        real_disp_y = (int)((dmg_y.get() - y_offset)/100.0)
        
        #Displacement from initial position in Map coordinates
        disp_x = (int)(real_disp_x*block_width/width)
        disp_y = (int)(real_disp_y*block_width/width)
        
        #New location to display vehicle at
        new_x = (int)(x_center+disp_x)
        new_y = (int)(y_center-disp_y)
        
        #PING
        t2 = time.time()
        if pygame.mixer and t2 - t1 >= 10.0:
            pygame.mixer.Sound.play(ping)
            t1 = t2
        
        update_display(new_x, new_y)
            
def update_display(x, y):
    screen.blit(background, (0,0))
    pygame.draw.circle(screen, (0,255,0), (x,y),7,0)
    pygame.display.flip()
    
if __name__ == "__main__":
    main()
