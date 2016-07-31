"""
mission_state_viewer_client.py

Reads mission state from mission_state_viewer.py
so that it can draw the robot's 'thoughts' onto
the screen with a pygame window.

Run mission_state_viewer.py on the vehicle and
this on a local computer.

Modify HOST to be the proper host IP address."""
import socket
import time
import pygame
from pygame.locals import *

#Make the client socket
HOST = 'localhost' #Set this to the vehicle's address
PORT = 32212 #Arbitrary port assignment.
buffsize = 1024
clientsocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

#Make pygame window
pygame.init()
pygame.font.init()
screen = pygame.display.set_mode((500,160))
pygame.display.set_caption("Mission State Viewer")
pygame.mouse.set_visible(1)

background = pygame.Surface(screen.get_size())
background = background.convert()
screen.blit(background, (0,0))
pygame.display.flip()

font = pygame.font.Font(None, 36)

try:
    clientsocket.connect( (HOST, PORT) )
    print "Connected successfully to " + HOST

    running = True
    while running:
        #Read pygame events
        for event in pygame.event.get():
            if event.type == QUIT:
                running = False

        #Read from server
        data = clientsocket.recv(buffsize)
        
        background.fill((250,250,250))
        textpos = (0,0)
        #Write out text in separate lines
        for line in data.split("\n"):
            text = font.render(line, 1, (10,10,10))
            background.blit(text,textpos)
            textpos = (textpos[0], textpos[1]+40)

        #Draw to screen
        screen.blit(background, (0,0))
        pygame.display.flip()

finally:
    clientsocket.close()
