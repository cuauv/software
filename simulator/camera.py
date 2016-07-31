'''
camera.py

Attaches Panda3D camera nodes to the vehicle and renders the camera views to textures. Sends the
images to vision via sockets.

To see the contents of the buffers that back the Cameras, set the boolean variable "show-buffers #t"
in the panda configuration file.

'''

# Python modules
import mmap
import struct
import sys

# Third-party modules
POSIX_IPC_LOADED = True
try:
    import posix_ipc
except ImportError, ie:
    POSIX_IPC_LOADED = False
    sys.stderr.write('''\033[93m
WARNING: The posix_ipc module could not be found. The simulator will still run,
but will not send imagery to visiond. For installation directions, see: \033[94m
https://cuauv.org/wiki/Software/Linux_Dependencies#Simulator_Dependencies
\033[0m\n''')

    
# Panda3D modules
import panda3d.core
from direct.task import Task
from panda3d.core import BitMask32
from panda3d.core import GraphicsEngine
from panda3d.core import NodePath
from panda3d.core import PNMImage
from panda3d.core import StringStream

# CUAUV modules
import shm

NUM_CHANNELS = 4

class Camera(panda3d.core.Camera):
    def __init__(self, name, lens):
        panda3d.core.Camera.__init__(self, name, lens)
        self.name = name
        self.path = NodePath(self)

        self.height, self.width = 512, 512
        self.dataSize = self.height * self.width * NUM_CHANNELS

        self.buffer = base.win.makeTextureBuffer(name + 'Buffer', self.height, self.width)

        # Set the sort order to a negative number so this buffer will be rendered before the
        # main window.
        self.buffer.setSort(-100)
        self.tex = self.buffer.getTexture()
        self.tex.setRenderToTexture(True)
        self.tex.makeRamImage()
        
        # Attach this camera to the buffer.
        base.makeCamera(self.buffer, useCamera=self.path)
        self.setCameraMask(BitMask32(2))

        # Make a PNMImage into which we'll copy screenshots from the buffer.
        self.image = PNMImage()
        self.stringStream = StringStream()
        self.gfxEngine = GraphicsEngine.getGlobalPtr()
        self.gsg = base.win.getGsg()

        self.currentFrame = 0

        # Only initialize IPC and the copy out task if the posix_ipc module
        # was loaded successfully.
        if POSIX_IPC_LOADED:
            self.initializeIpc()

    def initializeIpc(self):
        # This Struct gives the format for the 128-byte shared mem header.
        # The header contains the index of the current valid image (1 or 0),
        # the number of channels, width, height, and the offset in to the
        # shared memory file of each image.
        self.header = struct.Struct('HHHHII')
        
        # Create the shared memory and semaphore. The shared memory is large
        # enough to contain two RGBA images for double-buffering.
        memory = posix_ipc.SharedMemory('/'+self.name+'Shm', posix_ipc.O_CREAT,
                size=(self.header.size + 2 * self.dataSize))

        # Create semaphores. SemQ is a producer-consumer style semaphore. The
        # camera will release it each time a new image is ready.
        self.semQ = posix_ipc.Semaphore('/'+self.name+'SemQ',
                posix_ipc.O_CREAT, initial_value=0)

        self.mapfile = mmap.mmap(memory.fd, memory.size)
        memory.close_fd()
        self.mapfile.seek(0)

        self.mapfile.write(self.header.pack(
                0, NUM_CHANNELS, self.width, self.height,
                self.header.size, self.header.size + self.dataSize))
        
        # This task copies the rendered image from the GPU to shared memory. 
        self.copyOutTask = taskMgr.doMethodLater(0.075, self.copyOutTask, 
                self.name + 'CopyOutTask')

    def getPath(self):
        return self.path

    def copyOutTask(self, task):
        self.gfxEngine.extractTextureData(self.tex, self.gsg)
        ramImg = self.tex.getRamImage()

        self.mapfile.seek(self.header.size + self.currentFrame * self.dataSize)
        self.mapfile.write(ramImg.getData())

        # If no one has consumed the last image yet, eat it since we're about
        # to write over it.
        try:
            self.semQ.acquire(0)
        except posix_ipc.BusyError, e:
            pass
        
        # Update the image pointer.
        self.mapfile.seek(0)
        self.mapfile.write(self.header.pack(
                self.currentFrame, NUM_CHANNELS, self.width, self.height,
                self.header.size, self.header.size + self.dataSize))
        self.semQ.release()
        if self.currentFrame == 0:
            self.currentFrame = 1
        else:
            self.currentFrame = 0

        return Task.again

