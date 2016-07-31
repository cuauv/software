'''
This code is highly experimental. Do not use.


meh, this doesn't work as well as I had hoped.
'''

import string,cgi,time
from os import curdir, sep
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from time import sleep


class MyHandler(BaseHTTPRequestHandler):

    def do_GET(self):
  
        self.wfile.write("HTTP/1.0 200 OK\r\n")
        self.wfile.write("Server: mjpeg-streamer\r\n")
        self.wfile.write("Content-Type: multipart/x-mixed-replace;boundary=informs\r\n")
        self.wfile.write("Connection: keep-alive\r\n")
        self.wfile.write("--informs\r\n")
       

        self.wfile.write("\r\n")
        
        while True:
 
            f = open("/ramdisk/forward.jpg", 'r')
            img = f.read()

        
            self.wfile.write("--informs\r\n")
            self.wfile.write("Content-Type: image/jpeg\r\n")
            self.wfile.write("Content-Length: " + str(len(img)) + "\r\n")
            self.wfile.write("\r\n")
            
            self.wfile.write(img)

            f.close()
           
            del f

            self.wfile.write("\r\n")
            sleep(1.0 / 20)           
        
   
def main():
    try:
        server = HTTPServer(('', 1338), MyHandler)
        print 'started httpserver...'
        server.serve_forever()
    except KeyboardInterrupt:
        print '^C received, shutting down server'
        server.socket.close()

if __name__ == '__main__':
    main()

