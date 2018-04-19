import socket
import message_pb2
UDP_IP = "169.254.178.13"
UDP_PORT = 8898


print "UDP target IP:", UDP_IP
print "UDP target port", UDP_PORT

com =  message_pb2.Command()
com.PGAGainCommand.gain = int(raw_input("Enter gain (0,1,2,4,8,16,32,64)"))

print com

msg = com.SerializeToString()
print msg
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.sendto(msg,(UDP_IP,UDP_PORT))
