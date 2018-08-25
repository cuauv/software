#! /usr/bin/env python3
import zmq
import slam.proto.slam_msg_pb2 as slam_msg

class SlamClient:

    def __init__(self):
        self.ctx = zmq.Context()
        self.socket = self.ctx.socket(zmq.REQ)
        self.socket.connect("tcp://127.0.0.1:57411")

    def observe_landmark(self, obj_id, m_x, m_y, m_z, u_x=0, u_y=0, u_z=0, uncertainty=None):

        msg = slam_msg.SlamMsg()
        msg.id = obj_id
        msg.request = False
        msg.m_x = m_x
        msg.m_y = m_y
        msg.m_z = m_z
        if uncertainty is None:
            msg.u_x = u_x
            msg.u_y = u_y
            msg.u_z = u_z
        else:
            msg.u_x = uncertainty
            msg.u_y = uncertainty
            msg.u_z = uncertainty

        self.socket.send(msg.SerializeToString())
        msg = self.socket.recv()
        return msg == b'S'

    def request_landmark(self, obj_id):

        msg = slam_msg.SlamMsg()
        msg.id = obj_id
        msg.request = True

        self.socket.send(msg.SerializeToString())
        result = self.socket.recv()
        msg.ParseFromString(result)
        return ((msg.m_x, msg.m_y, msg.m_z), (msg.u_x, msg.u_y, msg.u_z))

       
    def request_position(self):

        msg = slam_msg.SlamMsg()
        msg.id = ""
        msg.request = True

        self.socket.send(msg.SerializeToString())
        result = self.socket.recv()
        msg.ParseFromString(result)
        return (msg.m_x, msg.m_y, msg.m_z)

    def close():
        self.ctx.destroy()
