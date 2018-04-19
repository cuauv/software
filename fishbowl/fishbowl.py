import socket
import struct

# See PROTOCOL.md for fishbowl protocol specification.

class Fishbowl:
  header = ">BI"
  OK = 0
  ERROR = 1
  COMPLETED = 255

  def connect(self, address="localhost", port=7772):
    self.sock = socket.socket()
    try:
      self.sock.connect((address, port))
    except ConnectionRefusedError:
      print("Could not connect. Is fishbowl running?")
      self.sock.close()
      return False

    return True

  def __enter__(self):
    return self

  def disconnect(self):
    self.sock.close()

  def __exit__(self, type, value, traceback):
    self.disconnect()

  def parse_error(self, data):
    code, msg_len = struct.unpack(">II", data[:8])
    return code, data[8:]

  def send_cmd(self, ID, spec, *args):
    # Header is type followed by body length
    self.sock.send(struct.pack(self.header + spec, ID,
                               struct.calcsize(">" + spec), *args))

    header_data = self.sock.recv(struct.calcsize(self.header))
    rtype, body_length = struct.unpack(self.header, header_data)

    body_data = self.sock.recv(body_length)

    # XXX: Here return messages can get out of sync. TODO
    if rtype != self.OK and rtype != self.COMPLETED:
      if rtype == self.ERROR:
        code, msg = self.parse_error(body_data)
        print("ERROR", code, msg.decode('utf-8'))
      else:
        print("Unexpected response type", type, body_data)

  def eset(self, entity_id, attribute_id, data_spec, *data):
    self.send_cmd(3, "IB" + data_spec, entity_id, attribute_id, *data)

  def eset_position(self, entity_id, x, y, z):
    self.eset(entity_id, 0, "ddd", x, y, z)

  def eset_velocity(self, entity_id, vx, vy, vz):
    self.eset(entity_id, 1, "ddd", vx, vy, vz)

  def eset_orientation(self, entity_id, q0, q1, q2, q3):
    self.eset(entity_id, 3, "dddd", q0, q1, q2, q3)

  def eset_angular_velocity(self, entity_id, w1, w2, w3):
    self.eset(entity_id, 4, "ddd", w1, w2, w3)

  def unpause(self, for_steps=0):
    self.send_cmd(254, "Q", for_steps)

  def pause(self):
    self.send_cmd(255, "")
