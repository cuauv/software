# ASLAM Python Client

import zmq, json

import request

def vchain(d):
  if type(d) is dict:
    res = list(map(vchain, d.values()))
    return res[0] if len(res) == 1 else res
  elif type(d) is list:
    res = list(map(vchain, d))
    return res[0] if len(res) == 1 else res
  else:
    return d
  
class ASLAMException(Exception): pass

class Client:
  def __init__(self, namespace):
    self.namespace = namespace
    self.sock = zmq.Context().socket(zmq.REQ)
    self.sock.connect('tcp://127.0.0.1:6666')
   
  def run(self, req):
    req = json.dumps(request.withschema(self.namespace, req))
    self.sock.send(req)
    res = json.loads(self.sock.recv())
    if 'error' in res:
      raise ASLAMException(res['error'])
    else:
      return vchain(res)    
