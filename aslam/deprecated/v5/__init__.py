# ASLAM Python Library

import nanomsg, json

sock = nanomsg.Socket(nanomsg.REQ)
sock.connect('tcp://127.0.0.1:8081')

def raw_run(req):
  sock.send(json.dumps(req))
  return json.loads(sock.recv().decode('ascii'))

def run(req):
  res = raw_run(req)
  key = list(res.keys())[0]
  res = res[key]
  if key == 'rok':
    return
  elif key == 'rerr':
    raise Exception('Remote raised: {}'.format(res))
  elif key == 'rexpr':
    return 
  else:
    raise Exception('Response not understood.')

def lift(val):
  if type(val) is float:
    return {'reale': val}
  else:
    raise Exception('Unliftable type.')

observe = lambda f, vs, e, u: run({'obscon': {'func': f, 'vars': vs, 'val': lift(e), 'uncertainty': u}})
