'''
ASLAM Python Core API
'''

import zmq, json

from .expr import *
from .type import *
from .repr import *

from .function import *
from .model import *
from .parameter import *
from .request import *
from .action import *
from .observation import *

lit = lambda n: Expr(lite(n))
var = lambda n: Expr(vare(n)) 
sin = lambda e: Expr(sine(e.raw))
cos = lambda e: Expr(cose(e.raw))
tan = lambda e: Expr(tane(e.raw))
atan2 = lambda x, y: Expr(atan2e(x.raw, y.raw))
let = lambda n, b, e: Expr(lete(n, b.raw, e.raw))
vec = lambda es: Expr(vece(es))

class Expr:
  def __init__(self, raw):
    self.raw = raw

  def __add__(self, other): return Expr(adde(self.raw, other.raw))
  def __sub__(self, other): return Expr(sube(self.raw, other.raw))
  def __mul__(self, other): return Expr(mule(self.raw, other.raw))
  def __div__(self, other): return Expr(dive(self.raw, other.raw))
  def __mod__(self, other): return Expr(mode(self.raw, other.raw)) 
  def __pow__(self, other): return Expr(powe(self.raw, other.raw))
  def __neg__(self): return Expr(nege(self.raw))
  
  def __lt__(self, other): return Expr(lte(self.raw, other.raw))
  def __le__(self, other): return Expr(lee(self.raw, other.raw))
  def __eq__(self, other): return Expr(eqe(self.raw, other.raw))
  def __ge__(self, other): return Expr(gee(self.raw, other.raw))
  def __gt__(self, other): return Expr(gte(self.raw, other.raw))
 
  def __getitem__(self, key): return Expr(inde(self.raw, key))  

class Instance:
  def __init__(self, namespace, model = None):
    self.namespace, self.model = namespace, model
  def observe(self, func, params, val, unc):
    return run(request(self.model, self.namespace, observe(observation(func, params, val, unc))))
  def estimate(self, obj):
    return run(request(self.model, self.namespace, estimate(obj)))
  def sample(self, obj):
    return run(request(self.model, self.namespace, sample(obj)))

def run(request):
  c = zmq.Context()
  s = c.socket(zmq.REQ)
  s.connect('tcp://127.0.0.1:5555')
  raw = json.dumps(request).encode('ascii')
  # print(raw)
  s.send(raw)
  return json.loads(s.recv().decode('ascii'))
