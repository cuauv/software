#>> ASLAM Py2.7/3.X Client Library

import nanomsg, enum
import ujson as json
# import json

# Type invariants should be enforced at the Python level.

class BuiltIn(enum.Enum):
  ADD = 'add'
  SUB = 'sub'
  DIV = 'div'
  MUL = 'mul'
  POW = 'pow'
  MOD = 'mod'
  NEG = 'neg'
  ABS = 'abs'
  GT = 'gt'
  GE = 'ge'
  EQ = 'eq'
  NE = 'ne'
  LE = 'le'
  LT = 'lt'
  SIN = 'sin'
  COS = 'cos'
  TAN = 'tan'
  ASIN = 'asin'
  ACOS = 'acos'
  ATAN = 'atan'
  ATAN2 = 'atan2'
  GAUSSIAN = 'gaussian'

class Backend(enum.Enum):
  INTERPRETED = 'interpreted'
  LLVMJIT = 'llvmjit'

class Resample(enum.Enum):
  TRANSFORM = []

class Wrapped:
  __slots__ = ['internal']

  def __init__(self, internal):
    assert type(internal) is dict
    self.internal = internal

  def __repr__(self):
    return 'Wrapped[{0}]<{1}>'.format(self.__class__, self.internal)

  def __str__(self):
    return repr(self)

class Type(Wrapped):
  @staticmethod
  def BOOL(): return Type({'boolt': []})
 
  @staticmethod 
  def INT(): return Type({'intt': []})

  @staticmethod
  def REAL(): return Type({'realt': []}) 

  @staticmethod
  def VEC(xs): 
    assert all(isinstance(x, Type) for x in xs)
    return Type({'vect': [x.internal for x in xs]})

  @staticmethod
  def OBJ(xs): 
    assert all(isinstance(xs[k], Type) for k in xs)
    return Type({'objt': {k: xs[k].internal for k in xs}})

class Eval(Wrapped):
  @staticmethod
  def ARGMAX(): return Eval({'argmax': []})
  
  @staticmethod
  def WEIGHTEDMEAN(): return Eval({'weightedmean': []})

  @staticmethod
  def PROB(e): 
    assert isinstance(e, Expr)
    return Eval({'prob': e.internal})

  @staticmethod
  def SAMPLE(n): 
    assert type(n) is int
    return Eval({'sample': n})

class Expr(Wrapped):
  @staticmethod
  def VAR(name): 
    assert type(name) is str
    return Expr({'vare': name})

  @staticmethod
  def APPLY(f, x):
    assert isinstance(f, Expr) and all(isinstance(y, Expr) for y in x)
    return Expr({'appe': [f.internal, [y.internal for y in x]]})

  @staticmethod
  def LET(x, y, z):
    assert type(x) is str and isinstance(y, Expr) and isinstance(z, Expr)
    return Expr({'lete': [x, y.internal, z.internal]})

  @staticmethod
  def IF(x, y, z):
    assert isinstance(x, Expr) and isinstance(y, Expr) and isinstance(z, Expr)
    return Expr({'ife': [x.internal, y.internal, z.internal]})

  @staticmethod
  def LAMBDA(x, y):
    assert all(type(v[0]) is str and isinstance(v[1], Type) for v in x) and isinstance(y, Expr)
    return Expr(internal = {'lame': [[(n, ty.internal) for (n, ty) in x], y.internal]})

  @staticmethod
  def LIFT(val):
    assert type(val) in (bool, int, float, str, list, dict) or isinstance(val, Expr) or isinstance(val, BuiltIn)
    if type(val) is bool:
      return Expr({'boole': val})
    elif type(val) is int:
      return Expr({'inte': val})
    elif type(val) is float:
      return Expr({'reale': val})
    elif type(val) is str:
      return Expr({'enume': val})
    elif type(val) is list:
      return Expr({'vece': [Expr.LIFT(x).internal for x in val]})
    elif type(val) is dict:
      return Expr({'obje': {k: Expr.LIFT(v).internal for (k, v) in val.items()}})
    elif isinstance(val, BuiltIn):
      return Expr({'builtine': val.value})
    elif isinstance(val, Expr):
      return val

  @staticmethod
  def UNLIFT(expr):
    key = list(expr.keys())[0]
    val = expr[key]
    if key in ('boole', 'inte', 'reale', 'enume'):
      return val
    if key == 'vece':
      return [Expr.UNLIFT(x) for x in val]
    if key == 'obje':
      return {k: Expr.UNLIFT(v) for (k, v) in val.items()}

  def __getitem__(self, key):
    assert type(key) in (str, int)
    if type(key) is str:
      return Expr({'meme': [self.internal, key]})
    elif type(key) is int:
      return Expr({'inde': [self.internal, key]})

  def __call__(self, vals):
    return Expr.APPLY(self, vals)

  def _dual(self, other, func):
    assert isinstance(func, BuiltIn)
    return Expr.APPLY(Expr.LIFT(func), [self, Expr.LIFT(other)])

  def __add__(self, other): return self._dual(other, BuiltIn.ADD)
  def __sub__(self, other): return self._dual(other, BuiltIn.SUB)
  def __mul__(self, other): return self._dual(other, BuiltIn.MUL)
  def __div__(self, other): return self._dual(other, BuiltIn.DIV)
  def __truediv__(self, other): return self._dual(other, BuiltIn.DIV)
  def __pow__(self, other): return self._dual(other, BuiltIn.POW)
  def __mod__(self, other): return self._dual(other, BuiltIn.MOD)
  def __neg__(self): return Expr.APPLY(Expr.LIFT(BuiltIn.NEG), [self])
  def __abs__(self): return Expr.APPLY(Expr.LIFT(BuiltIn.ABS), [self])

  def __gt__(self, other): return self._dual(other, BuiltIn.GT)
  def __ge__(self, other): return self._dual(other, BuiltIn.GE)
  def __eq__(self, other): return self._dual(other, BuiltIn.EQ)
  def __ne__(self, other): return self._dual(other, BuiltIn.NE)
  def __le__(self, other): return self._dual(other, BuiltIn.LE)
  def __lt__(self, other): return self._dual(other, BuiltIn.LT)

class Spec(Wrapped):
  @staticmethod
  def REALGRID(xs, n):
    assert all(type(x[0]) is float and type(x[1]) is float for x in xs) and type(n) is int
    return Spec({'realgrid': [xs, n]})

  @staticmethod
  def INTGRID(xs, n):
    assert all(type(x[0]) is int and type(x[1]) is int for x in xs) and type(n) is int
    return Spec({'intgrid': [xs, n]})

  @staticmethod
  def DIRECTPS(ps):
    assert isinstance(ps, PhaseSpace)
    return Spec({'directps': ps})

class Environment:
  __slots__ = ['_conn', '_ind', '_name']

  def __init__(self, name, addr = 'tcp://127.0.0.1:8080'):
    self._conn = nanomsg.Socket(nanomsg.REQ)
    self._conn.connect(addr)
    self._name = name

  def create(self, backend):
    assert isinstance(backend, Backend)
    return self._run({'mkenv': {'env': self._name, 'backend': backend.value}})
  
  def destroy(self):
    return self._run({'rmenv': {'env': self._name}}) 
  
  def con(self, name, expr):
    assert type(name) is str and isinstance(expr, Expr)
    return self._exec({'cond': {'var': name, 'expr': expr.internal}})

  def var(self, name, spec):
    assert type(name) is str and isinstance(spec, Spec)
    return self._exec({'vard': {'var': name, 'spec': spec.internal}})

  def eval(self, expr, etype):
    assert isinstance(expr, Expr) and isinstance(etype, Eval)
    return self._exec({'evald': {'expr': expr.internal, 'type': etype.internal}})

  def obs_con(self, expr, func, resample = None):
    assert isinstance(expr, Expr) and isinstance(func, Expr) and (resample is None or isinstance(resample, Resample))
    return self._exec({'obscond': {'expr': expr.internal, 'func': func.internal, 'resample': None if resample is None else resample.value}})

  def obs_del(self, expr, delt, uncer, resample = None):
    assert isinstance(expr, Expr) and isinstance(delt, Expr) and type(uncer) is float and (resample is None or isinstance(resample, Resample))
    return self._exec({'obsreld': {'expr': expr.internal, 'delt': delt.internal, 'uncer': uncer, 'resample': None if resample is None else resample.value}})

  def _exec(self, decl):
    return self._run({'execd': {'env': self._name, 'decl': decl}})

  def _run(self, cmd):
    self._conn.send(json.dumps(cmd))
    res = self._conn.recv()
    res = json.loads(res)
    key = list(res.keys())[0]
    val = res[key]
    if key == 'respsuccess':
      return
    if key == 'resperror':
      raise Exception('Remote raised error: {}'.format(val))
    if key == 'respexpr':
      return Expr.UNLIFT(val)
    if key == 'respphasespace':
      return val
