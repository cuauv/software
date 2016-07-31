import sys
class HeadingDelta:
    def __init__(self, delta):
        assert(delta >= -180 and delta <= 180)
        self.delta = float(delta)
        
    def __abs__(self):
        return HeadingDelta(abs(self.delta))
        
    def __add__(self, other):
        # just for heading deltas
        if isinstance(other, HeadingDelta):
            ret = self.delta + other.delta
            if ret >= 180:
                ret = (360-ret)*-1
            if ret <= -180:
                ret = (360+ret)
            return HeadingDelta(ret)
            
        return NotImplemented
    
    def __mul__(self, other):
        assert(other <= 1 and other >= -1)
        return HeadingDelta(self.delta*other)
        
    def __repr__(self):
        return "<HeadingDelta %d degrees>" % (self.delta)
        
    def __cmp__(self, other):
        if not other: return False
        
        if isinstance(other, HeadingDelta):
            return cmp(self.delta, other.delta)
        elif isinstance(other, int):
            return cmp(self.delta, HeadingDelta(other))
        else:
            return False
    
    def __float__(self):
        return float(self.delta)
        
    def __int__(self):
        return int(self.delta)
        
    def __nonzero__(self):
        return self.delta != 0
        
class Heading:
    def __init__(self, val):
        if isinstance(val, Heading):
            self.degrees = val.degrees
        else:
            while val < 0:
                if val is None:
                    #XXX: Intermittent error occurs here, so let's debug it.
                    head_redBG = '\033[1;41m'
                    sys.stdout.write(head_redBG)
                    print head_redBG + "\n******************\nINTERMITTENT ERROR DETECTED! \n Please debug me! haedingToOutput is None, should be int.\n Python debugger is started. TALK TO TOM!\n********************" + "\033[1;m"
                    import pdb
                    pdb.set_trace()
                val += 360
                
            self.degrees = val % 360
        
    def __radd__(self, delta):
        return self.__add__(delta)
        
    def __add__(self, d):
        if isinstance(d, HeadingDelta):
            delta = d
        elif isinstance(d, Heading):
            return NotImplemented
        else:
            delta = HeadingDelta(d)
        
        self_delta = self.degrees + delta.delta
        
        if self_delta < 0:
            return Heading(self_delta + 360)
        elif self_delta < 360:
            return Heading(self_delta)
        elif self_delta >= 360:
            return Heading(self_delta % 360)
            
    def __sub__(self, other):
        if isinstance(other, Heading):
            t = (self.degrees - other.degrees) % 360
            if t > 180:
                new_heading = t - 360
            else:
                new_heading = t
                
            return HeadingDelta(new_heading * -1)
        else:
            d = HeadingDelta(other)
            return self.__add__(HeadingDelta(-1*d.delta))
                    
    def __float__(self):
        return float(self.degrees)
        
    def __int__(self):
        return int(self.degrees)
        
    def __repr__(self):
        return "<Heading %d degrees>" % (self.degrees)
        
    def __cmp__(self, other):
        if not other:
            return True
        assert "SCREW YOU"
            
