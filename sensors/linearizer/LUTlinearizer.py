linearized = file('/home/software/cuauv/software/sensors/linearizer/linear_heading_LUT').read().split("\n")
linearized = [float(x) for x in linearized if len(x) > 0]

def linearize(reading):
    m = linearized[int(reading)]
    M = linearized[(int(reading)+1)%360]
    d = (M - m)%360
    t = reading - int(reading)

    inverted = m + d*t

    return inverted
